;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Functions for starting, killing, and communication with the
;;;; electron process.

(in-package :electron)

(define-class interface ()
  ((sockets-directory
    #-darwin
    (ensure-directories-exist (uiop:xdg-runtime-dir "cl-electron/") :mode #o700)
    #+darwin
    (ensure-directories-exist (pathname "~/Library/Caches/TemporaryItems/cl-electron/")
                              :mode #o700)
    :export t
    :reader t
    :writer nil
    :documentation "The directory where sockets are stored.")
   (server-socket-name
    "electron.socket"
    :export t
    :documentation "The name of the server socket.
See `server-socket-path'.")
   (socket-threads
    '()
    :documentation "A list of threads connected to sockets used by the system.")
   (process
    nil
    :documentation "The Electron process.")
   (protocols
    nil
    ;; The slot can't be set at initialization since protocol objects inherit
    ;; from remote-object, whose `interface' slot is only set after an object of
    ;; class `interface' has been initialized.
    :initarg nil
    :export t
    :reader t
    :writer nil
    :type (or list-of-protocols null)
    :documentation "A list of custom schemes (protocols).
The slot can only be set before invoking `launch'.")
   (server-path
    (asdf:system-relative-pathname :cl-electron "source/server.js")
    :export t
    :reader t
    :writer nil
    :type pathname
    :documentation "The path to a JavaScript file that specifies the IPC mechanism.

All of its content is evaluated before the app signals the ready event.  Not
meant to be overwritten but rather appended.  For instance, `protocols' are
required to be registered there."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Interface with an Electron instance."))

(export-always 'server-socket-path)
(defmethod server-socket-path ((interface interface))
  "The Electron process listens to this socket to execute JavaScript.
For each instruction it writes the result back to it."
  (with-slots (sockets-directory server-socket-name) interface
    (uiop:merge-pathnames* sockets-directory server-socket-name)))

(defmethod alive-p ((interface interface))
  "Whether the INTERFACE's Electron process is running."
  (with-slots (process) interface
    (and process (uiop:process-alive-p process))))

(defun to-tmp-file (pathname &optional s)
  "Return the pathname of tmp file featuring the concatenation of file PATHNAME and string S."
  (uiop:with-temporary-file (:pathname p :keep t :type "js")
    (uiop:copy-file pathname p)
    (when s
      (with-open-file (f p :direction :output :if-exists :append) (write-sequence s f)))
    p))

(defmethod (setf protocols) (value (interface interface))
  (if (alive-p interface)
      (error "Protocols need to be set before launching ~a." interface)
      (with-slots (protocols server-path) interface
        (setf server-path (to-tmp-file server-path (register value)))
        (setf protocols value))))

(defmethod interface-equal ((interface1 interface) (interface2 interface))
  "Return non-nil when interfaces are equal."
  (let ((process1 (process interface1))
        (process2 (process interface2)))
    (when (and process1 process2)
      (= (uiop:process-info-pid process1)
         (uiop:process-info-pid process2)))))

(export-always '*interface*)
(defvar *interface* nil)

(export-always 'launch)
(defun launch (&optional (interface *interface*))
  (cond ((alive-p interface)
         (restart-case (error (make-condition 'duplicate-interface-error))
           (kill ()
             :report "Kill the existing interface and start a new one."
             (terminate interface))))
        (t
         (when (uiop:file-exists-p (server-socket-path interface))
           (restart-case (error (make-condition 'socket-exists-error))
             (destroy ()
               :report "Destroy the existing socket."
               (uiop:delete-file-if-exists (server-socket-path interface)))))
         (setf (process interface)
               (uiop:launch-program `("npm" "start" "--"
                                      ,@(mapcar #'uiop:native-namestring
                                                (list (server-path interface)
                                                      (server-socket-path interface))))
                                    :output :interactive
                                    :directory (asdf:system-source-directory :cl-electron)))
         ;; Block until the socket is ready and responding with evaluated code.
         (loop for probe = (ignore-errors (message interface "0"))
               until (equalp "0" probe)))))

(defun create-socket-path (&key (interface *interface*) (id (new-integer-id)))
  "Generate a new path suitable for a socket."
  (uiop:merge-pathnames* (sockets-directory interface) (format nil "~a.socket" id)))

(defun create-socket (callback &key ready-semaphore
                                    (path (create-socket-path))
                                    (loop-connect-p t))
  (unwind-protect
       (iolib:with-open-socket (s :address-family :local
                                  :connect :passive
                                  :local-filename path)
         (isys:chmod path #o600)
         (when ready-semaphore (bt:signal-semaphore ready-semaphore))
         (loop do (iolib:with-accept-connection (connection s)
                    (loop for expr = (read-line connection nil)
                          until (null expr)
                          do (unless (uiop:emptyp expr)
                               (let* ((decoded-object (cl-json:decode-json-from-string expr))
                                      (callback-result (funcall callback decoded-object)))
                                 (when (stringp callback-result)
                                   (write-line callback-result connection)
                                   (write-line "" connection)
                                   (finish-output connection))))))
               while loop-connect-p))
    (uiop:delete-file-if-exists path)))

(defun create-socket-thread (callback &key ready-semaphore
                                           (interface *interface*)
                                           (loop-connect-p t))
  (let* ((id (new-id))
         (socket-path (uiop:native-namestring (create-socket-path :id id)))
         (socket-thread (bt:make-thread
                         (lambda ()
                           (create-socket callback
                                          :path socket-path
                                          :ready-semaphore ready-semaphore
                                          :loop-connect-p loop-connect-p)))))
    (push socket-thread (socket-threads interface))
    (values id socket-thread socket-path)))

(defun create-node-socket-thread (callback &key (interface *interface*)
                                                (loop-connect-p nil))
  (let ((socket-ready-semaphore (bt:make-semaphore)))
    (multiple-value-bind (thread-id socket-thread socket-path)
        (create-socket-thread callback
                              :ready-semaphore socket-ready-semaphore
                              :loop-connect-p loop-connect-p)
      (bt:wait-on-semaphore socket-ready-semaphore)
      (message
       interface
       (format nil "~a = new nodejs_net.Socket().connect('~a', () => { ~a.setNoDelay(true); });"
               thread-id socket-path thread-id))
      (values thread-id socket-thread socket-path))))

(defun create-node-synchronous-socket-thread (callback &key (interface *interface*)
                                                       (loop-connect-p nil))
  "Caution: SynchronousSocket blocks Node.js and can lead to deadlocks."
  (let ((socket-ready-semaphore (bt:make-semaphore)))
    (multiple-value-bind (thread-id socket-thread socket-path)
        (create-socket-thread
         callback
         :ready-semaphore socket-ready-semaphore
         :loop-connect-p loop-connect-p)
      (bt:wait-on-semaphore socket-ready-semaphore)
      (message
       interface
       (format nil "~a = new SynchronousSocket.SynchronousSocket('~a');"
               thread-id socket-path))
      (message interface (format nil "~a.connect();" thread-id))
      (values thread-id socket-thread socket-path))))

(export-always 'terminate)
(defun terminate (&optional (interface *interface*))
  (when (and (process interface) (uiop:process-alive-p (process interface)))
    (mapcar #'bt:destroy-thread (socket-threads interface))
    ;; `uiop:terminate-process' sends an async signal to delete the socket,
    ;; meaning that is may persistent for a while. It is safer to delete it
    ;; right away, otherwise chaining `terminate' and `launch' could raise
    ;; `socket-exists-error'.
    (uiop:delete-file-if-exists (server-socket-path interface))
    (uiop:terminate-process (process interface))
    (setf (process interface) nil)))

(defun new-id ()
  "Generate a new unique ID."
  (symbol-name (gensym "ID")))

(defun new-integer-id ()
  "Generate a new unique ID."
  (parse-integer (symbol-name (gensym ""))))

(define-class remote-object ()
  ((remote-symbol
    (new-id)
    :export t
    :reader t
    :writer nil
    :documentation "The internal variable name in the running `process'.")
   (interface
    *interface*
    :reader t
    :writer nil
    :type interface
    :documentation "The Electron `interface' the object will use for its whole lifetime.")
   (socket-threads
    '()
    :export t
    :documentation "A list of threads connected to sockets used by this object."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Represent objects living in Electron."))

(defmethod message ((interface interface) message-contents)
  (iolib:with-open-socket (s :address-family :local
                             :remote-filename (uiop:native-namestring
                                               (server-socket-path interface)))
    (write-line message-contents s)
    (finish-output s)
    (read-line s)))

(defmethod message ((remote-object remote-object) message-contents)
  (message (interface remote-object) message-contents))

(define-class browser-view (remote-object)
  ((web-contents
    nil
    :export t
    :reader nil
    :writer t
    :type web-contents
    :documentation "The `web-contents' object bound to the view.")
   (options
    ""
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "A string that specifies the views's behavior."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Embed additional web content into a `browser-window'.
It is like a child window, except that it is positioned relative to its owning
window."))

(define-class browser-window (remote-object)
  ((web-contents
    nil
    :export t
    :reader nil
    :writer t
    :type web-contents
    :documentation "The `web-contents' object bound to the window.")
   (options
    "{autoHideMenuBar: true}"
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "A string that specifies the window's behavior."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Create and control browser windows."))

(define-class web-contents (remote-object)
  ()
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "It is responsible for rendering and controlling a web page
(via events)."))

(define-class protocol (remote-object)
  ((scheme-name
    ""
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "Custom scheme name to handle.
HTTPS is an example of a scheme.")
   (privileges
    "{standard:true,secure:true,supportFetchAPI:true}"
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "A string that specifies the scheme's privileges.
See https://www.electronjs.org/docs/latest/api/structures/custom-scheme."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Define custom protocols and intercept existing protocol requests."))

(defun list-of-protocols-p (list)
  "Return non-nil when LIST is non-nil and elements are of type `protocol'."
  (and (consp list) (every #'protocolp list)))

(deftype list-of-protocols ()
  '(and list (satisfies list-of-protocols-p)))

(defun register (protocols)
  "Internal function, see the SETF method of `protocols' for the user-facing API."
  (declare (type list-of-protocols protocols))
  (format nil "protocol.registerSchemesAsPrivileged([~{{scheme:'~a',privileges:~a}~^, ~}]);"
          (loop for protocol in protocols
                collect (scheme-name protocol)
                collect (privileges protocol))))
