;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Functions for starting, killing, and communication with the
;;;; electron process.

(in-package :electron)

(define-class interface ()
  ((socket-directory
    #-darwin
    (ensure-directories-exist (uiop:xdg-runtime-dir "cl-electron/"))
    #+darwin
    (ensure-directories-exist (pathname "~/Library/Caches/TemporaryItems/cl-electron/"))
    :export t
    :documentation "The directory where sockets are stored.")
   (electron-socket-path
    #-darwin
    (uiop:xdg-runtime-dir "cl-electron/electron.socket")
    #+darwin
    (pathname "~/Library/Caches/TemporaryItems/cl-electron/electron.socket")
    :export t
    :documentation "The Electron process listens to this socket to execute
JavaScript.
For each instruction it writes the result back to this socket.")
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
    ;; class interface has been initialized.
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
required to be registered there.")
   (query-path
    (asdf:system-relative-pathname :cl-electron "source/query.js")
    :export t
    :reader t
    :writer nil
    :type pathname
    :documentation "The path to a JavaScript file that is used for synchronous IPC."))
  (:export-class-name-p t)
  (:predicate-name-transformer 'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "Interface with an Electron instance."))

(defmethod alive-p ((interface interface))
  "Whether the INTERFACE's Electron process is running."
  (with-slots (process) interface
    (and process (uiop:process-alive-p process))))

(defun to-tmp-file (pathname &optional s)
  "Return the pathname of tmp file featuring the concatenation of file PATHNAME and string S."
  (uiop:with-temporary-file (:pathname p :keep t :type "js")
    (uiop:copy-file pathname p)
    (str:to-file p s :if-exists :append)
    p))

(defmethod (setf protocols) (value (interface interface))
  (if (alive-p interface)
      (error "Protocols need to be set before launching ~a." interface)
      (with-slots (protocols server-path query-path) interface
        (setf protocols value)
        (setf server-path (to-tmp-file server-path (register protocols)))
        (setf query-path (to-tmp-file query-path)))))

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
  (unless (alive-p interface)
    (setf (process interface)
          (uiop:launch-program `("electron"
                                 ,@(mapcar #'uiop:native-namestring
                                           (list (server-path interface)
                                                 (electron-socket-path interface))))
                               :output :interactive))
    ;; Block until the socket is ready and responding with evaluated code.
    (loop for probe = (ignore-errors (send-message-interface interface "0"))
          until (equalp "0" probe))))

(defun create-socket-path (&key (prefix "cl-electron") (id (new-integer-id)))
  "Generate a new path suitable for a socket."
  #-darwin
  (uiop:native-namestring (uiop:xdg-runtime-dir (format nil "~a/~a.socket" prefix id)))
  #+darwin
  (uiop:native-namestring
   (pathname (format nil "~~/Library/Caches/TemporaryItems/~a/~a.socket" prefix id))))

(defun create-socket (callback &key ready-semaphore (path (create-socket-path)))
  (unwind-protect
       (iolib:with-open-socket (s :address-family :local
                                  :connect :passive
                                  :local-filename path)
         (setf (iolib/os:file-permissions path)
               (set-difference (iolib/os:file-permissions path)
                               '(:group-read :group-write :group-exec
                                 :other-read :other-write :other-exec)))
         (when ready-semaphore (bt:signal-semaphore ready-semaphore))
         (iolib:with-accept-connection (connection s)
           (loop for expr = (read-line connection nil)
                 until (null expr)
                 do (unless (uiop:emptyp expr)
                      (let* ((decoded-object (cl-json:decode-json-from-string expr))
                             (callback-result (funcall callback decoded-object)))
                        (when (stringp callback-result)
                          (write-line callback-result connection)
                          (write-line "" connection)
                          (finish-output connection)))))))
    (uiop:delete-file-if-exists path)))

(defun create-socket-thread (callback &key ready-semaphore (interface *interface*))
  (let* ((id (new-id))
         (socket-path (uiop:native-namestring (create-socket-path :id id)))
         (socket-thread (bt:make-thread
                         (lambda ()
                           (create-socket callback
                                          :path socket-path
                                          :ready-semaphore ready-semaphore)))))
    (push socket-thread (socket-threads interface))
    (values id socket-thread socket-path)))

(defun create-node-socket-thread (callback &key (interface *interface*))
  (let ((socket-ready-semaphore (bt:make-semaphore)))
    (multiple-value-bind (thread-id socket-thread socket-path)
        (create-socket-thread callback :ready-semaphore socket-ready-semaphore)
      (bt:wait-on-semaphore socket-ready-semaphore)
      (send-message-interface
       interface
       (format nil "~a = new nodejs_net.Socket().connect('~a', () => { ~a.setNoDelay(true); });"
               thread-id socket-path thread-id))
      (values thread-id socket-thread socket-path))))

(export-always 'terminate)
(defun terminate (&optional (interface *interface*))
  (when (and (process interface) (uiop:process-alive-p (process interface)))
    (mapcar #'bt:destroy-thread (socket-threads interface))
    (uiop:terminate-process (process interface))
    (setf (process interface) nil)))

(defun send-message-interface (interface message)
  (iolib:with-open-socket (s :address-family :local
                             :remote-filename (uiop:native-namestring
                                               (electron-socket-path interface)))

    (write-line message s)
    (finish-output s)
    (read-line s)))

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
  (:documentation "Represent objects living in Electron."))


(define-class browser-view (remote-object)
  ()
  (:export-class-name-p t))

(define-class browser-window (remote-object)
  ()
  (:export-class-name-p t))

(define-class web-contents (remote-object)
  ()
  (:export-class-name-p t))

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
  (:export-predicate-name-p t))

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
