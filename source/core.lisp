;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Functions for starting, killing, and communication with the
;;;; electron process.

(in-package :electron)

(alexandria:define-constant +default-wayland-opts+ '("--enable-features=UseOzonePlatform" "--ozone-platform=wayland")
  :test #'equal
  :documentation "The default options to be provided to Electron on the wayland platform")

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
   (server-socket-path
    #P""
    :export t
    :reader t
    :writer nil
    :documentation "The path of the server socket.")
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
required to be registered there.")
   (launch-options
    '()
    :export t
    :accessor t
    :documentation "A list of options to pass to the Electron invocation"))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Interface with an Electron instance."))

(defmethod initialize-instance :after ((interface interface) &key)
  (setf (slot-value interface 'server-socket-path)
        (uiop:merge-pathnames* (sockets-directory interface) "electron.socket")))

(export-always 'server-running-p)
(defmethod server-running-p ((interface interface))
  "Whether the Electron server is listening."
  (iolib:with-open-socket (s :address-family :local
                             :remote-filename (uiop:native-namestring
                                               (server-socket-path interface)))
    (iolib:socket-connected-p s)))

(defun wayland-p ()
  "Whether we are running on Wayland.

We check if WAYLAND_DISPLAY is defined and non-empty, or XDG_SESSION_TYPE
is 'wayland', which are the recommended ways to detect Wayland."
  (or (and (uiop:getenv "WAYLAND_DISPLAY")
           (not (string= (uiop:getenv "WAYLAND_DISPLAY") "")))
      (string= (uiop:getenv "XDG_SESSION_TYPE") "wayland")))

(defun default-launch-options ()
  "Return default launch options based on the environment.

At the moment, we only need special options on Wayland"
  (if (wayland-p)
      +default-wayland-opts+
      '()))

(defmethod alive-p ((interface interface))
  "Whether the INTERFACE's Electron process is running."
  (with-slots (process) interface
    (and process (uiop:process-alive-p process))))

(defmethod (setf protocols) (value (interface interface))
  (if (alive-p interface)
      (error "Protocols need to be set before launching ~a." interface)
      (with-slots (protocols server-path) interface
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
  (handler-case (server-running-p interface)
    (iolib/syscalls:enoent () nil)
    (iolib/syscalls:econnrefused ()
      (warn "Sanitizing ~a before launch."
            (server-socket-path interface))
      (uiop:delete-file-if-exists (server-socket-path interface)))
    (:no-error (_)
      (declare (ignore _))
      (warn "Connection at ~a already established, nothing to do."
            (server-socket-path interface))
      (return-from launch nil)))
  (let* ((appdir (uiop:getenv-pathname "APPDIR" :ensure-directory t))
         (executable-command
           (if appdir
               (list "cl-electron-server")
               (list "npm" "run" "start" "--")))
         (execution-directory
           (if appdir
               nil
               (asdf:system-source-directory :cl-electron)))
         (execution-launch-opts
           (or (launch-options interface)
               (default-launch-options))))
    (setf (process interface)
          (uiop:launch-program
           (append executable-command
                   execution-launch-opts
                   (list (uiop:native-namestring (server-path interface))
                         (uiop:native-namestring (server-socket-path interface))
                         (register (protocols interface))))
           :output :interactive
           :directory execution-directory
           :error-output :interactive)))
  ;; Block until the server is listening.
  (loop until (handler-case (server-running-p interface)
                (iolib/syscalls:enoent () (sleep 0.1)))
        finally (return t)))

(defun create-socket-path (&key (interface *interface*) (id (new-id)))
  "Generate a new path suitable for a socket."
  (uiop:merge-pathnames* (sockets-directory interface) (format nil "~a.socket" id)))

(defun create-socket (callback &key ready-semaphore (path (create-socket-path)))
  (unwind-protect
       (handler-case
           (iolib:with-open-socket (s :address-family :local
                                      :connect :passive
                                      :local-filename path)
             (isys:chmod path #o600)
             (when ready-semaphore (bt:signal-semaphore ready-semaphore))
             (iolib:with-accept-connection (connection s)
               (loop for message = (ignore-errors (read-line connection nil))
                     for decoded-object = (cl-json:decode-json-from-string message)
                     for callback-result = (funcall callback decoded-object)
                     when (stringp callback-result)
                       do (write-line (concatenate 'string callback-result "")
                                      connection)
                          (finish-output connection))))
         (iolib/syscalls:eaddrinuse () (warn "~a already in use." path)))
    (uiop:delete-file-if-exists path)))

(defun create-socket-thread (callback &key ready-semaphore (interface *interface*))
  (let* ((id (new-id))
         (socket-path (uiop:native-namestring (create-socket-path :id id)))
         (socket-thread (bt:make-thread
                         (lambda ()
                           (create-socket callback
                                          :path socket-path
                                          :ready-semaphore ready-semaphore))
                         :name (format nil "cl-electron-~a" id))))
    (push socket-thread (socket-threads interface))
    (values id socket-thread socket-path)))

(defun create-node-socket-thread (callback &key (interface *interface*))
  (let ((socket-ready-semaphore (bt:make-semaphore)))
    (multiple-value-bind (thread-id socket-thread socket-path)
        (create-socket-thread callback
                              :ready-semaphore socket-ready-semaphore)
      (bt:wait-on-semaphore socket-ready-semaphore)
      (message
       interface
       (format nil "~a = new nodejs_net.connect('~a');" thread-id socket-path))
      (values thread-id socket-thread socket-path))))

(defun create-node-synchronous-socket-thread (callback &key (interface *interface*))
  "Caution: SynchronousSocket blocks Node.js and can lead to deadlocks."
  (let ((socket-ready-semaphore (bt:make-semaphore)))
    (multiple-value-bind (thread-id socket-thread socket-path)
        (create-socket-thread callback
                              :ready-semaphore socket-ready-semaphore)
      (bt:wait-on-semaphore socket-ready-semaphore)
      (message
       interface
       (format nil "~a = new SynchronousSocket.SynchronousSocket('~a');"
               thread-id socket-path))
      (message interface (format nil "~a.connect();" thread-id))
      (values thread-id socket-thread socket-path))))

(defun destroy-thread* (thread)
  "Like `bt:destroy-thread' but does not raise an error.
Particularly useful to avoid errors on already terminated threads."
  (ignore-errors (bt:destroy-thread thread)))

(export-always 'terminate)
(defun terminate (&optional (interface *interface*))
  (when (or (not interface) (not (alive-p interface)))
    (warn "Already terminated, nothing to do.")
    (return-from terminate nil))
  (mapcar #'destroy-thread* (socket-threads interface))
  (uiop:terminate-process (process interface))
  (setf (process interface) nil)
  t)

(defun new-id ()
  "Generate a new unique ID."
  (symbol-name (gensym "ID")))

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
    (write-line "" s)
    (finish-output s)
    (read-line s)))

(defmethod message ((remote-object remote-object) message-contents)
  (message (interface remote-object) message-contents))

(define-class view (remote-object)
  ((web-contents
    nil
    :export t
    :reader nil
    :writer t
    :type (or web-contents null)
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
  (:documentation "Embed additional web content into a `window'.
It is like a child window, except that it is positioned relative to its owning
window."))

(define-class window (remote-object)
  ((web-contents
    nil
    :export t
    :reader nil
    :writer t
    :type (or web-contents null)
    :documentation "The `web-contents' object bound to the window.")
   (options
    "{autoHideMenuBar: true}"
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "A string that specifies the window's behavior.")
   (views
    nil
    :export t
    :type (or list-of-views null)
    :documentation "A list of `view's bound to window."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Create and control browser windows."))

(defun list-of-views-p (views)
  "Return non-nil when LIST is non-nil and elements are of type `view'."
  (and (consp views) (every #'viewp views)))

(deftype list-of-views ()
  '(and list (satisfies list-of-views-p)))

(define-class web-contents (remote-object)
  ((session
    nil
    :export t
    :reader nil
    :writer t
    :type (or session null)
    :documentation "The `session' object bound to `web-contents'."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "It is responsible for rendering and controlling a web page
(via events)."))

(define-class session (remote-object)
  ((download-items (make-hash-table :test 'equal)))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Manage browser sessions, cookies, cache, proxy settings, etc."))

(define-class extensions (remote-object)
  ()
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Manage browser extensions."))

(define-class extension (remote-object)
  ()
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Manage a single browser extension."))

(define-class download-item (remote-object)
  ((url "")
   (state "")
   (received-bytes 0)
   (total-bytes 0)
   (percent-complete 0)
   (save-path ""))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Represents a download item."))

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
  (declare (type (or list-of-protocols null) protocols))
  (if protocols
      (format nil "protocol.registerSchemesAsPrivileged([~{{scheme:'~a',privileges:~a}~^, ~}]);"
              (loop for protocol in protocols
                    collect (scheme-name protocol)
                    collect (privileges protocol)))
      ""))

(export-always 'format-listener)
(defun format-listener (object event-name callback-string &key once-p)
  "Encoding helper for `add-listener'."
  (format nil "~a.~a('~(~a~)', ~a)"
          (remote-symbol object)
          (if once-p "once" "on")
          event-name
          callback-string))

(export-always 'add-listener)
(defgeneric add-listener (object event-name callback &key once-p)
  (:method (object (event-name symbol) (callback function) &key once-p)
    (multiple-value-bind (thread-id socket-thread socket-path)
        (create-node-socket-thread (lambda (_) (declare (ignore _))
                                     (funcall callback object))
                                   :interface (interface object))
      (declare (ignore socket-path))
      (push socket-thread (socket-threads object))
      (message
       object
       (format-listener object
                        event-name
                        ;; Send dummy JSON data to trigger the callback.
                        (format nil "() => {~a.write(`${JSON.stringify('')}\\n`)}"
                                thread-id)
                        :once-p once-p))
      socket-thread))
  (:documentation "Register CALLBACK for OBJECT on event EVENT-NAME.

Since the argument signature of callbacks differs depending on the event,
methods must specialize on EVENT-NAME.  The general implementation assumes a
callback that takes OBJECT as its sole argument.

The callback is added to the end of the listeners array, without checking
whether it has already been added.  Callbacks are invoked in the order that they
were added.  When ONCE-P is non-nil, the callback runs once."))

(defmethod add-listener ((object remote-object)
                         (event (eql :before-input-event))
                         (callback function)
                         &key once-p)
  (declare (ignore once-p))
  (multiple-value-bind (thread-id socket-thread socket-path)
      (create-node-synchronous-socket-thread
       (lambda (input)
         (cl-json:encode-json-to-string
          (list (cons "preventDefault"
                      (apply callback (cons object
                                            input))))))
       :interface (interface object))
    (declare (ignore socket-path))
    (push socket-thread (socket-threads object))
    (message
     object
     (format-listener (if (web-contents-p object) object (web-contents object))
                      event
                      (format nil
                              "(event, input) => {
                                 ~a.write(JSON.stringify([ input ]) + '\\\n');
                                 response = ~a.read();
                                 if (JSON.parse(response.toString()).preventDefault) {
                                   event.preventDefault();
                                 }
                               }"
                              thread-id
                              thread-id)))))
