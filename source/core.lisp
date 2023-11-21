;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Functions for starting, killing, and communication with the
;;;; electron process.

(in-package :electron)

(define-class interface ()
  ((electron-socket-path
    (uiop:xdg-runtime-dir "electron.socket")
    :export t
    :documentation "The Electron process listens to this sockets to execute
JavaScript.
For each instruction it writes the result back to this socket.")
   (lisp-socket-path
    (uiop:xdg-runtime-dir "lisp.socket")
    :export t
    :documentation "The Electron process may be instructed to write to this
socket to execute Lisp side effects before returning the end result.")
   (lisp-socket-lock
    (bt:make-semaphore :name "electron-lisp-lock")
    :documentation "Synchronize the Electron process creation with the `listener' creation.")
   (process
    nil
    :documentation "The Electron process.")
   (listener
    nil
    :documentation "The thread that listens to `lisp-socket-path'.")
   (callbacks
    (make-hash-table)
    :documentation "Callbacks to execute when the `listener' reads an instruction on the `lisp-socket-path'.")
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
    :documentation "The path to a JS file that specifies the IPC mechanism.

All of its content is evaluated before the app signals the ready event.  Not
meant to be overwritten but rather appended.  For instance, `protocols' are
required to be registered there."))
  (:export-class-name-p t)
  (:predicate-name-transformer 'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "Interface with an Electron instance."))

(defmethod alive-p ((interface interface))
  "Whether the INTERFACE's Electron process is running."
  (with-slots (process) interface
    (and process (uiop:process-alive-p process))))

(defun to-tmp-file (pathname s)
  "Return the pathname of tmp file featuring the concatenation of file PATHNAME and string S."
  (uiop:with-temporary-file (:pathname p :keep t)
    (uiop:copy-file pathname p)
    (str:to-file p s :if-exists :append)
    (uiop:native-namestring p)))

(defmethod (setf protocols) (value (interface interface))
  (if (alive-p interface)
      (error "Protocols need to be set before launching ~a." interface)
      (with-slots (protocols server-path) interface
        (setf protocols value)
        (setf server-path (to-tmp-file server-path (register protocols))))))

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
  (setf (listener interface) (bordeaux-threads:make-thread
                              (lambda () (create-server interface))))
  (unless (alive-p interface)
    (bt:wait-on-semaphore (lisp-socket-lock interface))
    (setf (process interface)
          (uiop:launch-program `("electron"
                                 ,@(mapcar #'uiop:native-namestring
                                           (list (server-path interface)
                                                 (electron-socket-path interface)
                                                 (lisp-socket-path interface))))
                               :output :interactive))))

(defun create-server (&optional (interface *interface*))
  (unwind-protect
       (let ((native-socket-path (uiop:native-namestring (lisp-socket-path interface))))
         (iolib:with-open-socket (s :address-family :local
                                    :connect :passive
                                    :local-filename native-socket-path)
           ;; We don't want group members or others to flood the socket or, worse,
           ;; execute code.
           (setf (iolib/os:file-permissions native-socket-path)
                 (set-difference (iolib/os:file-permissions native-socket-path)
                                 '(:group-read :group-write :group-exec
                                   :other-read :other-write :other-exec)))
           (bt:signal-semaphore (lisp-socket-lock interface))
           (iolib:with-accept-connection (connection s)
             (loop for expr = (read-line connection nil)
                   until (null expr)
                   do (unless (uiop:emptyp expr)
                        (let ((dispatch-result (dispatch-callback expr interface)))
                          (when (stringp dispatch-result)
                            (write-line dispatch-result connection)
                            (finish-output connection))))))))
    (uiop:delete-file-if-exists (lisp-socket-path interface))))

(defun dispatch-callback (json-string &optional (interface *interface*))
  "Handle the callback."
  (let* ((decoded-object (cl-json:decode-json-from-string json-string))
         (callback-id (cdar decoded-object))
         (callback (gethash callback-id (callbacks interface)))
         (arguments-list (cdadr decoded-object)))
    (funcall callback (list arguments-list))))

(export-always 'terminate)
(defun terminate (&optional (interface *interface*))
  (when (and (process interface) (uiop:process-alive-p (process interface)))
    (uiop:terminate-process (process interface))
    (setf (process interface) nil))
  (when (and (lisp-socket-path interface) (bt:thread-alive-p (listener interface)))
    (bt:destroy-thread (listener interface))
    (setf (listener interface) nil)
    (uiop:delete-file-if-exists (lisp-socket-path interface))))

(defun send-message-interface (interface message &key (replace-newlines-p t))
  ;; The Lisp reader consumes the backslashes when writing, we add
  ;; more to create properly formed JavaScript.
  (let ((message (if replace-newlines-p
                     (str:replace-all "\\n" "\\\\n" message)
                     message)))
    (iolib:with-open-socket
        (s :address-family :local
           :remote-filename (uiop:native-namestring (electron-socket-path interface)))
      (write-line message s)
      (finish-output s)
      (read-line s))))

(defun new-id ()
  "Generate a new unique ID."
  (symbol-name (gensym "ID")))

(defun new-integer-id ()
  "Generate a new unique ID."
  (parse-integer (symbol-name (gensym ""))))

(define-class remote-object ()
  ((remote-symbol
    (new-id)
    :reader t
    :writer nil
    :documentation "The variable name used on the remotely running NodeJS system.")
   (interface
    *interface*
    :reader t
    :writer nil
    :type interface
    :documentation "The Electron `interface' the object will use for its whole lifetime."))
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
