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
    :documentation "Callbacks to execute when the `listener' reads an instruction on the `lisp-socket-path'."))
  (:export-class-name-p t)
  (:predicate-name-transformer 'nclasses:always-dashed-predicate-name-transformer)
  (:documentation "Interface with an Electron instance."))

(export-always '*interface*)
(defvar *interface* nil)

(export-always 'launch)
(defun launch (&optional (interface *interface*))
  (setf (listener interface) (bordeaux-threads:make-thread
                              (lambda () (create-server interface))))
  (unless (and (process interface)
               (uiop:process-alive-p (process interface)))
    (bt:wait-on-semaphore (lisp-socket-lock interface))
    (setf (process interface)
          (uiop:launch-program (list "electron"
                                     (uiop:native-namestring
                                      (asdf:system-relative-pathname
                                       :cl-electron "source/server.js"))
                                     (uiop:native-namestring (electron-socket-path interface))
                                     (uiop:native-namestring (lisp-socket-path interface)))
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
                        (dispatch-callback expr interface))))))
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

(defun send-message (target message &key (replace-newlines-p t))
  (send-message-interface (interface target) message :replace-newlines-p replace-newlines-p))

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
