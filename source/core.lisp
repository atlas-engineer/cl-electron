;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Functions for starting, killing, and communication with the
;;;; electron process.

(in-package :cl-electron)

(defvar *lisp-socket-lock* (bt:make-semaphore :name "electron-lisp-lock"))

(defun launch ()
  (setf *lisp-server-process* (bordeaux-threads:make-thread #'create-server))
  (unless (and *electron-process*
               (uiop:process-alive-p *electron-process*))
    (bt:wait-on-semaphore *lisp-socket-lock*)
    (setf *electron-process*
          (uiop:launch-program (list "electron"
                                     (uiop:native-namestring
                                      (asdf:system-relative-pathname
                                       :cl-electron "source/server.js"))
                                     (uiop:native-namestring *electron-socket-path*)
                                     (uiop:native-namestring *lisp-socket-path*))
                               :output t
                               :error-output t))))

(defun create-server ()
  (unwind-protect
       (let ((native-socket-path (uiop:native-namestring *lisp-socket-path*)))
         (iolib:with-open-socket (s :address-family :local
                                    :connect :passive
                                    :local-filename native-socket-path)
           ;; We don't want group members or others to flood the socket or, worse,
           ;; execute code.
           (setf (iolib/os:file-permissions native-socket-path)
                 (set-difference (iolib/os:file-permissions native-socket-path)
                                 '(:group-read :group-write :group-exec
                                   :other-read :other-write :other-exec)))
           (bt:signal-semaphore *lisp-socket-lock*)
           (loop as connection = (iolib:accept-connection s)
                 while connection
                 do (progn
                      (let ((expr (alex:read-stream-content-into-string connection)))
                        (unless (uiop:emptyp expr)
                          (dispatch-callback expr)))))))
    (uiop:delete-file-if-exists *lisp-socket-path*)))

(defun dispatch-callback (json-string)
  "Handle the callback."
  (let* ((decoded-object (cl-json:decode-json-from-string json-string))
         (callback-id (cdar decoded-object))
         (callback (gethash callback-id *callbacks*))
         (arguments-list (cdr decoded-object)))
    (funcall callback arguments-list)))

(defun terminate ()
  (when (and *electron-process* (uiop:process-alive-p *electron-process*))
    (uiop:terminate-process *electron-process*)
    (setf *electron-process* nil))
  (when (and *lisp-socket-path* (bt:thread-alive-p *lisp-server-process*))
    (bt:destroy-thread *lisp-server-process*)
    (uiop:delete-file-if-exists *lisp-socket-path*)))

(defun send-message (message)
  ;; TODO: Keep socket open?  Use `*socket-stream*'.
  (iolib:with-open-socket (s :address-family :local
                             :remote-filename (uiop:native-namestring *electron-socket-path*))
    (write-line message s)
    (finish-output s)
    (read-line s)))

(defun new-id ()
  "Generate a new unique ID."
  (symbol-name (gensym "ID")))

(defun new-integer-id ()
  "Generate a new unique ID."
  (parse-integer (symbol-name (gensym ""))))

(defclass remote-object ()
  ((remote-symbol :accessor remote-symbol
                  :initarg :remote-symbol
                  :initform (new-id)
                  :documentation "The variable name used on the remotely running NodeJS system.")))

(defclass browser-view (remote-object)
  ())

(defclass browser-window (remote-object)
  ())

(defclass web-contents (remote-object)
  ())
