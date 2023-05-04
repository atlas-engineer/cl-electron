;;;; Functions for starting, killing, and communication with the
;;;; electron process.

(in-package :cl-electron)

(defun launch ()
  (unless *electron-process*
    (setf *electron-process*
          (uiop:launch-program (list "electron" (uiop:native-namestring
                                                 (asdf:system-relative-pathname
                                                  :cl-electron "source/server.js"))))))
  (loop until
           (handler-case
               (let* ((us (usocket:socket-connect *host* *port*))
                      (st (usocket:socket-stream us)))
                 (setf *socket-stream* st))
             (usocket:connection-refused-error ())))
  (bordeaux-threads:make-thread (lambda ()
                                  (create-server 3001))))

(defun create-server (port)
  (let* ((socket (usocket:socket-listen "127.0.0.1" port))
	 (connection (usocket:socket-accept socket :element-type 'character))
         (stream (usocket:socket-stream connection)))
    (unwind-protect
         (handler-case
             (loop
               (dispatch-callback (read-line stream)))
           (end-of-file ()
             (usocket:socket-close connection)
             (usocket:socket-close socket)))
      (usocket:socket-close connection)
      (usocket:socket-close socket))))

(defun dispatch-callback (json-string)
  "Handle the callback."
  (let* ((decoded-object (cl-json:decode-json-from-string json-string))
         (callback-id (cdar decoded-object))
         (callback (gethash callback-id *callbacks*))
         (arguments-list (cdr decoded-object)))
    (funcall callback arguments-list)))

(defun terminate ()
  (when (uiop:process-alive-p *electron-process*)
    (uiop:terminate-process *electron-process*)
    (setf *electron-process* nil)))

(defun send-message (message)
  (write-line message *socket-stream*)
  (finish-output *socket-stream*)
  (read-line *socket-stream*))

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
