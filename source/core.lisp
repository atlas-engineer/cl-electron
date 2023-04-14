(in-package :cl-electron)

(defun launch ()
  (setf *electron-process*
        (uiop:launch-program (list "electron" (uiop:native-namestring
                                               (asdf:system-relative-pathname
                                                :cl-electron "source/start.js")))))
  (let* ((us (usocket:socket-connect *host* *port*))
         (st (usocket:socket-stream us)))
    (setf *socket-stream* st)))

(defun terminate ()
  (when (uiop:process-alive-p *electron-process*)
    (uiop:terminate-process *electron-process*)))

(defun send-message (message)
  (write-line message *socket-stream*)
  (finish-output *socket-stream*)
  (read-line *socket-stream*))

(defun new-id ()
  "Generate a new unique ID."
  (symbol-name (gensym "ID")))

(defclass remote-object ()
  ((remote-symbol :accessor remote-symbol
                  :initarg :remote-symbol
                  :initform (new-id)
                  :documentation "The variable name used on the remotely running NodeJS system.")))
