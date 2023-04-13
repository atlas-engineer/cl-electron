(in-package :cl-electron)

(defun launch ()
  (setf *electron-process*
        (uiop:launch-program (list "electron" "start.js"))))

(defun terminate ()
  (when (uiop:process-alive-p *electron-process*)
    (uiop:terminate-process *electron-process*)))

(defun send-message (message)
  (let* ((us (usocket:socket-connect *host* *port*))
         (st (usocket:socket-stream us)))
    (write-line message st)
    (finish-output st)
    (read-line st)))

(defun new-id ()
  "Generate a new unique ID."
  (symbol-name (gensym "ID")))

(defclass remote-object ()
  ((remote-symbol :accessor remote-symbol
                  :initarg :remote-symbol
                  :initform (new-id)
                  :documentation "The variable name used on the remotely running NodeJS system.")))
