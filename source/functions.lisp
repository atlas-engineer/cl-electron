(in-package :cl-electron)


(defun send-message (message)
  (let* ((us (usocket:socket-connect *host* *port*))
         (st (usocket:socket-stream us)))
    (write-line message st)
    (finish-output st)))


(defun create-window ()
  (send-message "app.whenReady().then(() => { createWindow() })"))
