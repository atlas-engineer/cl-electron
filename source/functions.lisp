(in-package :cl-electron)


(defun send-message (message)
  (let* ((us (usocket:socket-connect *host* *port*))
         (st (usocket:socket-stream us)))
    (write-line message st)
    (finish-output st)
    (format t "~A~%" (read-line st))))


(defun create-window ()
  (send-message
   "const mainWindow = new BrowserWindow({
        width: 800,
        height: 600,
    })"))

(defun exec-math ()
  (send-message
   "3+4"))
