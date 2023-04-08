(in-package :cl-electron)


(defun create-window ()
  (let* ((us (usocket:socket-connect "127.0.0.1" 3000))
         (st (usocket:socket-stream us)))
    (write-line "app.whenReady().then(() => { createWindow() })" st)
    (finish-output st)))
