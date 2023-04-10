(in-package :cl-electron)


(defun send-message (message)
  (let* ((us (usocket:socket-connect *host* *port*))
         (st (usocket:socket-stream us)))
    (write-line message st)
    (finish-output st)
    (read-line st)))

(defun new-id ()
  "Generate a new unique ID."
  (symbol-name (gensym "ID")))

(defun create-window ()
  (let ((id (new-id)))
    (send-message
     (format nil "~a = new BrowserWindow({})" id))
    id))

(defun delete-window (id)
  (send-message
   (format nil "~a.close()" id)))
