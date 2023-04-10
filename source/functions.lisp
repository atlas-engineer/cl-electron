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

(defun window-make ()
  (let ((id (new-id)))
    (send-message
     (format nil "~a = new BrowserWindow({})" id))
    id))

(defun window-delete (id)
  (send-message
   (format nil "~a.close()" id)))

(defun window-fullscreen (id)
  (send-message
   (format nil "~a.setFullScreen(true)" id)))

(defun window-unfullscreen (id)
  (send-message
   (format nil "~a.setFullScreen(false)" id)))

(defun window-maximize (id)
  (send-message
   (format nil "~a.maximize()")))

(defun window-unmaximize (id)
  (send-message
   (format nil "~a.unmaximize()")))
