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

(defun browser-window-make ()
  (let ((id (new-id)))
    (send-message
     (format nil "~a = new BrowserWindow({})" id))
    id))

(defun browser-window-delete (id)
  (send-message
   (format nil "~a.close()" id)))

(defun browser-window-fullscreen (id)
  (send-message
   (format nil "~a.setFullScreen(true)" id)))

(defun browser-window-unfullscreen (id)
  (send-message
   (format nil "~a.setFullScreen(false)" id)))

(defun browser-window-maximize (id)
  (send-message
   (format nil "~a.maximize()" id)))

(defun browser-window-unmaximize (id)
  (send-message
   (format nil "~a.unmaximize()" id)))

(defun browser-window-title (id)
  (send-message
   (format nil "~a.getTitle()" id)))

(defun browser-window-set-title (id title)
  (send-message
   (format nil "~a.setTitle(\"~a\")" id title)))

(defun browser-window-active (id)
  (send-message
   (format nil "~a.isFocused()" id)))

(defun browser-window-set-active (id)
  (send-message
   (format nil "~a.focus()" id)))

(defun browser-window-set-background-color (id color)
  (send-message
   (format nil "~a.setBackgroundColor(\"~a\")" id color)))

(defun browser-window-set-url (id url)
  (send-message
   (format nil "~a.loadURL(\"~a\")" id url)))
