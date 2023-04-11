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

(defun browser-window-new ()
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

(defun browser-view-new ()
  (let ((id (new-id)))
    (send-message
     (format nil "~a = new BrowserView({})" id))
    id))

(defun browser-view-set-bounds (id x y width height)
  (send-message
   (format nil "~a.setBounds({x: ~a, y: ~a, width: ~a, height: ~a})"
           id x y width height)))

(defun browser-view-get-bounds (id)
  (send-message
   (format nil "~a.getBounds()" id)))

(defun browser-view-set-auto-resize (id width height horizontal vertical)
  (send-message
   (format nil "~a.setAutoResize({width: ~a, height: ~a, horizontal: ~a, vertical: ~a})"
           id
           (if width "true" "false")
           (if height "true" "false")
           (if horizontal "true" "false")
           (if vertical "true" "false"))))
