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

(defun browser-view-get-web-contents (id)
  (let ((new-id (new-id)))
    (send-message
     (format nil "~a = ~a.webContents" new-id id))
    new-id))

(defun web-contents-download-url (id url)
  (send-message
   (format nil "~a.downloadURL(\"~a\")" id url)))

(defun web-contents-set-zoom-factor (id factor)
  (send-message
   (format nil "~a.setZoomFactor(\"~f\")" id factor)))

(defun web-contents-get-zoom-factor (id)
  (send-message
   (format nil "~a.getZoomFactor()" id)))

(defun web-contents-set-zoom-level (id level)
  (send-message
   (format nil "~a.setZoomLevel(~a)" id level)))

(defun web-contents-get-zoom-level (id)
  (send-message
   (format nil "~a.getZoomLevel()" id)))

(defun web-contents-set-muted (id muted)
  (send-message
   (format nil "~a.setAudioMuted(~a)"
           id
           (if muted
               "true"
               "false"))))

(defun web-contents-is-audio-muted (id)
  (send-message
   (format nil "~a.isAudioMuted()" id)))
