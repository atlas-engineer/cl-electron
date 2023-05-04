;;;; Electron browser-view object definition and methods.

(in-package :cl-electron)

(defmethod initialize-instance :after ((browser-view browser-view) &key (options ""))
  (send-message
   (format nil "~a = new BrowserView(~a)"
           (remote-symbol browser-view)
           options)))

(defmethod set-bounds ((browser-view browser-view) x y width height)
  (send-message
   (format nil "~a.setBounds({x: ~a, y: ~a, width: ~a, height: ~a})"
           (remote-symbol browser-view) x y width height)))

(defmethod get-bounds ((browser-view browser-view))
  (let ((new-id (new-id)))
    (send-message
     (format nil "~a = ~a.getBounds()" new-id (remote-symbol browser-view)))))

(defmethod set-auto-resize ((browser-view browser-view)
                            width
                            height
                            horizontal
                            vertical)
  (send-message
   (format nil "~a.setAutoResize({width: ~a, height: ~a, horizontal: ~a, vertical: ~a})"
           (remote-symbol browser-view)
           (if width "true" "false")
           (if height "true" "false")
           (if horizontal "true" "false")
           (if vertical "true" "false"))))

(defmethod web-contents ((browser-view browser-view))
  (let ((new-id (new-id)))
    (send-message
     (format nil "~a = ~a.webContents" new-id (remote-symbol browser-view)))
    (make-instance 'web-contents
                   :remote-symbol new-id)))
