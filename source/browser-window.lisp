(in-package :cl-electron)

(defclass browser-window (remote-object)
  ())

(defmethod initialize-instance :after ((browser-window browser-window) &key (options ""))
  (send-message
   (format nil "~a = new BrowserWindow(~a);"
           (remote-symbol browser-window)
           options))
  ;; We load about blank to initialize the web context (this is a renderer bug).
  (send-message
   (format nil "~a.loadURL('about:blank')"
           (remote-symbol browser-window))))

(defmethod register-before-input-event ((browser-window browser-window) lambda)
  (let ((identifier (new-integer-id)))
    (setf (gethash identifier *callbacks*)
          (lambda (arguments-list)
            (apply lambda (cons browser-window arguments-list))))
    (send-message
     (format nil
             "~a.webContents.on('before-input-event', (event, input) => {
               jsonString = JSON.stringify({ callback: ~a, input: input });
               client.write(`${jsonString} \\\n`);
               event.preventDefault();})"
             (remote-symbol browser-window)
             identifier))))

(defmethod kill ((browser-window browser-window))
  (send-message
   (format nil "~a.close()" (remote-symbol browser-window))))

(defmethod fullscreen ((browser-window browser-window))
  (send-message
   (format nil "~a.setFullScreen(true)" (remote-symbol browser-window))))

(defmethod unfullscreen ((browser-window browser-window))
  (send-message
   (format nil "~a.setFullScreen(false)" (remote-symbol browser-window))))

(defmethod maximize ((browser-window browser-window))
  (send-message
   (format nil "~a.maximize()" (remote-symbol browser-window))))

(defmethod unmaximize ((browser-window browser-window))
  (send-message
   (format nil "~a.unmaximize()" (remote-symbol browser-window))))

(defmethod get-title ((browser-window browser-window))
  (send-message
   (format nil "~a.getTitle()" (remote-symbol browser-window))))

(defmethod set-title ((browser-window browser-window) title)
  (send-message
   (format nil "~a.setTitle(\"~a\")" (remote-symbol browser-window) title)))

(defmethod is-focused ((browser-window browser-window))
  (send-message
   (format nil "~a.isFocused()" (remote-symbol browser-window))))

(defmethod focus ((browser-window browser-window))
  (send-message
   (format nil "~a.focus()" (remote-symbol browser-window))))

(defmethod get-bounds ((browser-window browser-window))
  (let ((new-id (new-id)))
    (send-message
     (format nil "~a = ~a.getBounds()" new-id (remote-symbol browser-window)))))

(defmethod set-bounds ((browser-window browser-window) x y width height)
  (send-message
   (format nil "~a.setBounds({x: ~a, y: ~a, width: ~a, height: ~a})"
           (remote-symbol browser-window) x y width height)))

(defmethod set-background-color ((browser-window browser-window) color)
  (send-message
   (format nil "~a.setBackgroundColor(\"~a\")" (remote-symbol browser-window) color)))

(defmethod add-browser-view ((browser-window browser-window) browser-view-id)
  (send-message
   (format nil "~a.addBrowserView(~a)"
           (remote-symbol browser-window)
           browser-view-id)))

(defmethod remove-browser-view ((browser-window browser-window) browser-view-id)
  (send-message
   (format nil "~a.removeBrowserView(~a)"
           (remote-symbol browser-window)
           browser-view-id)))

(defmethod get-browser-views ((browser-window browser-window))
  (send-message
   (format nil "~a.getBrowserViews()" (remote-symbol browser-window))))

(defmethod web-contents ((browser-window browser-window))
  (let ((new-id (new-id)))
    (send-message
     (format nil "~a = ~a.webContents" new-id (remote-symbol browser-window)))
    new-id)  )
