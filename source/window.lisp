;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron window object definition and methods.

(in-package :electron)

(defmethod initialize-instance :after ((window window) &key)
  (message
   window
   (format nil "~a = new BrowserWindow(~a);"
           (remote-symbol window) (options window))))

(export-always 'register-before-input-event)
(defmethod register-before-input-event ((window window) callback)
  (let ((synchronous-socket-id
          (create-node-synchronous-socket-thread
           (lambda (response)
             (cl-json:encode-json-to-string
              (list (cons "preventDefault"
                          (apply callback (cons window response))))))
           :interface (interface window))))
    (message
     window
     (format nil
             "~a.on('before-input-event', (event, input) => {
                  ~a.write(JSON.stringify([ input ]) + '\\\n');
                  response = ~a.read();
                  if (JSON.parse(response.toString()).preventDefault) {
                    event.preventDefault();
                  }
                })"
             (remote-symbol (web-contents window))
             synchronous-socket-id
             synchronous-socket-id))))

(export-always 'load-url)
(defmethod load-url ((window window) url)
  (message
   window
   (format nil "~a.loadURL('~a')" (remote-symbol window) url)))

(export-always 'kill)
(defmethod kill ((window window))
  (message
   window
   (format nil "~a.close()" (remote-symbol window))))

(export-always 'fullscreen)
(defmethod fullscreen ((window window))
  (message
   window
   (format nil "~a.setFullScreen(true)" (remote-symbol window))))

(export-always 'unfullscreen)
(defmethod unfullscreen ((window window))
  (message
   window
   (format nil "~a.setFullScreen(false)" (remote-symbol window))))

(export-always 'maximize)
(defmethod maximize ((window window))
  (message
   window
   (format nil "~a.maximize()" (remote-symbol window))))

(export-always 'unmaximize)
(defmethod unmaximize ((window window))
  (message
   window
   (format nil "~a.unmaximize()" (remote-symbol window))))

(export-always 'get-title)
(defmethod get-title ((window window))
  (message
   window
   (format nil "~a.getTitle()" (remote-symbol window))))

(export-always 'set-title)
(defmethod set-title ((window window) title)
  (message
   window
   (format nil "~a.setTitle(\"~a\")" (remote-symbol window) title)))

(export-always 'is-focused)
(defmethod is-focused ((window window))
  (message
   window
   (format nil "~a.isFocused()" (remote-symbol window))))

(export-always 'focus)
(defmethod focus ((window window))
  (message
   window
   (format nil "~a.focus()" (remote-symbol window))))

(export-always 'remove-menu)
(defmethod remove-menu ((window window))
  (message
   window
   (format nil "~a.removeMenu()" (remote-symbol window))))

(export-always 'get-bounds)
(defmethod get-bounds ((window window))
  "Return Rectangle object of WINDOW."
  (json:decode-json-from-string
   (message
    window
    (format nil "JSON.stringify(~a.getBounds())" (remote-symbol window)))))

(defun format-rectangle (&key x y width height)
  "Encode Rectangle object."
  (format nil "{~{~A~^, ~}}"
          (remove-if #'uiop:emptyp
                     (list (format nil "~@[x: ~A~]" x)
                           (format nil "~@[y: ~A~]" y)
                           (format nil "~@[width: ~A~]" width)
                           (format nil "~@[height: ~A~]" height)))))

(export-always 'set-bounds)
(defmethod set-bounds ((window window) &key x y width height)
  (message
   window
   (format nil "~a.setBounds(~a)"
           (remote-symbol window)
           (format-rectangle :x x :y y :width width :height height))))

(export-always 'set-background-color)
(defmethod set-background-color ((window window) color)
  (message
   window
   (format nil "~a.setBackgroundColor(\"~a\")" (remote-symbol window) color)))

(export-always 'content-view)
(defmethod content-view ((window window))
  (message
   window
   (format nil "~a.contentView" (remote-symbol window))))

(export-always 'get-content-view)
(defmethod get-content-view ((window window))
  (message
   window
   (format nil "~a.getContentView()" (remote-symbol window))))

(export-always 'add-view)
(defmethod add-view ((window window) (view view))
  "Add VIEW to WINDOW.

When view is already bound to window, it is shown at the top."
  (pushnew view (views window))
  (message
   window
   (format nil "~a.contentView.addChildView(~a)"
           (remote-symbol window)
           (remote-symbol view))))

(export-always 'add-bounded-view)
(defmacro add-bounded-view (window view &key window-bounds-alist-var x y width height)
  `(progn
     (let ((,window-bounds-alist-var (get-bounds ,window)))
       (set-bounds ,view :x ,x :y ,y :width ,width :height ,height))
     ;; As to avoid adding an existing listener.  Note that `add-bounded-view'
     ;; can be called multiple times over the same view and window, as to show
     ;; the former on top.
     (unless (find ,view (views ,window))
       (on-event ,window "resize"
                 (lambda (win)
                   (let ((,window-bounds-alist-var (get-bounds win)))
                     (set-bounds ,view :x ,x :y ,y :width ,width :height ,height)))))
     ;; `add-view' is called after `set-bounds', since it pushes view into
     ;; `views'.
     (add-view ,window ,view)))

(export-always 'remove-view)
(defmethod remove-view ((window window) (view view))
  (setf (views window) (remove view (views window)))
  (message
   window
   (format nil "~a.contentView.removeChildView(~a)"
           (remote-symbol window)
           (remote-symbol view))))

(export-always 'web-contents)
(defmethod web-contents ((window window))
  (or (slot-value window 'web-contents)
      (let ((new-id (new-id)))
        (message
         window
         (format nil "~a = ~a.webContents" new-id (remote-symbol window)))
        (setf (slot-value window 'web-contents)
              (make-instance 'web-contents
                             :remote-symbol new-id
                             :interface (interface window))))))

(export-always 'get-child-windows)
(defmethod get-child-windows ((window window))
  (message
   window
   (format nil "~a.getChildWindows()" (remote-symbol window))))

(export-always 'get-parent-window)
(defmethod get-parent-window ((window window))
  (message
   window
   (format nil "~a.getParentWindow()" (remote-symbol window))))

(export-always 'id)
(defmethod id ((window window))
  (message
   window
   (format nil "~a.id" (remote-symbol window))))

(export-always 'on)
(defmethod on ((window window) event-name code)
  (message
   window
   (format nil "~a.on('~a', () => {~a})" (remote-symbol window) event-name code)))

(export-always 'on-event)
(defmethod on-event ((window window) event-name callback)
  (let ((socket-thread-id
          (create-node-socket-thread (lambda (response)
                                       (declare (ignore response))
                                       (funcall callback window))
                                     :interface (interface window))))
    (on window event-name
        (format nil
                "jsonString = JSON.stringify([]);
                 ~a.write(`${jsonString}\\n`);"
                socket-thread-id))))

;; Static methods

(export-always 'get-all-windows)
(defun get-all-windows (interface)
  (message
   interface
   (format nil "BrowserWindow.getAllWindows()")))

(export-always 'test-get-focused-window)
(defun test-get-focused-window (interface)
  (message
   interface
   (format nil "BrowserWindow.getFocusedWindow()")))

(export-always 'window-from-web-contents)
(defun window-from-web-contents (interface web-contents)
  (message
   interface
   (format nil "BrowserWindow.fromWebContents(~a)" (remote-symbol web-contents))))

(export-always 'window-from-id)
(defun window-from-id (interface id)
  (message
   interface
   (format nil "BrowserWindow.fromId(~a)" id)))
