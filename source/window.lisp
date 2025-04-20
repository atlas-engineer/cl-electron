;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron window object definition and methods.

(in-package :electron)

(defmethod initialize-instance :after ((window window) &key)
  (message
   window
   (format nil "~a = new BrowserWindow(~a);"
           (remote-symbol window) (options window))))

(export-always 'load-url)
(defmethod load-url ((window window) url)
  (message
   window
   (format nil "~a.loadURL('~a')" (remote-symbol window) url)))

(export-always 'kill)
(defmethod kill ((window window))
  (mapcar #'destroy-thread* (socket-threads window))
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
  (string-equal "true"
                (message
                 window
                 (format nil "~a.isFocused()" (remote-symbol window)))))

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

(export-always 'get-content-bounds)
(defmethod get-content-bounds ((window window))
  "Return Rectangle object of WINDOW."
  (json:decode-json-from-string
   (message
    window
    (format nil "JSON.stringify(~a.getContentBounds())" (remote-symbol window)))))

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

(export-always 'view-count)
(defmethod view-count ((window window))
  (message
   window
   (format nil "~a.getContentView().children.length" (remote-symbol window))))

(export-always 'add-view)
(defmethod add-view ((window window) (view view) &key z-index)
  "Add VIEW to WINDOW.

When Z-INDEX is omitted, its value corresponds to `view-count' (such that VIEW
is shown as the topmost).

When VIEW is already bound to window and Z-INDEX is omitted, the Z-INDEX of all
of WINDOW's views is reset such that VIEW is shown as the topmost."
  (pushnew view (views window))
  (message
   window
   (format nil "~a.contentView.addChildView(~a~@[,~a~])"
           (remote-symbol window)
           (remote-symbol view)
           z-index)))

(export-always 'add-bounded-view)
(defmacro add-bounded-view (window view &key z-index window-bounds-alist-var x y width height)
  `(progn
     (let ((,window-bounds-alist-var (get-content-bounds ,window)))
       (set-bounds ,view :x ,x :y ,y :width ,width :height ,height))
     ;; As to avoid adding an existing listener.  Note that `add-bounded-view'
     ;; can be called multiple times over the same view and window, as to show
     ;; the former on top.
     (unless (find ,view (views ,window))
       ;; Even though the listener is added to window, it must be removed when
       ;; the view is removed from it.
       (push (add-listener ,window :resize
                           (lambda (win)
                             (let ((,window-bounds-alist-var (get-content-bounds win)))
                               (set-bounds ,view
                                           :x ,x :y ,y :width ,width :height ,height))))
             (socket-threads ,view)))
     ;; `add-view' is called after `set-bounds', since it pushes view into
     ;; `views'.
     (add-view ,window ,view :z-index ,z-index)))

(export-always 'remove-view)
(defmethod remove-view ((window window) (view view) &key (kill-view-p t))
  (setf (views window) (remove view (views window)))
  (message
   window
   (format nil "~a.contentView.removeChildView(~a)"
           (remote-symbol window)
           (remote-symbol view)))
  (when kill-view-p (kill view)))

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
