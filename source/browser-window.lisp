;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron browser-window object definition and methods.

(in-package :electron)

(defmethod initialize-instance :after ((browser-window browser-window)
                                       &key (options "{frame: false}"))
  (send-message
   browser-window
   (format nil "~a = new BrowserWindow(~a);"
           (remote-symbol browser-window) options)))

(export-always 'register-before-input-event)
(defmethod register-before-input-event ((browser-window browser-window) fn)
  (register-before-input-event (web-contents browser-window) fn))

(export-always 'load-url)
(defmethod load-url ((browser-window browser-window) url)
  (send-message
   browser-window
   (format nil "~a.loadURL('~a')" (remote-symbol browser-window) url)))

(export-always 'kill)
(defmethod kill ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.close()" (remote-symbol browser-window))))

(export-always 'fullscreen)
(defmethod fullscreen ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.setFullScreen(true)" (remote-symbol browser-window))))

(export-always 'unfullscreen)
(defmethod unfullscreen ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.setFullScreen(false)" (remote-symbol browser-window))))

(export-always 'maximize)
(defmethod maximize ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.maximize()" (remote-symbol browser-window))))

(export-always 'unmaximize)
(defmethod unmaximize ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.unmaximize()" (remote-symbol browser-window))))

(export-always 'get-title)
(defmethod get-title ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.getTitle()" (remote-symbol browser-window))))

(export-always 'set-title)
(defmethod set-title ((browser-window browser-window) title)
  (send-message
   browser-window
   (format nil "~a.setTitle(\"~a\")" (remote-symbol browser-window) title)))

(export-always 'is-focused)
(defmethod is-focused ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.isFocused()" (remote-symbol browser-window))))

(export-always 'focus)
(defmethod focus ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.focus()" (remote-symbol browser-window))))

(export-always 'get-bounds)
(defmethod get-bounds ((browser-window browser-window) parameter)
  "Return Rectangle Object's PARAMETER of BROWSER-WINDOW.
See `set-bounds' for the list of available parameters."
  (parse-integer
   (send-message
    browser-window
    (format nil "~a.getBounds().~(~a~)"
            (remote-symbol browser-window) parameter))))

(export-always 'set-bounds)
(defmethod set-bounds ((browser-window browser-window) x y width height)
  (send-message
   browser-window
   (format nil "~a.setBounds({x: ~a, y: ~a, width: ~a, height: ~a})"
           (remote-symbol browser-window) x y width height)))

(export-always 'set-background-color)
(defmethod set-background-color ((browser-window browser-window) color)
  (send-message
   browser-window
   (format nil "~a.setBackgroundColor(\"~a\")" (remote-symbol browser-window) color)))

(export-always 'add-browser-view)
(defmethod add-browser-view ((browser-window browser-window) (browser-view browser-view))
  (send-message
   browser-window
   (format nil "~a.addBrowserView(~a)"
           (remote-symbol browser-window)
           (remote-symbol browser-view))))

(export-always 'set-browser-view)
(defmethod set-browser-view ((browser-window browser-window) (browser-view browser-view))
  (send-message
   browser-window
   (format nil "~a.setBrowserView(~a)"
           (remote-symbol browser-window)
           (remote-symbol browser-view))))

(export-always 'set-top-browser-view)
(defmethod set-top-browser-view ((browser-window browser-window) (browser-view browser-view))
  (send-message
   browser-window
   (format nil "~a.setTopBrowserView(~a)"
           (remote-symbol browser-window)
           (remote-symbol browser-view))))

(export-always 'remove-browser-view)
(defmethod remove-browser-view ((browser-window browser-window) (browser-view browser-view))
  ;; Hides the view and doesn't require re-rendering it.
  (send-message
   browser-window
   (format nil "~a.removeBrowserView(~a)"
           (remote-symbol browser-window)
           (remote-symbol browser-view))))

(export-always 'get-browser-view)
(defmethod get-browser-view ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.getBrowserView()" (remote-symbol browser-window))))

(export-always 'get-browser-views)
(defmethod get-browser-views ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.getBrowserViews()" (remote-symbol browser-window))))

(export-always 'web-contents)
(defmethod web-contents ((browser-window browser-window))
  (let ((new-id (new-id)))
    (send-message
     browser-window
     (format nil "~a = ~a.webContents" new-id (remote-symbol browser-window)))
    (make-instance 'web-contents
                   :remote-symbol new-id
                   :interface (interface browser-window))))

(export-always 'get-child-windows)
(defmethod get-child-windows ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.getChildWindows()" (remote-symbol browser-window))))

(export-always 'get-parent-window)
(defmethod get-parent-window ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.getParentWindow()" (remote-symbol browser-window))))

;; Static methods
;; https://www.electronjs.org/docs/latest/api/browser-window#static-methods

(export-always 'get-all-windows)
(defun get-all-windows (interface)
  (send-message-interface
   interface
   (format nil "BrowserWindow.getAllWindows()")))

(export-always 'test-get-focused-window)
(defun test-get-focused-window (interface)
  (send-message-interface
   interface
   (format nil "BrowserWindow.getFocusedWindow()")))

(export-always 'window-from-web-contents)
(defun window-from-web-contents (interface web-contents)
  (send-message-interface
   interface
   (format nil "BrowserWindow.fromWebContents(~a)" (remote-symbol web-contents))))

(export-always 'window-from-id)
(defun window-from-id (interface id)
  (send-message-interface
   interface
   (format nil "BrowserWindow.fromId(~a)" id)))

(export-always 'id)
(defmethod id ((browser-window browser-window))
  (send-message
   browser-window
   (format nil "~a.id" (remote-symbol browser-window))))
