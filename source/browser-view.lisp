;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron browser-view object definition and methods.

(in-package :electron)

(defmethod initialize-instance :after ((browser-view browser-view) &key (options ""))
  (send-message
   browser-view
   (format nil "~a = new BrowserView(~a)"
           (remote-symbol browser-view)
           options)))

(export-always 'set-bounds)
(defmethod set-bounds ((browser-view browser-view) x y width height)
  (send-message
   browser-view
   (format nil "~a.setBounds({x: ~a, y: ~a, width: ~a, height: ~a})"
           (remote-symbol browser-view) x y width height)))

(export-always 'get-bounds)
(defmethod get-bounds ((browser-view browser-view) parameter)
  "Return Rectangle Object's PARAMETER of BROWSER-VIEW.
See `set-bounds' for the list of available parameters."
  (parse-integer
   (send-message
    browser-view
    (format nil "~a.getBounds().~(~a~)"
            (remote-symbol browser-view) parameter))))

(export-always 'set-background-color)
(defmethod set-background-color ((browser-view browser-view) color)
  (send-message
   browser-view
   (format nil "~a.setBackgroundColor(\"~a\")" (remote-symbol browser-view) color)))

(export-always 'set-auto-resize)
(defmethod set-auto-resize ((browser-view browser-view)
                            width
                            height
                            horizontal
                            vertical)
  (send-message
   browser-view
   (format nil "~a.setAutoResize({width: ~a, height: ~a, horizontal: ~a, vertical: ~a})"
           (remote-symbol browser-view)
           (if width "true" "false")
           (if height "true" "false")
           (if horizontal "true" "false")
           (if vertical "true" "false"))))

(export-always 'web-contents)
(defmethod web-contents ((browser-view browser-view))
  (let ((new-id (new-id)))
    (send-message
     browser-view
     (format nil "~a = ~a.webContents" new-id (remote-symbol browser-view)))
    (make-instance 'web-contents
                   :remote-symbol new-id
                   :interface (interface browser-view))))

(export-always 'load-url)
(defmethod load-url ((browser-view browser-view) url)
  (load-url (web-contents browser-view) url))

(export-always 'kill)
(defmethod kill ((browser-view browser-view))
  (kill (web-contents browser-view)))
