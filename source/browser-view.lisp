;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron browser-view object definition and methods.

(in-package :electron)

(defmethod initialize-instance :after ((browser-view browser-view) &key (options ""))
  (send-message-interface
   (interface browser-view)
   (format nil "~a = new BrowserView(~a)"
           (remote-symbol browser-view)
           options)))

(export-always 'set-bounds)
(defmethod set-bounds ((browser-view browser-view) x y width height)
  (send-message-interface
   (interface browser-view)
   (format nil "~a.setBounds({x: ~a, y: ~a, width: ~a, height: ~a})"
           (remote-symbol browser-view) x y width height)))

(export-always 'get-bounds)
(defmethod get-bounds ((browser-view browser-view) parameter)
  "Return Rectangle Object's PARAMETER of BROWSER-VIEW.
See `set-bounds' for the list of available parameters."
  (parse-integer
   (send-message-interface
    (interface browser-view)
    (format nil "~a.getBounds().~(~a~)"
            (remote-symbol browser-view) parameter))))

(export-always 'set-background-color)
(defmethod set-background-color ((browser-view browser-view) color)
  (send-message-interface
   (interface browser-view)
   (format nil "~a.setBackgroundColor(\"~a\")" (remote-symbol browser-view) color)))

(export-always 'set-auto-resize)
(defmethod set-auto-resize ((browser-view browser-view)
                            width
                            height
                            horizontal
                            vertical)
  (send-message-interface
   (interface browser-view)
   (format nil "~a.setAutoResize({width: ~a, height: ~a, horizontal: ~a, vertical: ~a})"
           (remote-symbol browser-view)
           (if width "true" "false")
           (if height "true" "false")
           (if horizontal "true" "false")
           (if vertical "true" "false"))))

(export-always 'web-contents)
(defmethod web-contents ((browser-view browser-view))
  (let ((new-id (new-id)))
    (send-message-interface
     (interface browser-view)
     (format nil "~a = ~a.webContents" new-id (remote-symbol browser-view)))
    (make-instance 'web-contents
                   :remote-symbol new-id
                   :interface (interface browser-view))))

(export-always 'register-before-input-event)
(defmethod register-before-input-event ((browser-view browser-view) callback)
  (let ((socket-thread-id
          (create-node-socket-thread (lambda (response)
                                       (apply callback (cons browser-view response))))))
    (send-message-interface
     (interface browser-view)
     (format nil
             "~a.webContents.on('before-input-event', (event, input) => {
                jsonString = JSON.stringify([ input ]);
                ~a.write(`${jsonString}\\n`);
                event.preventDefault();})"
             (remote-symbol browser-view) socket-thread-id))))

;; Helpers

(export-always 'load-url)
(defmethod load-url ((browser-view browser-view) url)
  (load-url (web-contents browser-view) url))

(export-always 'kill)
(defmethod kill ((browser-view browser-view))
  (kill (web-contents browser-view)))

(export-always 'focus)
(defmethod focus ((browser-view browser-view))
  (focus (web-contents browser-view)))

(export-always 'get-url)
(defmethod get-url ((browser-view browser-view))
  (get-url (web-contents browser-view)))

(export-always 'get-title)
(defmethod get-title ((browser-view browser-view))
  (get-title (web-contents browser-view)))

(export-always 'open-dev-tools)
(defmethod open-dev-tools ((browser-view browser-view) &key (options "{mode: 'undocked'}"))
  (open-dev-tools (web-contents browser-view) :options options))

(export-always 'close-dev-tools)
(defmethod close-dev-tools ((browser-view browser-view))
  (close-dev-tools (web-contents browser-view)))

(export-always 'is-focused)
(defmethod is-focused ((browser-view browser-view))
  (is-focused (web-contents browser-view)))
