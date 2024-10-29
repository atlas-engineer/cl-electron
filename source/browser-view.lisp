;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron browser-view object definition and methods.

(in-package :electron)

(defmethod initialize-instance :after ((browser-view browser-view) &key)
  (message
   browser-view
   (format nil "~a = new BrowserView(~a)"
           (remote-symbol browser-view) (options browser-view))))

(export-always 'set-bounds)
(defmethod set-bounds ((browser-view browser-view) x y width height)
  (message
   browser-view
   (format nil "~a.setBounds({x: ~a, y: ~a, width: ~a, height: ~a})"
           (remote-symbol browser-view) x y width height)))

(export-always 'get-bounds)
(defmethod get-bounds ((browser-view browser-view) parameter)
  "Return Rectangle Object's PARAMETER of BROWSER-VIEW.
See `set-bounds' for the list of available parameters."
  (parse-integer
   (message
    browser-view
    (format nil "~a.getBounds().~(~a~)"
            (remote-symbol browser-view) parameter))))

(export-always 'set-background-color)
(defmethod set-background-color ((browser-view browser-view) color)
  (message
   browser-view
   (format nil "~a.setBackgroundColor(\"~a\")" (remote-symbol browser-view) color)))

(export-always 'set-auto-resize)
(defmethod set-auto-resize ((browser-view browser-view)
                            width
                            height
                            horizontal
                            vertical)
  (message
   browser-view
   (format nil "~a.setAutoResize({width: ~a, height: ~a, horizontal: ~a, vertical: ~a})"
           (remote-symbol browser-view)
           (if width "true" "false")
           (if height "true" "false")
           (if horizontal "true" "false")
           (if vertical "true" "false"))))

(export-always 'web-contents)
(defmethod web-contents ((browser-view browser-view))
  (or (slot-value browser-view 'web-contents)
      (let ((new-id (new-id)))
        (message
         browser-view
         (format nil "~a = ~a.webContents" new-id (remote-symbol browser-view)))
        (setf (slot-value browser-view 'web-contents)
              (make-instance 'web-contents
                             :remote-symbol new-id
                             :interface (interface browser-view))))))

(export-always 'register-before-input-event)
(defmethod register-before-input-event ((browser-view browser-view) callback)
  (let ((synchronous-socket-id
          (create-node-synchronous-socket-thread
           (lambda (response)
             (cl-json:encode-json-to-string
              (list (cons "preventDefault"
                          (not (null (apply callback (cons browser-view response))))))))
           :interface (interface browser-view)
           :loop-connect-p t)))
    (message
     browser-view
     (format nil
             "~a.on('before-input-event', (event, input) => {
                  ~a.write(JSON.stringify([ input ]) + '\\\n');
                  response = ~a.read();
                  if (JSON.parse(response.toString()).preventDefault) {
                    event.preventDefault();
                  }
                })"
             (remote-symbol (web-contents browser-view))
             synchronous-socket-id
             synchronous-socket-id))))

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
