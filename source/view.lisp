;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron view object definition and methods.

(in-package :electron)

(defmethod initialize-instance :after ((view view) &key)
  (message
   view
   (format nil "~a = new WebContentsView(~a)"
           (remote-symbol view) (options view))))

(export-always 'set-bounds)
(defmethod set-bounds ((view view) &key x y width height)
  (message
   view
   (format nil "~a.setBounds(~a)"
           (remote-symbol view)
           (format-rectangle :x x :y y :width width :height height))))

(export-always 'get-bounds)
(defmethod get-bounds ((view view))
  "Return Rectangle object of WINDOW."
  (json:decode-json-from-string
   (message
    view
    (format nil "JSON.stringify(~a.getBounds())" (remote-symbol view)))))

(export-always 'set-background-color)
(defmethod set-background-color ((view view) color)
  (message
   view
   (format nil "~a.setBackgroundColor(\"~a\")" (remote-symbol view) color)))

(export-always 'web-contents)
(defmethod web-contents ((view view))
  (or (slot-value view 'web-contents)
      (let ((new-id (new-id)))
        (message
         view
         (format nil "~a = ~a.webContents" new-id (remote-symbol view)))
        (setf (slot-value view 'web-contents)
              (make-instance 'web-contents
                             :remote-symbol new-id
                             :interface (interface view))))))

(export-always 'register-before-input-event)
(defmethod register-before-input-event ((view view) callback)
  (let ((synchronous-socket-id
          (create-node-synchronous-socket-thread
           (lambda (response)
             (cl-json:encode-json-to-string
              (list (cons "preventDefault"
                          (apply callback (cons view response))))))
           :interface (interface view))))
    (message
     view
     (format nil
             "~a.on('before-input-event', (event, input) => {
                  ~a.write(JSON.stringify([ input ]) + '\\\n');
                  response = ~a.read();
                  if (JSON.parse(response.toString()).preventDefault) {
                    event.preventDefault();
                  }
                })"
             (remote-symbol (web-contents view))
             synchronous-socket-id
             synchronous-socket-id))))

(export-always 'on)
(defmethod on ((view view) event-name code)
  (message
   view
   (format nil "~a.on('~a', () => {~a})" (remote-symbol view) event-name code)))

(export-always 'on-event)
(defmethod on-event ((view view) event-name callback)
  (let ((socket-thread-id
          (create-node-socket-thread (lambda (response)
                                       (declare (ignore response))
                                       (funcall callback view))
                                     :interface (interface view))))
    (on view event-name
        (format nil
                "jsonString = JSON.stringify([]);
                 ~a.write(`${jsonString}\\n`);"
                socket-thread-id))))

;; Helpers

(export-always 'load-url)
(defmethod load-url ((view view) url)
  (load-url (web-contents view) url))

(export-always 'kill)
(defmethod kill ((view view))
  (kill (web-contents view)))

(export-always 'focus)
(defmethod focus ((view view))
  (focus (web-contents view)))

(export-always 'get-url)
(defmethod get-url ((view view))
  (get-url (web-contents view)))

(export-always 'get-title)
(defmethod get-title ((view view))
  (get-title (web-contents view)))

(export-always 'open-dev-tools)
(defmethod open-dev-tools ((view view) &key (options "{mode: 'undocked'}"))
  (open-dev-tools (web-contents view) :options options))

(export-always 'close-dev-tools)
(defmethod close-dev-tools ((view view))
  (close-dev-tools (web-contents view)))

(export-always 'is-focused)
(defmethod is-focused ((view view))
  (is-focused (web-contents view)))
