;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron view object definition and methods.

(in-package :electron)

(defmethod options ((view view))
  ;; If `options' is explicitly set, use it.
  (if (slot-value view 'options)
      (slot-value view 'options)
      (format nil "{~@[webPreferences:~a,~] ~@[webContents:~a~]}"
              (when (web-preferences view)
                (encode-json (web-preferences view)))
              (when (slot-value view 'web-contents)
                (remote-symbol (slot-value view 'web-contents))))))

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
