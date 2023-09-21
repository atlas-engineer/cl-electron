;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron web-contents object definition and methods.

(in-package :electron)

(defmethod download-url ((web-contents web-contents) url)
  (send-message
   web-contents
   (format nil "~a.downloadURL(\"~a\")" (remote-symbol web-contents) url)))

(defmethod set-zoom-factor ((web-contents web-contents) factor)
  (send-message
   web-contents
   (format nil "~a.setZoomFactor(\"~f\")" (remote-symbol web-contents) factor)))

(defmethod get-zoom-factor ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.getZoomFactor()" (remote-symbol web-contents))))

(defmethod set-zoom-level ((web-contents web-contents) level)
  (send-message
   web-contents
   (format nil "~a.setZoomLevel(~a)" (remote-symbol web-contents) level)))

(defmethod get-zoom-level ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.getZoomLevel()" (remote-symbol web-contents))))

(defmethod set-audio-muted ((web-contents web-contents) muted)
  (send-message
   web-contents
   (format nil "~a.setAudioMuted(~a)"
           (remote-symbol web-contents)
           (if muted
               "true"
               "false"))))

(defmethod is-audio-muted ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.isAudioMuted()" (remote-symbol web-contents))))

(defmethod set-user-agent ((web-contents web-contents) user-agent)
  (send-message
   web-contents
   (format nil "~a.setUserAgent(\"~a\")" (remote-symbol web-contents) user-agent)))

(defmethod get-user-agent ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.getUserAgent()" (remote-symbol web-contents))))

(defmethod load-url ((web-contents web-contents) url)
  (send-message
   web-contents
   (format nil "~a.loadURL(\"~a\")" (remote-symbol web-contents) url)))

(defmethod load-file ((web-contents web-contents) path)
  (send-message
   web-contents
   (format nil "~a.loadFile(\"~a\")" (remote-symbol web-contents) path)))

(defmethod get-url ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.getURL()" (remote-symbol web-contents))))

(defmethod open-dev-tools ((web-contents web-contents) &key (options "{mode: 'undocked'}"))
  (send-message
   web-contents
   (format nil "~a.openDevTools(~a)" (remote-symbol web-contents) options)))

(defmethod close-dev-tools ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.closeDevTools()" (remote-symbol web-contents))))

(defmethod get-title ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.getTitle()" (remote-symbol web-contents))))

(defmethod reload ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.reload()" (remote-symbol web-contents))))

(defmethod focus ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.focus()" (remote-symbol web-contents))))

(defmethod is-focused ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.isFocused()" (remote-symbol web-contents))))

(defmethod undo ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.undo()" (remote-symbol web-contents))))

(defmethod redo ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.redo()" (remote-symbol web-contents))))

(defmethod cut ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.cut()" (remote-symbol web-contents))))

(defmethod copy ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.copy()" (remote-symbol web-contents))))

(defmethod paste ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.paste()" (remote-symbol web-contents))))

(defmethod select-all ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.selectAll()" (remote-symbol web-contents))))

(defmethod kill ((web-contents web-contents))
  (send-message
   web-contents
   (format nil "~a.close()" (remote-symbol web-contents))))

(defmethod insert-css ((web-contents web-contents) css)
  (send-message
   web-contents
   (format nil "~a.insertCSS(\"~a\")" (remote-symbol web-contents) css)))

(defmethod remove-inserted-css ((web-contents web-contents) key)
  (send-message
   web-contents
   (format nil "~a.removeInsertedCSS(\"~a\")" (remote-symbol web-contents) key)))

(defmethod session ((web-contents web-contents))
  (let ((new-id (new-id)))
    (send-message
     web-contents
     (format nil "~a = ~a.session" new-id (remote-symbol web-contents)))
    new-id))

(defmethod execute-javascript ((web-contents web-contents) code &key (user-gesture "false"))
  (send-message
   web-contents
   (format nil "~a.executeJavaScript(\"~a\", \"~a\")"
           (remote-symbol web-contents) code user-gesture)))

(defmethod execute-javascript-in-isolated-world ((web-contents web-contents) world-id code
                                                 &key (user-gesture "false"))
  (send-message
   web-contents
   (format nil "~a.executeJavaScript(~a, {code: \"~a\"}, \"~a\")"
           (remote-symbol web-contents) world-id code user-gesture)))

(defmethod execute-javascript-with-promise-callback
    ((web-contents web-contents) code callback &key (user-gesture "false"))
  (let ((identifier (new-integer-id)))
    (setf (gethash identifier (callbacks (interface web-contents)))
          (lambda (args) (apply callback (cons web-contents args))))
    (send-message
     web-contents
     (format nil "~a.executeJavaScript(\"~a\", \"~a\").then((value) => {
                   jsonString = JSON.stringify({ callback: ~a, value: value });
                   client.write(`${jsonString}\\n`);})"
             (remote-symbol web-contents) code user-gesture identifier)
     :replace-newlines-p nil)))
