;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron web-contents object definition and methods.

(in-package :electron)

(export-always 'download-url)
(defmethod download-url ((web-contents web-contents) url)
  (message
   web-contents
   (format nil "~a.downloadURL(\"~a\")" (remote-symbol web-contents) url)))

(export-always 'set-zoom-factor)
(defmethod set-zoom-factor ((web-contents web-contents) factor)
  (message
   web-contents
   (format nil "~a.setZoomFactor(\"~f\")" (remote-symbol web-contents) factor)))

(export-always 'get-zoom-factor)
(defmethod get-zoom-factor ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.getZoomFactor()" (remote-symbol web-contents))))

(export-always 'set-zoom-level)
(defmethod set-zoom-level ((web-contents web-contents) level)
  (message
   web-contents
   (format nil "~a.setZoomLevel(~a)" (remote-symbol web-contents) level)))

(export-always 'get-zoom-level)
(defmethod get-zoom-level ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.getZoomLevel()" (remote-symbol web-contents))))

(export-always 'set-audio-muted)
(defmethod set-audio-muted ((web-contents web-contents) muted)
  (message
   web-contents
   (format nil "~a.setAudioMuted(~a)"
           (remote-symbol web-contents)
           (if muted
               "true"
               "false"))))

(export-always 'muted-p)
(defmethod muted-p ((web-contents web-contents))
  (if (string-equal "true"
                    (message web-contents
                             (format nil "~a.isAudioMuted()"
                                     (remote-symbol web-contents))))
      t
      nil))

(export-always 'set-user-agent)
(defmethod set-user-agent ((web-contents web-contents) user-agent)
  (message
   web-contents
   (format nil "~a.setUserAgent(\"~a\")" (remote-symbol web-contents) user-agent)))

(export-always 'get-user-agent)
(defmethod get-user-agent ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.getUserAgent()" (remote-symbol web-contents))))

(export-always 'load-url)
(defmethod load-url ((web-contents web-contents) url)
  (message
   web-contents
   (format nil "~a.loadURL(\"~a\")" (remote-symbol web-contents) url)))

(export-always 'load-file)
(defmethod load-file ((web-contents web-contents) path)
  (message
   web-contents
   (format nil "~a.loadFile(\"~a\")" (remote-symbol web-contents) path)))

(export-always 'get-url)
(defmethod get-url ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.getURL()" (remote-symbol web-contents))))

(export-always 'open-dev-tools)
(defmethod open-dev-tools ((web-contents web-contents) &key (options "{mode: 'undocked'}"))
  (message
   web-contents
   (format nil "~a.openDevTools(~a)" (remote-symbol web-contents) options)))

(export-always 'close-dev-tools)
(defmethod close-dev-tools ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.closeDevTools()" (remote-symbol web-contents))))

(export-always 'get-title)
(defmethod get-title ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.getTitle()" (remote-symbol web-contents))))

(export-always 'reload)
(defmethod reload ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.reload()" (remote-symbol web-contents))))

(export-always 'focus)
(defmethod focus ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.focus()" (remote-symbol web-contents))))

(export-always 'is-focused)
(defmethod is-focused ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.isFocused()" (remote-symbol web-contents))))

(export-always 'undo)
(defmethod undo ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.undo()" (remote-symbol web-contents))))

(export-always 'redo)
(defmethod redo ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.redo()" (remote-symbol web-contents))))

(export-always 'cut)
(defmethod cut ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.cut()" (remote-symbol web-contents))))

(export-always 'copy)
(defmethod copy ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.copy()" (remote-symbol web-contents))))

(export-always 'paste)
(defmethod paste ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.paste()" (remote-symbol web-contents))))

(export-always 'select-all)
(defmethod select-all ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.selectAll()" (remote-symbol web-contents))))

(export-always 'kill)
(defmethod kill ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.close()" (remote-symbol web-contents))))

(export-always 'insert-css)
(defmethod insert-css ((web-contents web-contents) css)
  (message
   web-contents
   (format nil "~a.insertCSS(\"~a\")" (remote-symbol web-contents) css)))

(export-always 'remove-inserted-css)
(defmethod remove-inserted-css ((web-contents web-contents) key)
  (message
   web-contents
   (format nil "~a.removeInsertedCSS(\"~a\")" (remote-symbol web-contents) key)))

(export-always 'session)
(defmethod session ((web-contents web-contents))
  (let ((new-id (new-id)))
    (message
     web-contents
     (format nil "~a = ~a.session" new-id (remote-symbol web-contents)))
    new-id))

(defun %quote-js (js-code)
  "Replace each backlash with 2, unless a \" follows it."
  (ppcre:regex-replace-all "\\\\(?!\")" js-code "\\\\\\\\"))

(export-always 'execute-javascript)
(defmethod execute-javascript ((web-contents web-contents) code &key (user-gesture "false"))
  (message
   web-contents
   (format nil "~a.executeJavaScript(`~a`, ~a)"
           (remote-symbol web-contents) (%quote-js code) user-gesture)))

(export-always 'execute-javascript-in-isolated-world)
(defmethod execute-javascript-in-isolated-world ((web-contents web-contents) world-id code
                                                 &key (user-gesture "false"))
  (message
   web-contents
   (format nil "~a.executeJavaScript(~a, {code: `~a`}, ~a)"
           (remote-symbol web-contents) world-id (%quote-js code) user-gesture)))

(export-always 'execute-javascript-with-promise-callback)
(defmethod execute-javascript-with-promise-callback
    ((web-contents web-contents) code callback &key (user-gesture "false"))
  (let ((socket-thread-id
          (bind-node-socket-thread web-contents
                                   (lambda (response)
                                     (apply callback (cons web-contents response))))))
    (message
     web-contents
      (format nil "~a.executeJavaScript(`~a`, ~a).then((value) => {
                    jsonString = JSON.stringify([ value ]);
                    ~a.write(`${jsonString}\\n`);}).catch(error => {
                      ~a.write('[\"ERROR\"]\\n');});"
             (remote-symbol web-contents) (%quote-js code) user-gesture
             socket-thread-id socket-thread-id))))

(export-always 'on)
(defmethod on ((web-contents web-contents) event-name code)
  (message
   web-contents
   (format nil "~a.on('~a', () => {~a})" (remote-symbol web-contents) event-name code)))

(export-always 'on-event)
(defmethod on-event ((web-contents web-contents) event-name callback)
  (let ((socket-thread-id
          (bind-node-socket-thread web-contents
                                   (lambda (response)
                                     (declare (ignore response))
                                     (funcall callback web-contents)))))
    (on web-contents event-name
        (format nil
                "jsonString = JSON.stringify([]);
                 ~a.write(`${jsonString}\\n`);"
                socket-thread-id))))

(export-always 'execute-javascript-synchronous)
(defmethod execute-javascript-synchronous ((web-contents web-contents) code
                                           &key (user-gesture "false"))
  (let ((p (lparallel:promise)))
    (execute-javascript-with-promise-callback
     web-contents
     code
     (lambda (web-contents result)
       (declare (ignore web-contents))
       (if (equal result "ERROR")
           (restart-case (error (make-condition 'javascript-renderer-error :code code))
             (ignore ()
               :report "Ignore error, fulfill promise with string 'ERROR'."
               (lparallel:fulfill p result))
             (reject ()
               :report "Return nil, indicating promise rejection."
               (lparallel:fulfill p nil)))
           (lparallel:fulfill p result)))
     :user-gesture user-gesture)
    (lparallel:force p)))
