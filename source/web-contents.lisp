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
   (format nil "~a.setZoomFactor(~a)" (remote-symbol web-contents) factor)))

(export-always 'get-zoom-factor)
(defmethod get-zoom-factor ((web-contents web-contents))
  (parse-number:parse-number
   (message
    web-contents
    (format nil "~a.getZoomFactor()" (remote-symbol web-contents)))))

(export-always 'set-zoom-level)
(defmethod set-zoom-level ((web-contents web-contents) level)
  (message
   web-contents
   (format nil "~a.setZoomLevel(~a)" (remote-symbol web-contents) level)))

(export-always 'get-zoom-level)
(defmethod get-zoom-level ((web-contents web-contents))
  (parse-number:parse-number
   (message
    web-contents
    (format nil "~a.getZoomLevel()" (remote-symbol web-contents)))))

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
  (when (string-equal "true"
                      (message web-contents
                               (format nil "~a.isAudioMuted()"
                                       (remote-symbol web-contents))))
    t))

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
  (when (string-equal "true"
                      (message
                       web-contents
                       (format nil "~a.isFocused()" (remote-symbol web-contents))))
    t))

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

(export-always 'insert-text)
(defmethod insert-text ((web-contents web-contents) text)
  (message
   web-contents
   (format nil "~a.insertText(\"~a\")" (remote-symbol web-contents) text)))

(export-always 'select-all)
(defmethod select-all ((web-contents web-contents))
  (message
   web-contents
   (format nil "~a.selectAll()" (remote-symbol web-contents))))

(export-always 'kill)
(defmethod kill ((web-contents web-contents))
  (mapcar #'destroy-thread* (socket-threads web-contents))
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
  (or (slot-value web-contents 'session)
      (let ((new-id (new-id)))
        (message
         web-contents
         (format nil "~a = ~a.session" new-id (remote-symbol web-contents)))
        (setf (slot-value web-contents 'session)
              (make-instance 'session
                             :remote-symbol new-id
                             :interface (interface web-contents))))))

(export-always 'override-window-open-handler)
(defmethod override-window-open-handler ((web-contents web-contents) callback)
  (message
   web-contents
   (format nil "~a.setWindowOpenHandler((details) => {
                    ~a.write(JSON.stringify(details) + '\\\n');
                    return { action: 'deny' };
                });"
	       (remote-symbol web-contents)
	       (create-node-socket-thread (lambda (response)
						                (funcall callback response)
						                (destroy-thread* (bt:current-thread)))
					                  :interface (interface web-contents)))))

(defun format-js-for-eval (js-code)
  "Escape JavaScript code so it can be safely inserted into a JS eval() call using double quotes."
  (let* ((escaped-backslashes
           (ppcre:regex-replace-all "\\\\" js-code "\\\\\\\\")) ; \ → \\
         (escaped-quotes
           (ppcre:regex-replace-all "\"" escaped-backslashes "\\\\\"")) ; " → \"
         (escaped-newlines
           (ppcre:regex-replace-all "\\n" escaped-quotes "\\\\n"))) ; newline → \n
    escaped-newlines))

(export-always 'execute-javascript)
(defmethod execute-javascript ((web-contents web-contents) code &key (user-gesture "false"))
  (message
   web-contents
   (format nil "~a.executeJavaScript(\"~a\", ~a)"
           (remote-symbol web-contents) (format-js-for-eval code) user-gesture)))

(export-always 'execute-javascript-in-isolated-world)
(defmethod execute-javascript-in-isolated-world ((web-contents web-contents) world-id code
                                                 &key (user-gesture "false"))
  (message
   web-contents
   (format nil "~a.executeJavaScript(\"~a\", {code: `~a`}, ~a)"
           (remote-symbol web-contents) world-id (format-js-for-eval code) user-gesture)))

(export-always 'execute-javascript-with-promise-callback)
(defmethod execute-javascript-with-promise-callback
    ((web-contents web-contents) code callback &key (user-gesture "false"))
  (multiple-value-bind (thread-id socket-thread socket-path)
      (create-node-socket-thread (lambda (response)
                                   (apply callback (cons web-contents response)))
                                 :interface (interface web-contents))
    (message
     web-contents
     (format nil "~a.executeJavaScript(\"~a\", ~a).then((value) => {
                    jsonString = JSON.stringify([ value ]);
                    ~a.write(`${jsonString}\\n`);}).catch(error => {
                      ~a.write('[\"ERROR\"]\\n');});"
             (remote-symbol web-contents) (format-js-for-eval code) user-gesture
             thread-id thread-id))
    (values thread-id socket-thread socket-path)))

(export-always 'execute-javascript-synchronous)
(defmethod execute-javascript-synchronous ((web-contents web-contents) code
                                           &key (user-gesture "false"))
  (let ((p (lparallel:promise)))
    (multiple-value-bind (thread-id socket-thread socket-path)
        (execute-javascript-with-promise-callback
         web-contents
         code
         (lambda (web-contents result)
           (declare (ignore web-contents))
           (if (equal result "ERROR")
               (progn (warn "Renderer view error running JavaScript: ~%~a~%" code)
                      (lparallel:fulfill p result))
               (lparallel:fulfill p result)))
         :user-gesture user-gesture)
      (declare (ignore socket-path thread-id))
      (prog1 (lparallel:force p)
        ;; Kill server and connections spawned by
        ;; `execute-javascript-with-promise-callback'.
        (bt:destroy-thread socket-thread)))))
