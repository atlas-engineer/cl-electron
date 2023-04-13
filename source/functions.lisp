(in-package :cl-electron)


(defun web-contents-download-url (id url)
  (send-message
   (format nil "~a.downloadURL(\"~a\")" id url)))

(defun web-contents-set-zoom-factor (id factor)
  (send-message
   (format nil "~a.setZoomFactor(\"~f\")" id factor)))

(defun web-contents-get-zoom-factor (id)
  (send-message
   (format nil "~a.getZoomFactor()" id)))

(defun web-contents-set-zoom-level (id level)
  (send-message
   (format nil "~a.setZoomLevel(~a)" id level)))

(defun web-contents-get-zoom-level (id)
  (send-message
   (format nil "~a.getZoomLevel()" id)))

(defun web-contents-set-audio-muted (id muted)
  (send-message
   (format nil "~a.setAudioMuted(~a)"
           id
           (if muted
               "true"
               "false"))))

(defun web-contents-is-audio-muted (id)
  (send-message
   (format nil "~a.isAudioMuted()" id)))

(defun web-contents-set-user-agent (id user-agent)
  (send-message
   (format nil "~a.setUserAgent(\"~a\")" id user-agent)))

(defun web-contents-get-user-agent (id)
  (send-message
   (format nil "~a.getUserAgent()" id)))

(defun web-contents-load-url (id url)
  (send-message
   (format nil "~a.loadURL(\"~a\")" id url)))

(defun web-contents-load-file (id path)
  (send-message
   (format nil "~a.loadFile(\"~a\")" id path)))

(defun web-contents-get-url (id)
  (send-message
   (format nil "~a.getURL()" id)))

(defun web-contents-open-dev-tools (id)
  (send-message
   (format nil "~a.openDevTools()" id)))

(defun web-contents-close-dev-tools (id)
  (send-message
   (format nil "~a.closeDevTools()" id)))

(defun web-contents-get-title (id)
  (send-message
   (format nil "~a.getTitle()" id)))

(defun web-contents-focus (id)
  (send-message
   (format nil "~a.focus()" id)))

(defun web-contents-is-focused (id)
  (send-message
   (format nil "~a.isFocused()" id)))

(defun web-contents-undo (id)
  (send-message
   (format nil "~a.undo()" id)))

(defun web-contents-redo (id)
  (send-message
   (format nil "~a.redo()" id)))

(defun web-contents-cut (id)
  (send-message
   (format nil "~a.cut()" id)))

(defun web-contents-copy (id)
  (send-message
   (format nil "~a.copy()" id)))

(defun web-contents-paste (id)
  (send-message
   (format nil "~a.paste()" id)))

(defun web-contents-select-all (id)
  (send-message
   (format nil "~a.selectAll()" id)))

(defun web-contents-close (id)
  (send-message
   (format nil "~a.close()" id)))

(defun web-contents-insert-css (id css)
  (send-message
   (format nil "~a.insertCSS(\"~a\")" id css)))

(defun web-contents-remove-inserted-css (id key)
  (send-message
   (format nil "~a.removeInsertedCSS(\"~a\")" id key)))

(defun web-contents-session (id)
  (let ((new-id (new-id)))
    (send-message
     (format nil "~a = ~a.session" new-id id))
    new-id))
