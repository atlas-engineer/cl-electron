;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/examples)

(defun electron-download-example ()
  (electron:launch)
  (let* ((win (make-instance 'electron:window))
         (session (electron:session (electron:web-contents win))))
    (electron:load-url win "https://github.com/atlas-engineer/nyxt/releases/")
    (electron:add-listener session :download-item-updated
                           (lambda (session item)
                             (declare (ignore session))
                             (unless (equal (electron:total-bytes item) 0)
                               (format t "~,1,2f%~%" (/ (electron:received-bytes item)
                                                        (electron:total-bytes item))))
                             (sleep 1)))
    ;; Call `electron:cancel' over an element of the hash table below to abort.
    (electron:download-items session)))
