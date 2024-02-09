;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :electron/demo
  (:nicknames :cl-electron/demo))

(in-package :electron/demo)

(defun electron-window-demo ()
 (setf electron:*interface* (make-instance 'electron:interface))
  (electron:launch)
  ;; See https://github.com/atlas-engineer/cl-electron/issues/15
  (sleep 1)
  (let ((win (make-instance 'electron:browser-window)))
    (electron:load-url win "https://en.wikipedia.org/wiki/Electron")
    (electron::register-before-input-event win
                                           (lambda (win input)
                                             (declare (ignore win))
                                             (print input)))
    win))
