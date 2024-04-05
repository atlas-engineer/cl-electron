;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/demos)

(defun electron-window-demo ()
  (setf electron:*interface* (make-instance 'electron:interface))
  (electron:launch)
  (let ((win (make-instance 'electron:browser-window)))
    (electron:load-url win "https://en.wikipedia.org/wiki/Electron")
    ;; Allow typing any character except "e".
    (electron:register-before-input-event
     win
     (lambda (win input)
       (declare (ignore win))
       (print input)
       (if (equal "e" (cdr (assoc :key input)))
           t
           nil)))
    win))
