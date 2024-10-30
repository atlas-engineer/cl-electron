;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/examples)

(defun electron-window-example ()
  (setf electron:*interface* (make-instance 'electron:interface))
  (electron:launch)
  (let ((win (make-instance 'electron:window)))
    (electron:load-url win "https://en.wikipedia.org/wiki/Electron")
    ;; Allow typing any character except "e".
    (electron:register-before-input-event
     win
     (lambda (win input)
       (declare (ignore win))
       (print input)
       (if (string-equal "e" (cdr (assoc :key input)))
           t
           nil)))
    win))
