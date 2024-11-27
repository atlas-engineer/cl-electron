;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/examples)

(defun electron-window-example ()
  (setf electron:*interface* (make-instance 'electron:interface))
  (electron:launch)
  (let ((win (make-instance 'electron:window)))
    (electron:load-url win "https://en.wikipedia.org/wiki/Electron")
    ;; Allow typing any character except "e".
    (electron:add-listener win :before-input-event
                           (lambda (win input) (declare (ignore win))
                             (print input)
                             (if (string-equal "e" (assoc-value input :key)) t nil)))
    win))
