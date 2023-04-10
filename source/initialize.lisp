(in-package :cl-electron)

(defun initialize ()
  (uiop:launch-program (list "electron" "start.js")))

