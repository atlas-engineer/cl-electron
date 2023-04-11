(in-package :cl-electron)

(defun launch ()
  (setf *electron-process*
        (uiop:launch-program (list "electron" "start.js"))))

(defun terminate ()
  (when (uiop:process-alive-p *electron-process*)
    (uiop:terminate-process *electron-process*)))

