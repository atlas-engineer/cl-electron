;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :electron/demo
  (:nicknames :cl-electron/demo))

(in-package :electron/demo)

(defun electron-protocol-demo ()
  (setf electron:*interface*
        (make-instance 'electron:interface)

        (electron:protocols electron:*interface*)
        (list (make-instance 'electron:protocol
                             :scheme-name "play"
                             :privileges "{stream:false}")
              (make-instance 'electron:protocol
                             :scheme-name "noplay"
                             :privileges "{stream:true}")))
  (electron:launch electron:*interface*)
  (electron:handle (find "noplay" (electron:protocols electron:*interface*)
                         :key #'electron:scheme-name :test #'string-equal)
                   "() => {return net.fetch('https://www.learningcontainer.com/wp-content/uploads/2020/02/Kalimba.mp3')}")
  (electron:handle (find "play" (electron:protocols electron:*interface*)
                         :key #'electron:scheme-name :test #'string-equal)
                   "() => {return net.fetch('https://www.learningcontainer.com/wp-content/uploads/2020/02/Kalimba.mp3')}")
  (let ((win (make-instance 'electron:browser-window :options "{frame: false}"))
        (view1 (make-instance 'electron:browser-view))
        (view2 (make-instance 'electron:browser-view)))
    (electron:add-browser-view win view1)
    (electron:add-browser-view win view2)
    (electron:set-bounds view1 0 0 400 200)
    (electron:set-bounds view2 0 200 400 200)
    (electron:load-url view1 "play:dummy-var")
    (electron:load-url view2 "noplay:dummy-var")))
