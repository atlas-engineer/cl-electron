;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/demos)

(defun electron-protocol-demo ()
  (setf electron:*interface*
        (make-instance 'electron:interface)

        (electron:protocols electron:*interface*)
        (list (make-instance 'electron:protocol
                             :scheme-name "test"
                             :privileges "{}")))
  (electron:launch electron:*interface*)
  (electron:handle (find "test" (electron:protocols electron:*interface*)
                           :key #'electron:scheme-name :test #'string-equal)
                   "() => {return new Response('Hello test scheme.')}")
  (let ((win (make-instance 'electron:browser-window :options "{frame: false}"))
        (view1 (make-instance 'electron:browser-view))
        (view2 (make-instance 'electron:browser-view)))
    (electron:add-browser-view win view1)
    (electron:add-browser-view win view2)
    (electron:set-bounds view1 0 0 400 200)
    (electron:set-bounds view2 0 200 400 200)
    (electron:load-url view1 "test:dummy-var")
    (electron:load-url view2 "test:dummy-var")))
