;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/examples)

(defun electron-protocol-example ()
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
  (let ((win (make-instance 'electron:window))
        (view1 (make-instance 'electron:view))
        (view2 (make-instance 'electron:view)))
    (electron:add-view win view1)
    (electron:add-view win view2)
    (electron:set-bounds view1 :x 0 :y 0   :width 400 :height 200)
    (electron:set-bounds view2 :x 0 :y 200 :width 400 :height 200)
    (electron:load-url view1 "test:dummy-var")
    (electron:load-url view2 "test:dummy-var")))
