;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/examples)

(define-class main-view (electron:view)
  ((url nil)))

(defmethod initialize-instance :after ((main-view main-view) &key window)
  (electron:add-bounded-view window
                             main-view
                             :window-bounds-alist-var bounds
                             :x 0
                             :y 0
                             :width (assoc-value bounds :width)
                             :height (assoc-value bounds :height))
  (electron:load-url main-view "https://www.example.com"))

(defun electron-context-menu-example ()
  (electron:launch)
  (let* ((win (make-instance 'electron:window))
         (view (make-instance 'main-view :window win)))
    (electron:add-listener view :context-menu
                           (lambda (object params)
                             (declare (ignore object))
                             (print params)
                             "[{label: 'Custom Action',
                                click: () => {
                                  console.log('Custom action clicked');}},
                               {type: 'separator'},]"))))
