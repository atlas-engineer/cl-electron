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
  (electron:load-url main-view "https://www.google.com"))

(defun electron-extension-example ()
  (electron:launch)
  (let* ((win (make-instance 'electron:window))
         (view (make-instance 'main-view :window win))
         (session (electron:session (electron:web-contents view))))
    (electron:load-extension session "/home/jmercouris/Source/Lisp/cl-electron/examples/test-extension/")
    (print (electron:get-all-extensions session))
    session))
