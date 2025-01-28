;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/examples)

(define-class example-view (electron:view)
  ((url nil)))

(defmethod initialize-instance :after ((main-view example-view) &key window)
  (electron:add-bounded-view window
                             main-view
                             :window-bounds-alist-var bounds
                             :x 0
                             :y 0
                             :width (assoc-value bounds :width)
                             :height (- (assoc-value bounds :height) 30))
  (electron:load-url main-view "https://en.wikipedia.org/wiki/Electron")
  (print (electron:execute-javascript-synchronous (electron:web-contents main-view)
                                                  "1 + 1")))

(defun electron-web-preferences-example ()
  (electron:launch)
  ;; Note: WebPreferences can only be set during object creation!
  (let ((win (make-instance 'electron:window)))
    (make-instance 'example-view :window win :options "{webPreferences: {images: false}}")
    win))
