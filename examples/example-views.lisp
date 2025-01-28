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
                             :height (- (assoc-value bounds :height) 30))
  (electron:add-listener (electron:web-contents main-view) :did-finish-load
                         (lambda (web-contents)
                           (setf (url main-view) (electron:get-url web-contents))))
  (electron:load-url main-view "https://en.wikipedia.org/wiki/Electron")
  (print (electron:execute-javascript-synchronous (electron:web-contents main-view)
                                                  "1 + 1")))

(define-class modeline (electron:view) ())

(defmethod initialize-instance :after ((modeline modeline) &key window)
  (electron:add-bounded-view window
                             modeline
                             :window-bounds-alist-var bounds
                             :x 0
                             :y (- (assoc-value bounds :height) 30)
                             :width (assoc-value bounds :width)
                             :height 30)
  (electron:handle-callback (make-instance 'electron:protocol :scheme-name "lisp")
                            (lambda (url)
                              (declare (ignorable url))
                              "Caution: Made with secret alien technology"))
  (electron:load-url modeline "lisp:hello"))

(define-class prompt (electron:view) ())

(defmethod initialize-instance :after ((prompt prompt) &key window)
  (electron:add-bounded-view window
                             prompt
                             :window-bounds-alist-var bounds
                             :x 0
                             :y (floor (* (- (assoc-value bounds :height) 30) 2/3))
                             :width (assoc-value bounds :width)
                             :height (ceiling (/ (- (assoc-value bounds :height) 30) 3)))
  (electron:set-background-color prompt "lightskyblue")
  (electron:load-url prompt "about:blank"))

(defun electron-views-example ()
  (electron:launch)
  (let ((win (make-instance 'electron:window)))
    (make-instance 'main-view :window win)
    (make-instance 'modeline :window win)
    (make-instance 'prompt :window win)
    win))
