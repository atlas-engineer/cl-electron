;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :electron/demo
  (:nicknames :cl-electron/demo)
  (:import-from :nclasses :define-class))

(in-package :electron/demo)

(define-class browser-view-bound-to-window (electron:browser-view)
  ((parent-window (error "Slot `parent-window' must be set."))))

(defmethod add-view ((view browser-view-bound-to-window)
                     &key (x 0) (y 0) (width 1000) (height 1000)
                          (width-p t) (height-p t) (horizontal-p t) (vertical-p t))
  (electron:add-browser-view (parent-window view) view)
  (electron:set-auto-resize view width-p height-p horizontal-p vertical-p)
  (electron:set-bounds view x y width height)
  view)

(define-class main-view (browser-view-bound-to-window) ())

(defmethod initialize-instance :after ((main-view main-view) &key)
  (add-view main-view
            :width (electron:get-bounds (parent-window main-view) 'width)
            :height (- (electron:get-bounds (parent-window main-view) 'height)
                       30)
            :vertical-p nil)
  (electron:load-url main-view "https://en.wikipedia.org/wiki/Electron"))

(define-class modeline (browser-view-bound-to-window) ())

(defmethod initialize-instance :after ((modeline modeline) &key)
  (add-view modeline
            :y (+ (electron:get-bounds (parent-window modeline) 'y)
                  (- (electron:get-bounds (parent-window modeline) 'height) 30))
            :width (electron:get-bounds (parent-window modeline) 'width)
            :height 30
            :height-p nil
            :vertical-p nil)
  (electron::handle-callback (make-instance 'electron:protocol :scheme-name "lisp")
                             (lambda (url)
                               (declare (ignorable url))
                               "Caution: Made with secret alien technology"))
  (electron:load-url modeline "lisp:hello"))

(define-class prompt (browser-view-bound-to-window) ())

(defmethod initialize-instance :after ((prompt prompt) &key)
  (add-view prompt
            :y (+ (electron:get-bounds (parent-window prompt) 'y)
                  (floor (* (- (electron:get-bounds (parent-window prompt) 'height) 30) 2/3)))
            :width (electron:get-bounds (parent-window prompt) 'width)
            :height (ceiling (/ (- (electron:get-bounds (parent-window prompt) 'height) 30) 3))
            :height-p nil
            :vertical-p nil)
  (electron:set-background-color prompt "lightskyblue")
  (electron:load-url prompt "about:blank"))

(defun electron-views-demo ()
 (setf electron:*interface* (make-instance 'electron:interface))
  (electron:launch)
  ;; See https://github.com/atlas-engineer/cl-electron/issues/15
  (sleep 1)
  (let ((win (make-instance 'electron:browser-window :options "{frame: false}")))
    (make-instance 'main-view :parent-window win)
    (make-instance 'modeline :parent-window win)
    (make-instance 'prompt :parent-window win)
    t))
