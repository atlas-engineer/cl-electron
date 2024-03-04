;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/demos)

(define-class browser-view-bound-to-window (electron:browser-view)
  ((parent-window (error "Slot `parent-window' must be set."))))

(defmethod add-view ((view browser-view-bound-to-window)
                     &key (x 0) (y 0) (width 1000) (height 1000)
                          (width-p t) (height-p t) (horizontal-p t) (vertical-p t))
  (electron:add-browser-view (parent-window view) view)
  (electron:set-auto-resize view width-p height-p horizontal-p vertical-p)
  (electron:set-bounds view x y width height)
  view)

(define-class main-view (browser-view-bound-to-window)
  ((url nil)))

(defmethod initialize-instance :after ((main-view main-view) &key)
  (add-view main-view
            :width (electron:get-bounds (parent-window main-view) 'width)
            :height (- (electron:get-bounds (parent-window main-view) 'height)
                       30)
            :vertical-p nil)
  (electron:on-event (electron:web-contents main-view) "did-finish-load"
                     (lambda (web-contents) (setf (url main-view) (electron:get-url web-contents))))
  (electron:load-url main-view "https://en.wikipedia.org/wiki/Electron")
  (print (electron:execute-javascript-synchronous (electron:web-contents main-view)
                                                  "1 + 1")))

(define-class modeline (browser-view-bound-to-window) ())

(defmethod initialize-instance :after ((modeline modeline) &key)
  (electron:on (parent-window modeline) "resize"
    (format nil "~a.setBounds({x:      0,
                               y:      ~a.getBounds().height - 30,
                               width:  ~a.getBounds().width,
                               height: 30})"
            (electron::remote-symbol modeline)
            (electron::remote-symbol (parent-window modeline))
            (electron::remote-symbol (parent-window modeline))))
  (add-view modeline
            :y (- (electron:get-bounds (parent-window modeline) 'height) 30)
            :width (electron:get-bounds (parent-window modeline) 'width)
            :height 30
            :height-p nil
            :vertical-p nil)
  (electron:handle-callback (make-instance 'electron:protocol :scheme-name "lisp")
                            (lambda (url)
                              (declare (ignorable url))
                              "Caution: Made with secret alien technology"))
  (electron:load-url modeline "lisp:hello"))

(define-class prompt (browser-view-bound-to-window) ())

(defmethod initialize-instance :after ((prompt prompt) &key)
  (electron:on (parent-window prompt) "resize"
    (format nil "~a.setBounds({x:      0,
                               y:      Math.floor((~a.getBounds().height - 30) * 2/3),
                               width:  ~a.getBounds().width,
                               height: Math.ceil((~a.getBounds().height - 30) / 3)})"
            (electron::remote-symbol prompt)
            (electron::remote-symbol (parent-window prompt))
            (electron::remote-symbol (parent-window prompt))
            (electron::remote-symbol (parent-window prompt))))
  (add-view prompt
            :y (floor (* (- (electron:get-bounds (parent-window prompt) 'height) 30) 2/3))
            :width (electron:get-bounds (parent-window prompt) 'width)
            :height (ceiling (/ (- (electron:get-bounds (parent-window prompt) 'height) 30) 3))
            :height-p nil
            :vertical-p nil)
  (electron:set-background-color prompt "lightskyblue")
  (electron:load-url prompt "about:blank"))

(defun electron-views-demo ()
  (setf electron:*interface* (make-instance 'electron:interface))
  (electron:launch)
  (let ((win (make-instance 'electron:browser-window :options "{frame: false}")))
    (values (make-instance 'main-view :parent-window win)
            (make-instance 'modeline :parent-window win)
            (make-instance 'prompt :parent-window win))))
