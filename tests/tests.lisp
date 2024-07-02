;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :electron/tests
  (:nicknames :cl-electron/tests)
  (:use :cl :lisp-unit2))

(in-package :electron/tests)

(defmacro with-electron-session (&body body)
  `(progn
     (setf electron:*interface*
           (make-instance 'electron:interface))
     (electron:launch)
     (let ((win (make-instance 'electron:browser-window :options "{show: false}")))
       (electron:load-url win "https://en.wikipedia.org/wiki/Electron")
       ,@body
       (electron:kill win))
     (electron:terminate)))

(define-test js-handling-single-quote ()
  (with-electron-session
    (let ((js (ps:ps "a'b'")))
      (assert-string= (electron:execute-javascript-synchronous (electron:web-contents win) js)
                      (electron::message (electron:interface win) js)))))

(define-test js-handling-double-quote ()
  (with-electron-session
    (let ((js (ps:ps "a\"b\"")))
      (assert-string= (electron:execute-javascript-synchronous (electron:web-contents win) js)
                      (electron::message (electron:interface win) js)))))

(define-test js-handling-multi-line ()
  (with-electron-session
    (let ((js (ps:ps ((lambda (x y) (+ x y)) 2 2))))
      (assert-eq (electron:execute-javascript-synchronous (electron:web-contents win) js)
                 (parse-integer (electron::message (electron:interface win) js))))))

(define-test js-handling-doc ()
  (with-electron-session
    (let ((html (spinneret:with-html-string (:head) (:body (:raw "a'b'")))))
      (assert-string=
       html
       (electron:execute-javascript-synchronous
        (electron:web-contents win)
        (ps:ps (setf (ps:chain document (get-elements-by-tag-name "html") 0 |innerHTML|)
                     (ps:lisp html))))))
    (let ((html (spinneret:with-html-string (:head) (:body (:raw "a\"b\"")))))
      (assert-string=
       html
       (electron:execute-javascript-synchronous
        (electron:web-contents win)
        (ps:ps (setf (ps:chain document (get-elements-by-tag-name "html") 0 |innerHTML|)
                     (ps:lisp html))))))))
