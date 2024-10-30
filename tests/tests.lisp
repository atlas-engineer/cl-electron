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
     (let ((win (make-instance 'electron:window :options "{show: false}")))
       (electron:load-url win "https://en.wikipedia.org/wiki/Electron")
       ,@body
       (electron:kill win))
     (electron:terminate)))

(define-test js-handling-quotes ()
  (with-electron-session
    (let ((js (ps:ps "a'b'c\"d\"e`f`g")))
      (assert-string= (electron:execute-javascript-synchronous (electron:web-contents win) js)
                      (electron::message (electron:interface win) js)))))

(define-test js-handling-multi-line ()
  (with-electron-session
    (let ((js (ps:ps ((lambda (x y) (+ x y)) 2 2))))
      (assert-eq (electron:execute-javascript-synchronous (electron:web-contents win) js)
                 (parse-integer (electron::message (electron:interface win) js))))))

(define-test js-handling-doc ()
  (with-electron-session
    (let ((html (spinneret:with-html-string (:head) (:body (:raw "a'b'c\"d\"e`f`g")))))
      (assert-string=
       html
       (electron:execute-javascript-synchronous
        (electron:web-contents win)
        (ps:ps (setf (ps:chain document (get-elements-by-tag-name "html") 0 |innerHTML|)
                     (ps:lisp html))))))))

(define-test sanitize-ipc-communication ()
  (with-electron-session
    (electron:register-before-input-event win
                                          (lambda (win input)
                                            (declare (ignore win))
                                            (print input) t))
    (electron:on-event (electron:web-contents win)
                       "did-finish-load"
                       (lambda (web-contents) (declare (ignore web-contents)) t))
    (dotimes (n 2000)
      (electron:execute-javascript-synchronous (electron:web-contents win)
                                               (ps:ps "hello world!"))))
  (let ((dir (electron:sockets-directory electron:*interface*)))
    ;; No dangling connections.
    (assert-true (uiop:emptyp (uiop:run-program (format nil "ss -ax | grep ~a" dir)
                                                :output '(:string :stripped t)
                                                :ignore-error-status t)))
    ;; No dangling socket files.
    (assert-false (uiop:directory-files dir))))
