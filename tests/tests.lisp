;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :electron/tests
  (:nicknames :cl-electron/tests)
  (:use :cl :lisp-unit2))

(in-package :electron/tests)

(setf electron:*interface* (make-instance 'electron:interface))

(defmacro with-electron-session (&body body)
  `(progn
     (electron:launch)
     (let ((win (make-instance 'electron:window :options "{show: false}")))
       (electron:load-url win "https://en.wikipedia.org/wiki/Electron")
       ,@body
       ;; To allow body to be computed.
       (sleep 0.1))
     (electron:terminate)
     ;; So that chaining electron:launch and electron:terminate is safe.
     (sleep 0.1)))

(define-test launch-terminate-idempotent ()
  (with-electron-session
    (with-slots ((original-process electron:process)) electron:*interface*
      (assert-warning 'simple-warning (assert-false (electron:launch)))
      (assert-eq original-process
                 (electron:process electron:*interface*))))
  (assert-warning 'simple-warning (assert-false (electron:terminate))))

(define-test dangling-server-socket-on-launch ()
  (open (electron:server-socket-path electron:*interface*) :if-does-not-exist :create)
  (assert-warning 'simple-warning (with-electron-session t)))

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
    (electron:add-listener win :before-input-event
                           (lambda (win input) (declare (ignore win)) (print input) t))
    (electron:add-listener (electron:web-contents win) :did-finish-load
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
