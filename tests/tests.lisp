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

(defun socket-connections-count ()
  (parse-integer
   (uiop:run-program (format nil "ss -x | grep ~a | wc -l"
                             (electron:sockets-directory electron:*interface*))
                     :output '(:string :stripped t)
                     :ignore-error-status t)))

(define-test sanitize-ipc-communication ()
  (with-electron-session
    (assert-number-equal
     (socket-connections-count)
     (progn
       (dotimes (n 2000)
         (electron:execute-javascript-synchronous (electron:web-contents win)
                                                  (ps:ps "hello world!")))
       (socket-connections-count)))
    (assert-number-equal
     (socket-connections-count)
     (let ((view (make-instance 'electron:view)))
       (electron:add-bounded-view win
                                  view
                                  :window-bounds-alist-var bounds
                                  :width (alexandria:assoc-value bounds :width))
       (electron:add-listener (electron:web-contents win) :did-finish-load
                              (lambda (web-contents) (declare (ignore web-contents)) t))
       (electron:add-listener win :before-input-event
                              (lambda (win input) (declare (ignore win input)) t))
       (electron:remove-view win view :kill-view-p t)
       (electron:kill (electron:web-contents win))
       (electron:kill win)
       (socket-connections-count))))

  ;; No dangling connections.
  (assert-number-equal 0 (socket-connections-count))
  ;; No dangling socket files.
  (assert-false (uiop:directory-files (electron:sockets-directory electron:*interface*))))
