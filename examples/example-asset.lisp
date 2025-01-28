;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron/examples)

(defun electron-asset-example ()
  (setf (electron:protocols electron:*interface*)
        (list (make-instance 'electron:protocol
                             :scheme-name "test"
                             :privileges "{}")
              (make-instance 'electron:protocol
                             :scheme-name "font"
                             :privileges "{secure:true}")))
  (electron:launch electron:*interface*)
  (let* ((protocols (electron:protocols electron:*interface*))
         (font-path (asdf:system-relative-pathname :cl-electron
                                                   "examples/dejavu-sans-mono.ttf"))
         (font (alexandria:read-file-into-byte-vector font-path)))
    (electron:handle (find "test" protocols
                           :key #'electron:scheme-name :test #'string-equal)
                     "() => {return new Response('')}")
    (electron:handle-callback (find "font" protocols
                                    :key #'electron:scheme-name :test #'string-equal)
                              (lambda (_) (declare (ignore _)) font)))
  (let ((win (make-instance 'electron:window)))
    (electron:load-url win "test:dummy-var")
    (electron:execute-javascript-synchronous
     (electron:web-contents win)
     (ps:ps (setf (ps:chain document (get-elements-by-tag-name "html") 0 |innerHTML|)
                  (ps:lisp (spinneret:with-html-string
                             (:head
                              (:style
                               (:raw
                                (lass:compile-and-write
                                 '(:font-face
                                   :font-family "dejavu sans mono"
                                   :src "url('font:dummy-var')")
                                 '(p
                                   :font-family "dejavu sans mono")))))
                             (:body (:p "Hello world")))))))
    win))
