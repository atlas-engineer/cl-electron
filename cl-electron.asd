;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#-(or sbcl ccl)
(warn "unsupported implementation, satisfaction uncertain!")

(defsystem "cl-electron"
  :version "0.0.0"
  :author "Atlas Engineer LLC"
  :description "Common Lisp interface to Electron."
  :license "BSD 3-Clause"
  :depends-on (alexandria
               uiop
               cl-json
               cl-base64
               iolib
               iolib/os
               cl-ppcre
               nclasses
               bordeaux-threads
               lparallel
               parse-number
               babel)
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "core" :depends-on ("package"))
                 (:file "window" :depends-on ("package" "core"))
                 (:file "view" :depends-on ("package" "core"))
                 (:file "web-contents" :depends-on ("package" "core"))
                 (:file "session" :depends-on ("package" "core"))
                 (:file "protocol" :depends-on ("package" "core")))))
  :in-order-to ((test-op (test-op "cl-electron/tests"))))

(defsystem "cl-electron/tests"
  :pathname "tests"
  :depends-on (cl-electron lisp-unit2 spinneret parenscript)
  :components ((:file "tests"))
  :perform (test-op (op c)
                    (eval-input
                     "(lisp-unit2:run-tests
                       :package :electron/tests
                       :run-contexts #'lisp-unit2:with-summary-context)")))

(defsystem "cl-electron/examples"
  :pathname "examples"
  :depends-on (cl-electron lass spinneret)
  :components ((:file "package")
               (:file "example-window")
               (:file "example-download")
               (:file "example-views")
               (:file "example-web-preferences")
               (:file "example-protocol")
               (:file "example-asset")))
