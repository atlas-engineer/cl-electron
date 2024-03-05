;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#-(or sbcl ccl)
(warn "unsupported implementation, satisfaction uncertain!")

(defsystem "cl-electron"
  :version "0.0.0"
  :author "Atlas Engineer LLC"
  :description "Common Lisp interface to Electron."
  :license "BSD 3-Clause"
  :depends-on (uiop
               cl-json
               iolib
               iolib/os
               str
               cl-ppcre
               nclasses
               parenscript
               bordeaux-threads
               lparallel)
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "condition" :depends-on ("package"))
                 (:file "core" :depends-on ("package"))
                 (:file "browser-window" :depends-on ("package" "core"))
                 (:file "browser-view" :depends-on ("package" "core"))
                 (:file "web-contents" :depends-on ("package" "core"))
                 (:file "protocol" :depends-on ("package" "core"))))))

(defsystem "cl-electron/demos"
  :pathname "demos"
  :depends-on (cl-electron)
  :components ((:file "package")
               (:file "demo-window")
               (:file "demo-views")
               (:file "demo-protocol")))
