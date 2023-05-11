#-(or sbcl ccl)
(warn "unsupported implementation, satisfaction uncertain!")

(defsystem "cl-electron"
  :version "0.0.0"
  :author "Atlas Engineer LLC"
  :description "Common Lisp interface to Electron."
  :license "BSD 3-Clause"
  :depends-on (uiop
               cl-json
               usocket
               parenscript
               bordeaux-threads)
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "global" :depends-on ("package"))
                 (:file "core" :depends-on ("package" "global"))
                 (:file "browser-window" :depends-on ("package" "core"))
                 (:file "browser-view" :depends-on ("package" "core"))
                 (:file "web-contents" :depends-on ("package" "core"))))))
