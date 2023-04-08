#-(or sbcl ccl)
(warn "unsupported implementation, satisfaction uncertain!")

(defsystem "cl-electron"
  :version "0.0.0"
  :author "Atlas Engineer"
  :license ""
  :depends-on (:uiop
               :usocket
               :parenscript)
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "global" :depends-on ("package"))
                 (:file "initialize" :depends-on ("package"))
                 (:file "functions" :depends-on ("global")))))
  :description "Common Lisp interface to Electron.")
