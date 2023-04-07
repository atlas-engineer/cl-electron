#-(or sbcl ccl)
(warn "unsupported implementation, satisfaction uncertain!")

(defsystem "cl-electron"
  :version "0.0.0"
  :author "Atlas Engineer"
  :license ""
  :depends-on (:uiop
               :dexador)
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "initialize" :depends-on ("package")))))
  :description "Common Lisp interface to Electron.")
