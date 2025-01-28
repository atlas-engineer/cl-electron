(uiop:define-package :electron/examples
  (:nicknames :cl-electron/examples)
  (:import-from :nclasses :define-class)
  (:import-from :alexandria :assoc-value))

(setf electron:*interface* (make-instance 'electron:interface))
