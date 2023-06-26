;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package #:cl-electron
  (:nicknames :electron)
  (:use :cl)
  (:import-from :nclasses :define-class))

(trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria :cl-electron)

(in-package :electron)
(defmacro export-always (symbols &optional (package nil package-supplied?))
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))
