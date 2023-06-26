;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package #:electron
  (:nicknames :cl-electron)
  (:use :cl)
  (:import-from :nclasses :define-class))

(in-package :electron)
(defmacro export-always (symbols &optional (package nil package-supplied?))
  "Like `export', but also evaluated at compile time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (export ,symbols ,@(and package-supplied? (list package)))))
