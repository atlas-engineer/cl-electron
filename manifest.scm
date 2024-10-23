;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;;
;; It doesn't handle Node.js since it expects an FHS-compliant file system.
;;
;; GNU Guix development manifest.  To start the CL REPL:
;;
;;   guix shell -m path/to/manifest.scm -- sbcl
;;
;;; Code:

(use-package-modules lisp lisp-xyz)

(packages->manifest
 (list sbcl
       cl-bordeaux-threads
       cl-iolib
       cl-lparallel
       cl-nclasses
       cl-parenscript
       cl-ppcre
       cl-spinneret
       cl-json))
