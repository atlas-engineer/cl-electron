;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;;
;; Follow the instruction available at README.org to setup Guix
;; channels.
;;
;; GNU Guix development package.  To start the REPL:
;;
;;   guix shell -D -f path/to/guix.scm sbcl -- sbcl
;;
;;; Code:

(use-modules (guix packages)
             (guix gexp)
             (gnu packages node)
             (gnu packages lisp-xyz)
             (nongnu packages electron)
             (nongnu packages lisp))

(package
  (inherit cl-electron)
  (version "dev")
  (inputs (list electron node
                sbcl-cl-json sbcl-iolib sbcl-cl-str sbcl-nclasses
                sbcl-cl-ppcre sbcl-bordeaux-threads sbcl-lparallel
                sbcl-parenscript sbcl-spinneret))
  (source (local-file (dirname (current-filename)) #:recursive? #t)))
