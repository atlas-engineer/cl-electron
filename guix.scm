;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;;
;; Follow the instruction available at README.org to setup Guix
;; channels.
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix shell -D -f guix.scm
;;
;;; Code:

(use-modules (guix packages)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (guix git-download)
             (guix build-system asdf)
             (gnu packages)
             (gnu packages lisp)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz)
             (gnu packages node)
             (nongnu packages electron))

(define-public sbcl-cl-electron
  (package
   (name "sbcl-cl-electron")
   (version "0.0.0")
   (source
    (local-file (dirname (current-filename)) #:recursive? #t))
   (build-system asdf-build-system/sbcl)
   (native-inputs (list cl-lisp-unit2 sbcl))
   (inputs (list electron node
                 cl-json cl-iolib cl-nclasses cl-parenscript cl-bordeaux-threads))
   (synopsis "Common Lisp interface to Electron")
   (home-page "https://github.com/atlas-engineer/cl-electron")
   (description "@command{cl-electron} is a binding to Electron for
Common Lisp.")
   (license license:bsd-3)))

(define-public cl-electron
  (sbcl-package->cl-source-package sbcl-cl-electron))

(define-public ecl-cl-electron
  (sbcl-package->ecl-package sbcl-cl-electron))

cl-electron
