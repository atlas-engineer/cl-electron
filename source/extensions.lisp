;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; Electron extensions object.

(in-package :electron)

(export-always 'load-extension)
(defmethod load-extension ((extensions extensions) path &key (options "{}"))
  (message
   extensions
   (format nil "~a.loadExtension('~a', ~a)" (remote-symbol extensions) path options)))
