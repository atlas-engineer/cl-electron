;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron)

(define-condition javascript-renderer-error (error)
  ((code :initarg :code :initform nil :reader code))
  (:documentation "A condition for JavaScript errors within the renderer view."))
