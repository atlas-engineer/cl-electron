;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron web preferences object.

(in-package :electron)

(define-class web-preferences ()
  ((properties
    (make-hash-table :test 'equal)
    :type (or hash-table null)
    :documentation "The table of properties set within the web-preferences object. This value will
be encoded into JSON for creating WebContentsViews."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "This object represents the Electron analog of a WebPreferences object. It is
used to specify the behavior of a WebContentsView."))

(export-always 'add-preference)
(defmethod add-preference ((web-preferences web-preferences) preference value)
  (setf (gethash preference (properties web-preferences)) value))

(export-always 'remove-preference)
(defmethod remove-preference ((web-preferences web-preferences) preference)
  (remhash preference (properties web-preferences)))

(export-always 'encode-json)
(defmethod encode-json ((web-preferences web-preferences))
  (cl-json:encode-json-to-string (properties web-preferences)))
