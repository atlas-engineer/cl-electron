;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Built-in Ghostery ad-blocking support.

(in-package :electron)

(define-class adblocker-electron (remote-object)
  ((reference-name
    "ElectronBlocker"
    :documentation
    "The name to use when importing the Library into the Global
 namespace."))
  (:export-class-name-p t)
  (:export-predicate-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Interface with an Electron instance."))

(defmethod initialize-instance :after ((adblocker adblocker-electron) &key)
  (message
   adblocker
   (format nil "const fetch = require('cross-fetch');
global.~a = require('@ghostery/adblocker-electron').ElectronBlocker;"
           (reference-name adblocker))))

(export-always 'default-block)
(defmethod default-block ((adblocker adblocker-electron))
  (message
   adblocker
   (format
    nil
    "global.~a.fromPrebuiltAdsAndTracking(fetch).then((blocker) => {
blocker.enableBlockingInSession(session.defaultSession);
});" (reference-name adblocker))))
