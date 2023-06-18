;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :electron)

(defvar *host* "127.0.0.1")
(defvar *port* 3000)
(defvar *script* (uiop:read-file-string (asdf:system-relative-pathname :cl-electron "source/server.js")))
(defvar *electron-process* nil)
(defvar *socket-stream*)
(defvar *callbacks* (make-hash-table))
