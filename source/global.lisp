;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :cl-electron)

(defvar *electron-socket-path* (uiop:xdg-runtime-dir "electron.socket"))
(defvar *lisp-socket-path* (uiop:xdg-runtime-dir "lisp.socket"))
(defvar *script* (uiop:read-file-string (asdf:system-relative-pathname :cl-electron "source/server.js")))
(defvar *electron-process* nil)
(defvar *lisp-server-process* nil)
(defvar *socket-stream*)
(defvar *callbacks* (make-hash-table))
