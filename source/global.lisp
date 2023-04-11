(in-package :cl-electron)

(defvar *host* "127.0.0.1")
(defvar *port* 3000)
(defvar *script* (uiop:read-file-string "start.js"))
(defvar *electron-process* nil)
