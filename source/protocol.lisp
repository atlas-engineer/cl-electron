;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron module to register a custom protocol and intercept existing
;;;; protocol requests.

(in-package :electron)

(export-always 'handle)
(defmethod handle ((protocol protocol) handler)
  (send-message-interface
   (interface protocol)
   (format nil "protocol.handle('~a', ~a)" (scheme-name protocol) handler)
   :replace-newlines-p nil))

(export-always 'handle-content)
(defmethod handle-content ((protocol protocol) content)
  (handle protocol (format nil "() => {return new Response('~a')}" content)))

(export-always 'handle-callback)
(defmethod handle-callback ((protocol protocol) callback)
  (let ((identifier (new-integer-id)))
    (setf (gethash identifier (callbacks electron:*interface*))
          (lambda (args)
            (cl-json:encode-json-to-string
             `((callback . ,identifier)
               ,@(multiple-value-bind (data-string data-type) (apply callback args)
                   (list `(data-string . ,data-string)
                         `(data-type . ,(or data-type "text/html;charset=utf8"))))))))
    (handle protocol
            (format nil
"(request) => {
    return new Promise((resolve, reject) => {
            jsonString = JSON.stringify({ callback: ~a, request: request.url });
            client.write(`${jsonString}\\n`);
            client.on('data', data => {
                const jsonObject = JSON.parse(data.toString());
                if (jsonObject.callback == ~a) {
                   const newResponse = new Response(jsonObject.dataString, {
                     headers: { 'content-type': jsonObject.dataType }
                   });
                   resolve(newResponse);
                }
            });
    });
}" identifier identifier))))
