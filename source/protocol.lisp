;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron module to register a custom protocol and intercept existing
;;;; protocol requests.

(in-package :electron)

(export-always 'handle)
(defmethod handle ((protocol protocol) handler)
  (message
   protocol
   (format nil "protocol.handle('~a', ~a)" (scheme-name protocol) handler)))

(export-always 'handle-content)
(defmethod handle-content ((protocol protocol) content)
  (handle protocol (format nil "() => {return new Response('~a')}" content)))

(export-always 'handle-callback)
(defmethod handle-callback ((protocol protocol) callback)
  (let ((socket-thread-id
          (create-node-socket-thread
           (lambda (url)
             (cl-json:encode-json-to-string
              (multiple-value-bind (data-string data-type) (funcall callback url)
                (list (cons "dataString"
                            (typecase data-string
                              ((simple-array (unsigned-byte 8))
                               (cl-base64:usb8-array-to-base64-string data-string))
                              (string
                               (cl-base64:string-to-base64-string data-string))
                              (null "")))
                      (cons "dataType" (or data-type "text/html;charset=utf8"))))))
           :interface (interface protocol))))
    (handle protocol
            (format nil
"(request) => {
    return new Promise((resolve, reject) => {
        ~a.write(`${JSON.stringify(request.url)}\\n`);
        new ProtocolSocket(~a, data => {
            const jsonObject = JSON.parse(data);
            const _buffer = Buffer.from(jsonObject.dataString, 'base64');
            const newResponse = new Response(_buffer, {
                headers: { 'content-type': jsonObject.dataType }
            });
            resolve(newResponse);
        });
    });
}" socket-thread-id socket-thread-id))))
