;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Electron module to register a custom protocol and intercept existing
;;;; protocol requests.

(in-package :electron)

(export-always 'handle)
(defmethod handle ((protocol protocol) handler)
  (send-message-interface
   (interface protocol)
   (format nil "protocol.handle('~a', ~a)" (scheme-name protocol) handler)))

(export-always 'handle-content)
(defmethod handle-content ((protocol protocol) content)
  (handle protocol (format nil "() => {return new Response('~a')}" content)))

(export-always 'handle-callback)
(defmethod handle-callback ((protocol protocol) callback)
  (let ((socket-thread-id
          (create-node-socket-thread
           (lambda (args)
            (cl-json:encode-json-to-string
             (multiple-value-bind (data-string data-type) (apply callback args)
               (list (cons "dataString" data-string)
                     (cons "dataType" (or data-type "text/html;charset=utf8")))))))))
    (handle protocol
            (format nil
 "(request) => {
    return new Promise((resolve, reject) => {
        jsonString = JSON.stringify([ request.url ]);
        ~a.write(`${jsonString}\\n`);
        new ProtocolSocket(~a, data => {
            const jsonObject = JSON.parse(data);
            const newResponse = new Response(jsonObject.dataString, {
                headers: { 'content-type': jsonObject.dataType }
            });
            resolve(newResponse);
        });
    });
}" socket-thread-id socket-thread-id))))
