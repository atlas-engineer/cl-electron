;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; Electron session object.

(in-package :electron)

(export-always 'default-session)
(defmethod default-session ((interface interface))
  (let ((new-id (new-id)))
    (message
     interface
     (format nil "~a = session.defaultSession" new-id))
    (make-instance 'session
                   :remote-symbol new-id
                   :interface interface)))

(defmethod add-listener ((session session)
                         (event (eql :download-item-updated))
                         (callback function)
                         &key once-p)
  ;; Callback runs on the updated and done instance events of DownloadItem.
  (declare (ignore once-p))
  (let ((socket-id (create-node-socket-thread
                    (lambda (result)
                      (if (equal "new" (assoc-value result :state))
                          (funcall callback
                                   session
                                   (setf (gethash (assoc-value result :id)
                                                  (download-items session))
                                         (make-instance
                                          'download-item
                                          :remote-symbol (assoc-value result :id))))
                          (let ((download-item (gethash (assoc-value result :id)
                                                        (download-items session))))
                            (setf (url download-item)
                                  (assoc-value result :url)

                                  (state download-item)
                                  (assoc-value result :state)

                                  (received-bytes download-item)
                                  (assoc-value result :received-bytes)

                                  (percent-complete download-item)
                                  (assoc-value result :percent-complete)

                                  (total-bytes download-item)
                                  (assoc-value result :total-bytes))
                            (funcall callback session download-item))))
                    :interface (interface session))))
    (message
     session
     (format-listener session
                      :will-download
                      (format nil
                              "(event, item, webContents) => {
                                 var id = uid();
                                 GLOBALS[id] = item;
                                 ~a.write(JSON.stringify({'id': id,
                                                          'state': 'new'}) + '\\\n');
                                 item.on('updated', (event, state) => {
                                   ~a.write(JSON.stringify({'id': id,
                                                            'url': item.getURL(),
                                                            'receivedBytes': item.getReceivedBytes(),
                                                            'totalBytes': item.getTotalBytes(),
                                                            'percentComplete': item.getPercentComplete(),
                                                            'state': state}) + '\\\n');
                                 })
                                 item.once('done', (event, state) => {
                                   ~a.write(JSON.stringify({'id': id,
                                                            'url': item.getURL(),
                                                            'receivedBytes': item.getReceivedBytes(),
                                                            'totalBytes': item.getTotalBytes(),
                                                            'percentComplete': item.getPercentComplete(),
                                                            'state': state}) + '\\\n');
                                 })
                               }"
                              socket-id
                              socket-id
                              socket-id)))))

(export-always 'cancel)
(defmethod cancel ((download-item download-item))
  (message
   download-item
   (format nil "GLOBALS['~a'].cancel();" (remote-symbol download-item))))

(export-always 'load-extension)
(defmethod load-extension ((session session) path &key (options "{}"))
  (message
   session
   (format nil "~a.loadExtension('~a', ~a)" (remote-symbol session) path options)))

(export-always 'get-all-extensions)
(defmethod get-all-extensions ((session session))
  (message
   session
   (format nil "~a.getAllExtensions()" (remote-symbol session))))

