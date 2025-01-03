;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; Electron session object.

(in-package :electron)

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
                                                            'state': state}) + '\\\n');
                                 })
                                 item.once('done', (event, state) => {
                                   ~a.write(JSON.stringify({'id': id,
                                                            'url': item.getURL(),
                                                            'receivedBytes': item.getReceivedBytes(),
                                                            'totalBytes': item.getTotalBytes(),
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
