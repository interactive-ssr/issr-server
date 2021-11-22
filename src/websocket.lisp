(in-package #:issr.server)

(defvar *show-errors-to-client* nil
  "When non-nil, errors after the initial connection can be seen in console.error.")

(defun make-ws-message (host port show-errors redis-host redis-port redis-pass)
  (lambda (socket message)
    (if (str:starts-with-p "id:" message)
        ;; first connection
        (let* ((id (subseq message 3))
               (info (get-id-client id)))
          (if (typep info 'request)
              (progn
                (set-client-request socket info)
                (set-id-client id socket))
              ;; (run-application-hook id "disconnect" host port)
              (pws:send socket (jojo:to-json (list (i:reconnect))))))
        ;; giving parameters to update page
        (handler-case
            (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
              (rr socket host port show-errors (jojo:parse message :as :alist)))
          (jojo:<jonathan-error> ()
            (pws:send socket (jojo:to-json (list (i:reconnect))))
            (pws:close socket))))))

(defun make-ws-close (redis-host redis-port redis-pass)
  (lambda (socket)
    (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
      (let ((id (some->> socket
                  get-client-request
                  request-headers
                  (yxorp:header :issr-id)))
            (channel (str:concat "issr-" (server-uuid))))
        (when (redis-channel-exists-p channel)
          (red:publish channel
                       (jojo:to-json (list "issr-disconnect" id)))
          ;; wait for disconnect hook to be done
          (let ((channel (str:concat "issr-" id "-disconnect-done")))
            (red:subscribe channel)
            (redis:expect :anything)
            (red:unsubscribe channel)))
        (red:del (issr-keys id :cookies-in))
        (red:del (issr-keys id :cookies-out))
        (red:del (issr-keys id :headers))
        (red:del (issr-keys id :query-arguments))
        (remove-id-client id))
      (remove-client-request socket))))

(defun ws-error (socket condition)
  (when *show-errors-to-client*
    (pws:send
     socket
     (jojo:to-json
      (list (i:error (format nil "~A" condition)))))))
