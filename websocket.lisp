(in-package #:issr)

(defvar *show-errors-to-client* nil
  "When non-nil, errors after the initial connection can be seen in console.error.")

(defun make-ws-message (host port)
  (lambda (socket message)
    (cond
      ;; first connection
      ((str:starts-with-p "id:" message)
       (let* ((id (parse-integer (subseq message 3)))
              (info (get-id-client id)))
         (if info
             (progn
               (set-client-request-page socket info)
               (set-id-client id socket)
               (format t "Connected to client with id:~a.~%" id)
               (run-application-hook id "disconnect" host port))
             (pws:send socket (jojo:to-json (list (i:reconnect)))))))
       ;; giving parameters to update page
       ((and (get-client-request-page socket)
             (or (str:starts-with-p "?" message)
                 (str:starts-with-p "post:" message)))
        (rr socket host port message))
       (:else
        (pws:send socket (jojo:to-json (list (i:reconnect))))
        (pws:close socket)))))


(defun make-ws-close (host port)
  (lambda (socket)
    (run-application-hook
     (->> socket
       client-request
       (yxorp:header :issr-id))
     "disconnect" host port)
    (remove-client-request-page socket)))
  
