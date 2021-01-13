(in-package #:hunchenissr)

(defmacro define-easy-handler (description lambda-list &body body)
  `(hunchentoot:define-easy-handler ,description ,lambda-list
     (let* ((*id* (generate-id))
            (page (block issr-redirect ,@body)))
       (unless *socket*
         (setf (gethash *id* clients)
               (list hunchentoot:*request* (clean (parse page)))))
       page)))

(defun handle-post-data (data)
  "Return a list of get/post parameters from json/hash-table DATA.
Create any files necessary."
  (let (params)
    (dolist (name (hash-keys data))
      (let ((value (gethash name data)))
        (if (listp value)
            (dolist (value value)
              (push (if (hash-table-p value)
                        (let ((path (hunchentoot::make-tmp-file-name))
                              (byte-array (cl-base64:base64-string-to-usb8-array
                                           (gethash "content" value))))
                          (ensure-directories-exist (directory-namestring path))
                          (when hunchentoot:*file-upload-hook*
                            (funcall hunchentoot:*file-upload-hook* path))
                          (with-open-file (outfile path :if-does-not-exist :create
                                                        :if-exists :supersede
                                                        :direction :output
                                                        :element-type '(unsigned-byte 8))
                            (write-sequence byte-array outfile))
                          (cons name (list path
                                           (gethash "name" value)
                                           (gethash "type" value))))
                        (cons name value))
                    params))
            (push (cons name value) params))))
    (reverse params)))

(defun rr (socket &optional (parameters "?"))
  "Send a Re-Render to SOCKET with query string PARAMETERS."
  (let* ((*socket* socket)
         (*first-time* nil)
         (info (gethash socket clients))
         (hunchentoot:*request* (car info))
         (hunchentoot:*session* (hunchentoot:session hunchentoot:*request*))
         (hunchentoot:*reply* (make-instance 'hunchentoot:reply))
         (handler (hunchentoot:dispatch-easy-handlers hunchentoot:*request*))
         (previous-page (cadr info)))
    ;; update query string for request
    (let ((previous-params (copy-tree (hunchentoot:get-parameters*))))
      (cond
        ;; query string
        ((str:starts-with-p "?" parameters)
         (setf (slot-value hunchentoot:*request* 'hunchentoot:query-string)
               (subseq parameters 1))
         ;; recaclculate parameters
         (hunchentoot:recompute-request-parameters :request hunchentoot:*request*))
        ;; multipart/form-data
        ((str:starts-with-p "post:" parameters)
         (let ((data (handle-post-data (jojo:parse (subseq parameters 5) :as :hash-table))))
           (setf (slot-value hunchentoot:*request* 'hunchentoot:get-parameters) data))))
      ;; fill in missing parameters from previous request
      (let ((temp-params (copy-tree (slot-value hunchentoot:*request* 'hunchentoot:get-parameters))))
        (dolist (elm previous-params)
          (unless (member (car elm) (slot-value hunchentoot:*request* 'hunchentoot:get-parameters) :key #'car :test #'string=)
            (setf temp-params
                  (append temp-params
                          (list elm)))))
        (setf (slot-value hunchentoot:*request* 'hunchentoot:get-parameters) temp-params))
      ;; generate page and instructions
      (let ((new-page (funcall handler))
            (instructions (list)))
        (when (= 200 (hunchentoot:return-code hunchentoot:*reply*))
          ;; cookies
          (with-slots (hunchentoot:cookies-out) hunchentoot:*reply*
            (when hunchentoot:cookies-out
              (setf (slot-value hunchentoot:*request* 'hunchentoot:cookies-in)
                    (mapcar (lambda (cookie)
                              (cons (car cookie)
                                    (slot-value (cdr cookie) 'hunchentoot::value)))
                            hunchentoot:cookies-out))
              (push (cons "cookie" (mapcar (lambda (cookie)
                                             (hunchentoot::stringify-cookie (cdr cookie)))
                                           hunchentoot:cookies-out))
                    instructions)))
          (if (listp new-page)
              ;; redirect or some other custom instruction
              (push new-page instructions)
              ;; dom instructions
              (let ((new-page (clean (parse new-page))))
                (setq instructions
                      (append (diff-dom previous-page new-page)
                              instructions))
                (setf (cadr (gethash *socket* clients)) new-page))))
        (pws:send socket (jojo:to-json instructions))))))

(defun ws-close (socket)
  (dolist (fun on-disconnect-hook)
    (funcall fun socket))
  (remhash socket clients))

(defun ws-message (socket message)
  (cond
    ;; first connection
    ((str:starts-with-p "id:" message)
     (let* ((id (parse-integer (subseq message 3)))
            (info (gethash id clients)))
       (if info
           (progn
             (setf (gethash socket clients) info)
             (format t "Connected to client with id:~a.~%" id)
             (remhash id clients)
             (dolist (fun on-connect-hook)
               (funcall fun socket)))
           (progn
             (warn "Uhhhhm, id:~a doesn't exist.~%" id)
             (pws:send socket (format nil "~a is not a valid id.~%" id))))))
    ;; reconnecting
    ((str:starts-with-p "http:" message)
     (let* ((state (jojo:parse (subseq message 5) :as :hash-table))
            (hunchentoot:*reply* (make-instance 'hunchentoot:reply))
            (request (make-instance
                      'hunchentoot:request
                      :headers-in (pws:header socket)
                      :uri (gethash "uri" state)
                      :method :GET
                      :acceptor hunchentoot:*acceptor*
                      :remote-addr (cdr (assoc :host (pws:header socket))))))
       ;; set page
       (setf (gethash socket clients)
             (list request (clean (parse (gethash "page" state)))))
       ;; set cookies
       (setf (slot-value request 'hunchentoot:cookies-in)
             (mapcar (lambda (cookie)
                       (cons (hunchentoot:url-decode (first cookie))
                             (hunchentoot:url-decode (second cookie))))
                     (mapcar (lambda (cookie) (str:split "=" cookie))
                             (str:split ";" (cdr (assoc :cookie (pws:header socket)))))))
       ;; restore session
       (setf (slot-value request 'hunchentoot:session)
             (hunchentoot:session-verify request))
       ;; set parameters
       ;; first set url parameters
       (setf (slot-value request 'hunchentoot:query-string)
             (gethash "query" state))
       (hunchentoot:recompute-request-parameters :request request)
       ;; set issr parameters
       (setf (slot-value request 'hunchentoot:get-parameters)
             (append (slot-value request 'hunchentoot:get-parameters)
                     (handle-post-data (gethash "params" state))))
       (dolist (fun on-connect-hook)
         (funcall fun socket))))
    ;; giving parameters to update page
    ((and (gethash socket clients)
          (or (str:starts-with-p "?" message)
              (str:starts-with-p "post:" message)))
     (rr socket message))
    (:else
     (pws:send socket (format nil "Wrong format.~%"))
     (warn "Suspicious websoket connection from ~a.~%" socket))))

(pws:define-resource "/"
  :message #'ws-message
  :close #'ws-close)

(defvar *gc-leeway* 15
  "The number of seconds a websocket has to connect after being noticed before
being deleted.")

(defvar *gc-frequency* 60
  "The number of seconds between garbage collections.")

(defparameter issr-gc "issr-gc"
  "The name of the garbage collection thread.")

(defmacro start (acceptor &key ws-port)
  `(progn
     (when ,ws-port (setq *ws-port* ,ws-port))
     (unless *ws-port* (setq *ws-port* 4433))
     ;; garbage collection of unconnected websockets
     (bordeaux-threads:make-thread
      (lambda ()
        (dolist (client (hash-keys clients))
          (when (numberp client)
            (sleep *gc-leeway*)
            (when (gethash client clients)
              (remhash client clients))))
        (sleep *gc-frequency*))
      :name issr-gc)
     ;; start websocket server and http server
     (list (hunchentoot:start ,acceptor)
           (pws:server *ws-port* :multi-thread))))

(defmacro stop (servers)
  `(progn
     (when (bordeaux-threads:thread-alive-p (second ,servers))
       (pws:server-close (second ,servers)))
     (setq hunchentoot:*acceptor* nil)
     ;; stop garbage collection
     (bt:destroy-thread (find issr-gc (bt:all-threads)
                              :key #'bt:thread-name
                              :test #'string=))
     ;; return and set server
     (setf ,servers (hunchentoot:stop (first ,servers)))))

(defmacro redirect (target)
  `(if *socket*
       (return-from issr-redirect (list "redirect" ,target))
       (hunchentoot:redirect ,target)))
