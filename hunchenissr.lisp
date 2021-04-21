(in-package #:hunchenissr)

(defvar *show-errors-to-client* nil
  "When non-nil, errors after the initial connection can be seen in console.error.")

(defun treat-page (query-names body)
  (let ((page (gensym))
        (page-dom (gensym)))
    `(progn
       (when (and (eq :post (hunchentoot:request-method*))
                  (string= "application/json"
                          (cdr (assoc :content-type
                                      (hunchentoot:headers-in hunchentoot:*request*)))))
         (setf (slot-value hunchentoot:*request* 'hunchentoot:get-parameters)
               (handle-post-data (jojo:parse (hunchentoot:raw-post-data
                                              :force-text t)
                                             :as :hash-table)))
         (setf (slot-value hunchentoot:*request* 'hunchentoot:post-parameters)
               (slot-value hunchentoot:*request* 'hunchentoot:get-parameters))
         (setf (slot-value hunchentoot:*request* 'hunchentoot::method)
               :get))
       (let* ((*id* (generate-id))
              ,@(map 'list
                    (lambda (query)
                      (let ((query
                              (if (stringp query)
                                  (intern (string-upcase query))
                                  query)))
                        (list query `(or (hunchentoot:get-parameter
                                          ,(string-downcase(string query)))
                                         ,query))))
                    query-names)
              (,page (catch 'issr-redirect ,@body)))
         (if *first-time*
             (let ((,page-dom (ensure-ids (clean (plump:parse ,page)))))
               (setf (gethash *id* -clients-)
                     (list hunchentoot:*request* ,page-dom))
               (plump:serialize ,page-dom nil))
             ,page)))))

(defmacro define-easy-handler (description lambda-list &body body)
  `(hunchentoot:define-easy-handler ,description ,lambda-list
     ;; when post json, parse using handle-post-data format
     ,(treat-page
       (map 'list
            (lambda (var)
              (if (consp var)
                  (first var)
                  var))
            lambda-list)
       body)))

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
         (info (gethash socket -clients-))
         (hunchentoot:*request* (car info))
         (hunchentoot:*acceptor* (hunchentoot:request-acceptor hunchentoot:*request*))
         (hunchentoot:*session* (hunchentoot:session hunchentoot:*request*))
         (hunchentoot:*reply* (make-instance 'hunchentoot:reply))
         (handler (some (lambda (dis)
                          (funcall dis hunchentoot:*request*))
                        hunchentoot:*dispatch-table*))
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
              (let ((new-page (clean (plump:parse new-page))))
                (setq instructions
                      (append (diff-dom previous-page new-page)
                              instructions))
                (setf (cadr (gethash *socket* -clients-)) new-page))))
        (pws:send socket (jojo:to-json instructions))))))

(defun ws-close (socket)
  (dolist (fun -on-disconnect-hook-)
    (funcall fun socket))
  (remhash socket -clients-))

(defun ws-message (socket message)
  (cond
    ;; first connection
    ((str:starts-with-p "id:" message)
     (let* ((id (parse-integer (subseq message 3)))
            (info (gethash id -clients-)))
       (if info
           (progn
             (setf (gethash socket -clients-) info)
             (format t "Connected to client with id:~a.~%" id)
             (remhash id -clients-)
             (dolist (fun -on-connect-hook-)
               (funcall fun socket)))
           (pws:send socket (jojo:to-json (list (list "reconnect")))))))
    ;; giving parameters to update page
    ((and (gethash socket -clients-)
          (or (str:starts-with-p "?" message)
              (str:starts-with-p "post:" message)))
     (rr socket message))
    (:else
     (pws:send socket (jojo:to-json (list (list "reconnect"))))
     (pws:close socket))))

(defun ws-error (socket condition)
  (when *show-errors-to-client*
    (pws:send socket (jojo:to-json
                  (list
                   (list "error"
                         (format nil "~a" condition)))))))

(pws:define-resource "/"
  :message #'ws-message
  :close #'ws-close
  :error #'ws-error)

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
        (loop
          (dolist (client (hash-keys -clients-))
            (when (numberp client)
              (sleep *gc-leeway*)
              (when (gethash client -clients-)
                (remhash client -clients-))))
          (sleep *gc-frequency*)))
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
     (let ((gc (find issr-gc (bt:all-threads)
                     :key #'bt:thread-name
                     :test #'string=)))
       (when gc (bt:destroy-thread gc)))
     ;; return and set server
     (setf ,servers (hunchentoot:stop (first ,servers)))))

(defmacro redirect (target)
  `(if *socket*
       (throw 'issr-redirect (list "redirect" ,target))
       (hunchentoot:redirect ,target)))
