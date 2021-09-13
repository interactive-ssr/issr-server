(in-package #:issr)

(defclass request ()
  ((previous-page :reader request-previous-page
                  :initarg :previous-page)
   (headers :initarg :headers
            :reader request-headers)
   (cookies-in :reader request-cookies-in
               :initarg :cookies-in
               :initform nil)
   (cookies-out :reader request-cookies-out
                :initarg :cookies-out
                :initform nil)
   (query-arguments :reader request-query-arguments
                    :initarg :query-arguments
                    :initform nil)))

(defun make-request (&key headers previous-page)
  (make-instance
   'request
   :previous-page previous-page
   :headers headers))

(defmethod initialize-instance :after ((request request) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let* ((str:*omit-nulls* t)
         (uri-parts
           (->> request
             request-headers
             (yxorp:header :uri)
             (str:split "?"))))
    (setf (slot-value request 'query-arguments)
          (->> uri-parts
            second
            (str:split "&")
            (map 'list (curry 'str:split "="))
            (map 'list
                 (lambda (pair)
                   (cons (urlencode:urldecode (or (first pair) ""))
                         (urlencode:urldecode (or (second pair) "")))))
            (append (request-query-arguments request)))
          (slot-value request 'cookies-in)
          (append (->> request request-headers
                       extract-request-cookies)
                  (->> request request-headers
                       extract-response-cookies
                       response-cookies-request-cookies))
          (slot-value request 'headers)
          (-<>> request
            request-headers
            (remove :cookie <> :key 'car)
            (remove :uri <> :key 'car)
            (acons :uri (first uri-parts))))))

(defun stringify-cookies (cookies)
  (->> cookies
    (map 'list
         (lambda (cookie)
           (let ((name (car cookie))
                 (value (cdr cookie)))
           (str:concat name "=" value))))
    (str:join "; ")))

(defmethod request-new-page ((request request) new-page)
  (with-slots (headers cookies-in cookies-out query-arguments)
      request
    (make-instance
     'request
     :previous-page new-page
     :headers headers
     :cookies-in cookies-in
     :cookies-out cookies-out
     :query-arguments query-arguments)))

(defmethod request-headers ((request request))
  (acons :cookie
         (stringify-cookies (request-cookies-in request))
         (slot-value request 'headers)))

(defmethod request-uri ((request request))
  (->> request
    request-headers
    (yxorp:header :uri)))

(defmethod request-url ((request request))
  (str:concat
   (->> request
     request-headers
     (yxorp:header :host))
   (request-uri request)))

(defmethod request-new-headers ((request request) new-headers)
  (with-slots (previous-page cookies-in cookies-out query-arguments)
      request
    (make-instance
     'request
     :previous-page previous-page
     :headers new-headers
     :cookies-in cookies-in
     :cookies-out cookies-out
     :query-arguments query-arguments)))

(defmethod request-new-query-arguments ((request request) new-query-arguments)
  (with-slots (previous-page headers cookies-in cookies-out query-arguments)
      request
    (make-instance
     'request
     :previous-page previous-page
     :headers headers
     :cookies-in cookies-in
     :cookies-out cookies-out
     :query-arguments
     (-> new-query-arguments
       (append query-arguments)
       (remove-duplicates :key 'car :test 'string= :from-end t)))))

(defun extract-request-cookies (headers)
  (-<>> headers
    (remove :cookie <> :key 'car :test-not 'eq)
    (map 'list 'cdr)
    (map 'list (lambda (cookie)
                 (str:split "; " cookie :limit 2)))
    (reduce 'append)
    (map 'list (curry 'str:split "="))
    (map 'list (curry 'apply 'cons))))

(defun extract-response-cookies (headers)
  (-<>> headers
    (remove :set-cookie <> :key 'car :test-not 'eq)
    (map 'list 'cdr)))

(defun response-cookies-request-cookies (cookies)
  (->> cookies
    (map 'list (lambda (cookie) (str:split "; " cookie :limit 2)))
    (map 'list 'first)
    (map 'list (curry 'str:split "="))
    (map 'list (curry 'apply 'cons))))

(defmethod request-new-cookies-in ((request request) new-cookies)
  (with-slots (previous-page headers cookies-in cookies-out query-arguments)
      request
    (make-instance
     'request
     :previous-page previous-page
     :headers headers
     :cookies-in new-cookies
     :cookies-out cookies-out
     :query-arguments query-arguments)))

(defmethod request-new-cookies-out ((request request) new-cookies)
  (with-slots (previous-page headers cookies-in cookies-out query-arguments)
      request
    (make-instance
     'request
     :previous-page previous-page
     :headers headers
     :cookies-in
     (-> new-cookies
       response-cookies-request-cookies
       (append cookies-in)
       (remove-duplicates :key 'car :test 'string= :from-end t))
     :cookies-out new-cookies
     :query-arguments query-arguments)))

(defmethod request-headers ((request request)))

(defmacro define-map-accessors-with-lock (name map lock)
  `(progn
     (defun ,(intern (str:concat "GET-" (symbol-name name))) (key)
       (bt:with-lock-held (,lock)
         (gethash key ,map nil)))
     (defun ,(intern (str:concat "SET-" (symbol-name name))) (key value)
       (bt:with-lock-held (,lock)
         (setf (gethash key ,map) value)))
     (defun ,(intern (str:concat "REMOVE-" (symbol-name name))) (key)
       (bt:with-lock-held (,lock)
         (remhash key ,map)))))

(defvar *clients*-lock (bt:make-lock))
(defvar *clients*
  (tg:make-weak-hash-table
   :test 'equalp
   :weakness :key)
  "Key: client socket; Value: request")
(define-map-accessors-with-lock client-request *clients* *clients*-lock)

(defun client-request (client)
  (first (get-client-request-page client)))

(defun client-page (client)
  (second (get-client-request-page client)))

(defvar *ids*-lock (bt:make-lock))
(defvar *ids*
  (tg:make-weak-hash-table
   :test 'equalp
   :weakness :value)
  "Key: client id; Value: client socket")
(define-map-accessors-with-lock id-client *ids* *ids*-lock)

(defun random-alphanum (&key (length 12) not-in)
  "Return alphanumeric string of length LENGTH not contained in NOT-IN."
  (loop with alphanum
          = (map 'string #'code-char
                 (loop repeat length
                       collect
                       (if (zerop (random 2))
                           (+ (random 10) 48)
                           (+ (random 26) 97))))
        while (member alphanum not-in)
        finally (return alphanum)))

(defun api-handler (app-server)
  (loop with instruction = (jojo:parse (read-line app-server)) do
    (match instruction
      ((list "rr" id))
      ((list "header" id key)
       (->> id
         get-id-client
         client-request
         (yxorp:header (make-keyword (str:upcase key)))
         (list "result")
         jojo:to-json
         (write :stream app-server))))))

(defun run-application-hook (id hook host port)
  (with-open-stream
      (stream (socket-stream (socket-connect host port)))
    (write
     (jojo:to-json (list hook id))
     :stream stream)))

(defun flatten-args (alist)
  (apply 'concatenate 'list
         (map 'list
              (lambda (cons)
                (let ((name (car cons))
                      (value (cdr cons)))
                  (if (and (listp value)
                           (typep (first value)
                                  '(or string list)))
                      (map 'list
                           (lambda (value)
                             (cons name value))
                           value)
                      ;; if the first value is t, then it is a file
                      (list cons))))
              alist)))

(defun alist-query-string (alist)
  ;; must be flattened
  (->> alist
    (map 'list
         (lambda (cons)
           (format nil "~A=~A"
                   (urlencode (car cons))
                   (urlencode (cdr cons)))))
    (str:join "&")))

(defun alist-write-files (alist)
  "Save file content in a /tmp file, and replace with path.
Return 0: alist with file names instead of content.
Return 1: t if contained files, nil otherwise."
  ;; must be flattened
  (map 'list
       (lambda (cons)
         (let ((name (car cons))
               (value (cdr cons)))
           (if (not (listp value))
               cons
               ;; first value is t if it is a file
               (let ((content (second value))
                     (tmp-file-name (make-pathname
                                     :name (symbol-name (gensym "issr-"))
                                     :directory "tmp")))
                 (with-open-file
                     (tmp-file tmp-file-name
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
                   (-> content
                     base64:base64-string-to-usb8-array
                     (write-sequence tmp-file)))
                 (cons name
                       (jojo:to-json
                        (map 'list 'cons
                             (list "file" "name" "content-type")
                             (apply 'list (princ-to-string tmp-file-name)
                                    (nthcdr 2 value)))
                        :from :alist))))))
       alist))

(defun write-args (args server-stream)
  (setf (yxorp:header :content-type) "application/x-www-form-urlencoded")
  (-> args
    flatten-args
    alist-write-files
    alist-query-string
    (flex:string-to-octets :external-format :utf8)
    (yxorp:write-body-and-headers server-stream)))

(defun rr (client host port show-errors args)
  (let ((request
          (-> client
            get-client-request
            (request-new-query-arguments args))))
    (with-open-stream
        (server (socket-stream
                 (socket-connect
                  host port :element-type '(unsigned-byte 8))))
      (yxorp::with-socket-handler-case server
        (let ((yxorp:*headers* (request-headers request)))
          (write-args (request-query-arguments request) server))
        (let ((yxorp:*headers* (yxorp::parse-response-headers server)))
          (cond
            ((<= 300 (yxorp:header :status) 399)
             (pws:send
              client
              (-> :location
                yxorp:header
                i:redirect
                list
                (jojo:to-json :from :list)))
             (return-from rr))
            ((<= 400 (yxorp:header :status) 599)
             (when show-errors
               (pws:send
                client
                (-> server
                  (yxorp::read-body (lambda (body) body))
                  (flex:octets-to-string :external-format :utf8)
                  i:error list
                  (jojo:to-json :from :list))))
             (return-from rr)))
          (let ((new-page
                  (-> server
                    (yxorp::read-body (lambda (body) body))
                    (flex:octets-to-string :external-format :utf8)
                    plump:parse
                    plump-dom-dom)))
            (insert-js-call new-page "")
            ;; this adds ids to new-page
            (let ((instructions (diff (request-previous-page request)
                                      new-page))
                  (new-cookies (extract-response-cookies yxorp:*headers*)))
              (unless (= (length (request-cookies-out request))
                         (length
                          (-> new-cookies
                            (append (request-cookies-out request))
                            (remove-duplicates :test 'string=))))
                (push (i:cookie) instructions))
              (set-client-request
               client
               (-> request
                 (request-new-page new-page)
                 (request-new-cookies-out new-cookies)))
              (when instructions
                (pws:send client (jojo:to-json instructions :from :list))))))))))
