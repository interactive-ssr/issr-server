(in-package #:issr.server)

(defun issr-keys (main-key &rest keys)
  (->> keys
    (map 'list (compose 'str:downcase 'princ-to-string))
    (cons (str:concat "issr-" (princ-to-string main-key)))
    (str:join ":")))

(defclass request ()
  ((id :reader request-id
       :type uuid
       :initarg :id)
   (previous-page :reader request-previous-page
                  :type plump:root
                  :initarg :previous-page
                  :accessor request-previous-page)))

(defun make-request (&key id headers previous-page cookies-in cookies-out query-arguments)
  (let* ((str:*omit-nulls* t)
         (uri-parts
           (some->> headers
             (yxorp:header :uri)
             (str:split "?")))
         (request
           (make-instance
            'request
            :id id
            :previous-page previous-page)))
    (prog1 request
      (when (or uri-parts query-arguments)
        (setf (query-arguments request)
              (some->> uri-parts
                second
                (str:split "&")
                (map 'list (curry 'str:split "="))
                (map 'list
                     (lambda (pair)
                       (cons (urlencode:urldecode (or (first pair) ""))
                             (urlencode:urldecode (or (second pair) "")))))
                (append query-arguments))))
      (when cookies-in
        (setf (cookies-in request)
              (append (extract-request-cookies headers)
                      (some->> headers
                        extract-response-cookies
                        response-cookies-request-cookies)
                      cookies-in)))
      (when cookies-out
        (setf (cookies-out request) cookies-out))
      (when headers
        (setf (headers request)
              (some-<>> headers
                (remove :cookie <> :key 'car)
                (remove :uri <> :key 'car)
                (acons :uri (first uri-parts))))))))

(defun get-redis-hash (id key)
  (let* ((hash (issr-keys id key))
         (keys (red:hkeys hash)))
    (loop for key in keys
          collect
          (cons key
                (block exit
                  (loop
                    (block continue
                      (handler-case
                          (return-from exit (jojo:parse (red:hget hash key)))
                        (jojo:<jonathan-incomplete-json-error> ()
                          (return-from continue))))))))))

(defun get-redis-hash-keywords (id key)
  (map 'list
       (lambda (cons)
         (cons (make-keyword (str:upcase (car cons)))
               (cdr cons)))
       (get-redis-hash id key)))

(defun set-redis-hash (id key alist)
  (let* ((hash (issr-keys id key)))
    (loop for (key . value) in alist do
      (red:hset hash key
                (jojo:to-json value)))))

(defun set-redis-hash-keywords (id key alist)
  (set-redis-hash
   id key
   (map 'list
        (lambda (cons)
          (cons (str:downcase (symbol-name (car cons)))
                (cdr cons)))
        alist)))

(defmethod headers ((request request))
  (headers (request-id request)))

(defmethod headers ((id uuid))
  (get-redis-hash-keywords id :headers))

(defmethod (setf headers) (alist (request request))
  (setf (headers (request-id request))
        alist))

(defmethod (setf headers) (alist (id uuid))
  (set-redis-hash-keywords id :headers alist))

(defmethod cookies-in ((request request))
  (cookies-in (request-id request)))

(defmethod cookies-in ((id uuid))
  (get-redis-hash-keywords id :cookies-in))

(defmethod (setf cookies-in) (alist (request request))
  (setf (cookies-in (request-id request))
        alist))

(defmethod (setf cookies-in) (alist (id uuid))
  (set-redis-hash-keywords id :cookies-in alist))

(defmethod cookies-out ((request request))
  (cookies-out (request-id request)))

(defmethod cookies-out ((id uuid))
  (get-redis-hash-keywords id :cookies-out))

(defmethod (setf cookies-out) (alist (request request))
  (setf (cookies-out (request-id request))
        alist))

(defmethod (setf cookies-out) (alist (id uuid))
  (set-redis-hash id :cookies-out alist))

(defun stringify-cookies (cookies)
  (->> cookies
    (map 'list
         (lambda (cookie)
           (let ((name (car cookie))
                 (value (cdr cookie)))
             (str:concat name "=" value))))
    (str:join "; ")))

(defmethod query-arguments ((id uuid))
  (get-redis-hash id :query-arguments))

(defmethod query-arguments ((request request))
  (query-arguments (request-id request)))

(defmethod (setf query-arguments) (alist (request request))
  (setf (query-arguments (request-id request)) alist))

(defmethod (setf query-arguments) (alist (id uuid))
  (let ((query-arguments (query-arguments id)))
    (loop with hash = (issr-keys id :query-arguments)
          for (key . value) in query-arguments do
            (red:hdel hash key))
    (set-redis-hash
     id :query-arguments
     (-> alist
       (append query-arguments)
       (remove-duplicates :key 'car :test 'string= :from-end t)))))

(defmethod request-headers ((request request))
  (request-headers (request-id request)))

(defmethod request-headers ((id uuid))
  (acons :cookie
         (stringify-cookies (cookies-in id))
         (headers (headers id))))

(defmethod request-uri ((request request))
  (red:hget (issr-key (request-id request)) "uri"))

(defmethod request-url ((request request))
  (str:concat
   (red:hget (issr-key (request-id request)) "host")
   (request-uri request)))

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

(defvar *ids*-lock (bt:make-lock))
(defvar *ids*
  (tg:make-weak-hash-table
   :test 'equalp
   :weakness :value)
  "Key: client id; Value: client socket (and sometimes request)")
(define-map-accessors-with-lock id-client *ids* *ids*-lock)

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
  (let ((request (get-client-request client)))
    (setf (query-arguments request) args)
    (with-open-stream
        (server (socket-stream
                 (socket-connect
                  host port :element-type '(unsigned-byte 8))))
      ;; (yxorp::with-socket-handler-case server
      (let ((yxorp:*headers* (headers request)))
        (write-args (query-arguments request) server))
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
              (unless (= (length (cookies-out request))
                         (length
                          (-> new-cookies
                            (append (cookies-out request))
                            (remove-duplicates :test 'string=))))
                (push (i:cookie) instructions))
              (setf (request-previous-page request) new-page)
              (setf (cookies-out request) new-cookies)
              (when instructions
                (pws:send client (jojo:to-json instructions :from :list)))))))));)
