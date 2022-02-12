(in-package #:issr.server.request)

(defclass request ()
  ((id :reader request-id
       :type uuid
       :initarg :id)
   (previous-page :accessor request-previous-page
                  :type plump:root
                  :initarg :previous-page)
   (cookies-out :accessor cookies-out
                :type list
                :initform (list))
   (element-id-counter :accessor request-element-id
                       :type integer
                       :initform -1)))

(defun make-request (&key id headers previous-page cookies-in cookies-out query-arguments)
  (let* ((str:*omit-nulls* t)
         (uri-parts
           (some->> headers
             (assoc :uri) cdr
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
        (setf (cookies-in request) cookies-in))
      (when cookies-out
        (setf (cookies-out request) cookies-out))
      (when headers
        (setf (headers request)
              (some-<>> headers
                (remove :cookie <> :key 'car)
                (remove :uri <> :key 'car)
                (acons :uri (first uri-parts))))))))

(defun issr-keys (main-key &rest keys)
  (->> keys
    (map 'list (compose 'str:downcase 'princ-to-string))
    (cons (str:concat "issr-" (princ-to-string main-key)))
    (str:join ":")))

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
  (get-redis-hash id :cookies-in))

(defmethod (setf cookies-in) (alist (request request))
  (setf (cookies-in (request-id request))
        alist))

(defmethod (setf cookies-in) (alist (id uuid))
  (set-redis-hash id :cookies-in alist))

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
         (headers id)))

(defmethod request-uri ((request request))
  (red:hget (issr-keys (request-id request)) "uri"))

(defmethod request-url ((request request))
  (str:concat
   (red:hget (issr-keys (request-id request)) "host")
   (request-uri request)))

(defun extract-request-cookies (headers)
  (-<>> headers
    (remove :cookie <> :key 'car :test-not 'eq)
    (map 'list
         (compose
          (lambda (cookie)
            (str:split "; " cookie :limit 2))
          'cdr))
    (reduce 'append)
    (map 'list
         (compose (curry 'apply 'cons)
                  (curry 'str:split "=")))))

(defun extract-response-cookies (headers)
  (-<>> headers
    (remove :set-cookie <> :key 'car :test-not 'eq)
    (map 'list 'cdr)))

(defun response-cookies-request-cookies (cookies)
  (->> cookies
    (map 'list
         (compose
          'first
          (curry 'str:split "=")
          (curry 'apply 'cons)
          (lambda (cookie) (str:split "; " cookie :limit 2))))))
