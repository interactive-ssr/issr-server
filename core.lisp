(in-package #:issr)

(defclass request ()
  ((previous-page :accessor request-previous-page :initarg :previous-page)
   (headers :accessor request-headers :initarg :headers)
   (query-arguments :accessor request-query-arguments)))

(defun make-request (&key headers previous-page)
  (make-instance
   'request
   :previous-page previous-page
   :headers headers))

(defmethod initialize-instance :after ((this request) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let ((uri-parts
          (->> this
            request-headers
            (yxorp:header :uri)
            (str:split "?"))))
    (setf (request-headers this)
          (copy-alist (request-headers this)))
    (setf (request-query-arguments this)
          (->> uri-parts
            second
            (str:split "&")
            (map 'list (curry 'str:split "="))
            (map 'list
                 (lambda (pair)
                   (cons (urlencode:urldecode (first pair))
                         (urlencode:urldecode (second pair)))))))
    (setf (yxorp:header :uri (request-headers this))
          (setf (yxorp:header :uri (request-headers this))
                (first uri-parts)))))

(defmethod request-uri ((this request))
  (->> this
    request-headers
    (yxorp:header :uri)))

(defmethod request-url ((this request))
  (str:concat
   (->> this
     request-headers
     (yxorp:header :host))
   (request-uri this)))

(defmethod (setf request-query-arguments) ((this request) new-query-arguments)
  (-<> new-query-arguments
    (append (request-query-arguments this))
    (remove-duplicates :key 'first :test 'string=)
    (setf (slot-value this 'query-arguments) <>)))

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

(defun rr (client host port message)
  (let ((request (get-client-request client)))
    (if (str:starts-with-p "?" message)
        (setf (request-query-arguments request)
              (jojo:parse (subseq message 1)
                          :as :alist)))
    (with-open-stream (socket-stream (socket-connect host port))
      )))
