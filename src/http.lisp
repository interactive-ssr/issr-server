(in-package #:issr.server)

(defmethod hunchentoot:session-cookie-name ((acceptor easy-acceptor)) "")

(defun set-hunchentoot-response-cookie (string)
  (let ((parts (->> string
                 (str:split "; ")
                 (map 'list (curry 'str:split "=")))))
    (flet ((find-part (part)
             (find part parts
                   :key 'first
                   :test (lambda (part string)
                           (str:containsp part string :ignore-case t)))))
      (hunchentoot:set-cookie
       (caar parts)
       :value (str:concat (cadar parts) "; SameSite="
                          (or (second (find-part "samesite"))
                              "Lax"))
       :expires (second (find-part "expires"))
       :max-age (second (find-part "max-age"))
       :path (second (find-part "path"))
       :domain (second (find-part "domain"))
       :secure (not (null (find-part "secure")))
       :http-only (not (null (find-part "httpOnly")))))))

(defun add-cookie-path (cookie)
  (if (str:containsp "path=" cookie :ignore-case t)
      cookie
      (str:concat cookie "; Path=/")))

(defun make-/cookie (redis-host redis-port redis-pass)
  (define-easy-handler (/cookie :uri "/-issr/cookie") (id)
    (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
      (prog1 nil
        (->> id
          get-id-client
          get-client-request
          cookies-out
          (map 'list 'add-cookie-path)
          (map 'list 'set-hunchentoot-response-cookie))))))

(defun make-/reconnect (host port redis-host redis-port redis-pass)
  (define-easy-handler (/reconnect :uri "/-issr/reconnect") ()
    (with-open-stream
        (server (socket-stream
                 (socket-connect
                  host port :element-type '(unsigned-byte 8))))
      (let ((uri (header-in* "issr-uri"))
            (args (jojo:parse (raw-post-data :external-format :utf8) :as :alist)))
        (let* ((yxorp:*headers*
                 (-> (list (cons :method "POST")
                           (cons :uri uri)
                           (cons :http-version "HTTP/1.1")
                           (cons :accept "text/html")
                           (cons :host (host))
                           (cons :user-agent (user-agent))
                           (cons :issr-id (princ-to-string (uuid:make-v4-uuid)))
                           (cons :cookie (stringify-cookies
                                          (hunchentoot:cookies-in*))))
                   (append (headers-in*))
                   (remove-duplicates :key 'car :from-end t)
                   alist->ht))
               (yxorp:*request-headers* yxorp:*headers*))
          (write-headers-body-args args server)
          (let* ((yxorp:*headers* (alist->ht (yxorp::parse-response-headers server)))
                 (encoding
                   (or (some-<>> :content-type
                         yxorp:header
                         (str:split ";")
                         (map 'list 'str:trim)
                         (find "charset" <> :test 'str:starts-with-p)
                         (str:split "=")
                         second
                         str:upcase
                         make-keyword)
                       (when (str:containsp "text" (yxorp:header :content-type))
                         :iso-8859-1))))
            (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
              (handler-bind
                  ((flexi-streams:external-format-encoding-error
                     (lambda (condition) (declare (ignore condition))
                       (invoke-restart (find-restart 'use-value)
                                       #\replacement_character))))
                (-> server
                  (yxorp::read-body 'process-response)
                  (flex:octets-to-string :external-format encoding))))))))))
