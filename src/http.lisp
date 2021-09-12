(in-package #:issr)

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

(define-easy-handler (/cookie :uri "/-issr/cookie") (id)
  (prog1 nil
    (->> id
      get-id-client
      get-client-request
      request-cookies-out
      (map 'list 'add-cookie-path)
      (map 'list 'set-hunchentoot-response-cookie))))

(defun make-/reconnect (host port)
  (define-easy-handler (/reconnect :uri "/-issr/reconnect") ()
    (with-open-stream
        (server (socket-stream
                 (socket-connect
                  host port :element-type '(unsigned-byte 8))))
      (yxorp::with-socket-handler-case server
        (let ((uri (header-in* "issr-uri"))
              (args (jojo:parse (raw-post-data :external-format :utf8) :as :alist)))
          (let* ((yxorp:*headers*
                   (-> (list (cons :method "POST")
                             (cons :uri uri)
                             (cons :http-version "HTTP/1.1")
                             (cons :accept "text/html")
                             (cons :host (host))
                             (cons :user-agent (user-agent))
                             (cons :cookie (stringify-cookies
                                            (hunchentoot:cookies-in*))))
                     (append (headers-in*))
                     (remove-duplicates :key 'car :from-end t)))
                 (yxorp:*request-headers* yxorp:*headers*))
            (write-args args server)
            (let* ((yxorp:*headers* (yxorp::parse-response-headers server))
                   (page
                     (-> server
                       (yxorp::read-body (lambda (body) body))
                       (flex:octets-to-string :external-format :utf8))))
              (add-ids-and-js-to-html page))))))))