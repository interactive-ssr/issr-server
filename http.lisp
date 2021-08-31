(in-package #:issr)

(defun add-cookie-path (cookie)
  (if (str:containsp "path=" cookie :ignore-case t)
      cookie
      (str:concat
       cookie
       "; Path=/")))

(define-easy-handler (cookie :uri "/-issr/cookie") (id)
  (prog1 nil
    (setf (header-out "set-cookie")
          (->> id
            get-id-client
            get-client-request
            request-headers
            (yxorp:header :cookie)
            add-cookie-path))))
