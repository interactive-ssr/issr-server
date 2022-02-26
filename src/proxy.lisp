(in-package #:issr.server)

(defvar *js-name* "issr.min.js")

(defun make-destinator (config)
  (lambda ()
    (if (str:starts-with-p "/-issr" (yxorp:header :uri))
        (if (yxorp:header :upgrade)
            (config-websocket-port config)
            (config-http-port config))
        (config-application-destination config))))

(defun insert-js-call (node id)
  (let ((head (some-<> node
                node-children
                (find :html <> :key 'node-name)
                node-children
                (find :head <> :key 'node-name)))
        (js (list
             (make-node :script `((:src . ,(str:concat "/-issr/" *js-name*))))
             (make-node :script '((:noupdate . "T"))
                        (format nil "connect(~S)" id)))))
    (if head
        (setf (node-children head)
              (append (node-children head) js))
        ;; else
        (setf (node-children node)
              (append (node-children node) js))))
  node)

(defun process-request (body)
  (setf (yxorp:header :issr-id)
        (princ-to-string (uuid:make-v4-uuid)))
  body)

(defun process-response (body)
  (let* ((id (yxorp:header :issr-id yxorp:*request-headers*))
         (request-headers (ht->alist yxorp:*request-headers*))
         (*id-counter-request*
           (make-request
            :id (uuid:make-uuid-from-string id)
            :headers
            (-<>> request-headers
              (acons :method "POST")
              (acons :content-type "application/x-www-form-urlencoded")
              (remove-duplicates <> :key 'car :from-end t))
            :cookies-in
            (append (extract-request-cookies request-headers)
                    (some->> (ht->alist yxorp:*headers*)
                      extract-response-cookies
                      response-cookies-request-cookies))))
         (page (-> body
                 plump:parse
                 plump-dom-dom
                 ensure-ids)))
    (insert-js-call page id)
    (setf (request-previous-page *id-counter-request*) page)
    (set-id-client id *id-counter-request*)
    (princ-to-string page)))

(defun make-response-filter (redis-host redis-port redis-pass)
  (lambda (body)
    (cond
      ;; add issr ids and js
      ((and (str:containsp "html" (yxorp:header :content-type)
                           :ignore-case t)
            (not (str:starts-with-p "/-issr/reconnect"
                                    (yxorp:header :uri yxorp:*request-headers*)))
            (and (ppcre:scan "rr\s*[(]" body)))
       (handler-case
           (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
             (process-response body))
         (usocket:connection-refused-error (c) (declare (ignore c))
           (format *error-output* "Could not make redis connection.~%"))))
      ;; use issr favicon
      ((and (string= "/favicon.ico" (yxorp:header :uri yxorp:*request-headers*))
            (= 404 (yxorp:header :status)))
       (prog1 ""
         (setf (yxorp:header :status) 307
               (yxorp:header :message) "Temporary Redirect"
               (yxorp:header :location) "/-issr/favicon.ico")))
      (:else body))))
