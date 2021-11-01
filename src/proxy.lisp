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
  (let* ((html (some-<> node
                 node-children
                 (find :html <> :key 'node-name)))
         (head (some-<> html
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

(defun process-response (body)
  (let ((page (-> body
                plump:parse
                plump-dom-dom
                ensure-ids))
        (id (random-alphanum)))
    (insert-js-call page id)
    (setf (yxorp:header :method yxorp:*request-headers*)
          "POST"
          (yxorp:header :content-type yxorp:*request-headers*)
          "application/x-www-form-urlencoded")
    (set-id-client id (make-request
                       :headers (append yxorp:*request-headers*
                                        yxorp:*headers*)
                       :previous-page page))
    (princ-to-string page)))

(defun response-filter (body)
  (cond
    ;; add issr ids and js
    ((and (str:containsp "html" (yxorp:header :content-type)
                         :ignore-case t)
          (not (str:starts-with-p "/-issr/reconnect"
                                  (yxorp:header :uri yxorp:*request-headers*)))
          (and (str:containsp "rr(" body :ignore-case nil)))
     (process-response body))
    ;; use issr favicon
    ((and (string= "/favicon.ico" (yxorp:header :uri yxorp:*request-headers*))
          (= 404 (yxorp:header :status)))
     (prog1 ""
       (setf (yxorp:header :status) 307
             (yxorp:header :message) "Temporary Redirect"
             (yxorp:header :location) "/-issr/favicon.ico")))
    (:else body)))
