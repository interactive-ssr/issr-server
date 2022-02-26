(in-package #:issr.server)

(defstruct (redis-config (:constructor redis-config))
  (destination 6379
   :type destination
   :read-only t)
  (password (princ-to-string (uuid:make-v4-uuid))
   :type string
   :read-only t))

(defstruct (config (:constructor config))
  (port 3000
   :type port
   :read-only t)
  (show-errors nil
   :type boolean
   :read-only t)
  (application-destination 8080
   :type destination
   :read-only t)
  (http-port 10000
   :type port
   :read-only t)
  (websocket-port 10001
   :type port
   :read-only t)
  (ssl nil
   :type (or ssl-config null)
   :read-only t)
  (redis (redis-config)
   :type redis-config
   :read-only t))

(defun read-config-from-string (string)
  (flet ((packageize (string)
           (str:concat "(progn #.(in-package #:issr-config) "
                       string ")")))
    (let ((package *package*))
      (prog1 (handler-case
                 (-> string
                     packageize
                     read-from-string
                     eval)
               (error (condition)
                      (format *error-output* "There is a problem with your config file:~%~A~%"
                              condition)))
        (setq *package* package)))))

(defun read-config (file)
  (read-config-from-string (uiop:read-file-string file)))

(defmacro env-or (env else &optional (binding 'env) (action 'env))
  `(let ((,binding (uiop:getenv ,env)))
     (if (not (str:blankp ,binding))
         ,action
         ,else)))
