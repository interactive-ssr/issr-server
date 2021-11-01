(in-package #:issr.server)

(deftype port ()
  'integer)

(deftype destination ()
  '(or string port null))

(defun valid-destination-p (destination)
  (typecase destination
    (null nil)
    (port t)
    (string
     (handler-case
         (-<> destination
           (str:split ":" <> :omit-nulls t)
           second
           parse-integer)
       (parse-error nil)))))

(defun destination-parts (destination)
  (typecase destination
    (port
     (values "localhost" destination))
    (string
     (let ((parts (str:split ":" destination :omit-nulls t)))
       (values (first parts)
               (parse-integer (second parts)))))))

(defstruct (config (:constructor config))
  (port 3000
   :type port
   :read-only t)
  (show-errors-to-client nil
   :type boolean
   :read-only t)
  (api-port nil
   :type (or port null)
   :read-only t)
  (application-destination 8080
   :type destination
   :read-only t)
  (application-hook-destination nil
   :type (or destination null)
   :read-only t)
  (http-port 10000
   :type port
   :read-only t)
  (websocket-port 10001
   :type port
   :read-only t)
  (ssl nil
   :type (or ssl-config null)
   :read-only t))

(defun read-config (file)
  (flet ((packageize (string)
           (str:concat "(progn #.(in-package #:issr-config) "
                       string ")")))
    (let ((package *package*))
      (prog1 (handler-case
                 (-> file
                   uiop:read-file-string
                   packageize
                   read-from-string
                   eval)
               (error (condition)
                 (format *error-output* "There is a problem with your config file:~%~A~%"
                         condition)))
        (setq *package* package)))))
