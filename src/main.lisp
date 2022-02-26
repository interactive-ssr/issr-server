(in-package #:issr.server)

(defvar *ws-server* nil)

(defvar *ht-server* nil)

(declaim (type function *stop-redis*))
(defvar *stop-redis* (constantly nil))

(defun server-uuid ()
  (let ((filename
          (merge-pathnames
           #p"issr/uuid.txt"
           (or (uiop:getenvp "XDG_DATA_HOME")
               #p"~/.local/share/"))))
    (unless (uiop:directory-exists-p (pathname-directory filename))
      (ensure-directories-exist filename))
    (if (uiop:file-exists-p filename)
        (uiop:read-file-string filename)
        (with-open-file (out filename
                             :direction :output
                             :if-does-not-exist :create)
          (let ((uuid (princ-to-string (uuid:make-v4-uuid))))
            (write-sequence uuid out))))))

(defun start (&optional (config (let ((user-config (merge-pathnames
                                                    "issr/config.lisp"
                                                    (or (uiop:getenv "XDG_CONFIG_HOME")
                                                        "~/.config/"))))
                                  (if (uiop:file-exists-p user-config)
                                      user-config
                                      "default-config.lisp")))
              &aux (config
                    (cond ((and (typep config '(or string pathname))
                                (uiop:file-exists-p config))
                           (read-config config))
                          ((config-p config) config)
                          (:else (config)))))
  (if config
      (format t "Using this config:~%~S~%" config)
      (uiop:quit))
  (multiple-value-bind (redis-host redis-port)
      (destination-parts (-> config config-redis redis-config-destination))
    (let ((redis-pass (-> config config-redis redis-config-password)))
      (handler-case
          (yxorp:start
           (yxorp:config
            :port (config-port config)
            :ssl (config-ssl config)
            :destinator (make-destinator config)
            :request-filter 'process-request
            :response-filter (make-response-filter redis-host redis-port redis-pass)))
        (usocket:address-in-use-error ()
          (format *error-output* "Could not start server on port ~A  because another program is using that port.~%"
                  (config-port config))
          (uiop:quit)))
      (multiple-value-bind (host port)
          (destination-parts (config-application-destination config))
        (make-/cookie redis-host redis-port redis-pass)
        (make-/reconnect host port redis-host redis-port redis-pass)
        (pws:define-resource "/-issr"
          :message (make-ws-message host port (config-show-errors config)
                                    redis-host redis-port redis-pass)
          :close (make-ws-close redis-host redis-port redis-pass)
          :error 'ws-error)
        (handler-case
            (setq *ws-server*
                  (pws:server
                   (config-websocket-port config)
                   :multi-thread))
          (usocket:address-in-use-error ()
            (format *error-output* "Could not start server on port ~A  because another program is using that port.~%"
                    (config-websocket-port config))
            (uiop:quit)))
        (handler-case
            (setq *ht-server*
                  (hunchentoot:start
                   (make-instance
                    'easy-acceptor
                    :port (config-http-port config)
                    :document-root (merge-pathnames "resources/"
                                                    *install-directory*))))
          (usocket:address-in-use-error ()
            (format *error-output* "Could not start server on port ~A because another program is using that port.~%"
                    (config-http-port config))
            (uiop:quit)))
            (setq *stop-redis* (constantly nil))
            (progn
              (uiop:run-program
               (format nil "redis-server ~A --requirepass '~A' &"
                       (merge-pathnames "redis.conf" (uiop:getcwd))
                       redis-pass)
               :force-shell t)
              (setq *stop-redis*
                    (lambda ()
                      (ignore-errors
                       (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
                         (red:shutdown)
                         (sleep 1)))
                      (setq *stop-redis* (constantly nil))))))
        (start-hook-listener host port (config-show-errors config) redis-host redis-port redis-pass)
        (start-id-gc redis-host redis-port redis-pass)))))

(defun main (&aux (args uiop:*command-line-arguments*))
  (let ((config (first args)))
    (handler-case
        (bt:join-thread
         (cond
           ((and config
                 (eq #\left_parenthesis (elt config 0)))
            (start (read-config-from-string config)))
           (config (start config))
           (:else (start))))
      (sb-sys:interactive-interrupt ()
        (handler-case (stop)
          (sb-sys:interactive-interrupt ()
            (continue)))
        (uiop:quit)))))

(defun stop ()
  (yxorp:stop)
  (when (and (bt:threadp *ws-server*)
             (bt:thread-alive-p *ws-server*))
    (pws:server-close *ws-server*))
  (when (hunchentoot:started-p *ht-server*)
    (hunchentoot:stop *ht-server*))
  ;; give the websockets time to disconnect
  (sleep 1)
  (stop-hook-listener)
  (stop-id-gc)
  (funcall *stop-redis*))

(defun start-id-gc (redis-host redis-port redis-pass)
  (bt:make-thread
   (lambda ()
     (loop
      (let ((keys (alexandria:hash-table-keys *ids*)))
        (loop for key in keys do
              (unless (typep (get-id-client key) 'portal:websocket)
                (sleep 2)
                (unless (typep (get-id-client key) 'portal:websocket)
                  (remove-id-client key)
                  (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
                                         (remove-client key)))))
        (sleep 60))))
   :name "issr-id-gc"))

(defun stop-id-gc ()
  (let ((gcs (remove "issr-id-gc"
                     (bt:all-threads)
                     :key 'bt:thread-name
                     :test-not 'string=)))
    (mapc 'bt:destroy-thread gcs)))
