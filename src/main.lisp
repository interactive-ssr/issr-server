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
      (yxorp:start
       (yxorp:config
        :port (config-port config)
        :ssl (config-ssl config)
        :destinator (make-destinator config)
        :request-filter 'process-request
        :response-filter (make-response-filter redis-host redis-port redis-pass)))
      (multiple-value-bind (host port)
          (destination-parts (config-application-destination config))
        (make-/cookie redis-host redis-port redis-pass)
        (make-/reconnect host port redis-host redis-port redis-pass)
        (pws:define-resource "/-issr"
          :message (make-ws-message host port (config-show-errors config)
                                    redis-host redis-port redis-pass)
          :close (make-ws-close redis-host redis-port redis-pass)
          :error 'ws-error)
        (setq *ws-server*
              (pws:server
               (config-websocket-port config)
               :multi-thread))
        (setq *ht-server*
              (hunchentoot:start
               (make-instance
                'easy-acceptor
                :port (config-http-port config)
                :document-root "resources/")))
        (if (not (equal 6379 (-> config config-redis redis-config-destination)))
            (setq *stop-redis* (constantly nil))
            (progn
              (uiop:run-program
               (format nil "redis-server ~A --requirepass '~A' &"
                       (merge-pathnames "redis.conf" (uiop:getcwd))
                       redis-pass)
               :force-shell t)
              (setq *stop-redis*
                    (lambda ()
                      (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
                        (red:shutdown))
        (sleep 1)
        (start-hook-listener host port (config-show-errors config) redis-host redis-port redis-pass)))))
                      (setq *stop-redis* (constantly nil))))))

(defun stop ()
  (yxorp:stop)
  (when (and (bt:threadp *ws-server*)
             (bt:thread-alive-p *ws-server*))
    (pws:server-close *ws-server*))
  (when (hunchentoot:started-p *ht-server*)
    (hunchentoot:stop *ht-server*))
  ;; give the websockets time to disconnect
  (stop-hook-listener)
  (sleep 1)
  (funcall *stop-redis*))
