(in-package #:issr.server)

(defvar *ws-server* nil)
(defvar *ht-server* nil)
(defvar *stop-redis* nil)

(defun start (&optional config
              &aux (config
                    (cond ((stringp config) (read-config config))
                          ((config-p config) config)
                          (:else (config)))))
  (multiple-value-bind (redis-host redis-port)
      (destination-parts (-> config config-redis redis-config-destination))
    (yxorp:start
     (yxorp:config
      :port (config-port config)
      :ssl (config-ssl config)
      :destinator (make-destinator config)
      :response-filter (make-response-filter redis-host redis-port
                                             (-> config config-redis redis-config-password))))
    (multiple-value-bind (host port)
        (destination-parts (config-application-destination config))
      (make-/cookie redis-host redis-port (-> config config-redis redis-config-password))
      (make-/reconnect host port redis-host redis-port
                       (-> config config-redis redis-config-password))
      (pws:define-resource "/-issr"
        :message (make-ws-message host port (config-show-errors config)
                                  redis-host redis-port
                                  (-> config config-redis redis-config-password))
        :error 'ws-error))
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
        (setq *stop-redis* nil)
        (progn
          (uiop:run-program
           (format nil "redis-server --requirepass ~A &"
                   (-> config config-redis redis-config-password)))
          (setq *stop-redis*
                (lambda ()
                  (redis:with-connection (:host redis-host :port redis-port
                                          :auth (-> config config-redis redis-config-password))
                    (red:shutdown))))))))

(defun stop ()
  (yxorp:stop)
  (when (and (bt:threadp *ws-server*)
             (bt:thread-alive-p *ws-server*))
    (pws:server-close *ws-server*))
  (when *ht-server*
    (hunchentoot:stop *ht-server*))
  (when *stop-redis*
    (funcall *stop-redis*)))
