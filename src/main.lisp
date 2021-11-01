(in-package #:issr.server)

(defvar *ws-server* nil)
(defvar *ht-server* nil)

(defun start (config
              &aux (config
                    (cond ((stringp config) (read-config config))
                          ((config-p config) config)
                          (:else (config)))))
  (yxorp:start
   (yxorp:config
    :port (config-port config)
    :ssl (config-ssl config)
    :destinator (make-destinator config)
    :response-filter 'response-filter))
  (multiple-value-bind (host port)
      (destination-parts (config-application-destination config))
    (make-/reconnect host port)
    (pws:define-resource "/-issr"
      :message (make-ws-message host port (config-show-errors-to-client config))
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
          :document-root "resources/"))))

(defun stop ()
  (yxorp:stop)
  (when (and (bt:threadp *ws-server*)
             (bt:thread-alive-p *ws-server*))
    (pws:server-close *ws-server*))
  (when *ht-server*
    (hunchentoot:stop *ht-server*)))
