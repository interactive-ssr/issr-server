(let ((default (config)))
  (config
   :port
   (env-or "ISSR_PORT" (config-port default) env
           (read-from-string env))
   :show-errors
   (env-or "ISSR_SHOW_ERRORS" (config-show-errors default) env
           (if (str:blankp env) nil t))
   :application-destination
   (env-or "ISSR_APPLICATION" (config-application-destination default) env
           (handler-case (parse-integer env)
             (parse-error () env)))
   :ssl (when (some (alexandria:compose 'not 'str:blankp)
                    (map 'list 'uiop:getenv
                         (list "ISSR_SSL_CERT"
                               "ISSR_SSL_KEY"
                               "ISSR_SSL_PASSWORD"
                               "ISSR_SSL_REDIRECT_PORT"
                               "ISSR_SSL_REDIRECT_TO")))
          (ssl-config
           :certificate
           (env-or "ISSR_SSL_CERT"
                   (yxorp::ssl-config-certificate (config-ssl default)))
           :key
           (env-or "ISSR_SSL_KEY"
                   (yxorp::ssl-config-key (config-ssl default)))
           :password
           (env-or "ISSR_SSL_PASSWORD"
                   (yxorp::ssl-config-password (config-ssl default)))
           :redirect-port
           (env-or "ISSR_SSL_REDIRECT_PORT"
                   (yxorp::ssl-config-redirect-port (config-ssl default))
                   env
                   (parse-integer env))
           :redirect-to
           (env-or "ISSR_SSL_REDIRECT_TO"
                   (yxorp::ssl-config-redirect-to (config-ssl default))
                   env
                   (parse-integer env))))
   :redis (redis-config
           :destination
           (env-or "ISSR_REDIS_DESTINATION"
                   (redis-config-destination (config-redis default))
                   env
                   (handler-case (parse-integer env)
                     (parse-error () env)))
           :password
           (env-or "ISSR_REDIS_PASSWORD"
                   (redis-config-password (config-redis default))))))
