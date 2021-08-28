(in-package #:issr)

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
    :response-filter #'add-ids-and-js-to-html)))
