(defpackage hunchenissr
  (:use #:cl #:plump #:websocket-driver)
  (:export :*clients*
           :*ws-port*
           :*socket*
           :*first-time*
           :*id*
           :define-easy-handler
           :start
           :stop
           :redirect
           :rr))

