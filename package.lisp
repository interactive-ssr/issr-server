(defpackage hunchenissr
  (:use #:cl #:plump)
  (:export :*clients*
           :on-connect-hook
           :on-disconnect-hook
           :*ws-port*
           :*socket*
           :*first-time*
           :*id*
           :define-easy-handler
           :start
           :stop
           :redirect
           :rr))

