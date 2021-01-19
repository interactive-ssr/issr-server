(defpackage hunchenissr
  (:use #:cl #:issr-core)
  (:export
   -clients-
   -on-connect-hook-
   -on-disconnect-hook-
   *show-errors-to-client*
   *ws-port*
   *socket*
   *first-time*
   *id*
   define-easy-handler
   start
   stop
   redirect
   rr))

