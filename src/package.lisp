(defpackage issr.server.request
  (:use #:cl)
  (:import-from #:alexandria
                #:curry
                #:compose
                #:make-keyword)
  (:import-from #:binding-arrows
                #:->
                #:->>
                #:-<>>
                #:some->>
                #:some-<>>)
  (:import-from #:uuid
                #:uuid)
  (:export
   #:request
   #:request-id
   #:request-previous-page
   #:cookies-out
   #:request-element-id
   #:make-request
   #:issr-keys
   #:headers
   #:cookies-in
   #:stringify-cookies
   #:query-arguments
   #:request-headers
   #:request-uri
   #:request-url))

(defpackage issr.server.dom
  (:use #:cl)
  (:import-from #:issr.server.request
                #:request-element-id)
  (:import-from #:plump)
  (:import-from #:alexandria
                #:curry
                #:compose
                #:make-keyword
                #:hash-table-keys)
  (:import-from #:binding-arrows
                #:->>)
  (:export
   #:node
   #:make-node
   #:node-id
   #:node-name
   #:node-attributes
   #:node-attribute
   #:node-children
   #:*id-counter-request*
   #:add-id
   #:copy-id
   #:ensure-ids
   #:plump-dom-dom))

(defpackage issr.server.instructions
  (:use #:cl)
  (:import-from #:issr.server.dom
                #:node
                #:ensure-ids)
  (:import-from #:binding-arrows
                #:->)
  (:shadow #:mod #:delete #:error)
  (:export
   #:mod
   #:delete
   #:insert
   #:cookie
   #:redirect
   #:reconnect
   #:error))

(defpackage issr.server
  (:use #:cl #:issr.server.dom #:issr.server.request)
  (:local-nicknames
   (#:i #:issr.server.instructions))
  (:import-from #:alexandria
                #:curry
                #:compose
                #:make-keyword)
  (:import-from #:binding-arrows
                #:some->>
                #:some-<>>
                #:-<>>
                #:some-<>
                #:->>
                #:some->
                #:->
                #:-<>)
  (:import-from #:trivia
                #:guard
                #:alist
                #:match)
  (:import-from #:yxorp
                #:ssl-config)
  (:import-from #:usocket
                #:socket-stream
                #:socket-connect)
  (:import-from #:hunchentoot
                #:headers-in*
                #:user-agent
                #:host
                #:raw-post-data
                #:header-in*
                #:header-out
                #:easy-acceptor
                #:define-easy-handler)
  (:import-from #:urlencode
                #:urlencode)
  (:import-from #:uuid
                #:uuid)
  (:import-from #:rutils
                #:alist->ht
                #:ht->alist)
  (:export #:main
           #:redis-config
           #:config
           #:env-or
           #:config-port
           #:config-show-errors
           #:config-application-destination
           #:config-ssl
           #:config-redis
           #:redis-config-destination
           #:redis-config-password))

(defpackage issr-config
  (:use #:cl)
  (:import-from #:yxorp
                #:ssl-config)
  (:import-from #:issr.server
                #:redis-config
                #:config
                #:env-or
                #:config-port
                #:config-show-errors
                #:config-application-destination
                #:config-ssl
                #:config-redis
                #:redis-config-destination
                #:redis-config-password))
