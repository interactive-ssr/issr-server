(defpackage issr.dom
  (:use #:cl)
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
   #:add-id
   #:copy-id
   #:ensure-ids
   #:plump-dom-dom))

(defpackage issr.instructions
  (:use #:cl)
  (:import-from #:issr.dom
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

(defpackage issr
  (:use #:cl #:issr.dom)
  (:local-nicknames
   (#:i #:issr.instructions))
  (:import-from #:alexandria
                #:curry
                #:compose
                #:make-keyword)
  (:import-from #:binding-arrows
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
                #:urlencode))

(defpackage issr-config
  (:use #:cl)
  (:import-from #:yxorp
                #:ssl-config))
