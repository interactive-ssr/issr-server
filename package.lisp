(defpackage issr.dom
  (:use #:cl)
  (:import-from #:plump)
  (:import-from #:alexandria
                #:curry
                #:compose
                #:make-keyword
                #:hash-table-keys)
  (:import-from #:binding-arrows)
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
                #:socket-connect))

(defpackage issr-config
  (:use #:cl)
  (:import-from #:yxorp
                #:ssl-config))
