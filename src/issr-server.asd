(in-package #:asdf-user)

(defsystem issr-server
  :description "Make Interactive-Server-Side-Rendered web pages with declaritive and recursive programming."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "AGPLv3"
  :version "1"
  :depends-on
  (#:alexandria
   #:bordeaux-threads
   #:cl-base64
   #:cl-redis
   #:do-urlencode
   #:hunchentoot
   #:jonathan
   #:plump
   #:portal
   #:rutils
   #:str
   #:trivia
   #:trivial-garbage
   #:uuid
   #:yxorp)
  :serial t
  :components
  ((:file "package")
   (:file "dom")
   (:file "instructions")
   (:file "diff")
   (:file "core")
   (:file "config")
   (:file "proxy")
   (:file "websocket")
   (:file "http")
   (:file "hooks")
   (:file "main")))
