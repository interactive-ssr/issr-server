(in-package #:asdf-user)

(defsystem issr
  :description "Make Interactive-Server-Side-Rendered web pages with declaritive and recursive programming."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "AGPLv3"
  :version "1"
  :depends-on
  (#:cl-base64
   #:alexandria
   #:trivia
   #:hunchentoot
   #:jonathan
   #:plump
   #:portal
   #:bordeaux-threads
   #:trivial-garbage
   #:do-urlencode
   #:str
   #:yxorp)
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
   (:file "main")))
