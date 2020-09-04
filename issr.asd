(defpackage issr-asd
  (:use #:cl #:asdf))
(in-package #:issr-asd)

(defsystem #:issr
  :description "Make Interactive-Server-Side-Rendered web pages with declaritive and recursive programming."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :version "0.1"
  :serial t
  :depends-on (#:jonathan
               #:plump
               #:websocket-driver-server
               #:str
               #:clack
               #:hunchentoot)
  :components ((:file "package")
               (:file "issr" :depends-on ("package"))))
