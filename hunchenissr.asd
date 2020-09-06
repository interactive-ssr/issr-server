(defpackage hunchenissr-asd
  (:use #:cl #:asdf))
(in-package #:hunchenissr-asd)

(defsystem #:hunchenissr
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
               (:file "hunchenissr" :depends-on ("package"))))
