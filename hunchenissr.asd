(defpackage hunchenissr-asd
  (:use #:cl #:asdf))
(in-package #:hunchenissr-asd)

(defsystem #:hunchenissr
  :description "Make Interactive-Server-Side-Rendered web pages with declaritive and recursive programming."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "LLGPL"
  :version "0.2"
  :serial t
  :depends-on (#:jonathan
               #:plump
               #:str
               #:portal
               #:cl-base64
               #:hunchentoot
               #:issr-core)
  :components ((:file "package")
               (:file "hunchenissr")))
