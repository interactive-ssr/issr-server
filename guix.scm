(define-module (guix))

(use-modules
  ((guix licenses) #:prefix license:)
  (guix packages)
  (guix git-download)
  (guix build-system asdf)
  (gnu packages uglifyjs)
  (gnu packages databases)
  (gnu packages lisp-xyz)
  (gnu packages lisp))

(define-public issr-server
  (let ((commit "498db84a5cb94d8161c9c54ccd51171b72baf489")
        (revision "1"))
    (package
      (name "issr-server")
      (version "1";; (git-version "1" revision commit)
               )
      (source "/home/charles/issr/issr-server"
              ;; (origin
              ;;  (method git-fetch)
              ;;  (uri (git-reference
              ;;        (url "https://github.com/interactive-ssr/issr-server")
              ;;        (commit commit)))
              ;;  (file-name (git-file-name "issr-server" version))
              ;;  (sha256
              ;;   (base32 "0pp1660sayhb2x6ry72bamn8nnj08aljlfz1126xbc50zlr5r1q1")))
              )
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir (lambda _ (chdir "src") #t))
           (add-after 'chdir 'copy-resources
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((dir "resources/-issr/")
                      (share (string-append
                              (assoc-ref outputs "out")
                              "/share/"))
                      (issr (string-append share dir)))
                 (install-file (string-append dir "favicon.ico") issr)
                 (install-file (string-append dir "issr.js") issr)
                 (install-file "default-config.lisp" share)
                 (install-file "redis.conf" share))
               #t))
           (add-after 'copy-resources 'minify
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "uglifyjs"
                       "resources/-issr/issr.js"
                       "--compress"
                       "--mangle"
                       "--output"
                       (string-append
                        (assoc-ref outputs "out")
                        "/share/resources/-issr/issr.min.js"))
               #t))
           (add-after 'create-asdf-configuration 'build-program
             (lambda* (#:key outputs #:allow-other-keys)
               (build-program
                (string-append (assoc-ref outputs "out") "/bin/issr-server")
                outputs
                #:entry-program
                `((uiop:chdir ,(string-append
                                (assoc-ref outputs "out")
                                "/share/"))
                  (setq *default-pathname-defaults* (uiop:getcwd))
                  (issr.server:main))))))))
      (native-inputs
       `(("uglifyjs" ,node-uglify-js)))
      (inputs
       `(("cl-base64" ,sbcl-cl-base64)
         ("cl-redis" ,sbcl-cl-redis)
         ("cl-str" ,sbcl-cl-str)
         ("do-urlencode" ,sbcl-do-urlencode)
         ("hunchentoot" ,sbcl-hunchentoot)
         ("jonathan" ,sbcl-jonathan)
         ("plump" ,sbcl-plump)
         ("portal" ,sbcl-portal)
         ("trivia" ,sbcl-trivia)
         ("uuid" ,sbcl-uuid)
         ("yxorp" ,sbcl-yxorp)))
      (propagated-inputs
       `(("redis" ,redis)))
      (home-page "https://github.com/interactive-ssr/issr-server")
      (synopsis "Interactive Server Side Rendering Server")
      (description
       "A reverse proxy server that provides its own resources to make your HTML
interactive by programming your web pages recursively via calling the rr on
Javascript events.")
      (license (list license:lgpl2.1       ;for issr.js
                     license:agpl3)))))

issr-server
