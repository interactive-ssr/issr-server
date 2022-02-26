(define-module (guix))

(use-modules
  ((guix licenses) #:prefix license:)
  (guix packages)
  (guix gexp)
  (guix git-download)
  (guix build-system asdf)
  (gnu packages uglifyjs)
  (gnu packages databases)
  (gnu packages lisp-xyz)
  (gnu packages lisp))

(let ((commit "b411092d04f9d38426ce80a47bf1770c0d29e910")
      (revision "1"))
  (package
   (name "issr-server")
   (version "1";; (git-version "1" revision commit)
            )
   (source (local-file (getcwd)
                       #:recursive? #t)
           ;; (origin
           ;;  (method git-fetch)
           ;;  (uri (git-reference
           ;;        (url "https://github.com/interactive-ssr/issr-server")
           ;;        (commit commit)))
           ;;  (file-name (git-file-name "issr-server" version))
           ;;  (sha256
           ;;   (base32 "1i82xx13nxczp2kqv58z9rcnmnx845jzan465sif6896iyihq6ba")))
           )
   (build-system asdf-build-system/sbcl)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'chdir (lambda _ (chdir "src") #t))
        (add-after 'unpack 'set-env
          (lambda* (#:key outputs #:allow-other-keys)
            (setenv "PREFIX" (assoc-ref outputs "out"))))
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
              (install-file "redis.conf" share))))
        (add-after 'copy-resources 'minify
          (lambda* (#:key outputs #:allow-other-keys)
            (invoke "uglifyjs"
                    "resources/-issr/issr.js"
                    "--compress"
                    "--mangle"
                    "--output"
                    (string-append
                     (assoc-ref outputs "out")
                     "/share/resources/-issr/issr.min.js"))))
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
   (native-inputs (list node-uglify-js))
   (inputs
    (list sbcl-cl-base64
          sbcl-cl-redis
          sbcl-cl-str
          sbcl-do-urlencode
          sbcl-hunchentoot
          sbcl-jonathan
          sbcl-plump
          sbcl-portal
          sbcl-rutils
          sbcl-trivia
          sbcl-uuid
          (package
           (inherit sbcl-yxorp)
           (source (local-file "/home/charles/cl-yxorp"
                               #:recursive? #t)))))
   (propagated-inputs (list redis))
   (home-page "https://github.com/interactive-ssr/issr-server")
   (synopsis "Interactive Server Side Rendering Server")
   (description
    "A reverse proxy server that provides its own resources to make your HTML
interactive by programming your web pages recursively via calling the rr on
Javascript events.")
   (license (list license:lgpl2.1       ;for issr.js
                  license:agpl3))))
