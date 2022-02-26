store=$(guix pack -f tarball \
             -RR \
             -S /bin=bin \
             -S /share=share \
             bash \
             -e '(load "guix.scm")')
cp $store ./issr-server.tar.gz
