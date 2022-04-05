store=$(guix pack -f tarball \
             -RR \
             -S /bin=bin \
             -S /share=share \
             bash \
             -e '(load "guix.scm")')
cp --remove-destination $store ./issr-server.tar.gz
