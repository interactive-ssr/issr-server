store=$(guix pack -f docker \
             --entry-point=/bin/issr-server \
             -S /bin=bin \
             -S /share=share \
             bash \
             -e '(load "guix.scm")')
cp --remove-destination $store ./issr-server-docker.tar.gz
