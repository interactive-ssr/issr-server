guix pack -f docker \
     --entry-point=/bin/issr-server \
     -S /bin=bin \
     -S /share=share \
     bash \
     -e '(load "guix.scm")'