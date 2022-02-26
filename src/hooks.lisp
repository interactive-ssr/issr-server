(in-package #:issr.server)

(defun server-uuid ()
  (let ((filename
          (merge-pathnames
           "issr/uuid.txt"
           (or (uiop:getenvp "XDG_DATA_HOME")
               "~/.local/share/"))))
    (unless (uiop:directory-exists-p (pathname-directory filename))
      (ensure-directories-exist filename))
    (if (uiop:file-exists-p filename)
        (uiop:read-file-string filename)
        (with-open-file (out filename
                             :direction :output
                             :if-does-not-exist :create)
          (let ((uuid (princ-to-string (uuid:make-v4-uuid))))
            (write-sequence uuid out))))))

(defvar *hook-listener-name* "issr-server-hook-listener")

(defun start-hook-listener (host port show-errors redis-host redis-port redis-pass)
  (bt:make-thread
   (lambda ()
     (block exit
       (loop
         (block continue
           (handler-case
               (handler-bind
                   ((redis:redis-error-reply
                      (lambda (c)
                        (when (str:containsp "WRONGPASS"
                                             (redis:redis-error-message c))
                          (format *error-output*
                                  (str:concat
                                   "Could not connect to redis. "
                                   "This is likely something wrong with your redis-config. "
                                   "Make sure you are connecting to the correct redis-server with the correct password. "
                                   "If you redis destination is just `6379', make sure there are no redis servers on that port since ISSR-server will try to start one there.~%"))
                          (uiop:quit)))))
                 (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
                   (format t "Connected to redis.~%")
                   (red:subscribe "issr-rr")
                   (loop
                     (let ((message (redis:expect :anything)))
                       (bt:make-thread
                        (lambda ()
                          (match (jojo:parse (third message) :as :alist)
                            ((guard (list uuid client-id args)
                                    (string= uuid (server-uuid)))
                             (redis:with-connection (:host redis-host :port redis-port :auth redis-pass)
                               (some-> client-id
                                 get-id-client
                                 (rr host port show-errors args))))))
                        :name "re-rendering")))))
             (end-of-file () (return-from exit))
             (usocket:connection-refused-error ()
               (sleep 2)
               (format t "Trying to connect to redis.~%")
               (return-from continue)))))))
   :name *hook-listener-name*))

(defun stop-hook-listener ()
  (let ((hook-listeners
          (remove *hook-listener-name*
                  (bt:all-threads)
                  :key 'bt:thread-name
                  :test-not 'string=)))
    (when hook-listeners
      (mapc 'bt:destroy-thread hook-listeners))))

