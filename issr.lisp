(mapc #'ql:quickload '(#:jonathan #:plump #:websocket-driver-server #:str #:clack #:hunchentoot))
(defpackage #:issr
  (:use #:cl #:plump #:websocket-driver))
(in-package #:issr)

(defvar *ws-port* nil
  "The port to host the websocket server on.")

(defun generate-id () )

(defun update-reply (socket-or-id reply) )

(defvar *socket* nil
  "The current socket being used. 
Do NOT set this globally; only bind dymaically.")
(defvar *first-time* t
  "T if it is the first time a connection is being made.
Do NOT set this globally; only bind dynamically.")

(defmacro define-easy-handler (description lambda-list &body body)
  `(hunchentoot:define-easy-handler description lambda-list
     (let* ((*id* (generate-id))
            (page ,(cons block (cons nil body))))
       ;; Do whatever needs to be done with responses and replies
       (when (null *socket*)
         (setf (gethash *id* *clients*)
               (list *request* (strip (parse page)))))
       (setq *out-reply* *reply*)
       page)))

(defvar clients (make-hash-table)
  "Key: socket, Value: (list *request* page).
Before connecting by websocket, the key is the  identifier.")

(defvar *out-reply* nil
  "The reply for the websocket. 
Do NOT set globally; only bind dynamically.")

(defmacro nlet (name bindings &body body)
  `(labels ((,name ,(mapcar #'first bindings) ,@body))
     (,name ,@(mapcar #'second bindings))))

(defun hash-keys (hash-table)
  (loop :for key :being :the :hash-keys :of hash-table
        :collect key))

(defun socket-handler (message)
  ;; first connection
  (when (string= "id:" (subseq message 0 3))
    (let* ((id (subseq message 3))
           (info (gethash id *client-info* nil)))
      (if info
          (progn
            (setf (gethash socket *client-info*) info)
            (remhash id *client-info*))
          (progn
            (warn (format nil "Uhhhhm, id \"~a\" doesn't exist." id))
            (return-from socket-handler)))))
  ;; giving parameters to update page
  (when (string= "?" (subseq message 0 1))
    (let* ((*socket* socket)
           (*first-time* nil)
           (info (gethash socket *client-info*))
           (*request* (car info))
           (*session* (slot-value *request* 'session))
           (*out-reply* nil)
           (handler (hunchentoot:dispatch-easy-handlers *request))
           (previous-page (cadr info)))
      (setf (slot-value request 'query-string) (subseq message 1))
      (hunchentoot:recompute-request-parameters :request *request*)
      (let ((new-page (funcall (hunchentoot:dispatch-easy-handlers *request*)))
            (instructions (list)))
        (when (*out-reply* (slot-value *out-reply* 'return-code))
          (with-slots (cookies-out) *out-reply*
            (when cookies-out
              (setf (slot-value *request* 'cookies-in)
                    (mapcar (lambda (cookie)
                        (cons (car cookie)
                              (slot-value (cdr cookie) 'value)))))
              (push (cons :cookie (hunchentoot::stringify-cookie cookies-out))
                    instructions)))
          (with-slots (session-data) *session*
            (when session-data
              (push (cons :session (slot-value *session* 'session-data))
                    instructions)))
          (when (string= "{" new-page)
            (push)))))
        ())
      (send socket (jojo:to-json '(:params message))))

(defun socket-server-handler (env)
  (let ((socket (make-server env)))
    (on :message socket socket-handler)
    (on :close socket
        (lambda (&key code reason) (declare (ignore code reason))
          (remhash socket *client-info*)))
    (lambda (responder) (declare (ignore responder))
      (start-connection socket))))

(defparameter socket-server
  (clack:clackup #'socket-server-handler :server :hunchentoot))

(defun start (acceptor-or-servers
              &key (ws-port 5000)
                (address "127.0.0.1")
                (debug t) silent (use-thread t)
                (use-default-middlewares t) &allow-other-keys)
  (if ws-port (setq *ws-port* ws-port))
  (if (listp acceptor-or-servers)
      (list (hunchentoot:start (car acceptor-or-servers))
            (clack:clackup (cadr acceptor-or-servers)))
      (list (hunchentoot:start acceptor-or-servers)
            (clack:clackup #'socket-server-handler
                           :server :hunchentoot :port ws-port
                           :address address :debug debug :silent silent
                           :use-thread use-thread :use-default-middlewares use-default-middlewares))))

(defun stop (servers)
  (clack:stop (cadr servers))
  (list (hunchentoot:stop (car servers))
        (cadr servers)))

(defmacro redirect (target &key (socket *socket*))
  `(if socket
       (return (:redirect ,target))
       (hunchentoot:redirect ,target)))
