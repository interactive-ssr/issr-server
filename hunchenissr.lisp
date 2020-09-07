(in-package #:hunchenissr)

(defvar *ws-port* nil
  "The port to host the websocket server on.")

(defun generate-id (&optional (length 9) (not-these (hash-keys *clients*)))
  "Genereate a random number that has LENGTH digits and is not a member of NOT-THESE.
No leading zeros."
  (let ((id (+ (expt 10 (- length 1)) (random (- (expt 10 length) 1)))))
    (if (member id not-these)
        (generate-id length not-these)
        id)))

(defvar *id* nil
  "Used to identify the socket at the first connection.")
(defvar *socket* nil
  "The current socket being used.
Do NOT set this globally; only bind dymaically.")
(defvar *first-time* t
  "T if it is the first time a connection is being made.
Do NOT set this globally; only bind dynamically.")

(defmacro define-easy-handler (description lambda-list &body body)
  `(hunchentoot:define-easy-handler ,description ,lambda-list
     (let* ((*id* (generate-id))
            (page ,(cons 'block (cons nil body))))
       (unless *socket*
         (setf (gethash *id* *clients*)
               (list hunchentoot:*request* (strip (parse page)))))
       page)))

(defvar *clients* (make-hash-table :test 'equalp)
  "Key: socket, Value: (list *request* page).
Before connecting by websocket, the key is the identifier.")

(defun hash-keys (hash-table)
  (loop :for key :being :the :hash-keys :of hash-table
        :collect key))

(defun descendant (node indexes)
  "Return the dom node which is the child of ancestor NODE.
INDEXES is a list of locations of the children list of NODE."
  (if (null indexes)
      node
      (descendant
       (aref (children node) (car indexes))
       (cdr indexes))))

(defun diff-dom (old-dom new-dom
                 ;; only for recursion
                 &optional (index 0) indexes instructions)
  "Return a list of instructions to update the dom of OLD to look like NEW.
OLD and NEW should both be plump virtual doms.
See issr.js for possible instructions.
Does not preserve old-dom.

INDEXES: Reversed list of indexes to reach the current parent.
INDEX: (aref (children parent) INDEX) to get current node."
  (let ((old-tree (descendant old-dom (reverse indexes)))
        (new-tree (descendant new-dom (reverse indexes))))
    (cond
      ;; base case: no more tree to traverse
      ((and (null indexes) (>= index (length (children new-tree))))
       instructions)
      ;; move to next sibling of parent with no instructions
      ((and (>= index (length (children old-tree)))
                (>= index (length (children new-tree))))
       (diff-dom old-dom new-dom (+ (car indexes) 1)
                 (cdr indexes) instructions))
      ;; insert rest of children from new
      ((>= index (length (children old-tree)))
       (let* ((children (children new-tree))
              (length-children (length children))
              (insert-instructions
                (loop for i from index to (- length-children 1)
                      collect
                      (list "insert"
                            (reverse (if (= i 0) indexes (cons (- i 1) indexes)))
                            0
                            (if (= i 0) "prepend" "after")
                            (serialize (aref children i) nil)))))
         (diff-dom old-dom new-dom length-children indexes
                   (append instructions insert-instructions))))
      ;; remove rest of children from old
      ((>= index (length (children new-tree)))
       (let* ((length-children (length (children old-tree)))
              (delete-instructions
                (loop for i from (- length-children 1) downto index
                      with node = (descendant old-tree (list index))
                      if (if (element-p node)
                             (gethash "noupdate" (attributes node) t)
                             t)
                        collect (cons "delete" (reverse (cons index indexes))))))
         (diff-dom old-dom new-dom length-children indexes
                   (append instructions delete-instructions))))
      ;;; start comparing the current node
      (:else
       (let ((old-node (descendant old-tree (list index)))
             (new-node (descendant new-tree (list index))))
         (cond
           ;; move to the next sibling with no instructions
           ((or (doctype-p old-node)
                (and (element-p old-node)
                     (gethash "noupdate" (attributes old-node) nil)))
            (diff-dom old-dom new-dom (+ index 1) indexes instructions))
           ;; update text if current node is a text node
           ((text-node-p old-node)
            (diff-dom old-dom new-dom (+ index 1) indexes
                      (if (and (text-node-p new-node)
                               (string= (text old-node)
                                        (text new-node)))
                          instructions
                          (progn
                            (insert-before old-node nil)   ;add a nil to shift the node array
                            (append instructions
                                    (list (list "insert" (reverse (cons index indexes)) 1 "before"
                                                (text new-node))))))))
           ;; add, delete, or replace
           ((or (not (eq (type-of old-node) (type-of new-node)))
                (string/= (if (element-p old-node) (tag-name old-node) "")
                          (if (element-p new-node) (tag-name new-node) ""))
                (string/= (gethash "id" (attributes old-node) "")
                          (gethash "id" (attributes new-node) "")))
            (let ((diff-length (- (length (family new-node))
                                  (length (family old-node)))))
              (cond
                ;; delete
                ((< diff-length 0)
                 (remove-child old-node) ;shift siblings
                 (diff-dom old-dom new-dom index indexes
                           (append instructions
                                   (list (cons "delete" (reverse (cons index indexes)))))))
                ;; add
                ((< 0 diff-length)
                 (insert-before old-node nil) ;shift siblings
                 (diff-dom old-dom new-dom (+ index 1) indexes
                           (append instructions
                                   (list (list "insert" (reverse (cons index indexes)) 0 "before"
                                               (serialize new-node nil))))))
                ;; replace
                (:else
                 (diff-dom old-dom new-dom (+ index 1) indexes
                           (append instructions
                                   (list (list "insert" (reverse (cons index indexes)) 0 "before"
                                               (serialize new-node nil))
                                         (cons "delete" (reverse (cons (+ index 1) indexes))))))))))
           ;; update attrs then descend into children
           (:else
            (let ((new-attrs
                    (and (element-p old-node)
                         (element-p new-node)
                         (remove-if #'null
                                    (mapcar (lambda (key)
                                              (let ((old-value (gethash key (attributes old-node) ""))
                                                    (new-value (gethash key (attributes new-node) "")))
                                                (when (string/= old-value new-value)
                                                  (list key new-value))))
                                            (union (hash-keys (attributes new-node))
                                                   (hash-keys (attributes old-node))
                                                   :test 'string=))))))
              (diff-dom old-dom new-dom 0 (cons index indexes)
                        (if new-attrs
                            (append instructions (list (append (list "mod") (list (reverse (cons index indexes))) new-attrs)))
                            instructions))))))))))

(defun unify-query-strings (old-query new-query)
  "Return a new query string return a new query string wtih all the values from
NEW-QUERY and any values from OLD-QUERY that are missing from NEW-QUERY.
\"?\" should not be included in the query strings."
  (let ((old-pairs (str:split "&" old-query))
        (new-pairs (str:split "&" new-query)))
    (str:join "&" (append new-pairs
                          (remove-if (lambda (old-pair)
                                       (member old-pair new-pairs
                                               :test (lambda (old-pair new-pair)
                                                       (string= (subseq old-pair 0 (position "=" old-pair :test 'string=))
                                                                (subseq new-pair 0 (position "=" new-pair :test 'string=))))))
                                     old-pairs)))))

(defun rr (socket &optional (parameters "?"))
  "Send a Re-Render to SOCKET with query string PARAMETERS."
  (let* ((*socket* socket)
         (*first-time* nil)
         (info (gethash socket *clients*))
         (hunchentoot:*request* (car info))
         (hunchentoot:*session* (hunchentoot:session hunchentoot:*request*))
         (hunchentoot:*reply* (make-instance 'hunchentoot:reply))
         (handler (hunchentoot:dispatch-easy-handlers hunchentoot:*request*))
         (previous-page (cadr info)))
    ;; update query string for request
    (setf (slot-value hunchentoot:*request* 'hunchentoot:query-string)
          (unify-query-strings (or (slot-value hunchentoot:*request* 'hunchentoot:query-string) "")
                               (subseq parameters 1)))
    (hunchentoot:recompute-request-parameters :request hunchentoot:*request*)
    ;; generate page and instructions
    (let ((new-page (funcall handler))
          (instructions (list)))
      (when (= 200 (hunchentoot:return-code hunchentoot:*reply*))
        ;; cookies
        (with-slots (hunchentoot:cookies-out) hunchentoot:*reply*
          (when hunchentoot:cookies-out
            (setf (slot-value hunchentoot:*request* 'hunchentoot:cookies-in)
                  (mapcar (lambda (cookie)
                            (cons (car cookie)
                                  (slot-value (cdr cookie) 'hunchentoot::value)))
                          hunchentoot:cookies-out))
            (push (cons "cookie" (mapcar (lambda (cookie)
                                           (hunchentoot::stringify-cookie (cdr cookie)))
                                         hunchentoot:cookies-out))
                  instructions)))
        ;; session
        (when hunchentoot:*session*
          (with-slots (hunchentoot::session-data) hunchentoot:*session*
            (when hunchentoot::session-data
              (push (list "session" (mapcar (lambda (data) (list (car data) (cdr data)))
                                            hunchentoot::session-data))
                    instructions))))
        (if (listp new-page)
            ;; redirect or some other custom instruction
            (push new-page instructions)
            ;; dom instructions
            (let ((new-page (strip (parse new-page))))
              (setq instructions
                    (append (diff-dom previous-page new-page)
                            instructions))
              (setf (cadr (gethash *socket* *clients*)) new-page))))
      (send socket (jojo:to-json instructions)))))

(defun socket-handler (socket message)
  ;; first connection
  (if (string= "id:" (subseq message 0 3))
      (let* ((id (parse-integer (subseq message 3)))
             (info (gethash id *clients*)))
        (if info
            (progn
              (setf (gethash socket *clients*) info)
              (format t "Connected to client with id ~a." id)
              (remhash id *clients*))
            (progn
              (warn "Uhhhhm, id \"~a\" doesn't exist." id)
              (return-from socket-handler))))
      ;; giving parameters to update page
      (if (and (gethash socket *clients*)
               (string= "?" (subseq message 0 1)))
          (rr socket message)
          (warn "Suspicious websoket connection from ~a." socket))))

(defun socket-server-handler (env)
  (let ((socket (make-server env)))
    (on :message socket (lambda (message) (socket-handler socket message)))
    (on :close socket
        (lambda (&key code reason) (declare (ignore code reason))
          (remhash socket *clients*)))
    (lambda (responder) (declare (ignore responder))
      (start-connection socket))))

(defun start (acceptor-or-servers
              &key (ws-port 5000)
                (address "0.0.0.0")
                (debug t) silent (use-thread t)
                (use-default-middlewares t) &allow-other-keys)
  (setq *ws-port* ws-port)
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

(defmacro redirect (target)
  `(if *socket*
       (return (list "redirect" ,target))
       (hunchentoot:redirect ,target)))
