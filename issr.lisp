(in-package #:issr)

(defvar *ws-port* nil
  "The port to host the websocket server on.")

(defun generate-id () )

(defvar *id* nil
  "Used to identify the socket at the first connection.")
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
       (unless *socket*
         (setf (gethash *id* *clients*)
               (list hunchentoot:*request* (strip (parse page)))))
       page)))

(defvar *clients* (make-hash-table)
  "Key: socket, Value: (list *request* page).
Before connecting by websocket, the key is the identifier.")

(defmacro nlet (name bindings &body body)
  `(labels ((,name ,(mapcar #'first bindings) ,@body))
     (,name ,@(mapcar #'second bindings))))

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
                 &optional (index 0) indexes instructions)
  "Return a list of instructions to update the dom of OLD to look like NEW.
OLD and NEW should both be plump virtual doms.
Possible instructions:
(\"delete\" (indexes)): .outerHTML = \"\";
(\"insert\" (indexes) position html-string): create nil node; .prepend(node), .before(node), or .after(node); node.outerHTML = html-string;
(\"mod\" (indexes) (attr-name attr-value)...): .setAttribute(attr-name, attr-value);
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
                            (reverse (if (= i 0) indexes (cons i indexes)))
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
      ;; move to the next sibling with no instructions
      ((or (doctype-p (descendant old-tree (list index)))
           (and (element-p (descendant old-tree (list index)))
                (gethash "noupdate" (attributes (descendant old-tree (list index))) nil)))
       (diff-dom old-dom new-dom (+ index 1) indexes instructions))
      ;; update text if current node is a text node
      ((text-node-p (descendant old-tree (list index)))
       (diff-dom old-dom new-dom (+ index 1) indexes
                 (let ((old-node (descendant old-tree (list index)))
                       (new-node (descendant new-tree (list index))))
                   (if (and (text-node-p new-node)
                          (string= (text old-node)
                                   (text new-node)))
                     instructions
                     (progn
                       (insert-before old-node nil)   ;add a nil to shift the node array
                       (append instructions
                               (list (list "insert" (reverse (cons index indexes)) "before"
                                                            (text new-node)))))))))
      ;; insert new before
      ((let ((old-node (descendant old-tree (list index)))
             (new-node (descendant new-tree (list index))))
         (or (not (eq (type-of old-node) (type-of new-node)))
             (string/= (if (element-p old-node) (tag-name old-node) "")
                       (if (element-p new-node) (tag-name new-node) ""))
             (string/= (gethash "id" (attributes old-node) "")
                       (gethash "id" (attributes new-node) ""))))
       (insert-before (descendant old-tree (list index)) nil)   ;add a nil to shift the node array
       (diff-dom old-dom new-dom (+ index 1) indexes
                 (append instructions
                         (list (list "insert" (reverse (cons index indexes)) "before"
                                     (serialize (descendant new-tree (list index)) nil))))))
      ;; update attrs then descend into children
      (:else
       (let* ((old-node (descendant old-tree (list index)))
              (new-node (descendant new-tree (list index)))
              (new-attrs
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
                       instructions)))))))

(defun rr (socket &optional (parameters "?"))
  "Send a Re-Render to SOCKET with query string PARAMETERS."
  (let* ((*socket* socket)
         (*first-time* nil)
         (info (gethash socket *clients*))
         (hunchentoot:*request* (car info))
         (hunchentoot:*request* (hunchentoot:session hunchentoot:*request*))
         (hunchentoot:*reply* (make-instance 'hunchentoot:reply))
         (handler (hunchentoot:dispatch-easy-handlers hunchentoot:*request*))
         (previous-page (cadr info)))
    ;; update query string for request
    (setf (slot-value hunchentoot:*request* 'hunchentoot:query-string) (subseq parameters 1))
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
            (push (list "cookie" (mapcar (lambda (cookie)
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
            (setq instructions
                  (append (diff-dom previous-page
                                    (strip (parse new-page)))
                          instructions))))
      (send socket (jojo:to-json instructions)))))

(defun socket-handler (socket message)
  ;; first connection
  (when (string= "id:" (subseq message 0 3))
    (let* ((id (subseq message 3))
           (info (gethash id *clients* nil)))
      (if info
          (progn
            (setf (gethash socket *clients*) info)
            (remhash id *clients*))
          (progn
            (warn (format nil "Uhhhhm, id \"~a\" doesn't exist." id))
            (return-from socket-handler)))))
  ;; giving parameters to update page
  (when (string= "?" (subseq message 0 1))
    (rr socket message)))

(defun socket-server-handler (env)
  (let ((socket (make-server env)))
    (on :message socket (lambda (message) (socket-handler socket message)))
    (on :close socket
        (lambda (&key code reason) (declare (ignore code reason))
          (remhash socket *clients*)))
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

(defmacro redirect (target)
  `(if *socket*
       (return (list "redirect" ,target)
       (hunchentoot:redirect ,target))))
