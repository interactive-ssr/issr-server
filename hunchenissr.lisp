(in-package #:hunchenissr)

(defvar *ws-port* nil
  "The port to host the websocket server on.")

(defvar *clients* (make-hash-table :test 'equalp)
  "Key: socket, Value: (list *request* page).
Before connecting by websocket, the key is the identifier.")

(defvar *id* nil
  "Used to identify the socket at the first connection.")

(defvar *socket* nil
  "The current socket being used.
Do NOT set this globally; only bind dymaically.")

(defvar *first-time* t
  "T if it is the first time a connection is being made.
Do NOT set this globally; only bind dynamically.")

(defun generate-id (&optional (length 9) (not-these (hash-keys *clients*)))
  "Genereate a random number that has LENGTH digits and is not a member of NOT-THESE.
No leading zeros."
  (let ((id (+ (expt 10 (- length 1)) (random (- (expt 10 length) 1)))))
    (if (member id not-these)
        (generate-id length not-these)
        id)))

(defmacro define-easy-handler (description lambda-list &body body)
  `(hunchentoot:define-easy-handler ,description ,lambda-list
     (let* ((*id* (generate-id))
            (page (block nil ,@body)))
       (unless *socket*
         (setf (gethash *id* *clients*)
               (list hunchentoot:*request* (strip (parse page)))))
       page)))

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
                (comment-p old-node)
                (and (element-p old-node)
                     (gethash "noupdate" (attributes old-node) nil)
                     (element-p new-node)
                     (gethash "noupdate" (attributes new-node) nil)))
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
                ((or (< diff-length 0)
                     (attribute new-node "noupdate"))
                 (remove-child old-node) ;shift siblings
                 (diff-dom old-dom new-dom index indexes
                           (append instructions
                                   (list (cons "delete" (reverse (cons index indexes)))))))
                ;; add
                ((or (< 0 diff-length)
                     (attribute old-node "noupdate"))
                 (insert-before old-node nil) ;shift siblings
                 (diff-dom old-dom new-dom (+ index 1) indexes
                           (append instructions
                                   (list (list "insert" (reverse (cons index indexes)) 0 "before"
                                               (serialize new-node nil))))))
                ;; replace
                (:else
                 (diff-dom old-dom new-dom (+ index 1) indexes
                           (append instructions
                                   (list (list "mod" (reverse (cons index indexes)) 0 "before"
                                               (list "outerHTML" (serialize new-node nil))))))))))
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

(defun handle-post-data (data)
  "Return a list of get/post parameters from json/hash-table DATA.
Create any files necessary."
  (let (params)
    (dolist (name (hash-keys data))
      (let ((value (gethash name data)))
        (if (listp value)
            (dolist (value value)
              (push (if (hash-table-p value)
                        (let ((path (hunchentoot::make-tmp-file-name))
                              (byte-array (base64:base64-string-to-usb8-array
                                           (gethash "content" value))))
                          (ensure-directories-exist (directory-namestring path))
                          (when hunchentoot:*file-upload-hook*
                            (funcall hunchentoot:*file-upload-hook* path))
                          (with-open-file (outfile path :if-does-not-exist :create
                                                        :if-exists :supersede
                                                        :direction :output
                                                        :element-type '(unsigned-byte 8))
                            (write-sequence byte-array outfile))
                          (cons name (list path
                                           (gethash "name" value)
                                           (gethash "type" value))))
                        (cons name value))
                    params))
            (push (cons name value) params))))
    (reverse params)))

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
    (let ((previous-params (copy-tree (hunchentoot:get-parameters*))))
      (cond
        ;; query string
        ((str:starts-with-p "?" parameters)
         (setf (slot-value hunchentoot:*request* 'hunchentoot:query-string)
               (subseq parameters 1))
         ;; recaclculate parameters
         (hunchentoot:recompute-request-parameters :request hunchentoot:*request*))
        ;; multipart/form-data
        ((str:starts-with-p "post:" parameters)
         (let ((data (handle-post-data (jojo:parse (subseq parameters 5) :as :hash-table))))
           (setf (slot-value hunchentoot:*request* 'hunchentoot:get-parameters) data))))
      ;; fill in missing parameters from previous request
      (let ((temp-params (copy-tree (slot-value hunchentoot:*request* 'hunchentoot:get-parameters))))
        (dolist (elm previous-params)
          (unless (member (car elm) (slot-value hunchentoot:*request* 'hunchentoot:get-parameters) :key #'car :test #'string=)
            (setf temp-params
                  (append temp-params
                          (list elm)))))
        (setf (slot-value hunchentoot:*request* 'hunchentoot:get-parameters) temp-params))
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
        (clws:write-to-client-text socket (jojo:to-json instructions))))))

(defclass issr-resource (clws:ws-resource) ())
(defmethod resource-client-connected ((resource issr-resource) socket) t)
(defmethod clws:resource-client-disconnected ((resource issr-resource) socket)
  (remhash socket *clients*))
(defmethod clws:resource-received-text ((resource issr-resource) socket message)
  (if (str:starts-with-p "id:" message)
      (let* ((id (parse-integer (subseq message 3)))
             (info (gethash id *clients*)))
        (if info
            (progn
              (setf (gethash socket *clients*) info)
              (format t "Connected to client with id:~a.~%" id)
              (remhash id *clients*))
            (progn
              (warn "Uhhhhm, id:~a doesn't exist.~%" id)
              (return-from clws:resource-received-text))))
      ;; giving parameters to update page
      (if (and (gethash socket *clients*)
               (or (str:starts-with-p "?" message)
                   (str:starts-with-p "post:" message)))
          (rr socket message)
          (warn "Suspicious websoket connection from ~a.~%" socket))))
(clws:register-global-resource
 "/" (make-instance 'issr-resource)
 #'clws:any-origin)
(bordeaux-threads:make-thread
 (lambda ()
   (clws:run-resource-listener
    (clws:find-global-resource "/")))
 :name "resource listener for issr")

(defmacro start (acceptor-or-servers &key ws-port)
  `(progn
     (when ,ws-port (setq *ws-port* ,ws-port))
     (unless *ws-port* (setq *ws-port* 4433))
     ,(let ((servers `(list (hunchentoot:start ,acceptor-or-servers)
                            (bordeaux-threads:make-thread
                             (lambda () (clws:run-server *ws-port*))
                             :name "issr-ws-server"))))
        (setq hunchentoot:*acceptor* (first servers))
        (if (symbolp acceptor-or-servers)
            `(setf ,acceptor-or-servers ,servers)
            servers))))

(defmacro stop (servers)
  `(progn
     (bordeaux-threads:destroy-thread (second ,servers))
     (setq hunchentoot:*acceptor* nil)
     (setf ,servers (hunchentoot:stop (first ,servers)))))

(defmacro redirect (target)
  `(if *socket*
       (return (list "redirect" ,target))
       (hunchentoot:redirect ,target)))
