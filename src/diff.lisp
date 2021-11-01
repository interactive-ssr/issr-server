(in-package #:issr.server)

(defun diff-attributes (old new)
  (let ((cons-list
          (lambda (pair)
            (list (car pair) (cdr pair))))
        (attrs-equal-p
          (lambda (attr1 attr2)
            (and (eq (car attr1)
                     (car attr2))
                 (string= (cdr attr1)
                          (cdr attr2))))))
    (if (not (str:emptyp (node-attribute new :update)))
        (list (i:mod (node-id new)
                     (map 'list cons-list
                          (remove :id (node-attributes new)
                                  :key 'car))))
        (let ((to-remove (set-difference
                          (node-attributes old)
                          (node-attributes new)
                          :key 'car))
              (to-add (set-difference
                       (node-attributes new)
                       (node-attributes old)
                       :test attrs-equal-p)))
          (list
           (i:mod (node-id new)
                  (append
                   (map 'list
                        (lambda (pair)
                          (list (car pair) ""))
                        to-remove)
                   (map 'list cons-list (remove :id to-add :key 'car)))))))))

(defun diff-siblings (old-children new-children
                      &optional instructions)
  (match (list old-children new-children)
    ((or (list nil _) (list _ nil))
     (values instructions old-children new-children))
    ;; a node was removed from the begnining
    ((guard
      (list (list* (node :attributes (alist (:id . old-id)))
                   rest-old)
            (list* (node :attributes (alist (:id . new-id))) _))
      (and new-id (string/= old-id new-id)
           (find new-id rest-old
                 :key 'node-id
                 :test 'string=)))
     (diff-siblings rest-old new-children
                    (append instructions
                            (list (i:delete (list old-id))))))
    ;; a node was added to the begining
    ((guard
      (list (list* (node :attributes (alist (:id . old-id))) _)
            (list* new-node rest-new))
      (and (string/= old-id (node-id new-node))
           (find old-id rest-new
                 :key 'node-id
                 :test 'string=)))
     (diff-siblings old-children rest-new
                    (append instructions
                            (list (i:insert old-id :before new-node)))))
    ;; diff the current nodes
    ((list (list* old rest-old) (list* new rest-new))
     (diff-siblings rest-old rest-new
                    (append instructions
                            (diff old new))))))

(defun diff-children (parent-id old-children new-children)
  (multiple-value-bind (instructions to-remove to-add)
      (diff-siblings old-children new-children)
    (append
     instructions
     (when to-add
       (map 'list (curry 'i:insert parent-id :append) to-add))
     (when to-remove
       (list (i:delete (map 'list 'node-id to-remove)))))))

(defun diff-strings (id old-text new-text)
  (unless (string= old-text new-text)
    (list (i:mod id (list (list "textContent" new-text))))))

(defun diff (old new)
  (remove
   nil
   (match (list old new)
     ((list (node :name :!doctype) (node :name :!doctype)) nil)
     ;; noupdate
     ((list (node :attributes
                  (alist
                   (:noupdate . (satisfies
                                 (lambda (value)
                                   (not (str:emptyp value)))))))
            new-node)
      (setf (node-attribute new-node :noupdate) "T")
      nil)
     ;; comment nodes
     ((list (node :name :tn :attributes (alist (:id . id))
                  :children (list (node :name :!comment
                                        :children (list old-text))))
            (node :name :tn
                  :children (list (node :name :!comment
                                        :children (list new-text)))))
      (copy-id old new)
      (diff-strings id old-text new-text))
     ;; text nodes
     ((guard
       (list (node :name old-name :attributes (alist (:id . id))
                   :children (list old-text))
             (node :name new-name
                   :children (list new-text)))
       (and (eq old-name new-name)
            (stringp old-text)
            (stringp new-text)))
      (copy-id old new)
      (diff-strings id old-text new-text))
     ;; diff attributes and string body
     ((list (node :name :tn :attributes (alist (:id . id))
                   :children (list old-text))
             (node :name :tn
                   :children (list new-text)))
      (copy-id old new)
      (diff-strings id old-text new-text))
     ;; diff attributes then recur on children
     ((guard
       (list (node :name old-name :attributes (alist (:id . id))
                   :children old-children)
             (node :name new-name
                   :children new-children))
       (eq old-name new-name))
      (copy-id old new)
      (append
       (diff-attributes old new)
       (diff-children id old-children new-children)))
     ;; replace
     ((list (node :attributes (alist (:id . id))) _)
      (list
       (i:insert id :after new)
       (i:delete (list id))))
     ;; root
     ((list (node :name :!root) (node :name :!root))
      (diff-children nil (node-children old) (node-children new))))))
