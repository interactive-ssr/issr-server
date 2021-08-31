(in-package #:issr.dom)

(defvar +void-tags+
  (list :br
        :hr
        :img
        :input
        :link
        :meta
        :area
        :base
        :col
        :command
        :embed
        :keygen
        :param
        :source
        :track
        :wbr))

(defclass node ()
  ((name :accessor node-name :initarg :name)
   (attributes :accessor node-attributes :initarg :attributes
               :initform nil)
   (children :accessor node-children :initarg :children
             :initform nil)))

(defun make-node (name attributes &rest children)
  (make-instance
   'node
   :name (if (stringp name)
             (make-keyword (str:upcase name))
             name)
   :children children
   :attributes (remove-duplicates attributes :key 'car :from-end t)))

(defmethod node-attribute ((this node) attribute)
  (cdr (assoc attribute (node-attributes this))))

(defmethod (setf node-attribute) (new-value (this node) attribute)
  (let ((place (assoc attribute (node-attributes this))))
    (if place
      (setf (cdr place)
            new-value)
      (setf (slot-value this 'attributes)
            (acons attribute new-value nil)))))

(defmethod node-id ((this node))
  (node-attribute this :id))

(defmethod print-object ((this node) out)
  (case (node-name this)
    (:!doctype
     (format out "<!doctype ~A>" (first (node-children this))))
    (:!comment
     (format out "<!--")
     (dolist (child (node-children this))
       (princ child out))
     (format out "-->"))
    (:!root
     (dolist (child (node-children this))
       (princ child out)))
    (otherwise
     (let ((name (str:downcase (symbol-name (node-name this)))))
       (format out "<~a" name)
       (loop for (key . value) in (node-attributes this) do
         (format out " ~A=~S"
                 (str:downcase (symbol-name key))
                 value))
       (format out ">")
       (dolist (child (node-children this))
         (princ child out))
       (unless (member (node-name this) +void-tags+)
         (format out "</~A>" name))))))

(defvar *in-pre* nil)

(defun plump-dom-dom (node)
  (flet ((domize-children (children)
           (remove
            nil
            (if (and (= 1 (length children))
                     (plump:text-node-p (elt children 0)))
                (list (plump:text (elt children 0)))
                (map 'list 'plump-dom-dom children)))))
    (typecase node
      (plump:doctype
       (make-node :!doctype nil (plump:doctype node)))
      (plump:text-node
       (when (or *in-pre* (not (str:blankp (plump:text node)))) 
         (make-node :tn nil
                    (plump:text node))))
      (plump:comment
       (make-node :tn nil
                  (make-node :!comment nil
                             (plump:text node))))
      (plump:root
       (apply 'make-node :!root nil
              (domize-children (plump:children node))))
      (otherwise
       (let ((*in-pre* (or *in-pre* (eq "pre" (plump:tag-name node)))))
         (apply 'make-node
                (plump:tag-name node)
                (node-attrs node)
                (if (member (plump:tag-name node)
                            (list "title" "script" "style")
                            :test 'string=)
                    (list (apply 'str:concat (map 'list 'plump:text (plump:children node))))
                    (domize-children (plump:children node)))))))))

(defun add-id (attributes &optional id)
  (if (not (member :id attributes :key 'car))
      (acons :id (or id (symbol-name (gensym "I"))) attributes)
      attributes))

(defun ensure-ids (node)
  (prog1 node
    (unless (stringp node)
      (case (node-name node)
        ((or :!comment :!doctype) nil)
        (:tn
         (setf (node-attributes node)
               (add-id (node-attributes node))))
        (:!root
         (map 'list 'ensure-ids (node-children node)))
         (otherwise
          (setf (node-attributes node)
               (add-id (node-attributes node)))
          (map 'list 'ensure-ids (node-children node)))))))

(defun copy-id (old new)
  (setf (node-attributes new)
        (add-id (node-attributes new) (node-id old))))
