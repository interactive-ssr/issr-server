(in-package #:issr.instructions)

(defmethod name ((this string))
  this)

(defmethod name ((this symbol))
  (str:downcase (symbol-name this)))

(defun mod (id key-values)
  (when key-values
    (apply 'list "mod" id
           (map 'list
                (lambda (pair)
                  (-> pair first name
                      (list (second pair))))
                key-values))))

(defun delete (ids)
  (when ids
    (apply 'list "delete" ids)))

(deftype placement ()
  '(member
    :before
    :after
    :append
    :prepend))

(defun insert (id position html)
  (declare (type string id)
           (type placement position)
           (type (or string node) html))
  (list "insert"
        id
        (str:downcase (symbol-name position))
        (princ-to-string (ensure-ids html))))

(defun cookie ()
  (list "cookie"))

(defun redirect (target)
  (declare (type string target))
  (list "redirect" target))

(defun reconnect ()
  (list "reconnect"))

(defun error (message)
  (declare (type string message))
  (list "error" message))
