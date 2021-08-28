(in-package #:issr.instructions)

(defun mod (id key-values)
  (when key-values
    (apply 'list "mod" id key-values)))

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
        (if (stringp html) 1 0)
        (str:downcase (symbol-name position))
        (if (typep html 'node)
            (princ-to-string (ensure-ids html))
            html)))

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
