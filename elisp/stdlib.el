(defvar else t)

(defun cons* (&rest things)
  (cl-reduce #'cons
             (butlast things)
             :from-end t
             :initial-value
             (car (last things))))

(defun apply* (proc arg &rest more)
  (apply proc
   (apply #'cons* (cons arg more))))

(defun zip (&rest lists)
  (apply* #'cl-mapcar #'list lists))

(defun circular-list (&rest items)
  (setf (cdr (last items)) items)
  items)

(defun flatten (xz)
  (apply #'append xz))

(defun fanout (p1 p2 arguments)
  (mapcar
   (lambda (argument)
     (list (funcall p1 argument)
           (funcall p2 argument)))
   arguments))



(defun filter (predicate list)
  (cl-remove-if predicate list))

(defun null? (obj) (null obj))


(defun symbol-upcase (symbol)
  (intern
   (upcase (symbol-name symbol))))
(defun symbol-append (&rest symbols)
  (intern
   (mapconcat #'symbol-name symbols)))
(defun keyword (symbol)
  (symbol-append ': symbol))
