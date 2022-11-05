;;; functional.el -- Functional programming procedures
;;;

(require 'cl)


(defmacro defun (name arglist &rest body)

  )


(defalias 'filter #'remove-if-not)

(defalias 'zero? #'zerop)
(defalias 'map #'mapcar)
(defalias '-1+ #'1-)

(cl-defun iota (count &optional (start 0) (step 1))
  (if (zero? count)
      '()
    (cons start (iota (-1+ count) (+ start step) step))))



(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))

(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))


(defun compose (&rest procedures)
  (fold (lambda (f g)
          (lambda (&rest args)
            (funcall f (apply g args))))
        procedures
        )

  )


(defmacro $let (&rest args)
  `(let ((lexical-scope t))
     (let ,@args)))

(defun generate-restore (func)
  `(fset ',func ',(symbol-function func)))
(defmacro fun-protect (funs &rest body)
  (declare (indent 1))
  `(cl-flet ((restore () ,@(mapcar #'generate-restore funs)))
     (unwind-protect
         (progn ,@body)
       (restore))))
