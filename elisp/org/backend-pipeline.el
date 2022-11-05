;; -*- lexical-binding: t; -*-
(setq lexical-binding t)
(defun list-formal-parameters (procedure)
  (help-function-arglist procedure))

(defun make-pass (pipeline-info keywords emitting-procedure)
  (let ((select (pipeline-info-get 'filter pipeline-info)))
    (let ((formal-parameters
           (butlast (list-formal-parameters select))))
      `(lambda ,formal-parameters
         (let ((select (curry #',select ,@formal-parameters)))
           (let ((arguments (funcall select ',keywords)))
             (when arguments
               (apply ,emitting-procedure
                      arguments))))))))


(defun make-pipeline-info (name arguments)
  (cons (list 'name name) arguments))
(defun pipeline-info-get (property pipeline-info)
  (cadr (assoc property pipeline-info #'equal)))

(defmacro define-pass ()
  (declare (indent 1)))

(defmacro define-pipeline (pipeline-name pipeline-arguments &rest pipeline-body)
  (declare (indent 1))
  (let ((pipeline-info (make-pipeline-info
                        pipeline-name pipeline-arguments)))
    `(let ((make-pass (curry #'make-pass ',pipeline-info))
           (constructed-pipeline '()))
       (cl-flet ((define-pass (keywords-to-select emitting-procedure)
                   (setf constructed-pipeline
                         (append
                          constructed-pipeline
                          (list (funcall
                                 make-pass
                                 keywords-to-select emitting-procedure))))))
         (setf ,pipeline-name
               (progn
                 ,@pipeline-body
                 constructed-pipeline))))))


(defun run-pipeline (pipeline &rest args)
  (apply #'concat
   (mapcar
    (lambda (pass)
      (apply pass args))
    pipeline)))
