(require 'transient)

(transient-define-prefix direnv ()
  "Modify, or create, an `.envirc' in the current directory"
  ["Environment Variables"
   ("VA" "add variable" envrc-add-variable)]
  ["Packages"
   ("pa" "add package" envrc-add-package)])


(defun envrc-compile (description)
  (mapcar
   (lambda (call)
     (apply
      (envrc-dispatch (car call))
      (cdr call)))
   description))

(bash
 (:environment-variables
  ("a" "b")))




(defun pass-syntax (pass)
  (plist-get (cdr pass) :syntax))
(defun pass-translator (pass)
  (plist-get (cdr pass) :translator))


(defun pass->function (pass)
  (let ((syntax (pass-syntax pass))
        (translator (pass-translator pass)))
    (eval
     `(lambda (&rest arguments)
        (mapcar
         (pcase-lambda
           (,syntax) (concat (apply ,translator ,syntax) "\n"))
         arguments)))))

(defmacro define-compiler (name &rest passes)
  (let ((functions (make-hash-table)))
    (mapcar (lambda (pass)
              (setf (gethash (car pass) functions)
                    (pass->function pass)))
            passes)
    `(defmacro ,name (&rest ast)
       `(apply #'concat
          ,@(mapcar
             (lambda (call)
               `(apply ,(gethash (car call) ,functions)
                       ',(cdr call)))
             ast)))))

(define-compiler bash
  ""
  (environment-variables
   :syntax `(,variable ,value)
   :translator
   (lambda (variable value)
     (format
      "export %s=%s"
      variable value))))

(define-compiler guix
  "use guix"

  (packages
   :syntax ,package
   :translator
   (lambda (package)
     (format
      "export %s=%s"
      variable value))))

(bash
 (environment-variables
  ("a" "b")
  ("a" "b1"))
 (raw "a"))


(defun newcdr (list)
  (pcase list
    (`((,l . ,ll)) ll)))
(newcdr '((a b c)))

(envrc-compile '((:variables ("a" "b"))))

(defun concat-with (delimiter list)
  (apply #'concat
         (mapcar (lambda (obj)
                   (concat obj delimiter))
                 list)))

(defun envrc-add-variable (variable value)
  (interactive)
  (envrc-add
   (current-directory)
   (format "export %s=%s" variable value)))

(defmacro define-compiler ()
  (declare (indent 1)))

(define-compiler guix
  :command "use guix"
  (switches
   (container? :translation "--container")
   (networking? :translation "--networking")
   (pure? :translation "--pure"))
  (packages
   :translation
   ))


(define-translation (bash variables))

(define-translation (guix container?) "-C")
(define-translation (guix networking?) "-N")
(define-translation (guix pure?) "--pure")


'((bash
   :variables
   ("HOME" "`pwd'"))
  (guix
   :container? t
   :networking? t
   :pure? t
   :packages
   "wine" "wine64"))
