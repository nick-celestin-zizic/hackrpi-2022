;; -*- lexical-binding: t; -*-
(setq lexical-binding t)
(defun concat-with (delimiter text-to-delimit)
  (declare (indent 1))
  (cl-reduce (lambda (delimited-text text)
               (concat delimited-text delimiter text))
             (cdr text-to-delimit)
             :initial-value (car text-to-delimit)))
(defun symbol-append (&rest symbols)
  (intern
   (mapconcat #'symbol-name symbols)))

(defun string-remove (thing string)
  (apply #'concat
         (split-string string thing t)))


(defun take (n xz)
  (cond ((null xz) '())
        ((zerop n) '())
        (t (cons (car xz)
                 (take (1- n)
                       (cdr xz))))))
(defun drop (n xz)
  (cond ((null xz) '())
        ((zerop n) xz)
        (t (drop (1- n) (cdr xz)))))


(defun group-by (n xz)
  (let ((group (take n xz))
        (universe (drop n xz)))
    (if (null group)
        '()
      (cons group
            (group-by n universe)))))



(defun flatten (lists)
  (cl-reduce
   (lambda (xs x)
     (append
      xs
      (if (listp x)
          (flatten x)
        (list x))))
   lists
   :initial-value '()))

(defun latex-emit (&rest strings)
  (apply #'concat (flatten strings)))
(defun latex-emit* (&rest strings)
  (concat "\n" (latex-emit strings) "\n"))

(defun latex-indent (&rest strings)
  (latex-emit
   (mapconcat
    (compose (rcurry #'concat "\n")
       (curry #'concat "  "))
    (split-string
     (mapconcat #'latex-emit strings)
     "\n" t))))

(defun latex-macro (macro-details &rest latex-body)
  (declare (indent 1))
  (cl-destructuring-bind (macro-name &rest macro-args)
      macro-details
    (latex-emit
     "\\" macro-name "{"
     latex-body
     (if macro-args
         (latex-emit "\\\\[" (concat-with ", " macro-args) "]"))
     "}")))

(defun latex-block (block-details &rest latex-body)
  (declare (indent 1))
  (cl-destructuring-bind (structure-name &optional arg1 arg2)
      block-details
    (latex-emit
     (latex-macro '("begin")
       structure-name)
     (if arg1
         (latex-emit "[" arg1 "]"))
     (if arg2
         (latex-emit "{" arg2 "}"))
     (latex-emit
      "\n"
      (latex-indent latex-body))
     (latex-macro '("end")
       structure-name))))


(defun latex-put-below (&rest strings)
  (latex-emit
   (concat-with "\\\\"
     (flatten strings))))
(defun latex-put-right (string &rest strings)
  (latex-emit
   string
   (latex-macro '("hfill")
     strings)))

(defun latex-put-right* (&rest strings)
  (latex-emit
   (latex-macro '("hfill")
     strings)))

(let ((latex-emphasis-table '()))
  (defun add-emphasis-type! (symbol-list latex-name)
    (declare (indent 1))
    (setf latex-emphasis-table
          (cons (cons symbol-list latex-name)
                latex-emphasis-table))

    (eval
     (cons 'progn
           (mapcar
            (lambda (emphasis-type)
              `(fset ',(symbol-append 'latex- emphasis-type)
                     (curry #'latex-emphasis ',emphasis-type)))
            symbol-list))))

  (defun latex-emphasis-name (symbol)
    (cdr (assoc symbol latex-emphasis-table
                (lambda (entry key)
                  (member key entry)))))

  (add-emphasis-type! '(bold) "textbf")
  (add-emphasis-type! '(small-caps) "textsc")
  (add-emphasis-type! '(italic italics) "textit")
  (add-emphasis-type! '(underline) "underline")
  latex-emphasis-table)


(defun latex-emphasis (type &rest strings)
  (let ((emphasis-name (latex-emphasis-name type)))
    (latex-macro (list emphasis-name)
      strings)))

(defun latex-tabbing (tabbing-details from &rest strings-to-tab)
  (declare (indent 1))
  (cl-destructuring-bind (&rest tab-positions)
      tabbing-details
    (latex-emit
     (latex-block '("tabbing")
       (latex-emit
        (mapconcat (lambda (tab-width)
                     (latex-emit
                      (latex-macro '("hspace")
                        tab-width)
                      "\\="))
                   tab-positions)
        "\\kill" "\n")
       from
       (latex-indent
        (mapcar
         (lambda (group)
           (apply #'concat
                  (append group '("\\\\\n"))))
         (group-by
          (length tab-positions)
          (mapcar
           (lambda (string)
             (latex-emit " \\>" string))
           (flatten strings-to-tab))))))
     (latex-macro '("vspace")
       "-35pt"))))

(defun latex-tabular (columns &rest n-rows)
  (declare (indent 1))
  (latex-emit*
   (latex-block '("tabular" "t" "lr")
     (mapcar (rcurry #'cons '("\\\\\n"))
             (mapcar (curry #'concat-with " & ")
                     (group-by (car columns) (flatten n-rows)))))
   "\\\\\n"))





;; (defun latex-emit* (&rest latex-to-emit)
;;   (apply #'concat
;;          (flatten latex-to-emit)))

;; (defalias 'latex-text #'latex-emit)
;; (defun latex-emit (&rest latex-to-emit)
;;   (concat
;;    (string-remove
;;     "\n\n" (concat-with " "
;;            (flatten latex-to-emit)))
;;    "\n\n"))

;; (defun latex-macro (macro-spec &rest text-to-wrap)
;;   (declare (indent 1))
;;   (cl-destructuring-bind (macro-name &rest macro-args)
;;       macro-spec
;;     (latex-emit
;;      (concat
;;       "\\" macro-name "{"
;;       (latex-emit text-to-wrap)
;;       (if macro-args
;;           (concat "\\\\[" (concat-with ", " macro-args) "]"))
;;       "}"))))

;; (defun latex-block (block-spec &rest text-to-wrap)
;;   (declare (indent 1))
;;   (cl-destructuring-bind (block-name &rest block-args)
;;       block-spec
;;     (latex-emit*
;;      (concat
;;       "\\begin{" block-name "}" "\n"
;;       (latex-emit* text-to-wrap) "\n"
;;       "\\end{" block-name "}"))))


;; (defun latex-put-rest-below (text &rest rest)
;;   (latex-emit
;;    (concat-with "\\\\ "
;;      (cons text (flatten rest)))))
;; (defun latex-push-right (left right)
;;   (latex-emit
;;    (concat left "\\hfill") right))




;; (defun latex-style-name (type)
;;   (cl-case type
;;     (bold "bf")
;;     ((italic italics) "it")))
;; (fset 'latex-bold (curry #'latex-stylize 'bold))
;; (fset 'latex-italics (curry #'latex-stylize 'italics))

;; (defun latex-stylize (type &rest text)
;;   (let ((style-name (latex-style-name type)))
;;     (apply #'latex-macro
;;            `((,(concat "text" style-name)) ,@text))))
