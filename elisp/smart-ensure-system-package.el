;;; smart-ensure-system-package.el -- A smarter version of
;;; use-package-ensure-system-package.el.
;;;
;;; The default behavior of use-package-ensure-system-package is to
;;; create a *system-package* buffer for each of the declared
;;; packages on each Emacs startup, until all packages have been installed
;;;
;;; This quickly becomes very annoying with configurations that make
;;; generous use of the facility, and especially on source-based
;;; distributions.
;;;
;;; Instead, we collect the missing declared system packages into a
;;; list, and then gently prompt the user for a one-shot install.
;;;
;;;
;;; TODO: Ambiguity resolution.
;;;

(require 'use-package)
(use-package system-packages
  :demand t)

(defvar *missing-packages* '()
  "List of missing system packages that the user demands installed.")

(defun make-system-binary (binary-name package-name)
  (mapcar #'prin1-to-string (list binary-name package-name)))

;;;###autoload
(defun use-package-maybe-install-packages ()
  (interactive)
  (when *missing-packages*
    (and
     (y-or-n-p
      (format
       "You have %s missing packages, would you like to install them?"
       (length *missing-packages*)))
     (system-packages-install
      (cl-reduce
       (lambda (package package-list)
         (cl-concatenate
          'string package " " package-list))
       *missing-packages*)))))

;; ;;;###autoload
;; (add-hook 'after-init-hook #'use-package-maybe-install-packages)

;;;###autoload
(defun use-package-handler/:ensure-system-package (name _keyword system-binary rest state)
  "Execute the handler for `:ensure-system-package' keyword in `use-package'."
  (let ((body (use-package-process-keywords name rest state)))
    (cl-destructuring-bind (binary-name package-name)
        system-binary
      (unless (executable-find binary-name)
        (push package-name *missing-packages*)))
    body))

;;;###autoload
(defun use-package-normalize/:ensure-system-package (_name-symbol keyword args)
  "Turn args into a list of system-binary structures."
  (use-package-as-one (symbol-name keyword) args
    (lambda (_label package-spec)
      (pcase package-spec
        (`(,binary-name)
         (make-system-binary binary-name binary-name))
        (`(,binary-name . ,package-name)
         (make-system-binary binary-name package-name))
        (`,binary-name
         (make-system-binary binary-name binary-name))))))

;;;###autoload
(add-to-list 'use-package-keywords :ensure-system-package t)

(provide 'smart-ensure-system-package)

;;; use-package-ensure-system-package.el ends here
