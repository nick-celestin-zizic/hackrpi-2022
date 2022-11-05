;;; persistent-zoom -- Propagate the zoom level of a buffer through
;;; out all the ones matching its major mode.

(defun buffer-value (symbol buffer)
  (with-current-buffer buffer
    (eval symbol)))

(defun buffer-major-mode (buffer)
  (buffer-value 'major-mode buffer))

(defun buffer-zoom-level (buffer)
  (buffer-value 'text-scale-mode-amount buffer))



(defvar *zoom-levels*
  (make-hash-table))

(defun save-zoom-level! (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (puthash (buffer-major-mode buffer)
           (buffer-zoom-level buffer)
           *zoom-levels*))

(defun get-zoom-level (&optional the-major-mode)
  (unless the-major-mode
    (setq the-major-mode major-mode))
  (or (gethash the-major-mode *zoom-levels*) 0))

(defun set-zoom-level! (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (unless (zoom-level-propagated? buffer)
    (text-scale-set
     (get-zoom-level (buffer-major-mode buffer)))))



(defun zoom-level-propagated? (buffer)
  (eq (get-zoom-level (buffer-major-mode buffer))
      (buffer-zoom-level buffer)))

(defun propagate-zoom-levels ()
  (interactive)
  (mapcar #'set-zoom-level! (buffer-list)))
