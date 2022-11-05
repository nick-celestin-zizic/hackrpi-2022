;;; lmfs-backups.el -- Lisp Machine like backups.
;;;

(defvar lmfs-aggressiveness
  'high
  "")

(defvar lmfs-backup-directory
  (concat user-emacs-directory "backups/")
  "")

(setq backup-directory-alist
      `(("." . ,lmfs-backup-directory)))
(setq auto-save-file-name-transforms
      `((".*"  ,lmfs-backup-directory t)))

(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-by-copying t)
(setq auto-save-interval 100)

(setq kept-old-versions 1000)
(setq kept-new-versions 1000)
(setq delete-old-versions nil)



(defun hash-points (start end)
  (md5 (buffer-substring start end)))

(defun hash-buffer (buffer)
  (save-excursion
    (switch-to-buffer buffer)
    (hash-points (point-min)
                 (point-max))))

(defun hash-file (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (hash-buffer (current-buffer))))



(defun buffer-backed-up? (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (setq buffer-backed-up
        (equal (hash-buffer buffer)
               (hash-file (diff-latest-backup-file
                           (buffer-file-name buffer))))))


;; Yuck!
(defmacro preserve-file-on-disk (file-on-disk &rest body)
  (declare (indent 2))
  `(let ((file-on-disk ,file-on-disk)
         (backup-inhibited t))
     (let ((return (current-buffer))
           (cursor-return (point))
           (top (window-start)))
       (with-temp-buffer
         (let ((preserved-file (current-buffer)))
           (insert-file file-on-disk)
           (set-buffer return)
           (progn ,@body)
           (set-buffer preserved-file)
           (write-file file-on-disk)

           ;; The long hairy way makes Emacs happier
           ;; (with-temp-buffer
           ;;   (let ((working-contents (current-buffer)))
           ;;     (replace-buffer-contents return)
           ;;     (set-buffer return)
           ;;     (replace-buffer-contents preserved-file)
           ;;     (save-buffer)
           ;;     (replace-buffer-contents working-contents)))
           ))
       (set-buffer return)
       ;; YUCK!YUCK!YUCK!YUCK!YUCK!YUCK!
       (goto-char top)
       (next-line)
       (recenter-top-bottom 1)
       (goto-char cursor-return))))

(defun lmfs-backup-buffer (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (unless backup-inhibited
    (unless (buffer-backed-up?)
      (message "Backing up %s" buffer)
      (preserve-file-on-disk (buffer-file-name buffer)
          (set-buffer buffer)
        (save-buffer)
        (let ((buffer-backed-up nil))
          (backup-buffer))))))

;; backup-inhibited
;; buffer-backed-up
;; (add-hook 'auto-save-hook #'lmfs-backup-buffer)
;; (add-hook 'before-save-hook #'lmfs-backup-buffer)

(provide 'lmfs-backups)
