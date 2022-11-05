;;; decrypt-and-cache -- decrypt a file and then save it to disk
;;;
;;; this is useful when you want to load a .gpg file in your init, but
;;; don't want to get prompted for decryption over and over.
;;;

(defun load-maybe-use-cache (gpg-file)
  (load (maybe-use-cache gpg-file ".cache")))

(defun maybe-use-cache (gpg-file type)
  (or (already-cached? gpg-file type)
      (decrypt-and-cache gpg-file type)))

(defun change-extension (gpg-file ext)
  ;; Emacs doesn't handle symlinks transparently?
  (expand-file-name
   (format "%s%s%s"
           (or (file-name-directory gpg-file) "")
           (file-name-base gpg-file) ext)))

(defun already-cached? (gpg-file type)
  (let ((cached-file (change-extension gpg-file type)))
    (and (file-exists-p cached-file)
         (not (file-newer-than-file-p gpg-file cached-file))
         cached-file)))

(defun decrypt-and-cache (gpg-file type)
  (let ((cached-file (change-extension gpg-file type)))
    (progn
      (epa-decrypt-file gpg-file cached-file)
      cached-file)))

(provide 'decrypt-and-cache)
