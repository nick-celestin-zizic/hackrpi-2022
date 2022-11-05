;; -*- lexical-binding: t; -*-
;;; block-keys.lisp -- Temporarily block and unblock certain keys.
;;;

(defvar *unblockers* '())

(defun block-key (raw-key)
  (interactive
   (list (read-key-sequence
          "Block the following key sequence: ")))
  (let ((parsed (key-description raw-key))
        (binding (key-binding raw-key))
        (keymap (help--key-binding-keymap raw-key)))
    (if (y-or-n-p
         (format "Block `%s' bound to `%s'?"
                 parsed binding))
        (progn
          (unbind-key (kbd parsed) keymap)
          (push
           (lambda () (bind-key (kbd parsed)
                           binding keymap)
             (message "Rebound `%s'" binding))
           *unblockers*)))))

(defun unblock-all ()
  (interactive)
  (mapcar #'funcall *unblockers*))


(kbd (car (help--read-key-sequence)))

(mapcar (lambda (x)
          (pcase-let* ((`(,seq . ,raw-seq) x)
                       (`(,brief-desc ,defn ,event ,_mouse-msg)
                        (help--analyze-key seq raw-seq))
                       (locus
                        (help--binding-locus
                         seq (event-start event))))
            `(,seq ,brief-desc ,defn ,locus)))
        )

(apply #'help--analyze-key (car (help--read-key-sequence)))


(funcall (key-binding (read-key-sequence "Press the key to unbind")))

(key-description "")
(key-binding "C-x C-f")

(block-key (read-key-sequence "Block the following sequence"))
