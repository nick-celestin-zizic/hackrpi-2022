(defun overlay-type? (type overlay)
  (equal (overlay-get overlay 'type)
         type))

(defun recalculate (start end)
  (remove-if-not
   (curry #'overlay-type? 'ours)
   (overlays-in start end)))

(defun word-at-point-highlight ()
  (interactive)
  (save-excursion
    (let ((end (and (backward-word)
                      (point)))
          (start (and (forward-word)
                      (point))))
      (cons start end))))

abcdef f
