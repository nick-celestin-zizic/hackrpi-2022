;;; sedit.lisp -- Experimental editing mode
;;;

(require 'cl)


(defun mouse-frame ()
  (car (mouse-pixel-position)))
(defun mouse-x-y ()
  (cdr (mouse-pixel-position)))
(defun mouse-x ()
  (car (mouse-x-y)))
(defun mouse-y ()
  (cdr (mouse-x-y)))

;; (track-mouse
;;   event-resolution)

(defun mouse-point ()
  (posn-point
   (posn-at-x-y
    (mouse-x) (mouse-y) (mouse-frame))))

(defun point-beginning-pos (point &optional n)
  (save-excursion
    (and (goto-char point)
         (line-beginning-position n))))

(defun hl-line-range-function ()
  (cons (point-beginning-pos (mouse-point))
        (point-beginning-pos (mouse-point) 2)))
(setq hl-line-range-function #'hl-line-range-function)

(define-minor-mode hl-line-mouse-mode
  ""
  :group 'hl-line-mouse
  (if hl-line-mouse-mode
      (hl-line-mouse-mode-enable)
    (hl-line-mouse-mode-disable)))

(recenter-top-bottom )

(recenter nil t)

(defun scroll-bar-top (event)
  (interactive "e")
  (recenter (min (max 0 scroll-margin)
                 (truncate (/ (window-body-height) 4.0)))
            t))

(defun scroll-bar-center (event)
  (interactive "e")
  (recenter-mouse))

(defun scroll-bar-toolkit-scroll (event)
  "Handle event EVENT on vertical scroll bar."
  (interactive "e")
  (let* ((end-position (event-end event))
         (window (nth 0 end-position))
         (part (nth 4 end-position))
         before-scroll)
    (cond
     ((eq part 'end-scroll))
     (t
      (with-current-buffer (window-buffer window)
        (setq before-scroll point-before-scroll))
      (save-selected-window
        (select-window window 'mark-for-redisplay)
        (setq before-scroll (or before-scroll (point)))
        (cond
         ((eq part 'above-handle)
          (message "above-handle"))
         ((eq part 'below-handle)
          (message "below-handle"))
         ((eq part 'ratio)
          (message "ratio"))
         ((eq part 'up)
          (message "up"))
         ((eq part 'down)
          (message "down"))
         ((eq part 'top)
          (message "top"))
         ((eq part 'bottom)
          (message "bot"))
         ((eq part 'handle)
          (message "handle")
          (recenter-mouse))))
      (sit-for 0)
      (with-current-buffer (window-buffer window)
        (setq point-before-scroll before-scroll))))))

(global-set-key [vertical-scroll-bar mouse-1]
                'scroll-bar-top)

(global-set-key [vertical-scroll-bar mouse-3]
                'scroll-bar-center)

(defun hl-line-mouse-mode-enable ()
  (add-hook 'mouse-change-line-hook #'hl-line-under-mouse)
  (add-hook 'mouse-change-line-hook #'unhl-line-under-mouse))
(defun hl-line-mouse-mode-disable ()
  (remove-hook 'mouse-change-line-hook #'hl-line-under-mouse)
  (remove-hook 'mouse-change-line-hook #'unhl-line-under-mouse)
  (unhl-line-under-mouse))

(run-at-time
 0.01 0.01
 (lambda ()
   (let ((new-point (mouse-point)))
     (if (or (not (in-scroll-bar?))
             (and old-point (= old-point new-point)))
         '()
       (setf old-point (mouse-point))
       (run-hooks 'mouse-change-line-hook)))))

(defface high-line
  '((((supports :underline (:style wave)))
     :extend t
     :underline (:style wave :color "White"))
    (t
     :underline t :inherit warning))
  "Flyspell face for words that appear twice in a row.
See also `flyspell-duplicate-distance'."
  :version "24.4"
  :group 'flyspell)

(defun make-hl-line ()
  (let ((point (mouse-point)))
    (let ((ol (make-overlay point point)))
      (overlay-put ol 'priority -50)    ;(bug#16192)
      (overlay-put ol 'face 'high-line)
      ol)))


;; cheat by calling recenter with cursor pos - mouse pos offset


(hl-line-under-mouse)
(unhl-line-under-mouse)

(defvar *overlays* '())
(defun hl-line-under-mouse ()
  (let ((overlay (make-hl-line)))
    (overlay-put overlay 'window nil)
    (hl-line-move overlay)
    (push overlay *overlays*)))
(defun unhl-line-under-mouse ()
  (mapcar #'delete-overlay *overlays*)
  (setf *overlays* '()))

(defun recenter-mouse ()
  (interactive)
  (goto-char (mouse-point))
  (recenter)
  (set-mouse-position (mouse-frame)
                      (car (window-absolute-pixel-position))
                      (cdr (window-absolute-pixel-position))))

(overlay-put (make-hl-line) 'window nil)

(hl-line-under-mouse)
(unhl-line-under-mouse)

(defvar old-point (mouse-point))
(run-at-time
 0.15 0.15
 (lambda ()
   (let ((new-point (mouse-point)))
     (if (and (= old-point new-point)
              (not (in-scroll-bar?)))
         '()
       (setf old-point (mouse-point))
       (run-hooks 'mouse-change-line-hook)))))

(run-at-time
 0.25 0.25
 (lambda ()
   (if (in-scroll-bar?)
       (run-hooks 'mouse-enter-scrollbar-hook)
     (run-hooks 'mouse-exit-scrollbar-hook))))

(add-hook 'mouse-enter-scrollbar-hook
          '(lambda () (hl-line-mouse-mode 1)))
(add-hook 'mouse-exit-scrollbar-hook
          '(lambda () (hl-line-mouse-mode -1)))

(setq mouse-autoselect-window t)

(defun in-scroll-bar? ()
  (> (frame-scroll-bar-width)
     (mouse-x)))

(over-scroll-bar?)

(the-mouse)

(mouse-x-y)


(mouse-point)

(frame-scroll-bar-width)





(defstruct word
  start end string)

(forward-word)
(nexte-word!)

(defun next-word! ()
  (let ((start (and (forward-word)
                    (backward-word)
                    (point)))
        (end (and (forward-word) (point))))
    (make-word
     :start start
     :end end
     :string (unless (reached-end?)
               (buffer-substring-no-properties
                start end)))))

(next-word!)
(defun reached-end? ()
  (= (point) (point-max)))

(reached-end?)

(defun map-over-words (procedure buffer)
  (cl-labels ((loop () (if (reached-end?)
                           '()
                         (cons (funcall procedure (next-word!))
                               (loop)))))

    (beginning-of-buffer)
    (loop)))

(defun reset ()
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

(map-over-words
 (lambda (word)
   (when (and (word-string word))
     (add-text-properties
      (word-start word) (word-end word)
      '(mouse-face
        highlight help-echo "mouse-2: test"))))
 (current-buffer))

(map-over-words #'identity (current-buffer))

(reached-end?)


(reset)

(window-scroll-bar-width)
