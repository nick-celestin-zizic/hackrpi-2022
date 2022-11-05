;; -*- lexical-binding: t; -*-
;;; genera-scroll-bar.el -- Genera-like scroll bars; Very fun to use!
;;;

(require 'cl)
(require 'hl-line)

(defun mouse-frame ()
  (car (mouse-pixel-position)))
(defun mouse-x-y ()
  (cdr (mouse-pixel-position)))
(defun mouse-x ()
  (car (mouse-x-y)))
(defun mouse-y ()
  (cdr (mouse-x-y)))

(defun mouse-point ()
  (and (mouse-x)
       (mouse-y)
       (posn-point
        (posn-at-x-y
         (mouse-x) (mouse-y) (mouse-frame)))))

(defun mouse-in-scroll-bar? ()
  (and (mouse-x)
       (> (frame-scroll-bar-width)
          (mouse-x))))

;;; TODO: abstract timers better
(defun create-mouse-enter-scroll-bar-timer ()
  (let ((already-in-scroll-bar?
         (mouse-in-scroll-bar?)))
    (defvar already-in-scroll-bar?
      (mouse-in-scroll-bar?))
    (run-at-time
     0.01 0.01
     (lambda ()
       (if (mouse-in-scroll-bar?)
           (unless already-in-scroll-bar?
             (message "Entered scroll-bar!")
             (setq already-in-scroll-bar? t)
             (run-hooks 'mouse-enters-scroll-bar-hook))
         (when already-in-scroll-bar?
           (message "Left scroll-bar!")
           (setq already-in-scroll-bar? nil)
           (run-hooks 'mouse-leaves-scroll-bar-hook)))))))

(defun create-mouse-change-line-timer ()
  (let ((old-point (mouse-point)))
    (defvar old-point
      (mouse-point))
    (run-at-time
     0.01 0.01
     (lambda ()
       (let ((new-point (mouse-point)))
         (and new-point
              (unless (= new-point old-point)
                (progn (message "%s" new-point)
                       (run-hooks 'mouse-changes-line-hook))
                (setq old-point new-point))))))))

(defun destroy-mouse-timers ())

(create-mouse-change-line-timer)
(create-mouse-enter-scroll-bar-timer)

(defvar genera-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<vertical-scroll-bar> <mouse-1>")
      'genera-scroll-bar-scroll)
    map))

(define-minor-mode genera-scroll-bars-mode
  ""
  :keymap genera-mode-map
  :group 'genera
  (if genera-scroll-bars-mode
      (enable-genera-scroll-bars)
    (disable-genera-scroll-bars)))

(defun enable-genera-scroll-bars ()
  (add-hook 'mouse-enters-scroll-bar-hook
            #'enable-guide-line)
  (add-hook 'mouse-leaves-scroll-bar-hook
            #'disable-guide-line))
(defun disable-genera-scroll-bars ()
  (remove-hook 'mouse-enters-scroll-bar-hook
            #'enable-guide-line)
  (remove-hook 'mouse-leaves-scroll-bar-hook
               #'disable-guide-line))

(defface guide-line
  '((((background light))
     :extend t
     :underline (:style wave :color "Black"))
    (((background dark))
     :extend t
     :underline (:style wave :color "White")))
  ""
  :group 'genera)
(defun make-guide-line ()
  (let ((point (mouse-point)))
    (let ((ol (make-overlay point point)))
      (overlay-put ol 'priority -50)    ;(bug#16192)
      (overlay-put ol 'face 'guide-line)
      ol)))

;; TODO: remove setq
(defvar *guide-line* '())
(defun enable-guide-line ()
  (setq *guide-line* (create-guide-line)))
(defun disable-guide-line ()
  (delete-overlay *guide-line*)
  (setq mouse-changes-line-hook '()))

(defun create-guide-line ()
  (let ((guide-line (make-guide-line)))
    (add-hook 'mouse-changes-line-hook
              (move-guide-line guide-line))
    guide-line))

(defun point-beginning-pos (point &optional n)
  (save-excursion
    (and (goto-char point)
         (line-beginning-position n))))

(defun move-to-mouse ()
  (cons (point-beginning-pos (mouse-point))
        (point-beginning-pos (mouse-point) 2)))
;; Elisp closures seem a bit broken. We need to use eval here.
(defun move-guide-line (guide-line)
  (lambda ()
    (let ((hl-line-range-function #'move-to-mouse))
      (hl-line-move guide-line))))
;; (defun move-guide-line (guide-line)
;;   (eval `(lambda ()
;;            (let ((hl-line-range-function #'move-to-mouse))
;;              (hl-line-move ,guide-line)))))


(enable-guide-line)
(disable-guide-line)

(defun reset ()
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

(reset)

(defun recenter-around-mouse (&optional n)
  (interactive)
  (goto-char (mouse-point))
  (recenter n))

(defun point-posn (point)
  (save-excursion
    (goto-char (point))
    (posn-at-point)))

(defun point-x (point)
  (car (posn-x-y
        (point-posn point))))
(defun point-y (point)
  (+ (cdr (posn-x-y
           (point-posn point)))
     5))

(point-y (point))

(defun move-mouse-to-point (point)
  (set-mouse-pixel-position
   (mouse-frame)
   (point-x point)
   (point-y point)))

(defun move-mouse-to-point-y (point)
  (set-mouse-pixel-position
   (mouse-frame)
   (mouse-x)
   (point-y point)))

(defun recenter-around-mouse-top ()
  (interactive)
  (recenter-around-mouse
   (min (max 0 scroll-margin)
        (truncate (/ (window-body-height) 4.0)))))

(defun genera-scroll-bar-scroll (event)
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
          (message "ratio")
          (recenter-around-mouse-top))
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
          (recenter-around-mouse))))
      (sit-for 0)
      (move-mouse-to-point-y (point))
      (with-current-buffer (window-buffer window)
        (setq point-before-scroll before-scroll))))))
