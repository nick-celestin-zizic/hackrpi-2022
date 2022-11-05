
(menu-bar-mode -1)			; Disable the top menubar.
(when (display-graphic-p)
  (toggle-scroll-bar -1))               ; Disable the scrollbar.
(tool-bar-mode -1)                      ; Disable the toolbar.

(setq frame-resize-pixelwise t)         ; Fix the tiny gap around the window.

;; For later when we're doing font stuff.
(defun large-screen? ()
  (or (> (display-pixel-width) 1366)
      (> (display-pixel-height) 768)))

;;; Frame splitting behavior
;; Default new frames to the right if on my
;; horizontal monitor, otherwise, default to the
;; bottom
(setq split-height-threshold
      `(,@(if (> (frame-pixel-height) 1440) 80 nil)))

;; Ensure new frames carry these properties.
;; NOTE: frame-inherited-parameters does not seem to work.
(defun apply-frame-parameter (frame parameter-pair)
  (let ((parameter (car parameter-pair))
        (value (cdr parameter-pair)))
    (and (set-frame-parameter frame parameter value)
         parameter)))
(defun make-frame-applier (frame)
  (lambda (parameter-pair)
    (apply-frame-parameter frame parameter-pair)))

(add-hook 'after-make-frame-functions
          (lambda (frame) (mapcar (make-frame-applier frame)
                             (frame-parameters))))

