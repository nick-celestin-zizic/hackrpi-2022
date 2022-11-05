;; -*- lexical-binding: t; -*-
;;;; First things first... ;;;;
;;; Use straight.el and use-package for package management
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(defmacro async-eval (&rest body)
  `(run-with-timer 0 nil (lambda () ,@body)))
(async-eval
 (let ((font (font-spec :family "Inconsolata" :size 18)))
   (when (list-fonts font)
     (set-frame-font font nil t))))

;;; Give M-x customize its own file
;; There should be nothing in here; but just in case.
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
;;; We use use-package
(use-package smart-ensure-system-package
    :straight nil
    :load-path "elisp" :demand t)
(use-package straight
  :ensure-system-package git)
(use-package browse-kill-ring
  :bind ("C-M-y" . browse-kill-ring))
;;; This package is great for hacking!
(use-package el-patch
  :config
  ;; Respect the value of `lexical-binding'.
  (defun el-patch-eval-template (name type)
    (interactive (el-patch--select-template))
    (eval (el-patch--resolve-template name type)
          lexical-binding)))

;;; Load sensitive information
;; API Keys, Passwords, and so on.
;; (the ones that can't sit in .authinfo.gpg)
(use-package decrypt-and-cache
  :straight nil
  :load-path "elisp" :demand t
  :ensure-system-package (gpg . gnupg)
  :config
  ;; (load-maybe-use-cache "~/.emacs.d/api-keys.gpg")
  )
;;; Themeing
;; Pick a theme depending on the time of day.
(use-package modus-themes)
(use-package monokai-pro-theme)
(use-package exotica-theme)
(use-package sublime-themes)
(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))
;; (use-package theme-magic
;;   :ensure-system-package (wal . pywal)
;;   :init (theme-magic-export-theme-mode))
(use-package day-night-switch
  :straight nil
  :load-path "elisp"
  :config
  ;; quick hack
  (defun compton-running? ()
    (cl-remove-if-not
     (lambda (x) (equal (cdr (assoc 'comm x)) "compton"))
     (mapcar #'process-attributes (list-system-processes))))
  (advice-add 'day? :after-while (lambda (&rest _) (not (compton-running?))))
  (setq day-theme 'tsdh-light)
  (setq night-theme 'junio)
  (setq sunset "20:00")
  (setq sunrise "8:00")
  :hook (window-setup . maybe-change-theme))
(use-package nyan-mode
  :preface
  (nyan-mode 1)
  :config
  (setq nyan-wavy-trail t)
  (defun nyan-wavy-rainbow-ascent (number)
    (if nyan-wavy-trail
        (min 100 (+ 90
                    (* 3 (abs (- (/ 6 2)
                                 (% (+ number nyan-current-frame)
                                    6))))))
      (if (zerop (% number 2)) 80 'center))))
(use-package helm
  :straight (:build (:not compile)))
;;; Avoid cluttering the file-system.
;;; Place all # and ~ files into their
;;; own directory.s
(setq backup-directory-alist
      '(("." ."~/.emacs.d/backups")))
;; Lisp Machine like backups
(use-package lmfs-backups
  :straight nil :defer nil
  :load-path "elisp"
  :config
  (require 'lmfs-backups)
  (setq lmfs-aggressiveness 'high)
  (setq lmfs-backup-directory
        (concat user-emacs-directory "backups/")))
                                        ;:;; Interface (X11, Term) ;;;;
;; Declutter interface
(async-eval
    (menu-bar-mode -1)                  ; Disable the top menubar.
  (when (display-graphic-p)
    (toggle-scroll-bar -1))             ; Disable the scrollbar.
  (tool-bar-mode -1))                    ; Disable the toolbar.

(setq use-dialog-box nil)      ; Do not popup menus
(setq frame-resize-pixelwise t)         ; Fix the tiny gap around the window.

;; For later when we're doing font stuff.
(defun large-screen? ()
  (or (> (display-pixel-width) 1400)
      (> (display-pixel-height) 1050)))
;;; Frame splitting behavior
;; Default new frames to the right if on my
;; horizontal monitor, otherwise, default to the
;; bottom
(defun update-thresholds ()
  (setq split-height-threshold
        `(,@(if (or (> (frame-pixel-height) 1440)
                    (< (frame-pixel-width) 1440))
                80 nil))))
(add-hook 'window-configuration-change-hook #'update-thresholds)
;; Ensure new frames carry these properties.
;; NOTE: this is because frame-inherited-parameters doesn't  seem to work.
;; FIXME: This method has issues with ediff
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
;;; Convenience
;; Make yes or no prompts 1 keystroke
(defalias 'yes-or-no-p 'y-or-n-p)
;; Don't ask at all on PDF buffers.
(setq revert-without-query '(".pdf"))
;; Auto-revert buffers by default.
(global-auto-revert-mode)
;; Define C-x O and C-x M-o to cycle backwards
(defun other-window-reverse ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x O") 'other-window-reverse)
(global-set-key (kbd "C-x M-o") 'other-window-reverse)
;; Rename current files with C-x w
(defun rename-current-file (newname)
  (interactive "FRename to: ")
  (rename-file (buffer-file-name (current-buffer))
               newname))
(global-set-key (kbd "C-x w") 'rename-current-file)
;; Helping procedure to revert a variable to its default value
(defun revert-to-default (symbol)
  (setq symbol (eval (car (get symbol 'standard-value)))))
;; Cleanup whitespace on save.
(add-hook 'before-save-hook #'whitespace-cleanup)
;; Tab should tab-complete.
(setq tab-always-indent 'complete)
;; A really nice mode which lets C-x C-f open all kinds
;; of URLs
(url-handler-mode)
;; Native-comp loves shoving warning messages in your face.
;; Don't do this
(setq warning-suppress-types '((comp)))
(setq warning-minimum-level :emergency)
;; Get rid of annoying warning when file is under VC.
(setq vc-follow-symlinks t)
;; Make terminal Emacs easier.
(xterm-mouse-mode)
;; After a C-u C-space, don't require a prefix
(setq set-mark-command-repeat-pop t)
;;; Silence!
;; Don't beep 'undefined' with certain keybindings!
(defun nothing ()
  (interactive)
  '⊥)
(defun silence-key! (kbd-specifier)
  (let ((key (kbd kbd-specifier)))
    (global-set-key key 'nothing)))
(mapcar #'silence-key!
        '("<XF86AudioMute>"
          "<XF86MonBrightnessUp>"
          "<XF86MonBrightnessDown>"
          "<XF86AudioLowerVolume>"
          "<XF86AudioRaiseVolume>"
          "<mouse-8>"
          "<drag-mouse-8>"
          "M-]"                         ; Mumble binding
          ))
;; Highlight the current line in the active buffer.
(setf hl-line-sticky-flag nil)
(global-hl-line-mode 1)
;; TODO: Let C-x 4 and C-x 5 properties propagate to M-x

;;;; Text Editing (What? Emacs edits text too?) ;;;;
;;; Pasting Behavior
(setq select-enable-primary t)          ; Use X11's primary clipboard for yanking.
(setq mouse-yank-at-point t)		; Middle mouse paste on Emacs cursor position.
(defadvice insert-for-yank (around indent-region)    ; Indent pasted code
  "Indents after yanking."                           ; def-advice is nice
  (let ((point-before (point)))
    ad-do-it
    (when (eq (get-mode-local-parent major-mode) 'prog-mode)
      (indent-region point-before (point)))))
(ad-activate 'insert-for-yank)
;;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
;;; Prettify some symbols
;; (agda2-mode replaces the need for anything fancier)
(progn
  (global-prettify-symbols-mode -1)
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("LAMBDA" . 955)))
  (global-prettify-symbols-mode 1))
;;; General Hooks
(add-hook 'text-mode-hook (lambda ()
                            (flyspell-mode)
                            (visual-line-mode)))
(add-hook 'prog-mode-hook (lambda ()
                            (require 'agda-input)
                            (set-input-method "Agda")
                            (flyspell-prog-mode)
                            (when (large-screen?)
                              (text-scale-increase 1))
                            (setq show-trailing-whitespace t)))
(add-hook 'dired-mode-hook (lambda ()
                            (when (large-screen?)
                              (text-scale-increase 1))))
;;;; Package Specific Configuration ;;;;
(require 'bind-key)        ; Allow for keybindings when byte-compiling
(require 'use-package)

(defmacro %use-package (&rest args)
  (declare (indent 1))
  (eval `(backquote (use-package ,@args))))
(defun gen-hooks (modes hook)
  (cl-map 'list (lambda (x) (cons x hook)) modes))
;;; Emacs Improvements
;; Improved File Navigation
;; Ido as a completion framework, ffap bindings, ...
;; (use-package ido
;;   :defer nil
;;   :straight nil
;;   :config
;;   (setq ido-everywhere t)
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-create-new-buffer 'always)
;;   (setq ido-use-filename-at-point 'guess)
;;   (setq ido-file-extensions-order
;;         '(".gpg" ".org" ".txt" ".scm" ".lisp" ".el"))
;;   (setq ido-save-directory-list-file nil)
;;   (setq ido-default-buffer-method 'selected-window)
;;   (ido-mode 1)
;;   (unbind-key "M-b" ido-file-dir-completion-map)
;;   (unbind-key "M-f" ido-file-dir-completion-map)
;;   :bind (:map ido-file-dir-completion-map
;;               ("M-d" . ido-enter-dired)))
(use-package ffap
  :defer nil
  :config
  (ffap-bindings))
(use-package auto-dim-other-buffers
  :defer nil
  :hook ((after-init-hook . auto-dim-other-buffers-mode)))

(use-package vertico
  :defer nil
  :config
  (setq enable-recursive-minibuffers t)
  (use-package orderless
    :defer nil
    :config
    ;; Vertico should use this.
    (setq completion-styles '(orderless basic))
    (setq orderless-matching-styles '(orderless-flex))
    ;; For filenames and tab-complete however; We should use the basic
    ;; style.
    (setq completion-category-overrides
          '((file (styles basic partial-completion))
            (symbol (styles basic partial-completion))))
    ;; TODO: bind all the matching styles to a key, programmatically.
    (defun orderless-use-literal ()
      (interactive)
      (setq-local orderless-matching-styles '(orderless-literal)))
    :bind (:map minibuffer-mode-map
                ("C-l" . orderless-use-literal)))
  ;; Vertico presentation engine.
  (use-package vertico-multiform
    :straight nil :defer nil
    :load-path "straight/build/vertico/extensions"
    :config
    ;; Enable sorting engine.
    (require 'vertico-multiform)
    (vertico-multiform-mode)
    ;; When listing files and directories, put dot-files at the end
    ;; and directories first.
    (setq vertico-multiform-categories
          '((file (vertico-sort-function . sort-file-list))))
    ;; NOTE: mutative for performance
    (defun sort-dotfiles-end (files)
      (nconc (seq-remove (curry #'string-prefix-p ".") files)
             (seq-filter (curry #'string-prefix-p ".") files)))
    (defun sort-directories-first (files)
      (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
             (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
    ;; Actual sort function is a composition of the two
    (defun sort-file-list (files)
      (funcall (compose #'sort-directories-first #'sort-dotfiles-end)
               (vertico-sort-alpha files))))
  ;; Regain some Ido-like functionality.
  (use-package vertico-directory
    :straight nil :defer nil
    :load-path "straight/build/vertico/extensions"
    :config
    (defun vertico-directory-enter-dired ()
      (interactive)
      (dired (file-name-directory (vertico--candidate)))
      (vertico-exit))
    :bind (:map vertico-map
                ("M-d" . vertico-directory-enter-dired)
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
  ;; Enable Vertico everywhere
  (vertico-mode 1))
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s g" . consult-ripgrep)
         ("M-s G" . consult-git-grep)
         ("M-s l" . consult-locate)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)))

(use-package marginalia
  :defer nil
  :after vertico
  :config
  ;; Fix some ugly cases where marginalia uses multiple lines
  (add-hook 'icomplete-minibuffer-setup-hook
            (lambda () (setq truncate-lines t)))
  (marginalia-mode 1))

;; Note: LMAO
;; (use-package ivy-posframe
;;   :defer nil
;;   :after ivy
;;   :config
;;   (defun ivy-partial-or-done ()
;;     "Complete the minibuffer text as much as possible.
;; If the text hasn't changed as a result, forward to `ivy-alt-done'."
;;     (interactive)
;;     (cond
;;      ((and (numberp completion-cycle-threshold)
;;            (< (length ivy--all-candidates) completion-cycle-threshold))
;;       (let ((ivy-wrap t))
;;         (ivy-next-line)))
;;      ((and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
;;            (or (and (equal ivy--directory "/")
;;                     (string-match-p "\\`[^/]+:.*\\'" ivy-text))
;;                (= (string-to-char ivy-text) ?/)))
;;       (let ((default-directory ivy--directory)
;;             dir)
;;         (minibuffer-complete)
;;         (ivy-set-text (ivy--input))
;;         (when (setq dir (ivy-expand-file-if-directory ivy-text))
;;           (ivy--cd dir))))
;;      (t
;;       (or (ivy-partial)
;;           (when (or (eq this-command last-command)
;;                     (eq ivy--length 1))
;;             ;; THE CHANGED LINE
;;             (ivy-alt-done t))))))
;;   (defun ivy-partial ()
;;     "Complete the minibuffer text as much as possible."
;;     (interactive)
;;     (if (ivy-state-dynamic-collection ivy-last)
;;         (let* ((bnd
;;                 (ignore-errors
;;                   (funcall
;;                    (ivy-state-collection ivy-last)
;;                    ivy-text nil (cons 'boundaries (buffer-substring (point) (line-end-position))))))
;;                (beg (+ (minibuffer-prompt-end)
;;                        (if bnd (cadr bnd) 0))))
;;           (delete-region beg (point-max))
;;           (insert
;;            (ivy-state-current ivy-last))
;;           t)
;;       (let* ((parts (or (ivy--split-spaces ivy-text) (list "")))
;;              (tail (last parts))
;;              (postfix (car tail))
;;              (case-fold-search (ivy--case-fold-p ivy-text))
;;              (completion-ignore-case case-fold-search)
;;              (new (try-completion (string-remove-prefix "^" postfix)
;;                                   (mapcar (lambda (str)
;;                                             (let ((i (string-match-p postfix str)))
;;                                               (and i (substring str i))))
;;                                           ivy--old-cands))))
;;         (cond
;;          ((eq new t) (ivy-done))
;;          ((string= (car tail) (car (ivy--split-spaces new))) nil)
;;          (new
;;           (delete-region (minibuffer-prompt-end) (point-max))
;;           (setcar tail
;;                   (if (= (string-to-char postfix) ?^)
;;                       (concat "^" new)
;;                     new))
;;           (ivy-set-text
;;            (concat
;;             (mapconcat #'identity parts " ")))
;;           (insert ivy-text)
;;           (ivy--partial-cd-for-single-directory)
;;           t))
;;         (unless (or (= (seq--elt-safe ivy-text (- (length ivy-text) 1)) ?\-)
;;                     (not (string-search "M-x" (minibuffer-prompt)))
;;                     (eq new t)
;;                     (= (length ivy--old-cands) 1))
;;           (delete-region (minibuffer-prompt-end) (point-max))
;;           (setcar tail
;;                   (if (= (string-to-char postfix) ?^)
;;                       (concat "^" new)
;;                     new))
;;           (ivy-set-text
;;            (concat
;;             (mapconcat #'identity parts " ")
;;             (and t (not (= (length ivy--old-cands) 1)) "-")))
;;           (insert ivy-text)
;;           (ivy--partial-cd-for-single-directory)
;;           (ivy-partial-or-done)))))
;;   (defun ivy--recompute-index (re-str cands)
;;   "Recompute index of selected candidate matching RE-STR.
;; CANDS are the current candidates."
;;   (let ((caller (ivy-state-caller ivy-last))
;;         (func (or (ivy-alist-setting ivy-index-functions-alist)
;;                   #'ivy-recompute-index-zero))
;;         (case-fold-search (ivy--case-fold-p re-str))
;;         (preselect (ivy-state-preselect ivy-last))
;;         (current (ivy-state-current ivy-last))
;;         (empty (string= re-str "")))
;;     (unless (or (memq this-command '(ivy-resume ivy-partial-or-done))
;;                 ivy--recompute-index-inhibit)
;;       (let ((index (cond
;;                     ((or empty (string= re-str "^"))
;;                      (ivy--preselect-index preselect cands))
;;                     ((and (> (length cands) 10000) (eq func #'ivy-recompute-index-zero))
;;                      0)
;;                     ((cl-position (string-remove-prefix "^" re-str)
;;                                   cands
;;                                   :test #'ivy--case-fold-string=))
;;                     ((and (ivy--completing-fname-p)
;;                           (cl-position (concat re-str "/")
;;                                        cands
;;                                        :test #'ivy--case-fold-string=)))
;;                     ((and (eq caller 'ivy-switch-buffer)
;;                           (not empty))
;;                      (or (cl-position current cands :test #'string=)
;;                          0))
;;                     (t 0))))
;;         (ivy-set-index index)))))
;;   (setq ivy-use-virtual-buffers nil)
;;   (setq enable-recursive-minibuffers t)
;;   (ivy-mode)
;;   (setq ivy-initial-inputs-alist nil)
;;   (setq ivy-tab-space t)
;;   (use-package flx
;;     :defer nil)
;;   (require 'flx)
;;   (setq ivy-sort-matches-functions-alist
;;         '((t . ivy--flx-sort)))
;;   (setq ivy-re-builders-alist
;;         '((swiper . ivy--regex-plus)
;;           (swiper-isearch . ivy--regex-plus)
;;           (swiper-isearch-backward . ivy--regex-plus)
;;           (t . ivy--regex-fuzzy))))
;; (use-package swiper
;;   :defer nil
;;   :config
;;   (global-set-key "\C-s" 'swiper-isearch)
;;   (global-set-key "\C-r" 'swiper-isearch-backward))
;; (use-package counsel
;;   :defer nil
;;   :config
;;   (counsel-mode)
;;   (setq counsel-describe-map
;;         (let ((map (make-sparse-keymap)))
;;           (define-key map (kbd "C-.") #'counsel-find-symbol)
;;           (define-key map (kbd "<SPC>") #'ivy-partial)
;;           (define-key map (kbd "C-,") #'counsel--info-lookup-symbol)
;;           map))

;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   (global-set-key (kbd "<f6>") 'ivy-resume)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
;;   (global-set-key (kbd "<f1> l") 'counsel-load-library)
;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   (global-set-key (kbd "C-c k") 'counsel-ag)
;;   (global-set-key (kbd "C-x l") 'counsel-locate)
;;   (global-set-key (kbd "C-S-o") 'counsel-rhythmbox))


;; (use-package hotfuzz
;;   :defer nil
;;   :config
;;   (setq completion-styles '(hotfuzz)))
;; (use-package icicles
;;   :defer nil
;;   :init
;;   (setq completion-base-size nil)
;;   (icicle-mode))
;; (use-package icomplete-vertical
;;   :init
;;   (setq icomplete-in-buffer t)
;;   (icomplete-mode)
;;   (icomplete-vertical-mode))
(use-package project)
;; (use-package embark
;;   :config
;;   (defun straight-visit-package-website (&optional package)
;;     (interactive)
;;     (let* ((melpa-recipe
;;             (if package (intern package) (straight-get-recipe)))
;;            (recipe (straight--convert-recipe melpa-recipe)))
;;       (straight--with-plist recipe (host repo)
;;         (pcase host
;;           ('github (browse-url (format "https://github.com/%s" repo)))
;;           ('gitlab (browse-url (format "https://gitlab.com/%s" repo)))
;;           (_ (browse-url (format "%s" repo)))))))
;;   (defmacro embark-extend-keymap (keymap &rest keys)
;;     (declare (indent 1))
;;     `(let ((map (make-sparse-keymap)))
;;        ,@(mapcar
;;           (pcase-lambda (`(,key ,fn))
;;             `(define-key map ,(if (stringp key) (kbd key) key)
;;                ,(if (symbolp fn) `#',fn fn)))
;;           keys)
;;        (setq ,keymap
;;              (make-composed-keymap map ,keymap))))
;;   (embark-extend-keymap embark-library-map
;;     ("s" straight-visit-package-website))
;;   :bind*
;;   (("C-." . embark-act)
;;    ("M-." . embark-dwim)))
(use-package dired
  :straight nil
  :config
  (require 'dired-x)
  (setq dired-dwim-target t)
  ;; Hide dotfiles in omit-mode
  ;; C-x M-o
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$"))
  ;; Human readable file sizes
  (setf dired-listing-switches "-alh")
  (use-package dired-aux
    :straight nil
    :ensure-system-package 7z
    :after dired
    :config
    ;; Register compression with 7z
    (add-to-list 'dired-compress-files-alist
                 '("\\.7z\\'" . "7z a -mx=9 %o %i"))
    (setf dired-compress-directory-default-suffix ".7z"))

  (use-package dired-rsync
    :ensure-system-package rsync
    :after dired
    :bind
    (:map dired-mode-map
          ("C" . dired-rsync)))
  (use-package dired-du
    :after dired
    :custom
    ((dired-du-size-format t)))
  (use-package dired-subtree
    :after dired
    :bind (:map dired-mode-map
                ("<tab>" . dired-subtree-toggle)
                ("M-<tab>" . dired-subtree-cycle)
                ("S-<tab>" . dired-subtree-cycle)))
  (use-package wdired
    :after dired
    :config
    (setq wdired-allow-to-change-permissions t)
    :bind (:map wdired-mode-map
                ("C-o" . dired-display-file)))
  ;; TODO: Better TRAMP handling. If we attempt to openwith a file on
  ;; a remote ssh host, open an sshfs connection to the file and then
  ;; proceed normally.
  ;;
  ;; This will probably make use of an (add-advice ... :filter-args ...)
  (use-package openwith
    :defer nil
    :config
    (eval-after-load 'tramp-sshfs
      '(defun openwith-file-handler (operation &rest args)
         (when (and openwith-mode (not (buffer-modified-p)) (zerop (buffer-size)))
           (let ((assocs openwith-associations)
                 (file ((lambda (&optional file)
                          (and file (stringp file)
                               (if (tramp-sshfs-file-name-p file)
                                   (tramp-fuse-local-file-name file)
                                 file)))
                        (car args)))
                 oa)
             (while assocs
               (setq oa (car assocs)
                     assocs (cdr assocs))
               (when (save-match-data (string-match (car oa) file))
                 (let ((params (mapcar (lambda (x) (if (eq x 'file) file x))
                                       (nth 2 oa))))
                   (when (or (not openwith-confirm-invocation)
                             (y-or-n-p (format "%s %s? " (cadr oa)
                                               (mapconcat #'identity params " "))))
                     (if (eq system-type 'windows-nt)
                         (openwith-open-windows file)
                       (openwith-open-unix (cadr oa) params))
                     (kill-buffer nil)
                     (when (featurep 'recentf)
                       (recentf-add-file file))
                     (error "Opened %s in external program"
                            (file-name-nondirectory file))))))))
         (let ((inhibit-file-name-handlers
                (cons 'openwith-file-handler
                      (and (eq inhibit-file-name-operation operation)
                           inhibit-file-name-handlers)))
               (inhibit-file-name-operation operation))
           (apply operation args))))

    (openwith-mode 1)
    (setq openwith-associations
          (list
           (list (openwith-make-extension-regexp
                  '("mpg" "mpeg" "mp3" "mp4" "webm"
                    "avi" "wmv" "wav" "mov" "flv"
                    "ogm" "ogg" "mkv" "flac"))
                 "mpv"
                 '(file))
           (list (openwith-make-extension-regexp
                  '("swf"))
                 "gnash"
                 '(file)))))
  (use-package picpocket)
  (use-package sxiv
    :ensure-system-package sxiv
    :commands sxiv)
  :bind (:map dired-mode-map
              ("l" . dired-up-directory))
  :hook (dired-mode . dired-hide-details-mode))

;; Recursive grepping that doesn't suck;
;; Follows symlinks and handles compressed files transparently
(use-package ripgrep
  :after grep :defer nil
  :ensure-system-package (rg . ripgrep)
  :commands rgrep
  :config
  (setq-default ripgrep-arguments '("--binary" "-z"))
  (defalias 'rgrep #'ripgrep-regexp))
(use-package grep
  :straight nil
  :after grep :defer nil
  :custom
  (grep-find-template
   "find -L <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +"))

;; Eshell
(use-package eshell
  :straight nil
  :hook
  (eshell-mode . (lambda () (eshell/alias "open" "find-file $1"))))
(use-package tramp
  :straight nil
  :config
  ;; GuixSD compatibility
  (setq tramp-remote-path
        (append tramp-remote-path
                '(tramp-own-remote-path))))
(use-package tramp-sshfs
  :straight nil
  :after tramp :defer nil)
(use-package esh-module
  :after eshell :defer nil
  :straight nil
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(use-package sudo-edit
  :custom
  ((sudo-edit-local-method "su")))
;;; NOTE: This breaks dump-emacs-portable
(use-package undo-tree
  :defer nil
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))
(use-package backup-walker)

(use-package flyspell
  :ensure-system-package
  (app-text/hunspell)
  :config
  (setq ispell-program-name "hunspell"))
(use-package iscroll
  :defer nil
  :config
  (defun point-y ()
    (cdr (posn-x-y (posn-at-point))))
  (defun y-pixels-from-window-center ()
    (- (point-y)
       (/ (window-pixel-height) 2)))

  (defun reasonable? (x)
    (or (> 10 (abs x))))

  ;; NOTE: Elisp is not TCO
  (defmacro until (test &rest body)
    (declare (indent 1))
    `(while (not ,test)
       ,@body))

  (defun positive? (x)
    (< 0 x))
  (defun negative? (x)
    (not (positive? x)))

  (defun recenter-considerate ()
    (interactive)
    (until (or (reasonable? (y-pixels-from-window-center))
               (posn-at-point (point-max))
               (posn-at-point (point-min)))
      (if (negative? (y-pixels-from-window-center))
          (iscroll-down)
        (iscroll-up))))

  (define-global-minor-mode global-iscroll-mode
    iscroll-mode iscroll-mode)
  (iscroll-mode)

  :commands recenter-considerate)
;; (use-package beacon
;;   :defer nil
;;   :config
;;   (beacon-mode))

(use-package switch-window
  :bind ("C-x M-f" . switch-window))
;;; Utilities
(use-package magit
  :ensure-system-package (perl)
  :config
  (require 'mpv)
  (add-hook
   'magit-post-commit-hook
   (lambda () (unless (mpv-live-p)
           (mpv-start "https://youtu.be/CDzBazMZynM?t=68"
                      "--no-video")))))
;; (use-package forge
;;   :after (magit emacsql))
;; (use-package emacsql)
(use-package multiple-cursors)
(use-package multifiles
  :commands mf/mirror-region-in-multifile)
(use-package nov
  :ensure-system-package (unzip)
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80))
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq pdf-info-epdfinfo-program
        (executable-find "epdfinfo"))
  :hook (pdf-view-mode . (lambda () (auto-revert-mode 1))))
;;; Big packages
(use-package erc
  :straight nil
  :config
  ;; Ignore auth-sources
  (add-hook 'erc-mode-hook
            #'(lambda () (setq auth-sources '())))
  ;; Connect to ZNC
  (setq erc-default-server "192.168.1.45")
  (setq erc-default-port-tls "6697")
  (setq erc-nick "armink")
  (setq erc-prompt-for-password nil)
  (setq erc-password (format "armink@%s/lainchan:armink"
                             (system-name)))
  ;; Useful with ZNC
  (setq erc-kill-buffer-on-part t)
  ;; Highlight mentions blue and beep
  (setq erc-current-nick-highlight-type 'all)
  (add-hook 'erc-text-matched-hook #'(lambda (&rest ignore) (beep)))
  ;; Command to rgrep IRC logs
  (defun erc-search-logs ()
    (interactive)
    (funcall-interactively #'consult-ripgrep "/media/archive/irc/"))
  ;; Install hooks away and back hooks
  ;; NOTE: Messiness is inherited from erc-cmd-AWAY
  (advice-add
   'erc-cmd-AWAY
   :after
   (lambda (line &rest _)
     (when (string-match "^\\s-*\\(.*\\)$" line)
       (let ((reason (match-string 1 line)))
         (if (string= "" reason)
             (run-hooks 'erc-back-hook)
           (run-hooks 'erc-away-hook))))))

  (defun reify-bindings (&rest symbols)
    (mapcar (lambda (symbol)
              (list symbol (eval symbol)))
            symbols))


  ;; When away, don't notify me.
  (add-hook
   'erc-away-hook
   (lambda ()
     (add-hook
      'erc-send-post-hook
      (lambda () (erc-cmd-AWAY "")))
     (setf erc-beep-match-types nil)))

  (add-hook
   'erc-back-hook
   (lambda ()
     (remove-hook
      'erc-send-post-hook
      (lambda () (erc-cmd-AWAY "")))
     (setf erc-beep-match-types '(current-nick))))

  ;; Port feature from another IRC client
  (define-minor-mode erc-hide-notices
    "Hide clutter per-buffer"
    :keymap
    :lighter "/hidden"
    :after-hook
    (mapcar
     (if erc-hide-notices
         #'add-to-invisibility-spec
       #'remove-from-invisibility-spec)
     '(notice timestamp)))

  (define-key erc-mode-map (kbd "M-+") #'erc-hide-notices)

  (defun add-text-property (string property &optional argument)
    (unless argument
      (setf argument t))
    (progn (put-text-property
            0 (length string) property argument string)
           string))

  (advice-add 'erc-highlight-notice :filter-return
              (compose (rcurry #'add-text-property 'invisible 'notice)
                 (rcurry #'concat "\n")))

  ;; (setq erc-text-matched-hook '())
  ;; (add-to-list 'erc-keywords
  ;;              "pomf2.lain.la")
  ;; (add-hook 'erc-text-matched-hook
  ;;           #'(lambda (type &rest ignored)
  ;;               (when (eq type 'keyword)
  ;;                 (erc-send-input-line
  ;;                  (buffer-name)
  ;;                  "lol"))))

  ;; Make erc's re-connection attempts more sane
  (setf erc-server-reconnect-attempts 30)
  (setf erc-server-reconnect-timeout 5)
  ;; Nice aliases
  (defalias 'erc-cmd-I #'erc-cmd-ME)
  (defalias 'erc-cmd-BACK #'erc-cmd-AWAY)
  (defun erc-reconnect ()
    (interactive)
    (erc-server-reconnect))
  ;; A more intuitive hook interface.
  (advice-add 'erc-display-message
              :after (lambda (_ general-event _ specific-event &rest _)
                       (run-hook-with-args 'erc-event-handlers
                                           (if (symbolp specific-event)
                                               specific-event general-event))))
  ;; ICQ style sounds?
  (setq lexical-binding t)
  (defun play-sound* (sound)
    (let ((pathname
           (sound-pathname sound)))
      (when pathname
        (start-process "sound" nil "mpv" pathname))))
  (defun sound-pathname (sound)
    (ensure-extension
     '(".flac" ".ogg" ".wav" ".mp3")
     (if (file-name-absolute-p sound)
         sound
       (expand-file-name
        sound (concat "~/.emacs.d/" "sounds/")))))
  (defun ensure-extension (whitelist pathname)
    (car (cl-remove-if-not
          #'file-exists-p
          (mapcar (lambda (x) (concat (file-name-sans-extension pathname) x))
                  whitelist))))
  (defun erc-add-sound (event sound)
    (add-hook 'erc-event-handlers
              (eval
               `(lambda (event-type)
                  (and (equal ',event event-type)
                       (play-sound* ,sound))))))
  (eval-after-load 'erc-dcc
    '(erc-add-sound 'dcc-send-offered "incoming-file-transfer"))

  ;; Easy-REPL
  (defun rest-of (string)
    (substring string
               (match-end 0) (length string)))
  (defun take-matches-of (rx string)
    (and (string-match rx string)
         (cons (substring string
                          (match-beginning 0)
                          (match-end 0))
               (take-matches-of rx (rest-of string)))))
  (defmacro with-substrings (pair thunk)
    (declare (indent 2))
    (let ((rx (car pair))
          (string (cadr pair)))
      `(mapcar ,thunk (take-matches-of ,rx ,string))))
  (defun easy-eval (string)
    (format "%s" (eval (read (string-trim-left string "@")))))
  (defun easy-repl (struct)
    (let ((text (erc-input-string struct))
          ;; Until I hack parse-sexp in.
          (keyword (rx (or (+ (and "!(" (* any) ")"))
                           (+ (and "@(" (* (not ")")) ")"))))))
      (with-substrings (keyword text)
          (lambda (substring)
            (setf (erc-input-string struct)
                  (replace-regexp-in-string
                   (rx-to-string `(and ,substring)) (easy-eval substring)
                   (erc-input-string struct)))))))
  ;; (add-hook 'erc-pre-send-functions 'easy-repl)
  (load-file "~/.emacs.d/erc-mpd.el")
  (defun erc-fill-variable ()
    "Fill from `point-min' to `point-max'."
    (unless (equal (buffer-name) "#programming")
      (let ((fill-prefix erc-fill-prefix)
            (fill-column (or erc-fill-column fill-column)))
        (goto-char (point-min))
        (if fill-prefix
            (let ((first-line-offset (make-string (erc-timestamp-offset) 32)))
              (insert first-line-offset)
              (fill-region (point-min) (point-max) t t)
              (goto-char (point-min))
              (delete-char (length first-line-offset)))
          (save-match-data
            (let* ((nickp (looking-at "^\\(\\S-+\\)"))
                   (nick (if nickp
                             (match-string 1)
                           ""))
                   (fill-column (- erc-fill-column (erc-timestamp-offset)))
                   (fill-prefix (make-string (min (+ 1 (length nick))
                                                  (- fill-column 1)
                                                  (or erc-fill-variable-maximum-indentation
                                                      fill-column))
                                             32)))
              (erc-fill-regarding-timestamp))))
        (erc-restore-text-properties))))
  ;; Procedures to go along with it.
  (defmacro spoiler (&rest words)
    `(format "01,01%s" (mapconcat 'prin1-to-string ',words " ")))
  (defun vim-face () "（　´_ゝ`）")
  ;; Set preferred font and theme
  :hook
  (erc-mode . (lambda () (custom-set-variables
                     '(custom-enabled-themes '(wheatgrass)))
                (when (member "DejaVu Sans Mono" (font-family-list))
                  (set-frame-font "DejaVu Sans Mono"))
                (when (large-screen?)
                  (text-scale-increase 3))
                (prettify-symbols-mode)
                (erc-notify-mode))))
(use-package erc-goodies
  :defer nil :after erc
  :straight nil
  :config
  (setq erc-interpret-mirc-color t)
  (setq erc-controls-highlight-regexp
        ;; thanks zdm for the regex
        (concat
         "\\(\\|\\|\\|\x1e\\|\\|\\|\\|\\|"
         "\\([0-9][0-9]?\\)?\\(,\\([0-9][0-9]?\\)\\)?\\)"
         "\\([^\x1e]*\\)")))
(use-package erc-hl-nicks
  :after erc
  :config
  (erc-hl-nicks-mode))
(use-package erc-spelling
  :straight nil
  :after erc
  :defer nil
  :config
  (erc-spelling-mode))

;; TODO: Un-selectable popup sidebar showing playlist. This will
;; involve a DSL.
(use-package emms-tag-editor
  :straight nil
  :after emms :defer nil
  :ensure-system-package (metaflac . flac))
(use-package emms
  :preface
  ;; (defun emms-peek-window ()
  ;;   (window-properties '(unselectable dedicated)
  ;;    (or (window-mode=?
  ;;         (window-most 'right)
  ;;         'emms-playlist-mode)
  ;;        (window-beside
  ;;         (window-most 'right)
  ;;         (make-window emms-playlist-buffer)))))

  (defun emms-smart-browse-quick ()
    (interactive)
    (require 'emms)
    (setq emms-browser-default-browse-type
          (prog1 emms-browser-default-browse-type
            (setq emms-browser-default-browse-type
                  'info-artist)
            (emms-smart-browse))))
  :config
  ;; Please don't make the playlist buffer hidden!
  (setf emms-playlist-buffer-name "*EMMS Playlist*")

  (require 'emms-score)
  (advice-add 'emms-random
              :before #'(lambda () (or (equal emms-score-current-mood 'default)
                                  (emms-score-down-playing))))
  (require 'emms-browser)
  (setq emms-browser-default-browse-type 'info-album)
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-browser-thumbnail-small-size 128)
  (setq emms-browser-thumbnail-medium-size 256)
  ;; Search should return results sorted by album.
  (defun emms-browser-render-search (tracks)
    (let ((entries
           (emms-browser-make-sorted-alist 'info-album tracks)))
      (dolist (entry entries)
        (emms-browser-insert-top-level-entry (car entry)
                                             (cdr entry)
                                             'info-artist))))
  (emms-cache-enable)
  ;; For some reason serializing the cache to the cache file isn't
  ;; done by emms-browser-cache-thumbnail???
  ;;
  ;; NOTE: This is how you serialize Lisp objects when you have a
  ;; object printers and reader macros! You don't do what the original
  ;; authors did in emms-cache-save.
  (advice-add 'emms-cache-save
              :filter-return
              (lambda (emms-cache-dirty)
                (when emms-cache-dirty
                  (message "Saving emms thumbnail cache... ")
                  (with-temp-buffer
                    (insert (format "(setq emms-browser--cache-hash %S)"
                                    emms-browser--cache-hash))
                    (append-to-file (point-min) (point-max)
                                    emms-cache-file))
                  (message "Saving emms thumbnail cache...done "))))

  ;; (defun process-name (pid)
  ;;   (funcall (compose #'cdr (curry #'assoc 'comm) #'process-attributes) pid))
  ;; (defun mpd-running? ()
  ;;   (member "mpd"
  ;;           (mapcar #'process-name (list-system-processes))))
  ;; ;; Connect to a MPD if one is running, otherwise fallback to mpv.
  ;; (if (mpd-running?)
  ;;     (progn

  (setq emms-source-file-default-directory
        "~/music/")
  (require 'emms-player-simple)
  (require 'emms-player-mpv)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (setq emms-player-list (list emms-player-mpv))

  (require 'emms-player-mpd)
  (setq emms-player-mpd-music-directory "/media/music/")
  (setq emms-player-list (list emms-player-mpd))
  ;; Redefine emms-player-mpd-connect to sync MPD with EMMS, restoring
  ;; the EMMS playlist from the previous session (this preservers
  ;; images).
  ;;
  ;; TODO: Re-do this using advising procedures.
  (require 'emms-history)
  ;; NOTE: Only saves the main playlist buffer (which I currently only
  ;; care for). To make it save all playlist buffers, you have to
  ;; maintain a history directory and map a saving function over the
  ;; buffers.
  (defun read-from-file (pathname)
    (with-temp-buffer
      (insert-file pathname)
      (read (current-buffer))))
  (defun reify-buffer (buffer-name)
    (with-current-buffer buffer-name
      (let ((print-length nil)
            (print-level nil))
        (prin1-to-string
         (buffer-substring (point-min)
                           (point-max))))))
  (defun reflect-buffer (string)
    (insert string))

  (defun emms-history-save ()
    (interactive)
    (with-temp-buffer
      (insert (reify-buffer emms-playlist-buffer))
      (write-file emms-history-file)))
  (defun emms-history-load ()
    (interactive)
    (with-current-emms-playlist
      (emms-browser-clear-playlist)
      (reflect-buffer
       (read-from-file emms-history-file))))
  ;; HACK: when we reify the playlist buffer like this, the properties
  ;; get added when the text starts, not from the beginning of the
  ;; line. This hack might lead to some undefined behavior, so best to
  ;; properly investigate a fix at some point (TODO). I suspect
  ;; something regarding #\newline characters.
  ;; (advice-add 'emms-property-region :filter-return
  ;;             (lambda (pos-pair)
  ;;               (cons (point-at-bol)
  ;;                     (point-at-eol))))

  ;; NOTE: The emms-player system puts things together like
  ;; clockwork... Absolute garbage...
  (defun emms-mpd-connect ()
    (let ((emms-playlist-buffer
           (emms-playlist-new "garbage")))
      (emms-player-mpd-connect)
      (kill-buffer emms-playlist-buffer))
    (emms-history-load))

  ;; NOTE: Here, emms is not written like shit! Proper abstraction
  ;; layers make adding TRAMP support very easy! (MAYBE)
  ;; (advice-add 'emms-track-get :filter-return
  ;;             (lambda (pathname)
  ;;               (if (tramp-sshfs-file-name-p pathname)
  ;;                   (tramp-fuse-local-file-name pathname)
  ;;                 pathname)))


  (defun emms-track-get (track name &optional default)
    "Return the value of NAME for TRACK.
If there is no value, return DEFAULT (or nil, if not given)."
    (emms-dictionary-get track name default))
  (require 'emms-cache)
  (emms-cache-enable)
  ;; TODO: Write refreshing procedure when for `g' is pressed. We'd
  ;; have to rebuild EMMS buffers for this to be useful; more
  ;; specifically, clear the buffer and rebuild by
  ;; emms-browser-render-hash again. Maybe we can reuse some other
  ;; emms-browser components.
  (defun emms-refresh ()
    (interactive)
    ;; (emms-cache-reset) minus the yes-or-no-p
    (setq emms-cache-db
          (make-hash-table
           :test (if (fboundp 'define-hash-table-test)
                     'string-hash
                   'equal)))
    (setq emms-cache-dirty t)
    (emms-cache-save)
    (emms-player-mpd-update-all-reset-cache))
  (run-with-idle-timer 300 t #'emms-refresh)

  ;; NOTE: If you notice multiple files being added to the playlist,
  ;; you have a cache with duplicate files in it. Either run the
  ;; refresh procedure or, (TODO), be smart and don't add duplicate
  ;; files to the playlist. This would probably involve redefining
  ;; emms-browser-bdata-data.
  (defun emms-browser-play ()
    (interactive)
    (save-excursion
      (emms-browser-add-tracks-and-play)))
  ;; TODO: Find a way of getting all markers/cursors (whatever the
  ;; proper terminology is) for a buffer, that way we can update all
  ;; of them to center onto the current track. This needs to be done
  ;; since the playlist buffer is usually inactive... (making this
  ;; mode more useful).
  (advice-add 'emms-playlist-mode-center-current
              :after #'recenter-considerate)
  (advice-add 'emms-playlist-mode-center-current
              :after #'hl-line-highlight)
  (defun emms-playlist-center-current ()
    (interactive)
    (run-with-timer
     0.1 nil
     #'emms-playlist-mode-center-current)
    (run-with-timer
     0.2 nil
     #'hl-line-highlight))
  (define-minor-mode emms-playlist-center-mode
    "Lock emms-playlist buffer to center on playing track."
    :lighter "/LOCK "
    (if (equal (current-buffer) emms-playlist-buffer)
        (if emms-playlist-center-mode
            (progn (emms-playlist-center-current)
                   (add-hook 'emms-playlist-selection-changed-hook
                             #'emms-playlist-center-current 0 t))
          (remove-hook 'emms-playlist-selection-changed-hook
                       #'emms-playlist-center-current))
      (message "Can only be used in an EMMS playlist buffer")))
  (add-hook 'emms-playlist-mode-hook #'emms-playlist-center-mode)

  ;; Center the playlist buffer and disable the modeline.
  (add-hook 'emms-playlist-mode-hook
            (lambda ()
              (with-current-buffer emms-playlist-buffer-name
                (setq-local writeroom-mode-line nil)
                (writeroom-mode 1))))

  ;; Do it
  (emms-refresh)
  (emms-mpd-connect)
  :bind
  ("<f9>" . emms-smart-browse)
  ("S-<f9>" . emms-smart-browse-quick)
  (:map emms-browser-mode-map
        ("<return>" . emms-browser-play)))
(use-package org
  :straight nil
  :ensure-system-package (tectonic biber)
  :mode ("\\.org\\'" . org-mode)
  :config
  (require 'ob-maxima)

  (setf (nth 4 org-emphasis-regexp-components) 10)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))
  (setq org-table-use-standard-references t)

  (setq org-directory "~/.emacs.d/org")

  (setq org-agenda-file "~/.emacs.d/agenda/inbox.org")
  (setq org-latex-pdf-process (list "tectonic -o %o %f"))
  (setq org-use-fast-todo-selection t)
  (defun org-open-clocks ()
    (find-file "~/.emacs.d/org-clocks.org"))

  (use-package org-agenda
    :straight nil :defer nil
    :config
    (setq jethro/org-agenda-directory "~/.emacs.d/agenda/")
    (add-to-list 'org-capture-templates
                 `("i" "inbox" entry (file ,(concat jethro/org-agenda-directory "inbox.org"))
                   "* TODO %?"))
    (add-to-list 'org-capture-templates
                 `("c" "org-protocol-capture" entry (file ,(concat jethro/org-agenda-directory "inbox.org"))
                   "* TODO [[%:link][%:description]]\n\n %i"
                   :immediate-finish t))
    (add-to-list 'org-capture-templates
                 `("w" "Weekly Review" entry (file+olp+datetree ,(concat jethro/org-agenda-directory "reviews.org"))
                   (file ,(concat jethro/org-agenda-directory "templates/weekly_review.org"))))
    (add-to-list 'org-agenda-custom-commands
                 `("r" "Reading" todo ""
                   ((org-agenda-file "reading.org"))))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))


    (setq org-tag-alist '(("@errand" . ?e)
                          ("@office" . ?o)
                          ("@home" . ?h)
                          ("@school" . ?s)
                          (:newline)
                          ("WAITING" . ?w)
                          ("HOLD" . ?H)
                          ("CANCELLED" . ?c)))
    (setq org-columns-default-format
          "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

    (defun jethro/set-todo-state-next ()
      "Visit each parent task and change NEXT states to TODO"
      (org-todo "NEXT"))

    (add-hook 'org-clock-in-hook 'jethro/set-todo-state-next 'append)

    (setq org-agenda-block-separator nil)
    (setq org-agenda-start-with-log-mode t)

    (setq org-log-done 'time
          org-log-into-drawer t
          org-log-state-notes-insert-after-drawers nil)

    (defun jethro/org-agenda-process-inbox-item ()
      "Process a single item in the org-agenda."
      (interactive)
      (org-with-wide-buffer
       (org-agenda-set-tags)
       (org-agenda-refile)))
    (defun org-agenda-bulk-process-entries ()
      (interactive)
      (if (not (null org-agenda-bulk-marked-entries))
          (let ((entries (reverse org-agenda-bulk-marked-entries))
                (processed 0)
                (skipped 0))
            (dolist (e entries)
              (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
                (if (not pos)
                    (progn (message "Skipping removed entry at %s" e)
                           (cl-incf skipped))
                  (goto-char pos)
                  (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
                  ;; `post-command-hook' is not run yet.  We make sure any pending log
                  ;; note is processed.
                  (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                            (memq 'org-add-log-note post-command-hook))
                    (org-add-log-note))
                  (cl-incf processed))))
            (org-agenda-redo)
            (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
            (message "Acted on %d entries%s%s"
                     processed
                     (if (= skipped 0)
                         ""
                       (format ", skipped %d (disappeared before their turn)"
                               skipped))
                     (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

    (defun org-inbox-capture ()
      "Process a single item in the org-agenda."
      (interactive)
      (org-capture nil "i"))
    (setq org-agenda-files "~/.emacs.d/agenda/")
    (setq org-agenda-todo-view
          '(" " "Agenda"
            ((agenda ""
                     ((org-agenda-span 'day)
                      (org-deadline-warning-days 365)))
             (todo "TODO"
                   ((org-agenda-overriding-header "To Refile")
                    (org-agenda-files "inbox.org")))
             (todo "NEXT"
                   ((org-agenda-overriding-header "In Progress")))
             (todo "TODO"
                   ((org-agenda-overriding-header "Projects")
                    (org-agenda-files "projects.org")))
             (todo "TODO"
                   ((org-agenda-overriding-header "One-off Tasks")
                    (org-agenda-files "next.org")
                    (org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'deadline 'scheduled))))
             nil)))

    (add-to-list 'org-agenda-custom-commands org-agenda-todo-view))

  :bind
  (("C-c a" . org-agenda)
   (:map org-mode-map
         ("C-c M-d" . org-deadline)
         ("C-c M-s" . org-schedule)))
  :hook
  ((org-mode . flyspell-mode)
   (org-mode . visual-line-mode)
   (org-mode . auto-fill-mode)))
(use-package org-remark
  :defer nil :after org
  :config
  (require 'org-remark-global-tracking)
  (org-remark-global-tracking-mode +1))
(use-package org-roam
  :straight nil
  :preface
  (setq org-roam-v2-ack t)
  :config
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-directory "~/.emacs.d/org-roam/")
  :bind
  ("C-c c" .  org-roam-capture))
(use-package org-ref
  :defer nil :after org-mode
  :config
  (setq bibtex-completion-bibliography
        "~/school/eng104/refs.bib")
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link))
(use-package ox-pandoc
  :ensure-system-package pandoc)
(use-package org-pandoc-import
  :ensure-system-package pandoc
  :straight (:host github
                   :repo "tecosaur/org-pandoc-import"
                   :files ("*.el" "filters" "preprocessors")))
(use-package hydra)
(use-package org-fc
  :straight
  (org-fc
   :type git :repo "https://git.sr.ht/~l3kn/org-fc"
   :files (:defaults "awk" "demo.org"))
  :custom
  (org-fc-directories '("~/.emacs.d/org/"))
  :config
  (require 'org-fc-hydra))
(use-package org-download)
(use-package ox-beamer
  :straight (:type built-in)
  :defer nil
  :after org
  :bind ("C-c p" . org-beamer-export-to-pdf))
(use-package org-pomodoro
  :ensure-system-package beep
  :config
  (defun beeper-args (patterns)
    (mapcar
     (lambda (pattern)
       (cl-case pattern
         (short "-l 100")
         (long "-l 500")))
     patterns))
  (defun beeper (patterns)
    (when patterns
      (save-window-excursion
        (async-shell-command
         (cl-reduce
          (lambda (command arg)
            (concat command
                    (unless (equal command "")
                      "&& sleep 0.1 && ")
                    (concat "beep -f 800 " arg)
                    " "))
          (beeper-args patterns)
          :initial-value "")))))
  (add-hook 'org-pomodoro-finished-hook
            (lambda ()
              (if (zerop (mod org-pomodoro-count org-pomodoro-long-break-frequency))
                  (beeper '(long short short))
                (beeper '(short short)))))
  (defun beep-pattern (type)
    (cl-case type
      (:tick '())
      (t '(long))))
  (defun org-pomodoro-play-sound (type)
    "Play an audio file specified by TYPE (:pomodoro, :short-break, :long-break)."
    (and org-pomodoro-play-sounds
         ;; (beeper (beep-pattern type))
         ))
  (defun org-pomodoro-maybe-play-sound (type)
    "Play an audio file specified by TYPE."
    (org-pomodoro-play-sound type)))
(use-package gif-screencast
  :ensure-system-package gifsicle)
(use-package writeroom-mode
  ;; :hook org-mode
  :custom
  ((writeroom-global-effects nil)
   (writeroom-mode-line t)))
(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-executable-path
        (expand-file-name (executable-find "plantuml")))
  (setq org-plantuml-exec-mode 'plantuml)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))
(use-package hledger-mode
  :commands (hledger-overall-report hledger-import)
  :config
  ;; NOTE: hledger-mode does not consider hledger account types.
  (setq hledger-ratios-liquid-asset-accounts
        "assets:bank assets:cash")
  (setq hledger-jfile
        "~/Documents/01-important/ledger/combined.journal")
  (setq hledger-currency-string "$")

  (defun shell-arguments (arguments)
    (cl-reduce (lambda (x y)
                 (concat x " " y))
               arguments))
  (defun build-hledger-command (&rest arguments)
    (lambda ()
      (string-trim
       (shell-command-to-string
        (format "cd %s && hledger -f %s %s"
                (shell-quote-argument
                 (expand-file-name (file-name-directory hledger-jfile)))
                hledger-jfile
                (shell-arguments arguments))))))
  (defun hledger-import-csv (csv)
    (let ((command (build-hledger-command
                    "import" csv)))
      (let ((hledger-jfile
             (concat
              (expand-file-name (file-name-directory hledger-jfile))
              (file-name-base csv) ".journal")))
        (message "%s" (funcall command)))))
  (defun hledger-import ()
    (interactive)
    (hledger-import-csv "checking.csv")
    (hledger-import-csv "savings.csv")))
(use-package guix)
(use-package direnv
  :ensure-system-package (direnv)
  :defer nil
  :config
  (direnv-mode))


;; TODO: Properly configure Gnus as a (reproducible!) general client
;; that handles both e-mail and RSS feeds for things like
;; YouTube. Properly configure some Gnus groups as well.. Also figure
;; out why Gnus "randomly" auto-loads itself.
(use-package gnus
  :config
  (setq gnus-directory "~/.emacs.d/gnus/")
  (setq gnus-cache-enter-articles
        '(ticked dormant read unread))
  (setq gnus-cache-remove-articles
        '())
  (setq gnus-fetch-old-headers t)
  (setq gnus-use-cache t)
  (setq gnus-keep-backlog t)
  (setq gnus-asynchronous t)
  (setq gnus-use-article-prefetch t)
  (setq gnus-use-dribble-file t)
  (setq gnus-always-read-dribble-file t)
  (setq gnus-treat-display-smileys nil)
  (setq gnus-auto-subscribed-groups nil)
  (setq gnus-auto-subscribed-categories nil)
  ;; This does not work as intended
  (setq gnus-options-subscribe
        (rx (or "INBOX" "gmane.lisp.scheme.mit-scheme.devel")))
  (setq gnus-check-new-newsgroups nil)
  (setq gnus-check-bogus-newsgroups nil)
  (setq gnus-use-cross-reference nil)
  (setq gnus-nov-is-evil nil)
  (spam-initialize)
  (defun wash-article ()
    ;; (gnus-article-fill-cited-article)
                                        ;fix this, W w too early?
    (gnus-article-unsplit-urls))
  (add-hook 'gnus-part-display-hook #'wash-article)

  (add-hook 'gnus-group-mode-hook #'gnus-topic-mode)



  ;; (gnus-demon-init)
  ;; (gnus-demon-add-scanmail)
  ;; (advice-add 'gnus-demon-scan-mail
  ;;             :after #'(lambda (&rest ignore) (make-thread #'gnus-jog-cache)))
  )
(use-package gnus-select-account
  :after gnus
  :hook (gnus-mode . gnus-select-account-enable))
(use-package shimbun
  :after gnus :defer nil
  :config
  (setq shimbun-verbose t)
  (setq shimbun-message-enable-logging t)
  :bind
  (:map gnus-group-mode-map
        ("Gn" . gnus-group-make-shimbun-group)
        ("GN" . gnus-group-make-shimbun-group)))

(use-package w3m
  :straight emacs-w3m
  :ensure-system-package (autoconf make w3m)
  :custom (mm-text-html-renderer 'w3m))
(use-package apel-autoloads
  :straight apel)
(use-package flim-autoloads
  :straight flim)
;; Matrix
(use-package plz
  :straight (:host github :repo "alphapapa/plz.el"))
(use-package ement
  :straight (:host github :repo "alphapapa/ement.el"))


;;; Readers
(defvar reader-map
  (make-sparse-keymap))
(global-set-key (kbd "M-r") reader-map)

(use-package elfeed
  :bind (:map reader-map
              ("r" . elfeed)))

(defmacro add-hook-and-remove-when (predicate hook-var function &rest additional-args)
    (let ((lifted-function (gensym)))
      `(progn
         (defun ,lifted-function (&rest hook-args)
           (if (apply ,predicate hook-args)
               (remove-hook ,hook-var #',lifted-function)
             (funcall ,function hook-args)))
         (add-hook ,hook-var #',lifted-function ,@additional-args))))

(use-package mpv
  :ensure-system-package mpv
  :config
  ;; A more sophisticated mpv-start using `make-process'.
  (defvar mpv-output-buffer
    "*mpv-player*")

  (el-patch-define-and-eval-template
   (defun mpv-start)
   (el-patch-swap
     (setq mpv--process ...)
     (setq mpv--process
           (make-process
            :name "mpv-player"
            :buffer mpv-output-buffer
            :command (cl-list* mpv-executable
                               (concat "--input-ipc-server=" socket)
                               (append mpv-default-options args))))))

  (add-hook
   'mpv-on-exit-hook
   (lambda () (kill-buffer mpv-output-buffer)))

  (defun mpv-read-event (event)
    (let ((event-raw (cdr (assoc 'event event))))
      (intern-soft
       (concat ":" event-raw))))
  (defun event-type=? (event event-type)
    (equal (mpv-read-event event) event-type))

  (defun mpv-until (event-type thunk)
    (declare (indent 1))
    (add-hook-and-remove-when
     (lambda (event)
       (event-type=? event event-type))
     'mpv-on-event-hook thunk))

  (defun mpv-when (event-type thunk)
    (declare (indent 1))
    (add-hook-and-remove-when
     (lambda (event)
       (when (event-type=? event event-type)
         (funcall thunk event)))
     'mpv-on-event-hook (lambda (e) "Just dimly waiting..."))))

(defun update-progress-bar (process-buffer string &optional total)
  (with-current-buffer process-buffer
    (message "%s%s" string
             (make-string
              (/ (count-lines (point-min) (point-max)) 2)
              ?.))))

;;; TODO: Playlist support.
(use-package ytel
  :ensure-system-package yt-dlp
  :config
  (require 'mpv)

  (setq ytel-invidious-api-url
        "https://invidious.snopyta.org")

  (defun ytel-video-url (video)
    "Get the URL of the current video under the point."
    (concat ytel-invidious-api-url "/watch?v="
            (ytel-video-id video)))

  (defun ytel-watch ()
    "Open video under point in mpv"
    (interactive)
    (mpv-until
        :file-loaded
      (lambda (event)
        (update-progress-bar
         (process-buffer mpv--process)
         "Girls are praying now, please wait warmly")))
    (mpv-when
        :file-loaded
      (lambda (event)
        (message nil)))
    (mpv-until
        :file-loaded
      (lambda (event)
        (when (equal last-command 'keyboard-quit)
          (kill-process mpv--process)
          (message "Hm, something smells weak~"))))
    (mpv-start
     (ytel-video-url (ytel-get-current-video))
     "--ytdl-format=bestvideo[height<=?720]+bestaudio/best"
     "--msg-level=ytdl_hook=debug")
    (mpv-run-command "enable_event" "all"))

  :bind (:map
         reader-map (("y" . ytel))
         :map
         ytel-mode-map (("<RET>" . ytel-watch))))

;;; Programming Languages (in alphabetical order)
;; Agda
(use-package agda2-mode
  :config
  (setq agda2-version "2.6.2.1")
  :bind
  (:map agda2-mode-map
        ("<M-tab>" . eri-indent-reverse)))
;; Assembly
(use-package asm-mode
  :config
  ;; Every-time a defconst is involved, the lisper should get an
  ;; electric shock.
  (defvar constant-label-fix
    (append
     '(("^\\(\\(\\@\\|\\sw\\|\\s_\\)+\\)\\>:?[ \t]*\\(\\sw+\\(\\.\\sw+\\)*\\)?"
        (1 font-lock-function-name-face) (3 font-lock-keyword-face nil t)))
     asm-font-lock-keywords)
    "Additional expressions to highlight in Assembler mode.")
  (add-hook asm-mode-hook
            (lambda ()
              (setq-local font-lock-defaults '(constant-label-fix)))))
;; C/C++
(use-package lsp-mode
  :hook c-mode-common
  :config
  (setq lsp-lens-enable nil
        gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.1)
  (setq lsp-keymap-prefix "C-c l"))
(use-package lsp-ui
  :defer nil :after lsp)
(use-package company-box
  :defer nil :after lsp)
(use-package ccls
  :hook
  ((c-mode-common . lsp)
   (c-mode-common . linum-mode)
   (c-mode-common . column-number-mode)))
(use-package flycheck
  :hook lsp)
(use-package tree-edit
  :hook lsp
  :straight (:host github
                   :repo "ethan-leba/tree-edit"))
;; Lisp Family
(defun lisp-hook (mode)
  (let ((lisp-modes
         '(lisp-mode
           sly-editing-mode
           scheme-mode
           scheme-interaction-mode
           emacs-lisp-mode
           comint-mode)))
    (gen-hooks lisp-modes mode)))
(%use-package lispy
  :hook ,(lisp-hook 'lispy-mode)
  :config
  (async-eval
   (el-patch-define-and-eval-template
    (defun (el-patch-swap lispy-newline-and-indent-plain lispy-newline-and-indent-repl))
    (cl-case major-mode
      (el-patch-remove ...)
      (t ...))))
  (setq-default lispy-no-space t)

  (unbind-key "M-." lispy-mode-map)
  (unbind-key "C-y" lispy-mode-map)
  :bind (:map lispy-mode-map
              ("C-j" . lispy-newline-and-indent-repl)))
(use-package le-lisp
  :straight nil
  :after lispy :defer nil
  :config
  ;; Make lispy eval forms in the buffers package.
  (declare-function
   slime-current-package "ext:slime")
  (declare-function
   sly-current-package "ext:sly")
  (defun lispy--eval-lisp (str)
    "Eval STR as Common Lisp code."
    (let* ((deactivate-mark nil)
           (package (if (lispy--use-sly-p)
                        (sly-current-package)
                      (slime-current-package)))
           (result (with-current-buffer (process-buffer (lispy--cl-process))
                     (if (lispy--use-sly-p)
                         (sly-eval `(slynk:eval-and-grab-output ,str) package)
                       (slime-eval `(swank:eval-and-grab-output ,str) package)))))
      (if (equal (car result) "")
          (cadr result)
        (concat (propertize (car result)
                            'face 'font-lock-string-face)
                "\n\n"
                (cadr result))))))
(use-package flx) ;; lispy depends on this

(%use-package flash-paren
  :hook ,(lisp-hook
          #'(lambda () (flash-paren-mode 1)))
  :config
  ;; Emacs 28 default overlaps with flash-paren
  (show-paren-mode -1))
(%use-package fold-this
:hook ,(lisp-hook 'fold-this-mode)
:config
(defun folded? ()
  (cl-remove-if-not
   (lambda (x) (eq (overlay-get x 'type) 'fold-this))
   (overlays-at (point))))
(defun toggle-fold-at-point ()
  (interactive)
  (if (folded?)
      (fold-this-unfold-at-point)
    (fold-this-sexp)))
:bind ("C-<tab>" . toggle-fold-at-point))
;; (use-package slime
;;   :config

;;   (setq slime-startup-animation t)
;;   (setq inferior-lisp-program "sbcl")

;;   (defun mit-scheme-start-swank (file encoding)
;;     (format
;;      "%S\n\n"
;;      `(begin
;;        (eval `(begin
;;                (define (swank-shim sexp)
;;                        (cons (intern
;;                               (string-concatenate
;;                                (cons "swank:"
;;                                      (cdr ((string-splitter
;;                                             'delimiter
;;                                             (read-from-string "#\\:"))
;;                                            (symbol->string (car sexp)))))))
;;                              (cdr sexp)))
;;                (define emacs-rex
;;                        (let ((shim emacs-rex))
;;                          (lambda (socket sexp pstring id)
;;                            (shim socket (swank-shim sexp) pstring id))))
;;                (define-message-handler
;;                 '(':emacs-rex form datum datum index)
;;                 (lambda (socket level sexp pstring thread id)
;;                   thread
;;                   (call-with-current-continuation
;;                    (lambda (k)
;;                      (bind-condition-handler
;;                       (list condition-type:serious-condition)
;;                       (lambda (condition)
;;                         (dynamic-wind
;;                          (lambda () (read-from-string "#f"))
;;                          (lambda () (invoke-sldb socket (+ level 1) condition))
;;                          (lambda ()
;;                            (write-message
;;                             `(:return (:abort ,(condition/report-string condition))
;;                                       ,id)
;;                             socket)
;;                            (k unspecific))))
;;                       (lambda ()
;;                         (pp sexp (notification-output-port))
;;                         (let ((result `(:return (:ok ,(emacs-rex socket sexp pstring id)) ,id)))
;;                           (pp result (notification-output-port))
;;                           (newline (notification-output-port))
;;                           (write-message result socket))))))))
;;                (define swank:init-presentations (lambda _ '())))
;;              (->environment '(runtime swank)))
;;        (start-swank ,file))))

;;   (defun mit-scheme-find-buffer-package ()
;;     (save-excursion
;;       (let ((case-fold-search t))
;;         (goto-char (point-min))
;;         (and (re-search-forward "^;+ package: \\(([^)]+)\\)" nil t)
;;              (match-string-no-properties 1)))))

;;   (defun mit-scheme-slime-mode-init ()
;;     (slime-mode t)
;;     (make-local-variable 'slime-find-buffer-package-function)
;;     (setq slime-find-buffer-package-function 'mit-scheme-find-buffer-package))

;;   (if (not (memq 'mit-scheme slime-lisp-implementations))
;;       (setq slime-lisp-implementations
;;             (cons '(mit-scheme ("mit-scheme")
;;                                :init mit-scheme-start-swank)
;;                   slime-lisp-implementations)))
;;   (setq slime-default-lisp 'mit-scheme)
;;   (add-hook 'scheme-mode-hook 'mit-scheme-slime-mode-init))
(use-package sly
  ;; :preface
  ;; TODO: Abstract these better
  ;; (defun sly-describe-function (prefix)
  ;;   (interactive "P")
  ;;   (apply
  ;;    (pcase prefix
  ;;      (4 #'sly-hyperspec-lookup)
  ;;      (16 #'describe-function)
  ;;      (t #'sly-documentation))
  ;;    (let ((fboundp (lambda (x) nil)))
  ;;      (set-fun)
  ;;      (function-called-at-point))))
  ;; (defun sly-describe-variable (prefix)
  ;;   (interactive "P")
  ;;   (apply
  ;;    (pcase prefix
  ;;      (4 #'sly-hyperspec-lookup)
  ;;      (16 #'describe-variable)
  ;;      (t #'sly-documentation))
  ;;    (help-fns--describe-function-or-command-prompt)))

  :config
  (setq inferior-lisp-program "ccl")
  ;; Patch for recent Emacs version
  (el-patch-define-and-eval-template
   (defun sly--completion-transient-mode-display-guard-p)
   (... (el-patch-swap
          buffer-name (buffer-name buffer-name))))
  ;; Muscle memory aids
  (defun slime ()
    (interactive)
    (sly))
  (defun slime-connect* ()
    (interactive)
    (use-package slime
      :defer nil)
    (require 'slime)
    (call-interactively #'slime-connect))
  ;; :bind
  ;; ((:map sly-mode-map
  ;;        ("C-h f" . sly-describe-function)))
  :commands (slime slime-connect*))
;; Elisp
;; (use-package elisp-mode
;;   :straight nil
;;   :config
;;   (defun maybe-byte-compile-file ()
;;     (defvar warning-minimum-level)
;;     (when (eql major-mode 'emacs-lisp-mode)
;;       (let ((warning-minimum-level :error)) ;dynamic bindings are useful
;;         (byte-compile-file (buffer-file-name)))))
;;   (add-hook 'after-save-hook 'maybe-byte-compile-file))
(use-package inspector
  :straight (:host github
                   :repo "mmontone/emacs-inspector"
                   :files (:defaults "inspector.el"))
  :commands inspect-expression)
;; Scheme
(use-package scheme
  :straight nil
  :mode ("\\.scm\\'" . scheme-mode)
  :commands run-scheme
  :config
  (require 'xscheme)
  (require 'info-look)
  (setq scheme-program-name "scheme")
  ;; xscheme is old :(
  ;; no mode hooks
  (advice-add 'scheme-debugger-mode-initialize
              :after #'(lambda () (lispy-mode -1)))
  (advice-add 'scheme-interaction-mode-initialize
              :after #'(lambda () (lispy-mode 1)))
  (info-lookup-maybe-add-help
   :mode 'scheme-mode
   :regexp "[^()`'‘’,\" \t\n]+"
   :ignore-case t
   ;; Aubrey Jaffer's rendition from <https://people.csail.mit.edu/jaffer/SCM>
   :doc-spec '(("(mit-scheme-ref)Binding Index" nil
                "^[ \t]+-+ [^:]+:[ \t]*" "\\b")))
  ;; :hook
  ;; (scheme-mode . (lambda () (font-lock-mode -1)))
  )
(use-package geiser
  :preface
  (defvar geiser-mode-auto-p nil))
(use-package geiser-guile
  :after geiser
  :config
  (add-to-list 'geiser-guile-load-path
               "/run/current-system/profile/share/guile/site/3.0"))
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Maxima
(use-package maxima
  :ensure-system-package (maxima)
  :mode ("\\.mac\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode)
  :hook
  '((maxima-mode-hook . maxima-hook-function)
    (maxima-inferior-mode-hook . maxima-hook-function)))











(defmacro lexical-let (&rest body)
  `(let ((restore lexical-binding))
     (setf lexical-binding t)
     (unwind-protect
         (let ,@body)
       (setf lexical-binding restore))))
(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
               (lambda (&rest more) (apply function (append arguments more)))))
(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append more arguments)))))
(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))
;;; compact display
(defun pretty-curry-compose ()
  (mapc (lambda (pair)
          (let ((regexp (car pair))
                (symbol (cdr pair)))
            (font-lock-add-keywords 'emacs-lisp-mode
              `((,regexp
                 (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                           ,symbol)
                           nil)))))))
        '(("(\\(compose\\)[ \t\n\r]" . ?\∘)
          ("(\\(curry\\)[ \t\n\r]" . ?\»)
          ("(\\(rcurry\\)[ \t\n\r]" . ?\«))))
(add-to-list 'emacs-lisp-mode-hook 'pretty-curry-compose)
;;; color these functions like keywords
(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(compose\\)[ \t\n\r]" 1 font-lock-keyword-face)
                          ("(\\(curry\\)[ \t\n\r]" 1 font-lock-keyword-face)
                          ("(\\(rcurry\\)[ \t\n\r]" 1 font-lock-keyword-face)))
(put 'narrow-to-page 'disabled nil)
(put 'emms-browser-delete-files 'disabled nil)
