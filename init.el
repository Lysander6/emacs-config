;; -*- coding: utf-8; no-byte-compile: t; lexical-binding: t; -*-

;; temporarily "disable" garbage collection to speed-up startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)
(add-hook 'emacs-startup-hook
          (defun set-proper-gc-values ()
            (setq gc-cons-threshold (* 100 1000 1000))
            (setq gc-cons-percentage 0.1)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq-default
 inhibit-startup-screen t

 ;; make search and filters case insensitive
 case-fold-search t
 completion-ignore-case t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t

 ;; use y/n instead if yes/no
 use-short-answers t

 ;; clean scratch buffer
 initial-scratch-message ""

 ;; do not create lock and backup files
 create-lockfiles nil
 make-backup-files nil

 ;; smoother scrolling
 scroll-margin 4
 scroll-step 1

 ;; faster scrolling with mouse wheel
 mouse-wheel-scroll-amount '(3
			     ((shift) . hscroll)
			     ((meta))
			     ((control meta) . global-text-scale)
			     ((control) . text-scale))

 visible-bell t

 ;; some completion settings
 tab-always-indent 'complete
 completion-cycle-threshold 3

 enable-recursive-minibuffers t

 ;; improve lsp-mode performance
 read-process-output-max (* 1024 1024)

 use-package-always-pin "melpa"

 ;; disable cursor blinking in terminal
 visible-cursor nil

 ;; better `:' completions
 completion-in-region-function #'consult-completion-in-region)

;; disable menu and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

(load-theme 'modus-vivendi)

;; enable line numbers in prog and text modes
(defun display-line-numbers-hook ()
  (setq display-line-numbers t))

(add-hook 'prog-mode-hook 'display-line-numbers-hook)
(add-hook 'text-mode-hook 'display-line-numbers-hook)

;; use seamless vertical window divider
(defun change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table (make-display-table))))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

(add-hook 'window-configuration-change-hook 'change-window-divider)

(use-package package
  :config
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize t))

(use-package xt-mouse
  :unless (display-graphic-p)
  :hook (after-init . xterm-mouse-mode))

(use-package hl-line
  :hook (text-mode prog-mode)
  :custom
  (hl-line-sticky-flag nil))

(use-package elec-pair
  :hook ((text-mode . electric-pair-mode)
	 (prog-mode . electric-pair-mode)))

(use-package recentf
  :hook (text-mode prog-mode))

;; window configuration undo/redo
(use-package winner
  :hook after-init)

(use-package windmove
  :hook after-init
  :custom (windmove-wrap-around t)
  :config (windmove-default-keybindings))

(use-package treesit
  :config
  (setq treesit-language-source-alist
        '((rust "https://github.com/tree-sitter/tree-sitter-rust")))

  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

  (if (not (treesit-language-available-p 'rust))
      (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))))

(use-package flyspell
  :custom
  (flyspell-default-dictionary "en_GB")
  (flyspell-issue-welcome-flag nil)
  :diminish
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package diminish
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :hook after-init
  :custom
  (evil-undo-system 'undo-redo)
  (evil-insert-state-cursor 'bar))

(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-want-unimpaired-p nil)
  :config (evil-collection-init))

(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :after evil
  :ensure t
  :config (evil-terminal-cursor-changer-activate))

(use-package editorconfig
  :ensure t
  :diminish
  :hook (text-mode prog-mode))

(use-package magit
  :ensure t
  :commands (magit magit-status))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure t
  :pin gnu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-echo-delay 0.1)
  ;; NOTE: `corfu-popupinfo-mode' overrides `corfu-info-documentation'
  ;; & `corfu-info-location' helpers and breaks them in terminal mode
  :hook ((text-mode . corfu-mode)
         (prog-mode . corfu-mode)
         (text-mode . corfu-echo-mode)
         (prog-mode . corfu-echo-mode))
  :bind
  ;; "M-h" - show docs for current candidate (prefix with "C-u" for
  ;; persistent window)
  ;;
  ;; "M-g" - go to candidate source
  (:map corfu-map
        ;; NOTE: use "M-SPC" to `corfu-insert-separator' - even though
        ;; first press will get intercepted by Windows™ any following
        ;; key press (like "M-SPC" for the second time) will make it
        ;; go through to the terminal application
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :pin nongnu
  :hook (text-mode prog-mode))

(use-package vertico
  :ensure t
  :pin gnu
  :hook after-init)

(use-package marginalia
  :ensure t
  :pin gnu
  :hook after-init
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package consult
  :ensure t
  :pin gnu
  :defer t
  :custom (consult-narrow-key "<")
  :hook (completion-list-mode . consult-preview-at-point-mode))

;;(defun corfu-enable-in-minibuffer ()
;;  "Enable Corfu in the minibuffer."
;;  (when (local-variable-p 'completion-at-point-functions)
;;    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
;;    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
;;                corfu-popupinfo-delay nil)
;;    (corfu-mode 1)))
;;
;;(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

;;(setq completion-in-region-function
;;      (lambda (&rest args)
;;        (apply (if vertico-mode
;;                   #'consult-completion-in-region
;;                 #'completion--in-region)
;;               args)))
