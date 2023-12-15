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

 use-package-always-pin "melpa")

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
                           ("elpa" . "https://elpa.gnu.org/packages/")))
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

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :hook after-init
  :custom
  (evil-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :ensure t
  :custom (evil-collection-want-unimpaired-p nil)
  :config (evil-collection-init))
