;; -*- coding: utf-8; no-byte-compile: t; lexical-binding: t; -*-

; `M-x eval-buffer` or `M-x load-file` to reload this file

; temporarily "disable" garbage collection to speed-up startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)
(add-hook 'emacs-startup-hook
	  (defun set-proper-gc-values ()
	    (setq gc-cons-threshold 100000000)
	    (setq gc-cons-percentage 0.1)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

; make search case insensitive
(setq-default case-fold-search t)

; do not create lock files
(setq-default create-lockfiles nil)

; use y/n in place of yes/no
(setq-default use-short-answers t)

; always start with empty scratch buffer
(setq-default initial-scratch-message "")

; disable menu and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

; smoother scrolling
(setq-default scroll-margin 4)
(setq-default scroll-step 1)

(load-theme 'modus-vivendi)

; enable line numbers in prog and text modes
(defun display-line-numbers-hook ()
  (setq display-line-numbers t))

(add-hook 'prog-mode-hook 'display-line-numbers-hook)
(add-hook 'text-mode-hook 'display-line-numbers-hook)

; use seamless vertical window divider
(defun change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table (make-display-table))))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

(add-hook 'window-configuration-change-hook 'change-window-divider)

; adjust vertical divider and line numbers colours
(set-face-foreground 'vertical-border (face-background 'line-number))
(set-face-background 'line-number "#000000")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize t)

; call `package-autoremove` to remove unused packages
(require 'use-package)

; enable mouse support in terminal
(use-package xt-mouse
  :hook (after-init . xterm-mouse-mode))

; enable window change undo/redo
(use-package winner
  :after evil
  :hook (after-init . winner-mode)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>wu") 'winner-undo)
  (evil-define-key 'normal 'global (kbd "<leader>wU") 'winner-redo))

; movement between windows with direction keys
(use-package windmove
  :after evil
  :hook (after-init . windmove-mode)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'windmove-up)
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'windmove-down)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'windmove-left)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'windmove-right))

; enable spell checking (`M-$` to correct a word)
(use-package flyspell
  :custom
  (flyspell-default-dictionary "en_GB")
  (flyspell-issue-welcome-flag nil)
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode)))

(use-package evil
  :ensure t
  :pin "melpa"
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-leader nil (kbd "M-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'normal (kbd ",") t)

  ; buffers
  (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)

  ; windows
  (evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>wm") 'delete-other-windows)
  (evil-define-key 'normal 'global (kbd "<leader>w1") 'delete-other-windows)
  (evil-define-key 'normal 'global (kbd "<leader>ws") 'split-window-below)
  (evil-define-key 'normal 'global (kbd "<leader>wv") 'split-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>w=") 'balance-windows)

  ; quitting
  (evil-define-key 'normal 'global (kbd "<leader>qq") 'save-buffers-kill-terminal))

(use-package evil-collection
  :after evil
  :ensure t
  :pin "melpa"
  :config (evil-collection-init))

(use-package magit
  :after evil
  ;:defer t
  :ensure t
  :pin "melpa"
  :bind (("<leader>gs" . magit-status)))

(use-package spaceline
  :ensure t
  :pin "melpa"
  :hook (after-init . spaceline-spacemacs-theme))

(use-package winum
  :after evil
  :ensure t
  :pin "melpa"
  :config
  ; bindings go here (instead of `:keymap` or `:bind` because plugin
  ; must be active anyway to give windows their numbers
  (evil-define-key 'normal 'global (kbd "<leader>1") 'winum-select-window-1)
  (evil-define-key 'normal 'global (kbd "<leader>2") 'winum-select-window-2)
  (evil-define-key 'normal 'global (kbd "<leader>3") 'winum-select-window-3)
  (evil-define-key 'normal 'global (kbd "<leader>4") 'winum-select-window-4)
  (evil-define-key 'normal 'global (kbd "<leader>5") 'winum-select-window-5)
  (evil-define-key 'normal 'global (kbd "<leader>6") 'winum-select-window-6)
  (evil-define-key 'normal 'global (kbd "<leader>7") 'winum-select-window-7)
  (evil-define-key 'normal 'global (kbd "<leader>8") 'winum-select-window-8)
  (evil-define-key 'normal 'global (kbd "<leader>9") 'winum-select-window-9)
  (evil-define-key 'normal 'global (kbd "<leader>0") 'winum-select-window-0-or-10)
  (evil-define-key 'normal 'global (kbd "<leader>ww") 'winum-select-window-by-number)
  :hook (after-init . winum-mode))
