;; -*- coding: utf-8; no-byte-compile: t -*-

; `M-x eval-buffer` or `M-x load-file` to reload this file

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

; make search case insensitive
(setq-default case-fold-search t)

; do not create lock files
(setq-default create-lockfiles nil)

; use y/n in place of yes/no
(setq-default use-short-answers t)

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
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

(add-hook 'window-configuration-change-hook 'change-window-divider)

; adjust vertical divider and line numbers colours
(set-face-foreground 'vertical-border (face-background 'line-number))
(set-face-background 'line-number "#000000")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize t)

(require 'use-package)

; enable mouse support in terminal
(use-package xt-mouse
  :config (xterm-mouse-mode))

; enable window change undo/redo
(use-package winner
  :config (winner-mode))

; enable spell checking (`M-$` to correct a word)
(use-package flyspell
  :defer t
  :custom (flyspell-default-dictionary "en_GB")
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode)))

(use-package evil
  :ensure t
  :pin "melpa"
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :pin "melpa"
  :config (evil-collection-init))

(use-package magit
  :defer t
  :ensure t
  :pin "melpa")
