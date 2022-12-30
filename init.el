;; -*- coding: utf-8; no-byte-compile: t -*-

; `M-x eval-buffer` or `M-x load-file` to reload this file

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq-default
 ; make search case insensitive
 case-fold-search nil
 create-lockfiles nil)

(load-theme 'modus-vivendi)

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
