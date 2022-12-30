;; -*- coding: utf-8; no-byte-compile: t -*-

(require 'use-package)

; (package-initialize t)


; `M-x eval-buffer` or `M-x load-file` to reload this file

; make search case insensitive
(setq-default
 case-fold-search nil
 create-lockfiles nil)

; enable mouse support in terminal
(use-package xt-mouse
  :config (xterm-mouse-mode))

; enable window change undo/redo
(use-package winner
  :config (winner-mode))

; enable spell checking (`M-$` to correct a word)
(use-package flyspell
  :defer t
  :custom
  (flyspell-default-dictionary "en_GB")
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode)))

(message "Hello, World!")
