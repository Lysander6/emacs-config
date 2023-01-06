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

;; make search and filters case insensitive
(setq-default case-fold-search t)
(setq-default completion-ignore-case t)
(setq-default read-file-name-completion-ignore-case t)
(setq-default read-buffer-completion-ignore-case t)

;; do not create lock files
(setq-default create-lockfiles nil)

;; do not create backup files
(setq-default make-backup-files nil)

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

(setq-default tab-always-indent 'complete)
(setq-default completion-cycle-threshold 3)
(setq-default enable-recursive-minibuffers t)

;; map custom file types to modes
(add-to-list 'auto-mode-alist '("\\.cdt$" . typescript-ts-mode))

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

;; adjust vertical divider and line numbers colours
(set-face-foreground 'vertical-border "#000000")
;; (set-face-background 'line-number "#000000")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize t)

;; use MELPA archive (repository) as default
(setq-default use-package-always-pin "melpa")
;; call `package-autoremove` to remove unused packages
(require 'use-package)

;; enable mouse support in terminal
(use-package xt-mouse
  :hook (after-init . xterm-mouse-mode))

;; enable window change undo/redo
(use-package winner
  :hook after-init)

(use-package recentf
  :hook after-init)

;; movement between windows with direction keys
(use-package windmove
  :hook after-init)

;; enable spell checking
(use-package flyspell
  :custom
  (flyspell-default-dictionary "en_GB")
  (flyspell-issue-welcome-flag nil)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :hook after-init
  :config
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode 1))

(use-package magit
  :ensure t
  :commands (magit-status))

(use-package spaceline
  :ensure t
  :hook (after-init . spaceline-spacemacs-theme)
  :config
  ;; for some reason does not work when set in `:custom`
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

(use-package winum
  :ensure t
  :hook after-init)

(use-package editorconfig
  :ensure t
  :hook (text-mode prog-mode))

(use-package vertico
  :ensure t
  :pin gnu ;; not available on MELPA
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package consult
  :ensure t
  :commands
  (consult-buffer
   consult-line
   consult-ripgrep
   consult-flymake)
  :custom (consult-narrow-key "<")
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package consult-project-extra
  :ensure t
  :commands
  (consult-project-extra-find
   consult-project-extra-find-other-window))

(use-package which-key
  :ensure t
  :hook after-init)

(use-package corfu
  :ensure t
  :pin gnu ;; not available on MELPA
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-echo-delay t) ;; no delay
  (corfu-cycle t)
  (corfu-preselect-first nil)
  :hook ((text-mode . corfu-mode)
         (prog-mode . corfu-mode)
         (text-mode . corfu-echo-mode)
         (prog-mode . corfu-echo-mode))
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package corfu-terminal
  :ensure t
  :pin nongnu
  :hook (text-mode prog-mode))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-global-mode))

;; (add-to-list 'eglot-server-programs '(html-mode "jqtpl-language-server"))

(use-package lsp-mode
  :ensure t
  :init (setq-default lsp-keymap-prefix "<leader>l")
  :commands lsp
  :hook ((js-ts-mode . lsp)
         (rust-ts-mode . lsp)
         (lsp . lsp-enable-which-key-integration)))

(use-package general
  :ensure t
  :after evil
  :config
  (setq general-use-package-emit-autoloads nil)

  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override

   "M-<up>"     '("other window up" . (lambda () (interactive) (scroll-other-window -1)))
   "M-<down>"   '("other window down" . (lambda () (interactive) (scroll-other-window 1))))

  ;; heavily inspired by https://github.com/tshu-w/.emacs.d/blob/master/lisp/core-keybinds.el

  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override
   :prefix-map 'tyrant-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer tyrant-def :keymaps 'tyrant-map)
  (tyrant-def "" nil)

  (general-create-definer despot-def
    :states '(normal insert motion emacs)
    :keymaps 'override
    :major-modes t
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  (despot-def "" nil)

  (general-def universal-argument-map
    "SPC u" 'universal-argument-more)

  (tyrant-def
    "SPC"        '("M-x" . execute-extended-command)
    "TAB"        '("last buffer" . (lambda () (interactive) (switch-to-buffer nil)))
    "!"          '("shell cmd" . shell-command)

    "1"          'winum-select-window-1
    "2"          'winum-select-window-2
    "3"          'winum-select-window-3
    "4"          'winum-select-window-4
    "5"          'winum-select-window-5
    "6"          'winum-select-window-6
    "7"          'winum-select-window-7
    "8"          'winum-select-window-8
    "9"          'winum-select-window-9
    "0"          'winum-select-window-0-or-10

    "; ;"        'comment-line

    "b"          (cons "buffers" (make-sparse-keymap))
    "bb"         'consult-buffer
    "bB"         'ibuffer
    "bd"         'kill-current-buffer
    "bs"         'scratch-buffer

    "c"          (cons "code" (make-sparse-keymap))
    "cc"         'compile
    "cn"         'next-error
    "cp"         'previous-error
    "cx"         'kill-compilation

    "e"          (cons "errors" (make-sparse-keymap))
    "el"         'consult-flymake

    "f"          (cons "files" (make-sparse-keymap))
    "ff"         'find-file
    "fr"         'consult-recent-file
    "fs"         'save-buffer

    "g"          (cons "git" (make-sparse-keymap))
    "gs"         'magit-status

    "h"          (cons "help" (make-sparse-keymap))
    "ha"         'apropos
    "hb"         'describe-bindings
    "hc"         'describe-char
    "hf"         'describe-function
    "hF"         'describe-face
    "hi"         'info-emacs-manual
    "hI"         'info-display-manual
    "hk"         'describe-key
    "hK"         'describe-keymap
    "hm"         'describe-mode
    "hM"         'woman
    "hp"         'describe-package
    "ht"         'describe-text-properties
    "hv"         'describe-variable

    "m"          (cons "major mode" (make-sparse-keymap))

    "p"          (cons "project" (make-sparse-keymap))
    "pf"         'consult-project-extra-find
    "pF"         'consult-project-extra-find-other-window

    "q"          (cons "quit" (make-sparse-keymap))
    "qq"         'save-buffers-kill-terminal
    "qQ"         'save-buffers-kill-emacs

    "s"          (cons "search" (make-sparse-keymap))
    "ss"         'consult-line
    "sp"         'consult-ripgrep

    "T"          (cons "toggles" (make-sparse-keymap))
    "Tw"         'whitespace-mode

    "u"          '("universal arg" . universal-argument)

    "w"          (cons "windows" (make-sparse-keymap))
    "w TAB"      'other-window
    "w1"         'delete-other-windows
    "w="         'balance-windows
    "wU"         'winner-redo
    "wd"         'delete-window
    "wh"         'windmove-left
    "wj"         'windmove-down
    "wk"         'windmove-up
    "wl"         'windmove-right
    "wm"         'delete-other-windows
    "ws"         'split-window-below
    "wu"         'winner-undo
    "wv"         'split-window-right
    "ww"         'winum-select-window-by-number))
