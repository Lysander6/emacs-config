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

(setq-default tab-always-indent 'complete)
(setq-default completion-cycle-threshold 3)
(setq-default enable-recursive-minibuffers t)

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

; use MELPA archive (repository) as default
(setq-default use-package-always-pin "melpa")
; call `package-autoremove` to remove unused packages
(require 'use-package)

; enable mouse support in terminal
(use-package xt-mouse
  :hook (after-init . xterm-mouse-mode))

; enable window change undo/redo
(use-package winner
  :hook (after-init . winner-mode)
  :bind
  (("<leader>wu" . winner-undo)
   ("<leader>wU" . winner-redo)))

(use-package recentf
  :hook (after-init . recentf-mode))

; movement between windows with direction keys
(use-package windmove
  :hook (after-init . windmove-mode)
  :bind
  (("<leader>wk" . windmove-up)
   ("<leader>wj" . windmove-down)
   ("<leader>wh" . windmove-left)
   ("<leader>wl" . windmove-right)))

; enable spell checking (`M-$` to correct a word)
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
  :config
  (evil-mode 1)

  ;; set leader and local leader
  (evil-set-leader nil (kbd "M-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'normal (kbd ",") t)

  (evil-define-key 'normal 'global
    ;; buffers
    (kbd "<leader>bd") '(lambda () (interactive) (kill-buffer nil))
    (kbd "<leader>TAB") '(lambda () (interactive) (switch-to-buffer nil))
    ;;(kbd "<leader>bb") 'switch-to-buffer

    ;; commands
    (kbd "<leader>:") 'execute-extended-command

    ;; commenting
    (kbd "<leader>; ;") 'comment-line

    ;; files
    (kbd "<leader>fs") 'save-buffer
    (kbd "<leader>ff") 'find-file

    ;; windows
    (kbd "<leader>wd") 'delete-window
    (kbd "<leader>wm") 'delete-other-windows
    (kbd "<leader>w1") 'delete-other-windows
    (kbd "<leader>ws") 'split-window-below
    (kbd "<leader>wv") 'split-window-right
    (kbd "<leader>w=") 'balance-windows

    ;; other windows
    (kbd "M-<up>") '(lambda () (interactive) (scroll-other-window -1))
    (kbd "M-<down>") '(lambda () (interactive) (scroll-other-window 1))

    ;; quitting
    (kbd "<leader>qq") 'save-buffers-kill-terminal)

  (evil-define-key 'visual 'global
    ;; commenting
    (kbd "; ;") 'comment-or-uncomment-region))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package magit
  :ensure t
  :bind (("<leader>gs" . magit-status)))

(use-package spaceline
  :ensure t
  :hook (after-init . spaceline-spacemacs-theme)
  :config
  ;; for some reason does not work when set in `:custom`
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

(use-package winum
  :ensure t
  :bind
  (("<leader>1" . winum-select-window-1)
   ("<leader>2" . winum-select-window-2)
   ("<leader>3" . winum-select-window-3)
   ("<leader>4" . winum-select-window-4)
   ("<leader>5" . winum-select-window-5)
   ("<leader>6" . winum-select-window-6)
   ("<leader>7" . winum-select-window-7)
   ("<leader>8" . winum-select-window-8)
   ("<leader>9" . winum-select-window-9)
   ("<leader>0" . winum-select-window-0-or-10)
   ("<leader>ww" . winum-select-window-by-number))
  :hook (after-init . winum-mode))

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
  :bind
  (("<leader>bb" . consult-buffer)
   ("<leader>fr" . consult-recent-file)
   ("<leader>ss" . consult-line)
   ("<leader>sp" . consult-ripgrep)
   ("<leader>el" . consult-flymake))
  :custom (consult-narrow-key "<")
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package consult-project-extra
  :ensure t
  :bind
  (("<leader>pf" . consult-project-extra-find)
   ("<leader>pF" . consult-project-extra-find-other-window)))

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
