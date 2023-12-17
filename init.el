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

(load-theme 'modus-vivendi-tinted)

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

(use-package eglot
  :commands (eglot)
  :init
  (setq eglot-workspace-configuration
   '(:rust-analyzer
     (:checkOnSave ( :command "clippy"
                     :extraArgs ["--" "-Wclippy::pedantic" "-Wclippy::perf"])
      :inlayHints ( :maxLength 120
                    :closureReturnTypeHints (:enable t)
                    :lifetimeElisionHints (:enable t :useParameterNames t)
                    :implicitDrops (:enable t))))))

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

(use-package evil-commentary
  :after evil
  :ensure t
  :hook (text-mode prog-mode))

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
  :hook after-init
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

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

(use-package winum
  :ensure t
  :hook after-init)

(use-package general
  :ensure t
  :after evil
  :init (setq general-use-package-emit-autoloads nil)
  :config

  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override
   :prefix-map 'tyrant-map
   :prefix "SPC"
   :non-normal-prefix "C-SPC")

  (general-create-definer tyrant-def :keymaps 'tyrant-map)
  (tyrant-def "" nil)

  (general-create-definer despot-def
    :states '(normal insert motion emacs)
    :keymaps 'override
    :major-modes t
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")
  (despot-def "" nil)

  (tyrant-def
    "SPC"   '("M-x" . execute-extended-command)
    "TAB"   '("last buffer" . (lambda () (interactive) (switch-to-buffer nil)))
    "!"     '("shell cmd" . shell-command)

    "1"     'winum-select-window-1
    "2"     'winum-select-window-2
    "3"     'winum-select-window-3
    "4"     'winum-select-window-4
    "5"     'winum-select-window-5
    "6"     'winum-select-window-6
    "7"     'winum-select-window-7
    "8"     'winum-select-window-8
    "9"     'winum-select-window-9
    "0"     'winum-select-window-0-or-10

    "f"     (cons "files" (make-sparse-keymap))
    "ff"    'find-file
    "fr"    'consult-recent-file
    "fs"    'save-buffer

    "g"     (cons "git" (make-sparse-keymap))
    "gs"    'magit-status

    "q"     (cons "quit" (make-sparse-keymap))
    "qq"    'save-buffers-kill-terminal
    "qQ"    'save-buffers-kill-emacs

    "s"     (cons "search" (make-sparse-keymap))
    "ss"    'consult-line

    "t"     (cons "toggle" (make-sparse-keymap))
    "ti"    'eglot-inlay-hints-mode

    "w"     (cons "windows" (make-sparse-keymap))
    "w TAB" 'other-window
    "w1"    'delete-other-windows
    "w="    'balance-windows
    "wd"    'delete-window
    "ws"    'split-window-below
    "wv"    'split-window-right))

(use-package which-key
  :ensure t
  :hook after-init)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings))
;;  :config
;;  (defun embark-which-key-indicator ()
;;    "An embark indicator that displays keymaps using which-key.
;;The which-key help message will show the type and value of the
;;current target followed by an ellipsis if there are further
;;targets."
;;    (lambda (&optional keymap targets prefix)
;;      (if (null keymap)
;;          (which-key--hide-popup-ignore-command)
;;        (which-key--show-keymap
;;         (if (eq (plist-get (car targets) :type) 'embark-become)
;;             "Become"
;;           (format "Act on %s '%s'%s"
;;                   (plist-get (car targets) :type)
;;                   (embark--truncate-target (plist-get (car targets) :target))
;;                   (if (cdr targets) "…" "")))
;;         (if prefix
;;             (pcase (lookup-key keymap prefix 'accept-default)
;;               ((and (pred keymapp) km) km)
;;               (_ (key-binding prefix 'accept-default)))
;;           keymap)
;;         nil nil t (lambda (binding)
;;                     (not (string-suffix-p "-argument" (cdr binding))))))))
;;
;;  (setq embark-indicators
;;        '(embark-which-key-indicator
;;          embark-highlight-indicator
;;          embark-isearch-highlight-indicator))
;;
;;  (defun embark-hide-which-key-indicator (fn &rest args)
;;    "Hide the which-key indicator immediately when using the completing-read prompter."
;;    (which-key--hide-popup-ignore-command)
;;    (let ((embark-indicators
;;           (remq #'embark-which-key-indicator embark-indicators)))
;;      (apply fn args)))
;;
;;  (advice-add #'embark-completing-read-prompter
;;              :around #'embark-hide-which-key-indicator)
  )

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))
