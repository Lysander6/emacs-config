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
;; (load-theme 'modus-vivendi-tinted)

;; use seamless vertical window divider
(defun change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table (make-display-table))))
    (set-display-table-slot display-table 5 ?█)
    (set-window-display-table (selected-window) display-table)))

(set-face-attribute 'vertical-border nil :foreground (face-attribute 'line-number :background))

(add-hook 'window-configuration-change-hook 'change-window-divider)

(use-package package
  :config
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize t))

(use-package display-line-numbers
  :hook (text-mode prog-mode)
  :custom
  (display-line-numbers-width 3)
  ;; (display-line-numbers-minor-tick 5)
  ;; (display-line-numbers-major-tick 15)
  :config
  (defun toggle-relative-line-numbers ()
    (interactive)
    (setq display-line-numbers-type
          (if (eq display-line-numbers-type t)
              'relative
            t))
    (display-line-numbers-mode)))

(use-package display-fill-column-indicator
  :custom (display-fill-column-indicator-character ?│)
  :config
  (add-hook 'display-fill-column-indicator-mode-hook
            (lambda () (set-face-attribute 'fill-column-indicator nil :background nil))))

(use-package xt-mouse
  :unless (display-graphic-p)
  :hook (after-init . xterm-mouse-mode))

(use-package hl-line
  ;; :hook (text-mode prog-mode)
  :defer t
  :custom
  (hl-line-sticky-flag nil))

(use-package autorevert
  :defer t
  :diminish
  :custom
  (auto-revert-mode-text "")) ;; diminish seems to not work in this case

(use-package elec-pair
  :hook ((text-mode . electric-pair-mode)
	 (prog-mode . electric-pair-mode)))

(use-package eldoc
  :defer t
  :diminish)

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
  (evil-insert-state-cursor 'bar)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search))

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
  :diminish
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
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
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
  :hook after-init
  :custom (winum-auto-setup-mode-line nil))

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

    "1"     '("window 1" . winum-select-window-1)
    "2"     '("window 2" . winum-select-window-2)
    "3"     '("window 3" . winum-select-window-3)
    "4"     '("window 4" . winum-select-window-4)
    "5"     '("window 5" . winum-select-window-5)
    "6"     '("window 6" . winum-select-window-6)
    "7"     '("window 7" . winum-select-window-7)
    "8"     '("window 8" . winum-select-window-8)
    "9"     '("window 9" . winum-select-window-9)
    "0"     '("window 0 or 10" . winum-select-window-0-or-10)

    "b"     (cons "buffers" (make-sparse-keymap))
    "bb"    'consult-buffer
    "bd"    'kill-current-buffer
    "bs"    'scratch-buffer

    "f"     (cons "files" (make-sparse-keymap))
    "ff"    'find-file
    "fr"    'consult-recent-file
    "fs"    'save-buffer

    "g"     (cons "git" (make-sparse-keymap))
    "gs"    'magit-status

    "p"     (cons "project" (make-sparse-keymap))
    "pf"    'project-find-file
    "pt"    'treemacs

    "q"     (cons "quit" (make-sparse-keymap))
    "qq"    'save-buffers-kill-terminal
    "qQ"    'save-buffers-kill-emacs

    "s"     (cons "search" (make-sparse-keymap))
    "sc"    '("clear highlight" . evil-ex-nohighlight)
    "ss"    'consult-line

    "t"     (cons "toggle" (make-sparse-keymap))
    "tf"    'display-fill-column-indicator-mode
    "ti"    'eglot-inlay-hints-mode
    "tl"    'hl-line-mode
    "tr"    'toggle-relative-line-numbers

    "w"     (cons "windows" (make-sparse-keymap))
    "ww"    'other-window
    "w1"    'delete-other-windows
    "wo"    'delete-other-windows
    "w="    'balance-windows
    "wd"    'delete-window
    "ws"    'split-window-below
    "wv"    'split-window-right
    "wr"    'evil-window-rotate-upwards
    "wj"    'evil-window-down
    "wk"    'evil-window-up
    "wh"    'evil-window-left
    "wl"    'evil-window-right
    "wu"    'winner-undo))

(use-package which-key
  :ensure t
  :diminish
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

(use-package copilot
  :ensure t
  :vc ( :url "https://github.com/zerolfx/copilot.el"
        :rev :newest)
  :bind
  (:map copilot-completion-map
        ("C-y" . copilot-accept-completion)))

(use-package git-gutter
  :ensure t
  :diminish
  :hook (text-mode prog-mode)
  :custom
  (git-gutter:unchanged-sign " ")
  (git-gutter:added-sign "┃")
  (git-gutter:modified-sign "┃")
  (git-gutter:deleted-sign "▁")
  :config

  (set-face-attribute 'line-number nil
                      :background (face-attribute 'default :background)
                      :foreground "#404040")

  (set-face-attribute 'line-number-current-line nil
                      :background (face-attribute 'default :background))

  (set-face-attribute 'git-gutter:unchanged nil
                      :background (face-attribute 'line-number :background))

  (set-face-attribute 'git-gutter:added nil
                      :background (face-attribute 'line-number :background)
                      :foreground (face-attribute 'modus-themes-fg-green-faint :foreground))

  (set-face-attribute 'git-gutter:modified nil
                      :background (face-attribute 'line-number :background)
                      :foreground (face-attribute 'modus-themes-fg-yellow-faint :foreground))

  (set-face-attribute 'git-gutter:deleted nil
                      :background (face-attribute 'line-number :background)
                      :foreground (face-attribute 'modus-themes-fg-red-faint :foreground)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :config
  (treemacs-hide-gitignored-files-mode nil))

(use-package treemacs-evil
  :ensure t
  :after (evil treemacs))

(use-package nerd-icons
  :ensure t
  :defer t)

(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs)
  :config
  (treemacs-load-theme "nerd-icons")

  (set-face-attribute 'treemacs-nerd-icons-file-face nil
                      :foreground (face-attribute 'modus-themes-fg-yellow-faint :foreground))
  (set-face-attribute 'treemacs-directory-face nil
                      :foreground (face-attribute 'default :foreground))
  (set-face-attribute 'treemacs-directory-collapsed-face nil
                      :foreground (face-attribute 'default :foreground))
  (set-face-attribute 'treemacs-git-modified-face nil
                      :foreground (face-attribute 'modus-themes-fg-yellow-warmer :foreground))
  (set-face-attribute 'treemacs-git-added-face nil
                      :foreground (face-attribute 'modus-themes-fg-green-faint :foreground)))

(use-package nerd-icons-corfu
  :ensure t
  :defer t
  :after (nerd-icons))

(use-package spaceline
  :ensure t
  :hook (after-init . spaceline-spacemacs-theme)
  :config
  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)

  (set-face-attribute 'spaceline-evil-normal nil
                      :background (face-attribute 'modus-themes-fg-blue-faint :foreground))
  (set-face-attribute 'spaceline-evil-insert nil
                      :background (face-attribute 'modus-themes-fg-green-faint :foreground))
  (set-face-attribute 'spaceline-evil-visual nil
                      :background (face-attribute 'modus-themes-fg-magenta-cooler :foreground)))

(use-package anzu
  :ensure t
  :diminish
  :hook (text-mode prog-mode)
  :custom (anzu-cons-mode-line-p nil))

(use-package evil-anzu
  :ensure t
  :after (evil anzu))

(use-package dimmer
  :ensure t
  :hook after-init
  :custom
  (dimmer-use-colorspace :rgb)
  :config
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-which-key)
  (add-to-list
   'dimmer-exclusion-regexp-list "^\\*Help\\*$"))
