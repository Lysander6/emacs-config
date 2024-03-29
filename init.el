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
 completion-in-region-function #'consult-completion-in-region

 mode-line-front-space " "
 mode-line-end-spaces "")

;; disable menu and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; fonts
(set-face-attribute 'default nil
                    :family "IosevkaTerm Nerd Font"
                    :weight 'medium
                    :height 120)

(load-theme 'modus-vivendi)
;; (load-theme 'modus-vivendi-tinted)

;; use seamless vertical window divider
(defun change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table (make-display-table))))
    (set-display-table-slot display-table 5 ?█)
    (set-window-display-table (selected-window) display-table)))
(unless (display-graphic-p)
  (set-face-attribute 'vertical-border nil :foreground (face-attribute 'line-number :background))
  (add-hook 'window-configuration-change-hook 'change-window-divider))

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
  (column-number-mode t)
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
  :unless (display-graphic-p)
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
  :diminish 'auto-revert-mode)

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
        '((rust "https://github.com/tree-sitter/tree-sitter-rust")
          (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
          (astro "https://github.com/virchau13/tree-sitter-astro")
          (css "https://github.com/tree-sitter/tree-sitter-css")))

  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))

  (mapc #'treesit-install-language-grammar
        (seq-remove #'treesit-language-available-p
                    (mapcar #'car
                            treesit-language-source-alist))))

(use-package flyspell
  :custom
  (flyspell-default-dictionary "en_GB")
  (flyspell-issue-welcome-flag nil)
  :diminish
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package eglot
  :commands (eglot)
  :custom
  ;; Effectively disabling eglot's log buffer greatly improves
  ;; performance when working with Godot's LSP server. It might have
  ;; similar effect when using other LSP servers
  (eglot-events-buffer-size 0)
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

(use-package evil-surround
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
  (completion-styles '(orderless flex basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure t
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
  :hook after-init
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

(use-package marginalia
  :ensure t
  :hook after-init
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package consult
  :ensure t
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
  :custom (winum-auto-setup-mode-line t))

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
    "wu"    'winner-undo)

  (general-define-key
   :states '(normal)
   :keymaps 'eglot-mode-map

   ",=="    'eglot-format-buffer
   ",rr"    'eglot-rename
   ",aa"    'eglot-code-actions))

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
  (git-gutter:unchanged-sign "")
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
  :after (treemacs evil))

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

;; (use-package spaceline
;;   :ensure t
;;   :hook (after-init . spaceline-spacemacs-theme)
;;   :config
;;   (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)

;;   (set-face-attribute 'spaceline-evil-normal nil
;;                       :background (face-attribute 'modus-themes-fg-blue-faint :foreground))
;;   (set-face-attribute 'spaceline-evil-insert nil
;;                       :background (face-attribute 'modus-themes-fg-green-faint :foreground))
;;   (set-face-attribute 'spaceline-evil-visual nil
;;                       :background (face-attribute 'modus-themes-fg-magenta-cooler :foreground)))

;; TODO: create spaceline segment with branch name and text icon like in nvim
;; https://github.com/TheBB/spaceline/blob/master/spaceline-segments.el#L127-L142

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

  (mapc (lambda (regexp)
          (add-to-list 'dimmer-buffer-exclusion-regexps regexp))
        '("^\\*Help\\*$"
          "^\\*corfu doc"
          "^\\*eldoc"
          "^ \\*eglot doc\\*$"
          "^\\*Minibuf-[0-9]+\\*")))

(use-package avy
  :ensure t
  :after (evil)
  :custom (avy-style 'post)
  :bind (:map evil-normal-state-map
              ("s" . avy-goto-char-2-below)
              ("S" . avy-goto-char-2-above)))

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :ensure t
  :after (yasnippet-snippets)
  :hook ((text-mode . yas-minor-mode)
         (prog-mode . yas-minor-mode))
  :config (yas-reload-all))

(use-package yasnippet-capf
  :ensure t
  :after (yasnippet))

(use-package cape
  :ensure t
  :after (yasnippet-capf)
  :config

  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf))))

  ;; FIXME: now some (non-yas-snippet) completions are too eagerly
  ;; automatically expanded - line word `new' in rust files

  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))

(use-package gdscript-mode
  :ensure t
  :defer t
  :hook (gdscript-ts-mode . eglot-ensure)
  :init
  (add-to-list 'auto-mode-alist '("\\.gd\\'" . gdscript-ts-mode)))

(use-package flymake-languagetool
  :ensure t
  ;; get your languagetool server from: https://languagetool.org/download/
  :custom
  (flymake-languagetool-server-jar "/home/lysander/LanguageTool-20231220-snapshot/LanguageTool-6.4-SNAPSHOT/languagetool-server.jar")
  ;; (flymake-languagetool-server-jar "/home/lysander/LibreGrammar-5.1/LibreGrammar-5.1/languagetool-server.jar")
  (flymake-languagetool-language "en-GB")
  :hook ((text-mode . flymake-languagetool-load)
         (markdown-mode . flymake-languagetool-load)
         (org-mode . flymake-languagetool-load)))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package astro-ts-mode
  :ensure t
  :defer t)
