;; -*- coding: utf-8; no-byte-compile: t; lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(load-theme 'modus-vivendi-tinted)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; must be explicitly set as below to work
(setq inhibit-startup-echo-area-message "lysander")

(setopt
 create-lockfiles nil

 enable-recursive-minibuffers t

 inhibit-startup-screen t
 initial-scratch-message ""

 make-backup-files nil

 ;; remove '-' characters from the mode-line
 mode-line-front-space " "
 mode-line-end-spaces ""

 ;; faster scrolling with a mouse wheel
 mouse-wheel-scroll-amount '(3
			     ((shift) . hscroll)
			     ((meta))
			     ((control meta) . global-text-scale)
			     ((control) . text-scale))

 read-extended-command-predicate #'command-completion-default-include-p

 ;; smoother scrolling
 scroll-margin 4
 scroll-step 1

 tab-always-indent 'complete

 use-short-answers t

 visible-bell t

 ;; disable cursor blinking in terminal
 visible-cursor nil)

(defun my/select-window-and-enable-insert-state (window)
  (select-window window)
  (evil-insert-state))

(setq display-buffer-alist ;; setopt
      '(((derived-mode . occur-mode)
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (window-height . 0.25)
         (body-function . select-window)
         (side . bottom))
        ("eshell\\*"
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (window-height . 0.25)
         (body-function . my/select-window-and-enable-insert-state)
         (side . bottom))
        ((derived-mode . magit-mode)
         (display-buffer-reuse-mode-window ;; display-buffer-in-previous-window
          display-buffer-use-some-window)
         (inhibit-same-window . t)
         (window-min-height . 20)
         (window-min-width . 80))
        ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
                (derived-mode . flymake-project-diagnostics-mode)))
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (mode . '(flymake-diagnostics-buffer-mode flymake-project-diagnostics-mode))
         (body-function . select-window)
         (window-height . 0.25)
         (side . bottom))))

;; use seamless vertical window divider
(defun change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table (make-display-table))))
    (set-display-table-slot display-table 'vertical-border ?█)
    ;; (set-window-display-table (selected-window) display-table)
    ))

(unless (display-graphic-p)
  (set-face-attribute 'vertical-border nil :foreground (face-attribute 'line-number :background))
  (add-hook 'window-configuration-change-hook 'change-window-divider))

(use-package package
  :config
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/"))
  (package-initialize t))

(use-package avy
  :ensure t
  :pin melpa
  :after (evil)
  :custom (avy-style 'post)
  :bind (:map evil-normal-state-map
              ("gs" . avy-goto-char-2-below)
              ("gS" . avy-goto-char-2-above)))

(use-package consult
  :ensure t
  :pin melpa
  :defer t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (consult-narrow-key "<")
  (consult-preview-partial-size 0 "Do not enable major modes in previews."))

(use-package consult-gh
  :if (executable-find "gh")
  :after consult
  :ensure t
  :pin melpa
  :defer t
  :custom
  (consult-gh-default-clone-directory "~/workspace/")
  (consult-gh-favorite-orgs-list '("Ringier-Axel-Springer-PL"))
  ;; (consult-gh-repo-maxnum 1000) ;; append something like ` -- -L 1000' to the search query, instead of changing the default value
  )

(use-package consult-gh-embark
  :after consult-gh
  :ensure t
  :pin melpa
  :config
  (consult-gh-embark-mode +1))

(use-package consult-gh-forge
  :after consult-gh
  :ensure t
  :pin melpa
  :config
  (consult-gh-forge-mode +1))

(defun my/consult-ripgrep (&optional dir given-initial)
  (interactive "P")
  (let ((initial
         (or given-initial
             (when (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))))))
    (consult-ripgrep dir initial)))

(use-package display-fill-column-indicator
  :unless (display-graphic-p)
  :defer t
  :custom
  (display-fill-column-indicator-character ?│)
  :custom-face
  (fill-column-indicator ((t (:background unspecified))))
  ;; :hook prog-mode
  )

(use-package editorconfig
  :hook (text-mode prog-mode))

(use-package eglot
  :hook (rust-ts-mode . eglot-ensure)
  :custom
  (eglot-events-buffer-config '(:size 0))
  :init
  (setopt eglot-workspace-configuration
   '(:rust-analyzer
     (
      ;; :checkOnSave ( :command "clippy"
      ;;                :extraArgs ["--" "-Wclippy::pedantic" "-Wclippy::perf"])
      :inlayHints ( :maxLength 120
                    :closureReturnTypeHints (:enable t)
                    :lifetimeElisionHints (:enable t :useParameterNames t)
                    :implicitDrops (:enable t))))))

(use-package elec-pair
  :hook ((text-mode . electric-pair-mode)
	 (prog-mode . electric-pair-mode)))

(use-package embark
  :ensure t
  :pin melpa
  :defer t
  :bind
  (:map minibuffer-local-map ;; :map vertico-map
        ("M-o" . embark-act))
  :custom
  (embark-indicators '(embark-minimal-indicator
                       ;; do not pop up buffer with all actions -
                       ;; after M-o use C-h to get a vertico
                       ;; completion minibuffer
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  ;; (embark-prompter #'embark-completing-read-prompter)
  )

(use-package embark-consult
  :ensure t
  :pin melpa
  :defer t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package evil
  :ensure t
  :pin melpa
  :hook after-init
  :init
  (setopt evil-want-keybinding nil)
  :custom
  (evil-cross-lines t "Lift current-line restriction from f/F/t/T motions.")
  (evil-split-window-below t "Place new windows below")
  (evil-undo-system 'undo-redo "Use build-in undo-redo system.")
  (evil-vsplit-window-right t "Place new windows to the right")
  (evil-want-fine-undo t "Do not aggregate changes into single undo step."))

(use-package evil-collection
  :ensure t
  :pin melpa
  :after evil
  :config (evil-collection-init))

(use-package evil-commentary
  :ensure t
  :pin melpa
  :hook (text-mode prog-mode))

(use-package evil-surround
  :ensure t
  :pin melpa
  :hook (text-mode prog-mode))

(use-package evil-terminal-cursor-changer
  :ensure t
  :pin melpa
  :after evil
  :config
  (evil-terminal-cursor-changer-activate))

(use-package flymake
  :defer t
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-default-dictionary "en_GB")
  (flyspell-issue-welcome-flag nil))

(use-package forge
  :ensure t
  :pin melpa
  :after magit)

(use-package format-all
  :defer t
  :ensure t
  :pin melpa)

(use-package general
  :ensure t
  :pin melpa
  :config
  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override
   :prefix-map 'tyrant-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer tyrant-def :keymaps 'tyrant-map)
  (tyrant-def "" nil)

  (tyrant-def
    "SPC"   '("M-x" . execute-extended-command)
    "TAB"   '("last buffer" . (lambda () (interactive) (switch-to-buffer nil)))

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

    "e"     (cons "errors" (make-sparse-keymap))
    "el"    'flymake-show-buffer-diagnostics

    "f"     (cons "files" (make-sparse-keymap))
    "ff"    'find-file
    "fr"    'consult-recent-file
    "fs"    'save-buffer

    "g"     (cons "git" (make-sparse-keymap))
    "gs"    'magit-status

    "j"     (cons "jump" (make-sparse-keymap))
    "jj"    'avy-goto-char-timer
    "jr"    'avy-resume

    "p"     (cons "project" (make-sparse-keymap))
    "pf"    'project-find-file

    "q"     (cons "quit" (make-sparse-keymap))
    "qq"    'save-buffers-kill-terminal
    "qQ"    'save-buffers-kill-emacs

    "s"     (cons "search" (make-sparse-keymap))
    "sp"    'my/consult-ripgrep
    "ss"    'consult-line

    "T"     (cons "toggle" (make-sparse-keymap))
    "Tf"    'display-fill-column-indicator-mode
    "Ti"    'eglot-inlay-hints-mode
    "Tl"    'hl-line-mode
    "Tn"    'display-line-numbers-mode

    "t"     (cons "tabs" (make-sparse-keymap))
    "td"    'tab-close
    "th"    'tab-bar-mode
    "tm"    'tab-move
    "tn"    'tab-new
    "tt"    'other-tab-prefix

    "w"     (cons "windows" (make-sparse-keymap))
    "ww"    'other-window
    "w1"    'delete-other-windows
    "wo"    'delete-other-windows
    "w="    'balance-windows
    "wd"    'delete-window
    "ws"    'evil-window-split
    "wv"    'evil-window-vsplit
    "wr"    'evil-window-rotate-upwards
    "wj"    'evil-window-down
    "wk"    'evil-window-up
    "wh"    'evil-window-left
    "wl"    'evil-window-right
    "wu"    'winner-undo)

  (general-create-definer despot-def
    :states '(normal emacs)
    :keymaps 'override
    :major-modes t
    :prefix "SPC m"
    :non-normal-prefix "SPC m")

  (despot-def
    ""      (cons "mode" (make-sparse-keymap))
    "=="    'eglot-format-buffer
    "rr"    'eglot-rename
    "aa"    'eglot-code-actions))

;; (use-package git-gutter
;;   :ensure t
;;   :pin melpa
;;   :hook (text-mode prog-mode)
;;   :custom
;;   (git-gutter:added-sign "┃")
;;   (git-gutter:deleted-sign "▁")
;;   (git-gutter:modified-sign "┃")
;;   (git-gutter:unchanged-sign "")
;;   :custom-face
;;   (git-gutter:added
;;    ((t (:background
;;         ,(face-attribute 'line-number :background)
;;         :foreground
;;         ,(face-attribute 'modus-themes-fg-green-faint :foreground)))))
;;   (git-gutter:modified
;;    ((t (:background
;;         ,(face-attribute 'line-number :background)
;;         :foreground
;;         ,(face-attribute 'modus-themes-fg-yellow-faint :foreground)))))
;;   (git-gutter:deleted
;;    ((t (:background
;;         ,(face-attribute 'line-number :background)
;;         :foreground
;;         ,(face-attribute 'modus-themes-fg-red-faint :foreground))))))

(defun my/read-copilot-key ()
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         ;; piggyback on the copilot.el and custom token retrieval script (TODO)
         ;; mkdir -p ~/.config/my-github-copilot/
         ;; curl -XGET -H "authorization: Bearer $(jq -r '.["github.com"].oauth_token' ~/.config/github-copilot/hosts.json)" -H 'content-type: application/json' https://api.github.com/copilot_internal/v2/token > ~/.config/my-github-copilot/token.json
         (json (json-read-file "~/.config/my-github-copilot/token.json"))
         (oauth-token (gethash "token" json)))
    oauth-token))

(use-package gptel
  :ensure t
  :pin melpa
  :defer t
  :custom
  (gptel-backend (gptel-make-openai "Github Copilot"
                   :header `(("Authorization" . ,(concat "Bearer " (my/read-copilot-key)))
                             ("Content-Type" . "application/json")
                             ("Copilot-Integration-Id" . "vscode-chat")
                             ("Editor-Version" . "emacs"))
                   :host "api.business.githubcopilot.com"
                   :endpoint "/chat/completions"
                   :stream t
                   :key #'my/read-copilot-key
                   :models '(claude-3.5-sonnet gpt-4o gpt-4o-mini)))
  (gptel-api-key #'my/read-copilot-key)
  (gptel-model 'claude-3.5-sonnet))

;; Built-in code folding
(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

(use-package hl-line
  :hook (text-mode prog-mode)
  :custom
  (hl-line-sticky-flag nil))

(use-package magit
  :ensure t
  :pin melpa
  :defer t
  :config
  (defun my/unbind-meta-nb-keys-in-magit-section-mode ()
    (define-key magit-section-mode-map (kbd "M-1") nil)
    (define-key magit-section-mode-map (kbd "M-2") nil)
    (define-key magit-section-mode-map (kbd "M-3") nil)
    (define-key magit-section-mode-map (kbd "M-4") nil)
    (define-key magit-section-mode-map (kbd "<normal-state> M-1") nil)
    (define-key magit-section-mode-map (kbd "<normal-state> M-2") nil)
    (define-key magit-section-mode-map (kbd "<normal-state> M-3") nil)
    (define-key magit-section-mode-map (kbd "<normal-state> M-4") nil))
  
  (add-hook 'magit-section-mode-hook 'my/unbind-meta-nb-keys-in-magit-section-mode))

(use-package marginalia
  :ensure t
  :hook after-init)

(use-package markdown-mode
  :ensure t
  :pin melpa
  :mode ("\\.md\\'" . gfm-mode))

(use-package modus-themes
  :config
  (setopt modus-themes-common-palette-overrides
          '((bg-tab-bar bg-main)
            (bg-tab-current bg-lavender) ;; bg-active)
            (bg-tab-other bg-dim))))

(use-package orderless
  :ensure t
  :pin melpa
  :custom
  (completion-styles '(orderless basic)))

(use-package recentf
  :hook (text-mode prog-mode))

(use-package repeat
  :hook after-init)

(use-package savehist
  :hook after-init)

;; (use-package smartparens
;;   :ensure t
;;   :pin melpa
;;   :hook (text-mode prog-mode))

;; (use-package smartparens-config
;;   :after smartparens)

(use-package tab-bar
  :defer t
  :config
  (setq tab-bar-separator "")

  ;; (require 'icons)
  (define-icon tab-bar-new nil
    `((text ,(propertize "  " 'face 'modus-themes-fg-magenta-cooler)))
    ;; `((text ,(propertize "  " 'face 'modus-themes-fg-magenta-cooler)))
    ;; `((text ,(propertize " 󰝜 " 'face 'modus-themes-fg-magenta-cooler)))
    "Icon for creating a new tab."
    :version "29.1"
    :help-echo "New tab")

  :custom
  (tab-bar-auto-width-max '((220) 32))
  (tab-bar-format '(tab-bar-format-history
                    ;; tab-bar-format-tabs
                    tab-bar-format-tabs-groups
                    ;; tab-bar-separator
                    tab-bar-format-add-tab
                    tab-bar-format-align-right
                    (lambda () (when (fboundp 'vc-git--current-branch)
                                 (if-let* ((branch (vc-git--current-branch)))
                                     (propertize (format " %s " branch)
                                                 'face 'custom-comment)))) 
                    tab-bar-format-global))
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-tab-name-format-functions '(;; tab-bar-tab-name-format-hints
                                       (lambda (name _tab i)
                                         (concat (propertize (format " %d " i)
                                                             'face 'custom-comment)
                                                 name))
                                       ;; tab-bar-tab-name-format-close-button
                                       (lambda (name _tab _i)
                                         (concat name (propertize " " ;; "󰅙 "
                                                                  'close-tab t
                                                                  'face 'custom-comment
                                                                  :help "Click to close tab")))
                                       tab-bar-tab-name-format-face))
  (tab-bar-tab-hints t)
  (tab-bar-select-tab-modifiers '(meta)))

(use-package treesit
  :config
  (setopt treesit-language-source-alist
          '((html "https://github.com/tree-sitter/tree-sitter-html")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
            (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")))

  (add-to-list 'auto-mode-alist '("\\.jqtpl\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))

  (mapc #'treesit-install-language-grammar
        (seq-remove #'treesit-language-available-p
                    (mapcar #'car
                            treesit-language-source-alist))))

(use-package vertico
  :ensure t
  :pin melpa
  :hook after-init
  :bind
  (:map vertico-map
	("C-j" . vertico-next)
	("C-k" . vertico-previous)))

(use-package which-key
  :hook after-init)

;; Window configuration undo/redo
(use-package winner
  :hook after-init)

(use-package winum
  :ensure t
  :hook after-init
  :custom
  (winum-auto-setup-mode-line t)
  ;; :bind (("M-1" . winum-select-window-1)
  ;;        ("M-2" . winum-select-window-2)
  ;;        ("M-3" . winum-select-window-3)
  ;;        ("M-4" . winum-select-window-4)
  ;;        ("M-5" . winum-select-window-5)
  ;;        ("M-6" . winum-select-window-6)
  ;;        ("M-7" . winum-select-window-7)
  ;;        ("M-8" . winum-select-window-8)
  ;;        ("M-9" . winum-select-window-9)
  ;;        ("M-0" . winum-select-window-0-or-10))
  )

(use-package xref
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(use-package xt-mouse
  :unless (display-graphic-p)
  :hook (after-init . xterm-mouse-mode))
