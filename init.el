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
  (set-face-attribute 'vertical-border nil :foreground (face-attribute 'mode-line-inactive :background))
  (add-hook 'window-configuration-change-hook 'change-window-divider))

(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize t))

(use-package astro-ts-mode
  :ensure t
  :pin melpa
  :defer t)

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

(defun my/read-json-key (file-path key-path)
  "Read nested value from JSON file at FILE-PATH using KEY-PATH.
FILE-PATH should be a string representing path to JSON file.
KEY-PATH can be either a single key or a list of keys for nested objects.

Example:
  (my/read-json-key \"~/.config/config.json\" '(\"user\" \"settings\" \"theme\"))
  (my/read-json-key \"~/.config/config.json\" \"simple_key\")"
  (let ((expanded-path (expand-file-name file-path)))
    (condition-case err
        (if (not (file-exists-p expanded-path))
            (error "JSON file not found at %s" expanded-path)
          (let ((json-object-type 'hash-table)
                (json-array-type 'list)
                (json-key-type 'string))
            (let* ((json-data (json-read-file expanded-path))
                   (keys (cond
                          ((stringp key-path) (list key-path))
                          ((listp key-path) key-path)
                          (t (error "KEY-PATH must be a string or list of strings"))))
                   (value (seq-reduce
                          (lambda (obj key)
                            (cond
                              ((null obj) nil)
                              ((hash-table-p obj) (gethash key obj))
                              (t (error "Cannot traverse through non-object value at %s"
                                      (seq-take keys (seq-position keys key))))))
                          keys
                          json-data)))
              (or value
                  (error "Path %s not found in JSON file"
                         (if (listp key-path)
                             (string-join key-path " -> ")
                           key-path))))))
      (error (error "Error reading JSON file: %s" err)))))


(defconst my/github-oauth-token-file-path "~/.config/github-copilot/hosts.json")

(defun my/read-github-oauth-token ()
  (my/read-json-key my/github-oauth-token-file-path '("github.com" "oauth_token")))

(defvar my/github-oauth-token
  (with-demoted-errors "Error reading GitHub OAuth token: %S"
    (my/read-github-oauth-token))
  "GitHub OAuth token.")

(defun my/update-github-oauth-token ()
  "Update the GitHub OAuth token value after login."
  (setq my/github-oauth-token (my/read-github-oauth-token)))

(use-package copilot
  :ensure t
  :defer t
  :commands (copilot-install-server copilot-login)
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (advice-add 'copilot-login :after #'my/update-github-oauth-token))

(use-package corfu
  :ensure t
  :pin melpa
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto nil))

(use-package corfu-popupinfo-mode
  :hook corfu-mode)

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

(use-package display-line-numbers
  :defer t
  ;; :hook (text-mode prog-mode)
  :custom
  ;; (display-line-numbers-type 'relative)
  (display-line-numbers-width 3))

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

(use-package evil-goggles
  :ensure t
  :pin melpa
  :hook (text-mode prog-mode)
  :config (evil-goggles-use-diff-faces))

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

(defconst my/github-copilot-token-file-path "~/.config/my-github-copilot/token.json")

(defvar my/github-copilot-token
  (with-demoted-errors "Error reading GitHub Copilot token: %S"
    (my/read-json-key my/github-copilot-token-file-path "token"))
  "GitHub Copilot token.")

(defvar my/github-copilot-token-expires-at
  (with-demoted-errors "Error reading GitHub Copilot token expiration timestamp: %S"
    (my/read-json-key my/github-copilot-token-file-path "expires_at"))
  "GitHub Copilot token expiration timestamp.")

(defun my/expired-p (timestamp)
  "Check if TIMESTAMP (Unix timestamp) is in the past."
  (> (time-convert nil 'integer) timestamp))

(defun my/ensure-directory-exists (file-path)
  (let ((directory (file-name-directory (expand-file-name file-path))))
    (unless (file-exists-p directory)
      (make-directory directory t))))

(defun my/fetch-github-copilot-token (auth-token)
  "Fetch GitHub Copilot token using AUTH-TOKEN for authentication.
Returns t if successful, nil otherwise."
  (let ((output-file (expand-file-name my/github-copilot-token-file-path)))
    (my/ensure-directory-exists my/github-copilot-token-file-path)

    (condition-case err
        (let ((exit-code
               (call-process "curl" nil nil nil
                            "-XGET"
                            "-sS" ;; silent but show errors
                            "-f"  ;; fail on HTTP errors
                            "-H" (concat "authorization: Bearer " auth-token)
                            "-H" "content-type: application/json"
                            "-o" output-file
                            "https://api.github.com/copilot_internal/v2/token")))
          (if (= exit-code 0)
              t
            (message "Curl failed with exit code %d" exit-code)
            nil))
      (error
       (message "Error running curl: %s" err)
       nil))))

(defun my/read-github-copilot-key ()
  "Get GitHub Copilot key, refreshing if expired.
Returns the key as string or nil if unsuccessful."
  ;; Return cached token if it's valid
  (if (and my/github-copilot-token
           my/github-copilot-token-expires-at
           (not (my/expired-p my/github-copilot-token-expires-at)))
      my/github-copilot-token

    ;; Otherwise, fetch new token
    (let ((token-file my/github-copilot-token-file-path))
      (unless my/github-oauth-token
        (error "GitHub Copilot auth token not set"))

      (message "Fetching new GitHub Copilot token...")
      (if (my/fetch-github-copilot-token my/github-oauth-token)
          (progn
            (setq my/github-copilot-token-expires-at
                  (with-demoted-errors "Error reading expires_at: %S"
                    (my/read-json-key token-file "expires_at"))
                  my/github-copilot-token
                  (with-demoted-errors "Error reading token: %S"
                    (my/read-json-key token-file "token")))
            (if (and my/github-copilot-token my/github-copilot-token-expires-at)
                my/github-copilot-token
              (progn
                (message "Failed to read new token or expiration timestamp")
                nil)))
        (progn
          (message "Failed to fetch new token")
          nil)))))

(use-package gptel
  :ensure t
  :pin melpa
  :defer t
  :custom
  ;; Consult [CopilotChat.nvim README](https://github.com/CopilotC-Nvim/CopilotChat.nvim/blob/dbce8a231d1ac72c68ce00b86b415c9304417102/README.md?plain=1#L244-L248)
  ;; for the model names and [gptel.el source](https://github.com/karthink/gptel/blob/4ab198a904f1706a8daede1145386db4dc960aa1/gptel-anthropic.el#L390)
  ;; for their properties.
  (gptel-backend (gptel-make-openai "Github Copilot"
                   :header (lambda () (when-let (key (my/read-github-copilot-key))
                                        `(("Authorization" . ,(concat "Bearer " key))
                                          ("Content-Type" . "application/json")
                                          ("Copilot-Integration-Id" . "vscode-chat")
                                          ("Editor-Version" . "emacs"))))
                   :host "api.business.githubcopilot.com"
                   :endpoint "/chat/completions"
                   :stream t
                   :key #'my/read-github-copilot-key
                   :models '((claude-3.5-sonnet
                              :description "Highest level of intelligence and capability"
                              :capabilities (media tool-use cache)
                              :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
                              :context-window 200
                              :input-cost 3
                              :output-cost 15
                              :cutoff-date "2024-04")
                             (gemini-2.0-flash-001
                              :description "Next generation features, superior speed, native tool use"
                              :capabilities (tool-use json media)
                              :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                                           "application/pdf" "text/plain" "text/csv" "text/html")
                              :context-window 1000
                              :cutoff-date "2024-12")
                             (gpt-4o
                              :description "Advanced model for complex tasks; cheaper & faster than GPT-Turbo"
                              :capabilities (media tool-use json url)
                              :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                              :context-window 128
                              :input-cost 2.50
                              :output-cost 10
                              :cutoff-date "2023-10")
                             (gpt-4o-mini
                              :description "Cheap model for fast tasks; cheaper & more capable than GPT-3.5 Turbo"
                              :capabilities (media tool-use json url)
                              :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                              :context-window 128
                              :input-cost 0.15
                              :output-cost 0.60
                              :cutoff-date "2023-10")
                             (o1
                              :description "Reasoning model designed to solve hard problems across domains"
                              :capabilities (nosystem media reasoning)
                              :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                              :context-window 200
                              :input-cost 15
                              :output-cost 60
                              :cutoff-date "2023-10"
                              :request-params (:stream :json-false))
                             (o3-mini
                              :description "High intelligence at the same cost and latency targets of o1-mini"
                              :context-window 200
                              :input-cost 3
                              :output-cost 12
                              :cutoff-date "2023-10"
                              :capabilities (nosystem reasoning)
                              :request-params (:stream :json-false)))))
  (gptel-api-key #'my/read-github-copilot-key)
  (gptel-model 'claude-3.5-sonnet)
  :config
  (unless my/github-oauth-token
    (copilot-install-server)
    (copilot-login)))

;; Built-in code folding
(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

(use-package hl-line
  :defer t
  ;; :hook (text-mode prog-mode)
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
            (bg-tab-other bg-dim)
            (fg-line-number-inactive "gray50")
            (fg-line-number-active fg-main)
            (bg-line-number-inactive unspecified)
            (bg-line-number-active unspecified))))

(use-package nerd-icons-completion
  :ensure t
  :pin melpa
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :pin melpa
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package orderless
  :ensure t
  :pin melpa
  :custom
  (completion-styles '(orderless basic)))

(use-package org
  :custom
  (org-capture-templates
   '(("a" "Prawa Autorskie - Unreleased" entry (file+headline "~/org/work/rasp/prawa-autorskie/unreleased.org" "Unreleased")
      "* %i%?\n"))))

(use-package paren
  :custom
  (show-paren-context-when-offscreen t))

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
          '((astro "https://github.com/virchau13/tree-sitter-astro")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (html "https://github.com/tree-sitter/tree-sitter-html")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
            (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
            (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
            (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

  (add-to-list 'auto-mode-alist '("\\.jqtpl\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

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
