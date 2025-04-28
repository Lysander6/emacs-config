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
 auto-save-default nil
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

(defconst my/github-namespaces
  '("business"
    "fakt"
    "omp"
    "widgets-platform")
  "List of GitHub repository namespaces to select from.")

(defconst my/github-organization "Ringier-Axel-Springer-PL"
  "The GitHub organization to search repositories in.")

(defvar consult-gh-repo-action)

(defun my/github-clone-repo-from-namespace ()
  "Clone a repository from a selected namespace in GitHub organization.
User selects namespace from a fixed list, then chooses a repository to clone."
  (interactive)
  (let* ((namespace (completing-read "Select namespace: " my/github-namespaces nil t))
         (query-string (format "props.namespace:%s -- --owner %s --limit 100#"
                               namespace
                               my/github-organization))
         (consult-gh-repo-action #'consult-gh--repo-clone-action))
    (consult-gh-search-repos query-string)))

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


(defconst my/github-oauth-token-file-path "~/.config/github-copilot/apps.json")

(defun my/read-github-oauth-token ()
  (let ((oauth-token-key (car (hash-table-keys (my/read-json-key my/github-oauth-token-file-path '())))))
    (my/read-json-key my/github-oauth-token-file-path `(,oauth-token-key "oauth_token"))))

(defvar my/github-oauth-token
  (with-demoted-errors "Error reading GitHub OAuth token: %S"
    (my/read-github-oauth-token))
  "GitHub OAuth token.")

(defun my/update-github-oauth-token ()
  "Update the GitHub OAuth token value after login."
  (setq my/github-oauth-token (my/read-github-oauth-token)))

(use-package copilot
  :ensure t
  :pin melpa
  :defer t
  :commands (copilot-install-server copilot-login)
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
  :hook (conf-mode text-mode prog-mode))

(use-package evil-goggles
  :ensure t
  :pin melpa
  :hook (text-mode prog-mode)
  :config (evil-goggles-use-diff-faces))

(use-package evil-surround
  :ensure t
  :pin melpa
  :hook (conf-mode text-mode prog-mode))

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
  :after magit
  :init
  (setq forge-add-default-bindings nil))

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
                             (claude-3.7-sonnet
                              :description "Hybrid model capable of standard thinking and extended thinking modes"
                              :capabilities (media tool-use cache)
                              :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
                              :context-window 200
                              :input-cost 3
                              :output-cost 15
                              :cutoff-date "2025-02")
                             (claude-3.7-sonnet-thought
                              :description "Hybrid model capable of standard thinking and extended thinking modes"
                              :capabilities (media cache)
                              :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
                              :context-window 200
                              :input-cost 3
                              :output-cost 15
                              :cutoff-date "2025-02")
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
  (gptel-model 'claude-3.7-sonnet)
  (gptel-use-tools t)
  :config
  (unless my/github-oauth-token
    (copilot-install-server)
    (copilot-login))

  ;; Tools

  ;; Filesystem Tools

  (gptel-make-tool
   :function (lambda (filepath)
               (with-temp-buffer
                 (insert-file-contents (expand-file-name filepath))
                 (buffer-string)))
   :name "read_file"
   :description "Read and display the contents of a file"
   :args (list '(:name "filepath"
                       :type string
                       :description "Path to the file to read. Supports relative paths and ~."))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (directory)
               (mapconcat #'identity
                          (directory-files directory)
                          "\n"))
   :name "list_directory"
   :description "List the contents of a given directory"
   :args (list '(:name "directory"
                       :type string
                       :description "The path to the directory to list"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (parent name)
               (condition-case nil
                   (progn
                     (make-directory (expand-file-name name parent) t)
                     (format "Directory %s created/verified in %s" name parent))
                 (error (format "Error creating directory %s in %s" name parent))))
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :args (list '(:name "parent"
                       :type string
                       :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                       :type string
                       :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
                       :type string
                       :description "The directory where to create the file")
               '(:name "filename"
                       :type string
                       :description "The name of the file to create")
               '(:name "content"
                       :type string
                       :description "The content to write to the file"))
   :category "filesystem")

  (defun my/gptel--edit_file (file-path file-edits)
    "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
    (if (and file-path (not (string= file-path "")) file-edits)
        (with-current-buffer (get-buffer-create "*edit-file*")
          (insert-file-contents (expand-file-name file-path))
          (let ((inhibit-read-only t)
                (case-fold-search nil)
                (file-name (expand-file-name file-path))
                (edit-success nil))
            ;; apply changes
            (dolist (file-edit (seq-into file-edits 'list))
              (when-let ((line-number (plist-get file-edit :line_number))
                         (old-string (plist-get file-edit :old_string))
                         (new-string (plist-get file-edit :new_string))
                         (is-valid-old-string (not (string= old-string ""))))
                (goto-char (point-min))
                (forward-line (1- line-number))
                (when (search-forward old-string nil t)
                  (replace-match new-string t t)
                  (setq edit-success t))))
            ;; return result to gptel
            (if edit-success
                (progn
                  ;; show diffs
                  (ediff-buffers (find-file-noselect file-name) (current-buffer))
                  (format "Successfully edited %s" file-name))
              (format "Failed to edited %s" file-name))))
      (format "Failed to edited %s" file-path)))

  (gptel-make-tool
   :function #'my/gptel--edit_file
   :name "edit_file"
   :description "Edit file with a list of edits, each edit contains a line-number,
a old-string and a new-string, new-string will replace the old-string at the specified line."
   :args (list '(:name "file-path"
                       :type string
                       :description "The full path of the file to edit")
               '(:name "file-edits"
                       :type array
                       :items (:type object
                                     :properties
                                     (:line_number
                                      (:type integer :description "The line number of the file where edit starts.")
                                      :old_string
                                      (:type string :description "The old-string to be replaced.")
                                      :new_string
                                      (:type string :description "The new-string to replace old-string.")))
                       :description "The list of edits to apply on the file"))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (script_program script_file script_args)
               (with-temp-message "Executing command ..."
                 (shell-command-to-string
                  (concat script_program " "
                          (expand-file-name script_file) " "
                          script_args))))
   :name "run_script"
   :description "Run script"
   :args (list
          '(:name "script_program"
                  :type string
                  :description "Program to run the the script.")
          '(:name "script_file"
                  :type string
                  :description "Path to the script to run. Supports relative paths and ~.")
          '(:name "script_args"
                  :type string
                  :description "Args for script to run."))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (command)
               (with-temp-message (format "Running command: %s" command)
                 (shell-command-to-string command)))
   :name "run_command"
   :description "Run a command."
   :args (list
          '(:name "command"
                  :type "string"
                  :description "Command to run."))
   :category "command")

  (defun my/run_async_command (callback command)
    "Run COMMAND asynchronously and pass output to CALLBACK."
    (condition-case error
        (let ((buffer (generate-new-buffer " *async output*")))
          (with-temp-message (format "Running async command: %s" command)
            (async-shell-command command buffer nil))
          (let ((proc (get-buffer-process buffer)))
            (when proc
              (set-process-sentinel
               proc
               (lambda (process _event)
                 (unless (process-live-p process)
                   (with-current-buffer (process-buffer process)
                     (let ((output (buffer-substring-no-properties (point-min) (point-max))))
                       (kill-buffer (current-buffer))
                       (funcall callback output)))))))))
      (t
       ;; Handle any kind of error
       (funcall callback (format "An error occurred: %s" error)))))

  (gptel-make-tool
   :function #'my/run_async_command
   :name "run_async_command"
   :description "Run an async command."
   :args (list
          '(:name "command"
                  :type "string"
                  :description "Command to run."))
   :category "command"
   :async t
   :include t)

  ;; Emacs Tools

  (gptel-make-tool
   :function (lambda (text)
               (message "%s" text)
               (format "Message sent: %s" text))
   :name "echo_message"
   :description "Send a message to the *Messages* buffer"
   :args (list '(:name "text"
                       :type string
                       :description "The text to send to the messages buffer"))
   :category "emacs")

  (defun my/gptel-read-documentation (symbol)
    "Read the documentation for SYMBOL, which can be a function or variable."
    (let ((sym (intern symbol)))
      (cond
       ((fboundp sym)
        (documentation sym))
       ((boundp sym)
        (documentation-property sym 'variable-documentation))
       (t
        (format "No documentation found for %s" symbol)))))

  (gptel-make-tool
   :name "read_documentation"
   :function #'my/gptel-read-documentation
   :description "Read the documentation for a given function or variable"
   :args (list '(:name "name"
                       :type string
                       :description "The name of the function or variable whose documentation is to be retrieved"))
   :category "emacs")

  ;; Web Tools

  (gptel-make-tool
   :function (lambda (url)
               (with-current-buffer (url-retrieve-synchronously url)
                 (goto-char (point-min))
                 (forward-paragraph)
                 (let ((dom (libxml-parse-html-region (point) (point-max))))
                   (run-at-time 0 nil #'kill-buffer (current-buffer))
                   (with-temp-buffer
                     (shr-insert-document dom)
                     (buffer-substring-no-properties (point-min) (point-max))))))
   :name "read_url"
   :description "Fetch and read the contents of a URL"
   :args (list '(:name "url"
                       :type string
                       :description "The URL to read"))
   :category "web")

  (defvar brave-search-api-key "<YOUR API KEY>"
    "API key for accessing the Brave Search API.")

  (defun my/brave-search-query (query)
    "Perform a web search using the Brave Search API with the given QUERY."
    (let ((url-request-method "GET")
          (url-request-extra-headers `(("X-Subscription-Token" . ,brave-search-api-key)))
          (url (format "https://api.search.brave.com/res/v1/web/search?q=%s" (url-encode-url query))))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (when (re-search-forward "^$" nil 'move)
          (let ((json-object-type 'hash-table)) ; Use hash-table for JSON parsing
            (json-parse-string (buffer-substring-no-properties (point) (point-max))))))))

  (gptel-make-tool
   :function #'my/brave-search-query
   :name "brave_search"
   :description "Perform a web search using the Brave Search API"
   :args (list '(:name "query"
                       :type string
                       :description "The search query string"))
   :category "web")

  (defun my/gptel-youtube-metadata (callback url)
    (let* ((video-id (and (string-match (concat
                                         "^\\(?:http\\(?:s?://\\)\\)?\\(?:www\\.\\)?\\(?:youtu\\(?:\\(?:\\.be\\|be\\.com\\)/\\)\\)"
                                         "\\(?:watch\\?v=\\)?"
                                         "\\([^?&]+\\)")
                                        url)
                          (match-string 1 url)))
           (dir (file-name-concat temporary-file-directory "yt-dlp" video-id)))
      (if (file-directory-p dir)
          (delete-directory dir t))
      (make-directory dir t)
      (let ((default-directory dir)
            (idx 0)
            (data (list :description nil :transcript nil)))
        (make-process :name "yt-dlp"
                      :command `("yt-dlp" "--write-description" "--skip-download" "--output" "video" ,url)
                      :sentinel (lambda (proc status)
                                  (cl-incf idx)
                                  (let ((default-directory dir))
                                    (when (file-readable-p "video.description")
                                      (plist-put data :description
                                                 (with-temp-buffer
                                                   (insert-file-contents "video.description")
                                                   (buffer-string)))))
                                  (when (= idx 2)
                                    (funcall callback (gptel--json-encode data))
                                    (delete-directory dir t))))
        (make-process :name "yt-dlp"
                      :command `("yt-dlp" "--skip-download" "--write-auto-subs" "--sub-langs" "en,-live_chat" "--convert-subs" "srt" "--output" "video" ,url)
                      :sentinel (lambda (proc status)
                                  (cl-incf idx)
                                  (let ((default-directory dir))
                                    (when (file-readable-p "video.en.srt")
                                      (plist-put data :transcript
                                                 (with-temp-buffer
                                                   (insert-file-contents "video.en.srt")
                                                   (buffer-string)))))
                                  (when (= idx 2)
                                    (funcall callback (gptel--json-encode data))
                                    (delete-directory dir t)))))))

  (gptel-make-tool
   :name "youtube_video_metadata"
   :function #'my/gptel-youtube-metadata
   :description "Find the description and video transcript for a youtube video. Return a JSON object containing two fields:

\"description\": The video description added by the uploader
\"transcript\": The video transcript in SRT format"
   :args '((:name "url"
                  :description "The youtube video URL, for example \"https://www.youtube.com/watch?v=H2qJRnV8ZGA\""
                  :type string))
   :category "web"
   :async t
   :include t)

  ;; SQL Tools

  (defun my/ejc-sql-eval-query (query &optional analyze-p connection-name)
    "Evaluate a SQL query using ejc-sql, ensuring a connection exists.
It takes a SQL query and the name of an existing ejc connection.
If no connection name is provided, it defaults to \"Default\".
If ANALYZE-P is non-nil, performs EXPLAIN ANALYZE on the query.
It will create a temporary buffer, connect to the database specified
by CONNECTION-NAME, evaluate the query, and return the result as a string.
It expects the connection CONNECTION-NAME to exist
using `ejc-connect'."
    (interactive)
    (let ((buffer (generate-new-buffer " *temp-ejc-sql-buffer*"))
          (result "")
          (actual-connection-name (or connection-name "Default"))
          (max-wait-time 30) ; Maximum wait time in seconds
          (wait-interval 0.1))
      (with-current-buffer buffer
        (ejc-connect actual-connection-name)
        (if analyze-p
            (insert (concat "EXPLAIN (ANALYZE true, COSTS true, FORMAT json) " query ";"))
          (insert (concat "SELECT json_agg(row_to_json (t)) FROM (" query ") t;")))
        (ejc-eval-user-sql-at-point))
      (let ((wait-time 0))
        (while (and (not (get-buffer "*ejc-sql-output*"))
                    (< wait-time max-wait-time))
          (sit-for wait-interval)
          (setq wait-time (+ wait-time wait-interval))))

      (when (get-buffer "*ejc-sql-output*")
        (with-current-buffer "*ejc-sql-output*"
          (setq result (buffer-string)))
        (kill-buffer "*ejc-sql-output*"))

      (kill-buffer buffer)
      result))

  (gptel-make-tool
   :name "select_from_db"
   :function (lambda (query &optional analyze-p connection-name)
               (my/ejc-sql-eval-query query analyze-p connection-name))
   :description "Evaluate a SQL query using ejc-sql, ensuring a connection exists.
It takes a SQL query and the name of an existing ejc connection.
If no connection name is provided, it defaults to \"Default\".
If ANALYZE-P is non-nil, performs EXPLAIN ANALYZE on the query.
It will create a temporary buffer, connect to the database specified
by CONNECTION-NAME, evaluate the query, and return the result as a string."
   :args (list '(:name "query"
                       :type string
                       :description "The SQL query to evaluate")
               '(:name "analyze_p"
                       :type string
                       :optional t
                       :description "If ANALYZE-P is non-nil, performs EXPLAIN ANALYZE on the query instead of executing it directly, returning query execution plan and statistics.")
               '(:name "connection_name"
                       :type string
                       :optional t
                       :description "The name of the ejc-sql connection to use. This connection must already exist. Defaults to 'Default' if not provided."))
   :category "sql")

  ;; Buffer/File Editing Tools

  (gptel-make-tool
   :function (lambda (buffer)
               (unless (buffer-live-p (get-buffer buffer))
                 (error "Error: buffer %s is not live." buffer))
               (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer"
   :args (list '(:name "buffer"
                       :type string
                       :description "The name of the buffer whose contents are to be retrieved"))
   :category "emacs")

  (gptel-make-tool
   :function (lambda (buffer text)
               (with-current-buffer (get-buffer-create buffer)
                 (save-excursion
                   (goto-char (point-max))
                   (insert text)))
               (format "Appended text to buffer %s" buffer))
   :name "append_to_buffer"
   :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
   :args (list '(:name "buffer"
                       :type string
                       :description "The name of the buffer to append text to.")
               '(:name "text"
                       :type string
                       :description "The text to append to the buffer."))
   :category "emacs")

  (defun my/codel-edit-buffer (buffer-name old-string new-string)
    "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
    (with-current-buffer buffer-name
      (let ((case-fold-search nil))  ;; Case-sensitive search
        (save-excursion
          (goto-char (point-min))
          (let ((count 0))
            (while (search-forward old-string nil t)
              (setq count (1+ count)))
            (if (= count 0)
                (format "Error: Could not find text to replace in buffer %s" buffer-name)
              (if (> count 1)
                  (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)
                (goto-char (point-min))
                (search-forward old-string)
                (replace-match new-string t t)
                (format "Successfully edited buffer %s" buffer-name))))))))

  (gptel-make-tool
   :name "edit_buffer"
   :function #'my/codel-edit-buffer
   :description "Edits Emacs buffers"
   :args '((:name "buffer_name"
                  :type string
                  :description "Name of the buffer to modify"
                  :required t)
           (:name "old_string"
                  :type string
                  :description "Text to replace (must match exactly)"
                  :required t)
           (:name "new_string"
                  :type string
                  :description "Text to replace old_string with"
                  :required t))
   :category "edit")

  (defun my/codel-replace-buffer (buffer-name content)
    "Completely replace contents of BUFFER-NAME with CONTENT."
    (with-current-buffer buffer-name
      (erase-buffer)
      (insert content)
      (format "Buffer replaced: %s" buffer-name)))

  (gptel-make-tool
   :name "replace_buffer"
   :function #'my/codel-replace-buffer
   :description "Completely overwrites buffer contents"
   :args '((:name "buffer_name"
                  :type string
                  :description "Name of the buffer to overwrite"
                  :required t)
           (:name "content"
                  :type string
                  :description "Content to write to the buffer"
                  :required t))
   :category "edit"))

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

(use-package mcp-hub
  :ensure t
  :after gptel
  :defer t
  :vc (:url "https://github.com/lizqwerscott/mcp.el"
            :rev :newest
            :branch "master")
  :custom
  (mcp-hub-servers
   '(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" "/home/")))))
  :config
  (defun gptel-mcp-register-tool ()
    (interactive)
    (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
      (mapcar #'(lambda (tool)
                  (apply #'gptel-make-tool
                         tool))
              tools)))

  ;; TODO: seems that it should be called only after servers are running
  (gptel-mcp-register-tool))

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
            (go "https://github.com/tree-sitter/tree-sitter-go")
            (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
            (html "https://github.com/tree-sitter/tree-sitter-html")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
            (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
            (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
            (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
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
