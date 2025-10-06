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

 browse-url-browser-function #'eww-browse-url

 enable-recursive-minibuffers t

 indent-tabs-mode nil

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
    (set-face-attribute 'vertical-border nil :foreground (face-attribute 'mode-line-inactive :background))
    ))

(unless (display-graphic-p)
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
  (consult-gh-default-clone-directory
   (pcase (getenv "USER")
     ("lysander" "~/projects/")
     (_ "~/workspace/")))
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
    "ucs2"
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
         (query-string (format "props.namespace:%s -- --owner %s --limit 300#"
                               namespace
                               my/github-organization))
         (consult-gh-repo-action #'consult-gh--repo-clone-action))
    (consult-gh-search-repos query-string)))

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

(use-package dumb-jump
  :ensure t
  :pin melpa
  :defer t
  :custom
  (dumb-jump-force-searcher 'rg)
  ;; (dumb-jump-rg-search-args "--pcre2 -g '!*.{spec,test}.js'")
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (add-to-list 'dumb-jump-language-file-exts '(:language "javascript" :ext "jqtpl" :agtype nil :rgtype nil)))

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

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

(use-package gptel
  :ensure t
  :pin melpa
  :defer t
  :custom
  (gptel-use-tools t)
  :config
  (setq gptel-expert-commands t)
  (define-key gptel-mode-map (kbd "S-<return>") nil)
  (define-key gptel-mode-map (kbd "<visual-state> S-<return>") nil)
  (define-key gptel-mode-map (kbd "<normal-state> S-<return>") nil)
  (define-key gptel-mode-map (kbd "S-RET") nil)
  (define-key gptel-mode-map (kbd "<visual-state> S-RET") nil)
  (define-key gptel-mode-map (kbd "<normal-state> S-RET") nil)

  (define-key gptel-mode-map (kbd "M-<return>") #'gptel-menu)
  (define-key gptel-mode-map (kbd "M-RET") #'gptel-menu)

  ;; source: https://github.com/jamesponddotco/llm-prompts/blob/2606d48cb4c1c52f5ff48e65e29b335646b00f7a/data/socratic-coder.md
  (gptel-make-preset 'socratic-coder
    :description "Expert software engineer who helps you polish software ideas and write detailed specifications by asking one focused, open-ended question at a time"
    :system "You are an expert software engineer with a PhD in computer science. Your task is to help develop a thorough, step-by-step specification for a software idea by asking the user one question at a time.

The user will provide the idea you will be working with as the first message between <idea> tags.

Follow these instructions carefully:

1. Ask only one question at a time. Each question should build on the user's previous answers and aim to gather more detailed information about the idea.

2. Focus your questions on different aspects of the software development process, such as:
   - Functionality and features
   - User interface and user experience
   - Data management and storage
   - Security and privacy considerations
   - Scalability and performance
   - Integration with other systems
   - Testing and quality assurance
   - Deployment and maintenance

3. When formulating your questions:
   - Be specific and targeted
   - Avoid yes/no questions; instead, ask open-ended questions that encourage detailed responses
   - Use technical terminology appropriate for a software engineering context
   - If clarification is needed on a previous answer, ask for it before moving on to a new topic

4. After each user response:
   - Analyze the information provided
   - Identify areas that need further exploration
   - Determine the most logical next question to ask based on the current information and what's still unknown

5. Maintain a coherent flow of conversation:
   - Keep track of what has been discussed
   - Ensure that all crucial aspects of the software idea are covered
   - Circle back to previous topics if new information necessitates it

Your output should consist solely of your questions to the user, one at a time. Do not include any other commentary or explanations unless explicitly asked by the user.")

  ;; source: https://github.com/jamesponddotco/llm-prompts/blob/2606d48cb4c1c52f5ff48e65e29b335646b00f7a/data/brainstorm-specification.md
  (gptel-make-preset 'brainstorm-specs
    :description "Turns a conversation you had with @socratic-coder into a developer-ready specification"
    :system "Now that we have wrapped up the brainstorming process, you are tasked with compiling our findings into a comprehensive, developer-ready specification, similar to an RFC (Request for Comments). Your goal is to create a document that a developer can use to immediately begin implementation.

First, carefully review the brainstorming session we had so far. Analyze the session thoroughly, identifying key requirements, architectural decisions, data handling approaches, and any other relevant information for the project.

Based on your analysis, create a structured specification document with the following sections:

1. Introduction
   - Briefly describe the project's purpose and goals
   - Provide any necessary context or background information

2. Requirements
   - List all functional and non-functional requirements
   - Prioritize requirements if possible (e.g., must-have, should-have, nice-to-have)

3. Architecture
   - Describe the overall system architecture
   - Include any diagrams or flowcharts if mentioned in the brainstorming notes
   - Explain key components and their interactions

4. Data Handling
   - Detail data models and structures
   - Explain data flow within the system
   - Address any data storage, retrieval, or processing considerations

5. API Design (if applicable)
   - Define API endpoints, request/response formats, and authentication methods

6. Error Handling
   - Outline strategies for handling various types of errors
   - Include error codes and messages where appropriate

7. Performance Considerations
   - Discuss any performance requirements or optimizations

8. Security Measures
   - Address security concerns and proposed solutions

9. Testing Plan
   - Outline a comprehensive testing strategy
   - Include unit testing, integration testing, and any specific test scenarios

10. Implementation Timeline (if available from the brainstorming notes)
    - Provide estimated timeframes for different phases of the project

11. Open Questions and Future Considerations
    - List any unresolved issues or areas that need further discussion

When creating this specification:
- Use clear, concise language suitable for technical readers
- Provide sufficient detail for developers to begin implementation
- Ensure consistency throughout the document
- Use numbered lists, bullet points, or tables where appropriate to improve readability
- Include any relevant code snippets, pseudocode, or examples mentioned in the brainstorming notes

Your final output should be a well-structured, comprehensive specification document. Begin your response with <specification> and end it with </specification>. The content within these tags should be the complete, developer-ready specification without any additional commentary or meta-discussion.")

  ;; source: https://github.com/jamesponddotco/llm-prompts/blob/2606d48cb4c1c52f5ff48e65e29b335646b00f7a/data/brainstorm-critique.md
  (gptel-make-preset 'brainstorm-critique
      :description "After turning conversation with @socratic-coder and turning it into a developer-ready specification with @brainstorm-specs, you can use this prompt to create a critique of the plan so you can improve on the specification."
      :system "Now that we have wrapped up the brainstorming process, acting like a third-party software engineer tasked with critically analyzing a project idea that has just completed its brainstorming phase, poke holes in the idea. Your goal is to identify potential flaws, challenges, and areas of improvement in the project concept. Approach this task with a constructive yet critical mindset, drawing from your expertise as an experienced software engineer.

First, review the brainstorm session so far, then examine the results of our brainstorming session.

Your task is to critically analyze the project idea and brainstorming results. Consider the following aspects:

1. Technical feasibility: Are there any technical challenges or limitations that may have been overlooked?
2. Scalability: How well would this solution scale as user base or data volume grows?
3. Security and privacy: Are there potential vulnerabilities or data protection issues?
4. User experience: Could there be usability problems or friction points for the end-users?
5. Market fit: Is there a clear need for this solution? How does it compare to existing alternatives?
6. Resource requirements: Are the necessary skills, time, and budget realistically accounted for?
7. Regulatory compliance: Are there any legal or regulatory hurdles that might affect implementation?
8. Maintenance and support: What long-term challenges might arise in maintaining and supporting this solution?

Provide a thorough analysis, pointing out potential issues, risks, or oversights in the current project concept. For each point you raise, briefly explain why it's a concern and, if possible, suggest a potential mitigation strategy or area for further investigation.

Present your analysis in a clear, organized manner. Use bullet points or numbered lists where appropriate to enhance readability. Be specific in your critiques, referencing particular aspects of the project summary or brainstorming results where relevant.

Remember, your goal is to help improve the project by identifying potential weaknesses or oversights. Maintain a professional and constructive tone throughout your analysis.

Your final output should be structured as follows:

<critical_analysis>
[Your detailed analysis here, organized by the aspects mentioned above or other relevant categories]
</critical_analysis>

<recommendations>
[A concise list of key recommendations or areas for further exploration based on your analysis]
</recommendations>

Ensure that your final output contains only the <critical_analysis> and <recommendations> sections, without any additional commentary or explanation.")

  (defun my/gptel-mode-set-project-root ()
    "Set default-directory to project root when gptel-mode is enabled.

This ensures that commands run from gptel buffers use the project root
as their current working directory, rather than inheriting the working
directory from the buffer where gptel was invoked (which might be a
nested subdirectory within the project)."
    (when-let ((project (project-current)))
      (setq default-directory (project-root project))))

  (add-hook 'gptel-mode-hook #'my/gptel-mode-set-project-root)

  (defun my/gptel-context-with-filepath (orig-fun buffer contexts)
    "Advice for gptel-context--insert-buffer-string to show file paths instead of buffer names.

This advice temporarily replaces buffer names with file paths in the
context information sent to LLMs. For buffers visiting files, the full
file path is displayed; for buffers not associated with files (like
*scratch*), the original buffer name is preserved."
    (let ((original-buffer-name (symbol-function 'buffer-name)))
      (unwind-protect
          (progn
            (fset 'buffer-name
                  (lambda (&optional buf)
                    (let ((buffer (or buf (current-buffer))))
                      (let ((file-name (buffer-file-name buffer)))
                        (if file-name
                            (abbreviate-file-name file-name)
                          (funcall original-buffer-name buffer))))))
            (funcall orig-fun buffer contexts))
        ;; Always restore the original function
        (fset 'buffer-name original-buffer-name))))

  (advice-add 'gptel-context--insert-buffer-string :around #'my/gptel-context-with-filepath)

  ;; Tools

  ;; Human

  (defun my/gptel-ask-human (callback question)
    "Ask the human a QUESTION and return their answer via CALLBACK.
Opens a temporary buffer for the user to input their response.
Behaves like git commit: saving with content submits, saving empty cancels."
    (let* ((buffer-name (format "*Question from AI: %s*"
                                (format-time-string "%H:%M:%S")))
           (question-buffer (get-buffer-create buffer-name))
           (original-window-config (current-window-configuration))
           (callback-called nil)) ; Track if callback was already called

      (with-current-buffer question-buffer
        ;; Set up the buffer
        (erase-buffer)
        (insert (format "<!-- AI Question: %s -->\n" question))
        (insert "<!-- Please type your answer below and use C-c C-c to submit or C-c C-k to cancel -->\n")
        (insert "<!-- Saving with empty content will cancel the question -->\n\n")

        ;; Use markdown-ts-mode if available, fall back to markdown-mode or text-mode
        (cond
         ((fboundp 'markdown-ts-mode) (markdown-ts-mode))
         ((fboundp 'markdown-mode) (markdown-mode))
         (t (text-mode)))

        ;; Make it read-write
        (setq buffer-read-only nil)

        ;; Function to extract and process answer
        (defun my/process-answer ()
          (unless callback-called ; Prevent multiple calls
            (setq callback-called t)
            (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                   ;; Remove comment lines and get actual content
                   (lines (split-string content "\n"))
                   (content-lines (seq-filter
                                   (lambda (line)
                                     (not (string-match-p "^\\s-*<!--" line)))
                                   lines))
                   (answer (string-join content-lines "\n")))
              (setq answer (string-trim answer))
              (kill-buffer (current-buffer))
              (set-window-configuration original-window-config)
              (if (string-empty-p answer)
                  (progn
                    (message "Empty response - cancelling question")
                    (funcall callback "User cancelled the question (empty response)."))
                (progn
                  (message "Answer submitted to AI")
                  (funcall callback answer))))))

        ;; Function to cancel
        (defun my/cancel-question ()
          (unless callback-called
            (setq callback-called t)
            (kill-buffer (current-buffer))
            (set-window-configuration original-window-config)
            (funcall callback "User cancelled the question.")))

        ;; Set up keybindings
        (local-set-key (kbd "C-c C-c")
                       (lambda ()
                         (interactive)
                         (my/process-answer)))

        (local-set-key (kbd "C-c C-k")
                       (lambda ()
                         (interactive)
                         (when (y-or-n-p "Cancel question? ")
                           (my/cancel-question))))

        ;; Handle window/buffer deletion gracefully
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (my/cancel-question))
                  nil t)

        ;; Position cursor after the comments
        (goto-char (point-max)))

      ;; Display the buffer
      (pop-to-buffer question-buffer)
      (message "Answer the question and use C-c C-c to submit or C-c C-k to cancel.")))

  (gptel-make-tool
   :name "ask_human"
   :function #'my/gptel-ask-human
   :description "Ask the human user a question and wait for their response."
   :args (list '(:name "question"
                       :type string
                       :description "The question to ask the human user"))
   :category "human"
   :async t
   :include t)

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
   :category "filesystem"
   :confirm t)

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
   :category "filesystem"
   :confirm t)

  (defun my/trim-trailing-whitespace-multiline (string)
    "Remove trailing whitespace from each line in a multi-line string."
    (mapconcat (lambda (line)
                 (string-trim-right line))
               (split-string string "\n")
               "\n"))

  (defun my/attempt-string-replacement (old-string new-string search-start line-number)
    "Attempt string replacement with smart whitespace handling.
    Returns (success . result-message)."
    (save-excursion
      (goto-char search-start)

      ;; Strategy 1: Exact match (preserves all whitespace)
      (if (search-forward old-string nil t)
          (progn
            (replace-match new-string t t)
            (cons t "Success"))

        ;; Strategy 2: Fallback - trim trailing whitespace from old_string
        (let ((trimmed-old-string (my/trim-trailing-whitespace-multiline old-string)))
          (if (and (not (string= old-string trimmed-old-string))
                   (search-forward trimmed-old-string nil t))
              (progn
                (replace-match new-string t t)
                (cons t "Success (matched after trimming trailing whitespace)"))

            (let ((line-content (buffer-substring-no-properties
                                 search-start (line-end-position))))
              (cons nil (format "String not found starting at line %d. Line content: '%s'"
                                line-number line-content))))))))

  (defun my/show-edit-diff (file-name edit-buffer)
    "Show diff between original file and edited version."
    (let ((saved-window-config (current-window-configuration))
          (original-buffer (find-file-noselect file-name)))
      (letrec ((cleanup-fn (lambda ()
                             (when (buffer-live-p original-buffer)
                               (with-current-buffer original-buffer
                                 (save-buffer)))
                             (set-window-configuration saved-window-config)
                             (remove-hook 'ediff-quit-hook cleanup-fn))))
        (add-hook 'ediff-quit-hook cleanup-fn)
        (ediff-buffers original-buffer edit-buffer))))

  (defun my/validate-string-uniqueness (search-string buffer)
    "Validate that SEARCH-STRING appears exactly once in BUFFER.
Returns (unique-p . context-info)"
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((matches '())
              (match-count 0)
              (case-fold-search nil))

          ;; Find all matches for exact string
          (while (search-forward search-string nil t)
            (setq match-count (1+ match-count))
            (let* ((match-end (point))
                   (match-start (- match-end (length search-string)))
                   (line-number (line-number-at-pos match-start)))
              (push (cons line-number match-start) matches)))

          ;; If no exact matches, try with trimmed trailing whitespace
          (when (= match-count 0)
            (let ((trimmed-search-string (my/trim-trailing-whitespace-multiline search-string)))
              (when (not (string= search-string trimmed-search-string))
                (goto-char (point-min))
                (while (search-forward trimmed-search-string nil t)
                  (setq match-count (1+ match-count))
                  (let* ((match-end (point))
                         (match-start (- match-end (length trimmed-search-string)))
                         (line-number (line-number-at-pos match-start)))
                    (push (cons line-number match-start) matches))))))

          (cond
           ((= match-count 1)
            ;; Exactly one match - success
            (cons t nil))

           ((= match-count 0)
            ;; No matches
            (cons nil (format "String '%s' not found in file" search-string)))

           (t
            ;; Multiple matches - collect context
            (let ((context-info (my/collect-match-context (reverse matches) search-string)))
              (cons nil (format "Found %d matches for '%s'. Please provide a more specific string.\n\n%s"
                                match-count search-string context-info)))))))))

  (defun my/collect-match-context (matches search-string)
    "Collect 2-3 lines of context around each match position.
MATCHES is a list of (line-number . position) pairs."
    (mapconcat
     (lambda (match)
       (let ((line-number (car match))
             (position (cdr match)))
         (save-excursion
           (goto-char position)
           (let* ((context-start (save-excursion
                                   (forward-line -2)
                                   (point)))
                  (context-end (save-excursion
                                 (forward-line 3)
                                 (point)))
                  (context-text (buffer-substring-no-properties context-start context-end)))
             (format "Line %d:\n%s" line-number context-text)))))
     matches
     "\n"))

  (defun my/prompt-rejection-reason ()
    "Prompt user for reason when rejecting changes."
    (condition-case nil
        (let ((reason (read-string "Reason for rejecting changes: ")))
          (if (string= reason "")
              "No reason provided"
            reason))
      (quit "User cancelled reason input")))

  (defun my/setup-ediff-completion-handler (callback summary edit-buffer original-buffer file-name)
    "Setup ediff quit hook to handle completion and call CALLBACK."
    (let ((saved-window-config (current-window-configuration)))
      (letrec ((cleanup-fn
                (lambda ()
                  ;; Remove hook first to prevent recursion
                  (remove-hook 'ediff-quit-hook cleanup-fn)

                  ;; Detect if changes were saved
                  (let ((changes-saved
                         (with-current-buffer original-buffer
                           (not (buffer-modified-p)))))

                    ;; Restore window configuration
                    (set-window-configuration saved-window-config)

                    ;; Kill the edit buffer
                    (when (buffer-live-p edit-buffer)
                      (kill-buffer edit-buffer))

                    ;; Prepare final message and call callback
                    (if changes-saved
                        (progn
                          ;; Save the file - redundant
                          ;; (with-current-buffer original-buffer
                          ;;   (save-buffer))
                          (funcall callback (format "%s\n\nChanges saved to %s" summary file-name)))

                      ;; Changes rejected - prompt for reason
                      (let ((rejection-reason (my/prompt-rejection-reason)))
                        (funcall callback (format "Changes rejected by user: %s" rejection-reason))))))))

        ;; Add the cleanup function to ediff quit hook
        (add-hook 'ediff-quit-hook cleanup-fn))))

  (defun my/gptel--edit_file (callback file-path file-edits)
    "Asynchronously edit FILE-PATH by applying FILE-EDITS with uniqueness validation."
    (if (not (and file-path (not (string= file-path "")) file-edits))
        (funcall callback "Error: Invalid parameters - file-path and file-edits are required")

      (let ((file-name (expand-file-name file-path)))
        ;; Validation checks
        (cond
         ((not (file-exists-p file-name))
          (funcall callback (format "Error: File does not exist: %s" file-name)))
         ((not (file-readable-p file-name))
          (funcall callback (format "Error: File is not readable: %s" file-name)))
         ((not (file-writable-p file-name))
          (funcall callback (format "Error: File is not writable: %s" file-name)))
         (t
          ;; Proceed with editing
          (let* ((base-buffer-name (format "*edit-file %s*" (file-name-nondirectory file-name)))
                 (buffer-name (generate-new-buffer-name base-buffer-name))
                 (edit-buffer (get-buffer-create buffer-name))
                 (original-buffer (find-file-noselect file-name))
                 (edit-results '())
                 (successful-edits 0)
                 (failed-edits 0))

            (with-current-buffer edit-buffer
              (condition-case err
                  (insert-file-contents file-name)
                (error (funcall callback (format "Error: Failed to read file contents: %s" (error-message-string err)))
                       (return)))

              ;; First pass: validate all edits for uniqueness
              (let ((validation-failed nil)
                    (edit-index 0))

                (dolist (file-edit (seq-into file-edits 'list))
                  (let* ((old-string (plist-get file-edit :old_string))
                         (new-string (plist-get file-edit :new_string)))

                    (cond
                     ;; Check for required fields
                     ((not (and old-string new-string))
                      (funcall callback (format "Error: Missing required fields (old_string, new_string) in edit %d" edit-index))
                      (setq validation-failed t)
                      (return))

                     ;; Check for empty old_string
                     ((string= old-string "")
                      (funcall callback (format "Error: old_string cannot be empty in edit %d" edit-index))
                      (setq validation-failed t)
                      (return))

                     ;; Validate uniqueness
                     (t
                      (let ((validation-result (my/validate-string-uniqueness old-string edit-buffer)))
                        (unless (car validation-result)
                          (funcall callback (format "Error in edit %d: %s" edit-index (cdr validation-result)))
                          (setq validation-failed t)
                          (return)))))

                    (setq edit-index (1+ edit-index))))

                ;; If validation failed, we already called callback, so return
                (when validation-failed
                  (when (buffer-live-p edit-buffer)
                    (kill-buffer edit-buffer))
                  (return)))

              ;; All validations passed - apply edits sequentially
              (dolist (file-edit (seq-into file-edits 'list))
                (let* ((old-string (plist-get file-edit :old_string))
                       (new-string (plist-get file-edit :new_string)))

                  ;; Apply the edit using the existing replacement function
                  (save-excursion
                    (goto-char (point-min))
                    (let ((replacement-result (my/attempt-string-replacement old-string new-string (point-min) 1)))
                      (if (car replacement-result)
                          (progn
                            (setq successful-edits (1+ successful-edits))
                            (push (list :old_string old-string
                                        :new_string new-string
                                        :result (cdr replacement-result)) edit-results))
                        (progn
                          (setq failed-edits (1+ failed-edits))
                          (push (list :old_string old-string
                                      :new_string new-string
                                      :result (cdr replacement-result)) edit-results))))))))

            ;; Generate summary message
            (let ((summary (if (> successful-edits 0)
                               (if (= failed-edits 0)
                                   (format "Successfully applied all %d edits" successful-edits)
                                 (format "Applied %d/%d edits" successful-edits (+ successful-edits failed-edits)))
                             "Failed to apply any edits")))

              (if (> successful-edits 0)
                  ;; Show ediff and wait for completion
                  (progn
                    (my/setup-ediff-completion-handler callback summary edit-buffer original-buffer file-name)
                    (ediff-buffers original-buffer edit-buffer))

                ;; No successful edits - call callback immediately
                (progn
                  (when (buffer-live-p edit-buffer)
                    (kill-buffer edit-buffer))
                  (let ((failure-details
                         (mapconcat (lambda (result)
                                      (format "  %s -> %s: %s"
                                              (plist-get result :old_string)
                                              (plist-get result :new_string)
                                              (plist-get result :result)))
                                    (reverse edit-results) "\n")))
                    (funcall callback (format "%s\n\nFailure details:\n%s" summary failure-details))))))))))))

  (gptel-make-tool
   :function #'my/gptel--edit_file
   :name "edit_file"
   :description "Edit file by finding and replacing strings. Each old_string must have exactly one match in the file - if multiple matches are found, the tool will fail with context around each match to help you choose a more specific string. Supports both single-line and multi-line string replacements."
   :args (list '(:name "file-path"
                       :type string
                       :description "The full path of the file to edit")
               '(:name "file-edits"
                       :type array
                       :items (:type object
                                     :properties
                                     (:old_string
                                      (:type string :description "Exact string to find and replace (must be unique in file)")
                                      :new_string
                                      (:type string :description "Replacement string")))
                       :description "List of edits to apply"))
   :category "filesystem"
   :async t
   :confirm t)

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
   :name "run_command"
   :description "Run an command."
   :args (list
          '(:name "command"
                  :type "string"
                  :description "Command to run."))
   :category "filesystem"
   :async t
   :include t
   :confirm t)

  (gptel-make-tool
   :function (lambda (pattern directory file_type max_depth max_results)
               (let ((cmd (format "fd %s %s %s %s %s"
                                  (shell-quote-argument (or pattern ""))
                                  (if (and file_type (not (string-empty-p file_type)))
                                      (format "--type %s" (shell-quote-argument file_type))
                                    "")
                                  (if max_depth
                                      (format "--max-depth %d" max_depth)
                                    "")
                                  (if max_results
                                      (format "--max-results %d" max_results)
                                    "")
                                  (shell-quote-argument (expand-file-name directory))))
                     (default-directory (expand-file-name directory)))
                 (with-temp-message (format "Finding files matching: %s" (or pattern "any file"))
                   (let ((output (shell-command-to-string cmd)))
                     (if (string-empty-p output)
                         "No files found matching the criteria."
                       output)))))
   :name "find_files"
   :description "Find files matching a pattern using fd (modern find replacement)"
   :args (list '(:name "pattern"
                       :type string
                       :description "Search pattern for file names (leave empty to find all files)")
               '(:name "directory"
                       :type string
                       :description "Directory to search in. Supports relative paths and ~.")
               '(:name "file_type"
                       :type string
                       :description "Optional file type filter: 'f' for files, 'd' for directories, 'l' for symlinks")
               '(:name "max_depth"
                       :type integer
                       :description "Optional maximum directory depth to search")
               '(:name "max_results"
                       :type integer
                       :description "Optional maximum number of results to return"))
   :category "filesystem")

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

  ;; Code Search Tools

  (gptel-make-tool
   :function (lambda (pattern directory file_pattern max_results)
               (let ((cmd (format "rg --line-number --no-heading --with-filename %s \"%s\" %s %s"
                                  (if max_results (format "--max-count=%d" max_results) "")
                                  (shell-quote-argument pattern)
                                  (if (and file_pattern (not (string-empty-p file_pattern)))
                                      (format "-g \"%s\"" (shell-quote-argument file_pattern))
                                    "")
                                  (shell-quote-argument (expand-file-name directory))))
                     (default-directory (expand-file-name directory)))
                 (with-temp-message (format "Searching for: %s" pattern)
                   (let ((output (shell-command-to-string cmd)))
                     (if (string-empty-p output)
                         "No matches found."
                       output)))))
   :name "search_code"
   :description "Search for code or text patterns in files using ripgrep (rg)"
   :args (list '(:name "pattern"
                       :type string
                       :description "Search pattern to find in code files")
               '(:name "directory"
                       :type string
                       :description "Directory to search in. Supports relative paths and ~.")
               '(:name "file_pattern"
                       :type string
                       :description "Optional glob pattern to filter files to search (e.g. '*.py' for Python files)")
               '(:name "max_results"
                       :type integer
                       :description "Optional maximum number of results per file"))
   :category "code-search")

  ;; Tmux Interaction Tools

  ;; Variables
  (defvar my/tmux-session-name "emacs-llm-session"
    "Name of the persistent tmux session for LLM interactions.")

  (defvar my/tmux-last-marker nil
    "Last completion marker used for command detection.")

  (defvar my/tmux-default-lines 50
    "Default number of lines to return from tmux output.")

  (defvar my/tmux-default-wait-time 10
    "Default wait time in seconds for tmux_send operations.")

  (defvar my/tmux-read-default-wait-time 2
    "Default wait time in seconds for tmux_read operations.")

  (defvar my/tmux-max-output-size 10240
    "Maximum output size in characters to prevent context overflow.")

  ;; Session management
  (defun my/tmux-session-exists-p ()
    "Check if the tmux session exists."
    (= 0 (call-process "tmux" nil nil nil "has-session" "-t" my/tmux-session-name)))

  (defun my/tmux-create-session ()
    "Create a new tmux session with zsh."
    (let ((result (call-process "tmux" nil nil nil
                                "new-session" "-d" "-s" my/tmux-session-name
                                "/usr/bin/zsh")))
      (= 0 result)))

  (defun my/tmux-ensure-session ()
    "Ensure tmux session exists, create if necessary."
    (unless (my/tmux-session-exists-p)
      (unless (my/tmux-create-session)
        (error "Failed to create tmux session: %s" my/tmux-session-name))))

  (defun my/tmux-cleanup-session ()
    "Clean up tmux session on Emacs exit."
    (when (my/tmux-session-exists-p)
      (call-process "tmux" nil nil nil "kill-session" "-t" my/tmux-session-name)))

  ;; Output processing
  (defun my/tmux-strip-ansi (text)
    "Remove ANSI escape sequences from TEXT."
    (replace-regexp-in-string "\x1b\\[[0-9;]*[mGKHJABCDEFghlnrstuv]" "" text))

  (defun my/tmux-clean-output (output)
    "Remove all tmux interaction artifacts from OUTPUT.
This includes:
- EMACS_MARKER=....; prefix
- ; echo \"COMPLETED: ...\" suffix
- COMPLETED: EMACS_MARKER_... lines"
    (let ((cleaned output))
      ;; Remove EMACS_MARKER=EMACS_MARKER_XXXXXXXXX_XXXX; prefix
      (setq cleaned (replace-regexp-in-string
                     "EMACS_MARKER=EMACS_MARKER_[0-9_]+; " "" cleaned))

      ;; Remove ; echo "COMPLETED: $EMACS_MARKER" suffix
      (setq cleaned (replace-regexp-in-string
                     "; echo \"COMPLETED: \\$EMACS_MARKER\"" "" cleaned))

      ;; Remove COMPLETED: EMACS_MARKER_XXXXXXXXX_XXXX lines
      (setq cleaned (replace-regexp-in-string
                     ".*COMPLETED: EMACS_MARKER_[0-9_]+.*\n?" "" cleaned))

      cleaned))

  (defun my/tmux-capture-output (&optional lines)
    "Capture and process tmux pane output."
    (let ((lines (or lines my/tmux-default-lines))
          (temp-buffer (generate-new-buffer " *tmux-capture*")))
      (unwind-protect
          (progn
            (let ((result (call-process "tmux" nil temp-buffer nil
                                        "capture-pane" "-t" my/tmux-session-name
                                        "-p" "-S" (format "-%d" lines))))
              (if (= 0 result)
                  (with-current-buffer temp-buffer
                    (let ((output (buffer-substring-no-properties (point-min) (point-max))))
                      (my/tmux-strip-ansi output)))
                (error "Failed to capture tmux output"))))
        (kill-buffer temp-buffer))))

  (defun my/tmux-send-text (text)
    "Send TEXT to the tmux session."
    (let ((result (call-process "tmux" nil nil nil
                                "send-keys" "-t" my/tmux-session-name
                                text "Enter")))
      (= 0 result)))

  (defun my/tmux-send-text-no-enter (text)
    "Send TEXT to the tmux session without pressing Enter."
    (let ((result (call-process "tmux" nil nil nil
                                "send-keys" "-t" my/tmux-session-name
                                text)))
      (= 0 result)))

  ;; Password detection and handling
  (defun my/tmux-detect-password-prompt (output)
    "Detect if OUTPUT contains a password prompt."
    (string-match-p "\\(?:[Pp]assword\\|[Pp]assphrase\\).*:" output))

  (defun my/tmux-handle-password-prompt (callback)
    "Handle password prompt by requesting user input."
    (let ((password (read-passwd "Password required for tmux command: ")))
      (when password
        (my/tmux-send-text-no-enter password)
        (my/tmux-send-text-no-enter "")  ; Send Enter
        (setq password nil)  ; Clear password
        ;; Wait a bit and then capture output
        (run-with-timer 1 nil
                        (lambda ()
                          (let ((output (my/tmux-capture-output my/tmux-default-lines)))
                            (funcall callback (format "Password provided. Current output:\n%s" output))))))))

  ;; Command completion detection
  (defun my/tmux-generate-marker ()
    "Generate a unique completion marker."
    (format "EMACS_MARKER_%s_%d"
            (format-time-string "%s")
            (random 10000)))

  (defun my/tmux-check-for-marker (marker)
    "Check if MARKER appears in current tmux output."
    (let ((output (my/tmux-capture-output 10)))  ; Check last 10 lines for marker
      (string-match-p (regexp-quote (concat "COMPLETED: " marker)) output)))

  (defun my/tmux-wait-for-completion (marker wait-time callback lines)
    "Wait for completion marker or timeout, then call CALLBACK."
    (let ((start-time (current-time))
          (poll-timer nil))
      (setq poll-timer
            (run-with-timer
             0.1 0.1
             (lambda ()
               (let ((elapsed (float-time (time-subtract (current-time) start-time))))
                 (cond
                  ;; Marker found - command completed
                  ((my/tmux-check-for-marker marker)
                   (cancel-timer poll-timer)
                   (let ((output (my/tmux-capture-output lines)))
                     ;; Remove the marker line from output
                     (setq output (my/tmux-clean-output output))
                     (funcall callback (my/tmux-format-output output "completed" lines))))

                  ;; Timeout reached
                  ((>= elapsed wait-time)
                   (cancel-timer poll-timer)
                   (let ((output (my/tmux-capture-output lines)))
                     (if (my/tmux-detect-password-prompt output)
                         (my/tmux-handle-password-prompt callback)
                       (funcall callback (my/tmux-format-output
                                          output "timeout" lines
                                          (format "Command timed out after %d seconds. Use tmux_read to check current status." wait-time)))))))))))))

  ;; Output formatting
  (defun my/tmux-format-output (output status lines &optional message)
    "Format output with metadata."
    (let* ((output-lines (split-string output "\n"))
           (actual-lines (length (remove "" output-lines)))
           (truncated (> (length output) my/tmux-max-output-size))
           (final-output (if truncated
                             (substring output 0 my/tmux-max-output-size)
                           output)))
      (concat
       (when message (concat message "\n\n"))
       "=== Command Output ===\n"
       final-output
       (when (string-suffix-p "\n" final-output) "" "\n")
       "\n=== Status ===\n"
       (format "Status: %s\nLines returned: %d\nTruncated: %s"
               status actual-lines truncated))))

  ;; Main tool functions
  (defun my/tmux-send-command (callback text &optional lines wait-time)
    "Send TEXT to tmux session and return output via CALLBACK."
    (condition-case error
        (progn
          (my/tmux-ensure-session)
          (let ((lines (or lines my/tmux-default-lines))
                (wait-time (or wait-time my/tmux-default-wait-time))
                (marker (my/tmux-generate-marker)))

            (setq my/tmux-last-marker marker)

            ;; Send the command with completion marker (EMACS_MARKER used as a way to avoid regexp matching the string)
            (unless (my/tmux-send-text (format "EMACS_MARKER=%s; %s; echo \"COMPLETED: $EMACS_MARKER\"" marker text))
              (error "Failed to send command to tmux"))

            ;; Wait for completion
            (my/tmux-wait-for-completion marker wait-time callback lines)))
      (error
       (funcall callback (format "Error in tmux_send: %s" error)))))

  (defun my/tmux-read-output (callback &optional lines wait-time)
    "Read current tmux output and return via CALLBACK."
    (condition-case error
        (progn
          (my/tmux-ensure-session)
          (let ((lines (or lines my/tmux-default-lines))
                (wait-time (or wait-time my/tmux-read-default-wait-time)))

            ;; Simple wait then capture
            (run-with-timer
             wait-time nil
             (lambda ()
               (let ((output (my/tmux-capture-output lines)))
                 ;; Remove the marker line from output
                 (setq output (my/tmux-clean-output output))
                 (if (my/tmux-detect-password-prompt output)
                     (my/tmux-handle-password-prompt callback)
                   (funcall callback (my/tmux-format-output output "read" lines))))))))
      (error
       (funcall callback (format "Error in tmux_read: %s" error)))))

  ;; Cleanup hook
  (add-hook 'kill-emacs-hook #'my/tmux-cleanup-session)

  ;; Tool definitions
  (gptel-make-tool
   :function #'my/tmux-send-command
   :name "tmux_send"
   :description "Send command or text to persistent tmux session and return output. Use this for executing commands and interacting with processes. For commands that take longer than expected, use tmux_read with appropriate wait_time rather than re-running commands."
   :args (list
          '(:name "text"
                  :type "string"
                  :description "Command or text to send to the shell")
          '(:name "lines"
                  :type "integer"
                  :description "Maximum number of output lines to return (default: 50). Tool returns only actual output lines if fewer than requested.")
          '(:name "wait_time"
                  :type "integer"
                  :description "Maximum seconds to wait before capturing output (default: 10). Tool returns early if discrete command completes."))
   :category "shell"
   :async t
   :include t
   :confirm t)

  (gptel-make-tool
   :function #'my/tmux-read-output
   :name "tmux_read"
   :description "Read current output from tmux session without sending commands. Use this when you need to check output after waiting longer, or when you need more lines than initially captured. Prefer single tmux_read with appropriate wait_time over multiple rapid calls."
   :args (list
          '(:name "lines"
                  :type "integer"
                  :description "Maximum number of output lines to return (default: 50)")
          '(:name "wait_time"
                  :type "integer"
                  :description "Seconds to wait before capturing output (default: 2)"))
   :category "shell"
   :async t
   :include t
   :confirm t))

(use-package gptel-gh
  :after gptel
  :defer t
  :custom
  (gptel-backend (gptel-make-gh-copilot "Github Copilot"
                   :host "api.business.githubcopilot.com"))
  (gptel-model 'claude-sonnet-4.5))

;; Built-in code folding
(use-package hideshow
  :hook (prog-mode . hs-minor-mode))

(use-package hl-line
  :defer t
  ;; :hook (text-mode prog-mode)
  :custom
  (hl-line-sticky-flag nil))

(use-package html-ts-mode
  :config
  ;; Mark dot character as a punctuation character (symbol separator)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
  ;; https://emacs.stackexchange.com/a/19564
  (modify-syntax-entry ?. "." html-ts-mode-syntax-table))

(use-package jira
  :ensure t
  :pin melpa
  :defer t
  :custom
  (jira-base-url "https://jira.ringieraxelspringer.pl")
  (jira-token-is-personal-access-token t)
  (jira-api-version 2))

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

;; (use-package markdown-mode
;;   :ensure t
;;   :pin melpa
;;   :mode ("\\.md\\'" . gfm-mode))

(use-package markdown-ts-mode
  :config
  (add-to-list 'markdown-ts--code-block-language-map '("js" . javascript))
  (add-to-list 'markdown-ts--code-block-language-map '("scss" . css)))

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
            (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
            (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
            (rust "https://github.com/tree-sitter/tree-sitter-rust")
            (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
            (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
            (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

  (add-to-list 'auto-mode-alist '("\\.astro\\'" . astro-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jqtpl\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-ts-mode))
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
