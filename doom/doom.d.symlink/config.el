;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;; Identity & Appearance

(setq user-full-name "David Zmick"
      user-mail-address "david@dpzmick.com")

(setq doom-font (font-spec :family "Source Code Pro" :size 12))

(setq base16-theme-256-color-source "colors")
(setq doom-theme 'base16-tomorrow-night)

;; make line numbers not look terrible
;; "color-18" is a base16-shell extended palette color — only valid when
;; base16-shell has configured the terminal's color slots.
(set-face-inverse-video 'line-number-current-line nil)
(set-face-foreground 'line-number-current-line "orange")
(when (display-graphic-p)
  (set-face-background 'line-number-current-line "color-18"))

(setq display-line-numbers-type t)

;; make comment delimiters not invisible
(set-face-foreground 'font-lock-comment-delimiter-face "brightblack")

(setq fill-column 80)
(global-display-fill-column-indicator-mode)

;;;; General Editor Tweaks

;; OSC-52 clipboard via clipetty (enabled by tty +osc module).
;; Allows yank to reach system clipboard through tmux/ssh.

;;;; Evil

(setq-default evil-escape-key-sequence "jk")

;; doom disables both of these indicators in goggles, but I like them
(use-package! evil-goggles
  :init
  (setq evil-goggles-enable-delete t
        evil-goggles-enable-change t))

(use-package! evil
  :config
  ;; bind the jumping around keys to meta
  ;; since I want to be able to use tab in a terminal
  (define-key evil-normal-state-map (kbd "M-i") 'evil-jump-forward)
  (define-key evil-normal-state-map (kbd "M-o") 'evil-jump-backward)
  (evil-set-toggle-key "M-!"))

;;;; Org

;; org-directory, org-agenda-files, org-books-file set in config-local.el

(after! org
  ;; try and disable all of the visual quirks that doom turns on
  (setq org-hide-leading-stars nil)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented nil)

  (setq org-log-done 'time)  ; Timestamp when TODOs completed

  ;; org-capture-templates set in config-local.el

  (setq org-agenda-custom-commands
        '(("h" "Habits & Unscheduled"
           ((agenda "" ((org-agenda-span 3)
                        (org-agenda-show-all-dates nil)
                        (org-agenda-time-grid nil)
                        (org-agenda-start-day nil)))
            (alltodo "" ((org-agenda-overriding-header "Unscheduled Tasks")
                         (org-agenda-todo-ignore-scheduled 'all)
                         (org-agenda-todo-ignore-deadlines 'all)))))))

  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers)

  ;; Diary integration set up in config-local.el
  )

(use-package! org-habit
  :after org
  :config
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-show-habits-only-for-today nil
        org-habit-following-days 3
        org-habit-preceding-days 14
        org-habit-graph-column 50))

;; Make j/k/0/$ move by *visual* (wrapped) lines, but only in Org buffers.
(after! evil
  (defun my/org-evil-visual-line-motions ()
    (map! :map org-mode-map
          :n "j" #'evil-next-visual-line
          :n "k" #'evil-previous-visual-line
          :n "0" #'evil-beginning-of-visual-line
          :n "$" #'evil-end-of-visual-line))

  (add-hook 'org-mode-hook #'my/org-evil-visual-line-motions))

;;;; TRAMP

(after! tramp
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Aggressive caching — safe when you're the only editor on the remote
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-completion-reread-directory-timeout nil)

  ;; Auto-save and backups to local machine so they never stall
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-autosave/" doom-cache-dir))
  (setq tramp-backup-directory-alist
        `(("." . ,(expand-file-name "tramp-backup/" doom-cache-dir))))
  (setq remote-file-name-inhibit-auto-save-visited t)
  (setq remote-file-name-inhibit-locks t)

  ;; Large files use out-of-band (scp) transfer
  (setq tramp-copy-size-limit 1000000)

  ;; Disable VC on remote files — huge speedup, use magit manually
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

;; tramp-rpc: faster RPC-based file operations (requires Emacs 30.1+)
(use-package! tramp-rpc
  :after tramp)

;; Projectile: don't stall on remote
(after! projectile
  (setq projectile-enable-caching t)
  (setq projectile-mode-line-function
        (lambda ()
          (if (file-remote-p default-directory)
              " Proj[remote]"
            (format " Proj[%s]" (projectile-project-name))))))

;; Project search on remote: use rg on the remote if available, else grep.
;; Local rg can't search remote files, so we must run the tool remotely.
(defadvice! my/search-project-remote (orig-fn &rest args)
  :around #'+default/search-project
  (if (file-remote-p default-directory)
      (let ((dir (or (doom-project-root) default-directory)))
        (if (executable-find "rg" t)
            (consult-ripgrep dir)
          (consult-grep dir)))
    (apply orig-fn args)))

;; recentf: don't probe dead TRAMP connections on cleanup
(after! recentf
  (add-to-list 'recentf-keep #'file-remote-p))

;;;; Tmux pane navigation
;; Only activate when running inside tmux (terminal emacs).
;; C-h/j/k/l move between emacs windows; at the edge, tmux-pane
;; tells tmux to switch to the adjacent tmux pane instead.
;; The tmux side (tmux.conf) has matching binds that detect when
;; the active pane is running emacs and forwards the keys to us.
;;
;; We bind directly in evil states rather than using tmux-pane-mode's
;; override keymap, which conflicts with C-h (help prefix) and causes
;; input delay from key sequence timeout.

(use-package! tmux-pane
  :when (getenv "TMUX")
  :config
  (map! :nvi "C-h" #'tmux-pane-omni-window-left
        :nvi "C-j" #'tmux-pane-omni-window-down
        :nvi "C-k" #'tmux-pane-omni-window-up
        :nvi "C-l" #'tmux-pane-omni-window-right))

;;;; Language Modes

(after! cc-mode
  (setq-default c-basic-offset 2))

;;;; Packages

(setq monet-diff-tool #'monet-simple-diff-tools)

(use-package! emacs-mcp-server
  :defer t)

(let ((cache (make-hash-table :test 'equal)))
  (defun my/op-read (item field)
    "Read a secret from 1Password, caching per session."
    (when noninteractive
      (error "my/op-read called in batch mode"))
    (let ((key (format "%s/%s" item field)))
      (or (gethash key cache)
          (puthash key
                   (string-trim (shell-command-to-string
                                 (format "op read \"op://Private/%s/%s\"" item field)))
                   cache)))))

;; Declare backend/model vars — set by config-local.el
(defvar my/ai-commentary-backend nil
  "Backend for AI commentary (fast, no thinking).")
(defvar my/ai-commentary-model nil
  "Model symbol for AI commentary.")
(defvar my/ai-complete-backend nil
  "Backend for AI completion (no thinking).")
(defvar my/ai-complete-model nil
  "Model symbol for AI completion.")

(after! gptel
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (setq gptel-display-buffer-action nil)
  (setq gptel-default-mode 'org-mode)

  ;; Remove the default ChatGPT backend
  (setq gptel--known-backends nil)

  ;; gptel-backend, gptel-model, my/ai-commentary-backend,
  ;; my/ai-complete-backend are all set in config-local.el
  )

;;;; gptel-agent

(after! gptel
  ;; Register ask_user tool so the agent can ask multiple-choice questions
  (gptel-make-tool
   :name "AskUser"
   :function (lambda (question options)
               (completing-read (format "%s " question)
                                (append options nil)))
   :description "Ask the user a clarifying question with multiple choice options.
Use this BEFORE writing code when the implementation is ambiguous.
Good questions: error handling approach, naming conventions, edge cases,
which algorithm/pattern to use. Keep options short (2-5 words each)."
   :args '((:name "question"
            :type string
            :description "The question to ask the user")
           (:name "options"
            :type array
            :description "2-4 short options to choose from"
            :items (:type string)))
   :category "gptel-agent"
   :confirm t)

  (gptel-make-tool
   :name "ReadBuffer"
   :function (lambda (buffer-name)
               (if-let ((buf (get-buffer buffer-name)))
                   (with-current-buffer buf
                     (buffer-substring-no-properties (point-min) (point-max)))
                 (format "Buffer '%s' not found. Open buffers: %s"
                         buffer-name
                         (mapconcat #'buffer-name
                                    (seq-filter #'buffer-file-name (buffer-list))
                                    ", "))))
   :description "Read the current content of an open Emacs buffer, including unsaved changes.
Prefer this over Read when the user may have unsaved edits. The buffer name
is typically the filename (e.g. 'test.py', 'config.el')."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer (e.g. 'test.py')"))
   :category "gptel-agent")

  (gptel-make-tool
   :name "EditBuffer"
   :function (lambda (buffer-name old-str new-str)
               (let ((buf (get-buffer buffer-name)))
                 (if (not buf)
                     (format "Buffer '%s' not found" buffer-name)
                   (with-current-buffer buf
                     (let ((pos (progn (goto-char (point-min))
                                       (search-forward old-str nil t))))
                       (if pos
                           (progn
                             (delete-region (- pos (length old-str)) pos)
                             (goto-char (- pos (length old-str)))
                             (insert new-str)
                             (format "Replaced in buffer %s" buffer-name))
                         (format "Could not find exact text in buffer %s" buffer-name)))))))
   :description "Replace text in an open Emacs buffer. Changes appear immediately,
support undo, and do not require saving. Prefer this over Edit for files the user
has open. The old_str must match exactly (including whitespace/indentation)."
   :args '((:name "buffer_name"
            :type string
            :description "Name of the buffer (e.g. 'test.py')")
           (:name "old_str"
            :type string
            :description "Exact text to find and replace")
           (:name "new_str"
            :type string
            :description "Replacement text"))
   :category "gptel-agent"
   :confirm t))

(defun my/ai-implement ()
  "Launch gptel-agent to implement the selected code or defun at point."
  (interactive)
  (require 'gptel)
  (require 'gptel-agent)
  ;; Save buffer so Read/Edit tools see current content
  (when (and (buffer-file-name) (buffer-modified-p))
    (save-buffer))
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'defun t)))
         (file (or (buffer-file-name) (buffer-name)))
         (line (line-number-at-pos))
         (lang (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
         (prompt (format "Implement the stub at line %d of %s (buffer: %s).
Use ReadBuffer to check surrounding code for context if needed.
Use EditBuffer to make changes.

```%s
%s
```" line file (buffer-name) lang (or code "(no code at point)")))
         ;; Create the gptel buffer directly (returns the buffer)
         (buf (gptel (generate-new-buffer-name "*gptel-implement*")
                     nil nil nil)))
    ;; Set up agent environment in the buffer
    (with-current-buffer buf
      (setq default-directory default-directory)
      (gptel-agent-update)
      (gptel--apply-preset
       'gptel-agent
       (lambda (sym val) (set (make-local-variable sym) val)))
      ;; Inject custom tools (not in the preset snapshot)
      (cl-pushnew (gptel-get-tool "AskUser") gptel-tools :test #'equal)
      (cl-pushnew (gptel-get-tool "ReadBuffer") gptel-tools :test #'equal)
      (cl-pushnew (gptel-get-tool "EditBuffer") gptel-tools :test #'equal)
      (setq-local gptel--system-message
                  "You are a code implementation assistant working inside Emacs.

Your job: implement code stubs, TODOs, and function skeletons.

Workflow:
1. Read the code the user provides. Use ReadBuffer to check surrounding context if needed.
2. If the intent is clear, just implement it directly with EditBuffer. Most code is obvious — a sum function sums, a sort function sorts. DO NOT ask questions when the answer is obvious.
3. Only use AskUser when there is a genuine design decision with multiple reasonable approaches (e.g. error handling strategy, choice of algorithm, ambiguous requirements). Simple functions should never trigger questions.
4. Use EditBuffer to implement the code in-place.

Rules:
- Match the style of surrounding code exactly
- Keep implementations minimal and correct
- Use ReadBuffer/EditBuffer instead of Read/Edit for files the user has open (supports unsaved changes and undo)
- Do NOT add comments unless the logic is non-obvious
- Default to the simplest reasonable implementation")
      (setq-local gptel-max-tokens 8192)
      (goto-char (point-max))
      (insert prompt))
    ;; Show the buffer and send
    (pop-to-buffer buf)
    (evil-local-set-key 'normal "q" #'quit-window)
    (gptel-send)))

(defvar my/ai-complete-buffer "*AI Complete*")

(defun my/ai-complete ()
  "Complete code at point using Claude. With region, replaces it."
  (interactive)
  (require 'gptel)
  (let* ((gptel-backend my/ai-complete-backend)
         (gptel-model my/ai-complete-model)
         (has-region (use-region-p))
         (region-beg (and has-region (region-beginning)))
         (region-end-marker (and has-region (copy-marker (region-end))))
         (marker (if has-region (copy-marker region-beg) (point-marker)))
         (progress-buf (let ((buf (get-buffer-create my/ai-complete-buffer)))
                         (with-current-buffer buf (erase-buffer)) buf))
         (code-before (buffer-substring-no-properties
                      (save-excursion (goto-char (or region-beg (point)))
                                      (forward-line -5) (point))
                      (or region-beg (point))))
         (code-after (buffer-substring-no-properties
                      (or (and has-region (region-end)) (point))
                      (save-excursion (goto-char (or (and has-region (region-end)) (point)))
                                      (forward-line 5) (point))))
         (selected (and has-region
                       (buffer-substring-no-properties region-beg (region-end))))
         (lang (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
         (file (or (buffer-file-name) (buffer-name)))
         (prompt (if has-region
                     (format "Replace ONLY the selected region. Return ONLY the replacement.
Do NOT include surrounding code in your output.

File: %s (buffer: %s)
Immediately before selection:
```%s
%s
```
SELECTED (replace this):
```%s
%s
```
Immediately after selection:
```%s
%s
```" file (buffer-name) lang code-before lang selected lang code-after)
                   (format "Insert code at <CURSOR>. Return ONLY the new code.

File: %s (buffer: %s)
```%s
%s<CURSOR>%s
```" file (buffer-name) lang code-before code-after))))
    ;; Show progress window
    (with-current-buffer progress-buf
      (erase-buffer)
      (insert "Completing..."))
    (display-buffer progress-buf
                    '(display-buffer-in-side-window (side . bottom) (slot . 0) (window-height . 4)))
    ;; Non-streaming: one callback with full response
    (gptel-request prompt
      :system "You are a code completion engine. Return ONLY the raw code to replace the selected region. Rules:
- Match the exact indentation level of surrounding code
- Include any necessary imports
- Do NOT wrap in markdown code fences
- Do NOT add any explanation
- Just output the literal replacement code, nothing else"
      :callback (lambda (response _info)
                  (if (stringp response)
                      (let ((clean (replace-regexp-in-string
                                    "\\`\\s-*```[a-z]*\n?" ""
                                    (replace-regexp-in-string
                                     "\n?```\\s-*\\'" "" response))))
                        ;; Show result in progress buffer briefly
                        (with-current-buffer progress-buf
                          (erase-buffer)
                          (insert clean))
                        ;; Insert into source buffer (replace region if active)
                        (with-current-buffer (marker-buffer marker)
                          (save-excursion
                            (goto-char marker)
                            (when region-end-marker
                              (delete-region marker region-end-marker))
                            (insert clean)))
                        ;; Close progress window after 1.5s
                        (run-at-time 1.5 nil
                                     (lambda ()
                                       (when-let ((win (get-buffer-window my/ai-complete-buffer t)))
                                         (delete-window win)))))
                    ;; Error case
                    (with-current-buffer progress-buf
                      (erase-buffer)
                      (insert (format "Error: %s" response))))))))

;;;; AI Commentary

(defvar my/ai-commentary-buffer "*AI Commentary*")

(defun my/ai--show-response (response)
  "Display RESPONSE in the AI commentary buffer."
  (when (stringp response)
    (with-current-buffer (get-buffer-create my/ai-commentary-buffer)
      (erase-buffer)
      (markdown-mode)
      (insert response))))

(defun my/ai--request (prompt)
  "Send PROMPT to Claude Haiku with no thinking, show result in side window."
  (let ((buf (get-buffer-create my/ai-commentary-buffer))
        (gptel-backend my/ai-commentary-backend)
        (gptel-model my/ai-commentary-model))
    (with-current-buffer buf (erase-buffer) (markdown-mode) (insert "Waiting...\n"))
    (display-buffer buf '(display-buffer-in-side-window (side . bottom) (slot . 0) (window-height . 0.3)))
    (gptel-request prompt
      :buffer buf
      :system "Be concise. Use markdown formatting."
      :callback (lambda (response _info) (my/ai--show-response response)))))

(defun my/ai-explain ()
  "Explain the selected region or current defun using Claude Haiku."
  (interactive)
  (require 'gptel)
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'defun t)))
         (file (buffer-name))
         (lang (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))
    (unless code (user-error "No code at point"))
    (my/ai--request (format "Briefly explain this %s code from %s.\n\n```%s\n%s\n```"
                            lang file lang code))))

(defun my/ai-ask ()
  "Ask a question about the selected region or current defun."
  (interactive)
  (require 'gptel)
  (let* ((code (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'defun t)))
         (file (buffer-name))
         (lang (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
         (question (read-string "Ask about this code: ")))
    (unless code (user-error "No code at point"))
    (my/ai--request (format "%s\n\nCode from %s:\n```%s\n%s\n```"
                            question file lang code))))

;;;; Keybindings

(defun my/git-grep-thing-at-point ()
  "Run `consult-git-grep` for the symbol at point."
  (interactive)
  (consult-git-grep nil (thing-at-point 'symbol t)))

(map! :leader
  :desc "align dwim" "RET" #'align
  :desc "Git grep thing at point" "s G" #'my/git-grep-thing-at-point

  (:prefix "w"
   :desc "split horizontally" :nv "S" #'split-window-horizontally)

  (:prefix "s"
   :desc "clear search" :nv "c" #'evil-ex-nohighlight)

  (:prefix ("l" . "AI")
   :desc "explain code" :nv "e" #'my/ai-explain
   :desc "ask about code" :nv "q" #'my/ai-ask
   :desc "implement"   :nv "i" #'my/ai-implement
   :desc "complete"    :nv "c" #'my/ai-complete))

;;;; Local Overrides (work/home profiles)
;; Load config-local.el — org paths, capture templates, LLM backends/models.
;; At work, replace this file with work-specific settings (litellm, etc).
(load! "config-local" doom-user-dir t)

