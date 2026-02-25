;;; config-local.el --- Home profile overrides -*- lexical-binding: t; -*-
;;; At work, replace this file with work-specific org paths and LLM backends.

;;;; Org — paths, capture templates, diary

;; Must be set before org loads
(setq org-directory "~/icloud/org/")
(setq org-agenda-files '("/Users/dpzmick/icloud/org"))
(setq org-books-file "~/icloud/org/book-list.org")

(after! org
  (setq org-capture-templates
        '(("t" "Dated TODO" entry (file+datetree "~/icloud/org/journal.org")
           "**** TODO %?\n:PROPERTIES:\n:CREATED:: %U\n:END:")
          ("a" "Article" entry (file+datetree "~/icloud/org/article-list.org")
           "**** QUEUED %?\n:PROPERTIES:\n:CREATED:: %U\n:END:")
          ("h" "Habit (daily)" entry (file "~/icloud/org/habits.org")
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (current-time) t nil nil nil \" ++%^{Every N days}d\")\n:PROPERTIES:\n:STYLE:    habit\n:CREATED:: %U\n:END:")
          ("H" "Habit (weekly)" entry (file "~/icloud/org/habits.org")
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (current-time) t nil nil nil \" ++%^{Every N weeks}w\")\n:PROPERTIES:\n:STYLE:    habit\n:CREATED:: %U\n:END:")))

  ;; Diary integration (ICS calendar)
  (setq org-agenda-include-diary t)
  (setq diary-file (expand-file-name "~/.doom.d/diary"))
  (setq diary-number-of-entries 14)

  (defvar my/ics-update-interval (* 60 60)
    "Minimum seconds between ICS diary refreshes.")

  (defvar my/ics-last-update-time nil
    "Time of last successful ICS diary update.")

  (defun my/update-diary-from-ics ()
    "Fetch ICS calendar from 1Password and convert to Emacs diary.
Skips if last update was less than `my/ics-update-interval' seconds ago."
    (interactive)
    (let ((now (float-time)))
      (when (or (called-interactively-p 'any)
                (null my/ics-last-update-time)
                (> (- now my/ics-last-update-time) my/ics-update-interval))
        (let* ((ics-url (my/op-read "fastmail-ics-file" "credential"))
               (script "/Users/dpzmick/dotfiles/bin/ics_to_emacs_diary.py")
               (out diary-file)
               (cmd (format "%s --url %s --out %s || true"
                            (shell-quote-argument script)
                            (shell-quote-argument ics-url)
                            (shell-quote-argument out))))
          (shell-command cmd "*diary-update*" "*diary-update-errors*")
          (setq my/ics-last-update-time now)
          (message "ICS diary updated — reopen agenda to see new events")))))

  (advice-add #'org-agenda :before #'my/update-diary-from-ics))

;;;; LLM backends — Anthropic API (home)

(defun my/fetch-anthropic-models ()
  "Fetch available model IDs from the Anthropic API."
  (let* ((api-key (my/op-read "anthropic-gptel" "credential"))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("x-api-key" . ,api-key)
            ("anthropic-version" . "2023-06-01")))
         (buf (url-retrieve-synchronously "https://api.anthropic.com/v1/models" t)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "\n\n")
            (let* ((json (json-read))
                   (data (cdr (assq 'data json))))
              (mapcar (lambda (m) (intern (cdr (assq 'id m))))
                      data)))
        (kill-buffer buf)))))

(after! gptel
  (let ((models (or (my/fetch-anthropic-models)
                    '(claude-opus-4-6
                      claude-sonnet-4-6
                      claude-haiku-4-5-20251001))))
    (setq gptel-backend
          (gptel-make-anthropic "Claude"
            :stream t
            :models models
            :request-params '(:thinking (:type "enabled" :budget_tokens 16384)
                              :max_tokens 32768)
            :key (lambda () (my/op-read "anthropic-gptel" "credential")))))
  (setq gptel-model 'claude-sonnet-4-6)

  (setq my/ai-commentary-backend
        (gptel-make-anthropic "Claude-fast"
          :stream t
          :models '(claude-haiku-4-5-20251001)
          :request-params '(:max_tokens 4096)
          :key (lambda () (my/op-read "anthropic-gptel" "credential"))))
  (setq my/ai-commentary-model 'claude-haiku-4-5-20251001)

  (setq my/ai-complete-backend
        (gptel-make-anthropic "Claude-complete"
          :stream t
          :models '(claude-sonnet-4-6)
          :request-params '(:max_tokens 8192)
          :key (lambda () (my/op-read "anthropic-gptel" "credential"))))
  (setq my/ai-complete-model 'claude-sonnet-4-6))
