;;; config-local.el --- Home profile overrides -*- lexical-binding: t; -*-
;;; At work, replace this file with work-specific org paths and LLM backends.

;;;; Org — paths, capture templates, diary

;; Must be set before org loads
(setq org-directory "~/dropbox/org/")
;; Explicit agenda files (not the whole dir) so notes / the design doc /
;; bookmarks with stray TODO/DONE keywords don't pollute the agenda. The diary
;; (calendar) fold is separate, via org-agenda-include-diary. Add files here as
;; new agenda sources appear.
(setq org-agenda-files
      (mapcar (lambda (f) (expand-file-name f "~/dropbox/org/"))
              '("journal.org" "tasks.org" "habits.org" "book-list.org")))
(setq org-books-file "~/dropbox/org/book-list.org")

(after! org
  (defun my/bookmark-link ()
    "Prompt for a URL and return an [[url][title]] link for capture.
Fetches the page title via org-cliplink. The clipboard is used only to
pre-fill the prompt, and only when it actually looks like a URL, so non-URL
clipboard junk can't leak in (and captures can't recursively wrap each other).
Falls back to a manually typed title if the title fetch fails."
    (require 'org-cliplink)
    (let* ((clip (ignore-errors (string-trim (or (current-kill 0 t) ""))))
           (default (and (stringp clip) (string-match-p "\\`https?://" clip) clip))
           (url (read-string (if default "Bookmark URL (RET = clipboard): " "Bookmark URL: ")
                             nil nil default))
           (title (or (ignore-errors (org-cliplink-retrieve-title-synchronously url))
                      (read-string "Title (fetch failed): "))))
      (org-link-make-string url title)))

  (setq org-capture-templates
        '(("t" "Task" entry (file "~/dropbox/org/tasks.org")
           "* TODO %?\nSCHEDULED: %T\n:PROPERTIES:\n:CREATED: %U\n:END:"
           :prepend t)
          ;; Non-interactive variant for the `send-todo` shell script: text comes
          ;; in via org-capture-string (%i) and files immediately, no prompt.
          ("T" "Task (immediate, from shell)" entry (file "~/dropbox/org/tasks.org")
           "* TODO %i\nSCHEDULED: %T\n:PROPERTIES:\n:CREATED: %U\n:END:"
           :prepend t :immediate-finish t)
          ;; What I worked on -> journal.org datetree (under today's date). Tagged
          ;; :log: to mark these notes apart from the DONE records that also land
          ;; in the datetree, so the reflector can pull just the worked-on notes.
          ("j" "Journal — worked on" entry
           (file+olp+datetree "~/dropbox/org/journal.org")
           "* %?  :log:\n:PROPERTIES:\n:CREATED: %U\n:END:")
          ("b" "Bookmark" entry (file "~/dropbox/org/bookmarks.org")
           "* %(my/bookmark-link)\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?"
           :prepend t)
          ;; NOTE: these habit templates deliberately do NOT set :OWH_CONFIG:.
          ;; org-window-habit needs a per-habit :OWH_CONFIG: plist to draw a
          ;; consistency graph (there is no default -- no plist, no graph), so
          ;; habits captured here render without one until it is added by hand.
          ;;
          ;; That is on purpose. beorg does not use org-capture-templates -- it
          ;; has its own capture -- so it cannot produce :OWH_CONFIG: no matter
          ;; what is written here. Setting it only on this side would split
          ;; behaviour across the two writers of habits.org: habits captured at
          ;; the desk would get graphs, habits captured on the phone would not.
          ;; A consistent absence beats an inconsistent presence, especially for
          ;; something that is display-only decoration (org-window-habit runs
          ;; with repeat-to-scheduled/repeat-to-deadline nil; the plain .+N
          ;; repeater owns SCHEDULED). Habits are created a few times a year --
          ;; add the plist by hand on the rare occasion a graph is wanted.
          ("h" "Habit (daily)" entry (file "~/dropbox/org/habits.org")
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (current-time) t nil nil nil \" .+%^{Every N days}d\")\n:PROPERTIES:\n:STYLE:    habit\n:CREATED: %U\n:END:")
          ("H" "Habit (weekly)" entry (file "~/dropbox/org/habits.org")
           "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (current-time) t nil nil nil \" .+%^{Every N weeks}w\")\n:PROPERTIES:\n:STYLE:    habit\n:CREATED: %U\n:END:")))

  ;; Log completions into a LOGBOOK drawer so beorg and Emacs write the same
  ;; shape, and org-window-habit reads clean completion history.
  (setq org-log-into-drawer "LOGBOOK")

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

  (advice-add #'org-agenda :before #'my/update-diary-from-ics)

  ;; Habit consistency graphs only recompute on a full agenda rebuild, not on
  ;; the inline line update — so the bar goes stale after marking a habit done.
  ;; Rebuild after any completion done from the agenda to refresh it. This is
  ;; cheap: `org-agenda-redo' re-runs the agenda command but does NOT re-trigger
  ;; the ICS fetch (that advice is on `org-agenda', not `org-agenda-redo').
  (advice-add 'org-agenda-todo :after
              (lambda (&rest _)
                (when (derived-mode-p 'org-agenda-mode)
                  ;; Save first: by the time `org-agenda-todo' returns, a habit's
                  ;; repeater reset + LAST_REPEAT/log line are fully written, so
                  ;; this captures them (the state-change hook can fire too early
                  ;; for repeaters). Then rebuild so the consistency graph updates.
                  (org-save-all-org-buffers)
                  (org-agenda-redo t)))))

;;;; Habits — org-window-habit display

;; Replaces org-habit's agenda graph with a window-based consistency view (it
;; requires and wraps org-habit, configured in config.el). Habits are recognised
;; by :STYLE: habit + a SCHEDULED repeater; completions are read from the LOGBOOK
;; drawer (beorg and Emacs both write there). Window sized to match the old feel.
(use-package! org-window-habit
  :after org
  :config
  (setq org-window-habit-preceding-intervals 14
        org-window-habit-following-days 3
        ;; Default glyphs are multibyte (✓ ☐) and trip a unibyte-string bug in
        ;; the graph renderer; ASCII glyphs avoid it and the cell colors carry
        ;; the conforming / not-conforming signal anyway.
        org-window-habit-completed-glyph ?\s
        org-window-habit-completion-needed-today-glyph ?o
        ;; Display-only: never let OWH rewrite a date. With both flags nil its
        ;; conformance re-arm is a no-op, so the plain `.+Nd'/`++2w' repeater —
        ;; the same one beorg uses — is the sole writer of SCHEDULED. Completions
        ;; land identically whether done on the phone or at the desk; OWH just
        ;; draws the consistency graph from the LOGBOOK.
        org-window-habit-repeat-to-deadline nil
        org-window-habit-repeat-to-scheduled nil)
  (org-window-habit-mode +1))

;; The 80-col fill indicator is global (config.el); it's just clutter in the
;; agenda, so turn it off there.
(add-hook 'org-agenda-mode-hook (lambda () (display-fill-column-indicator-mode -1)))

;;;; Weather + AQI in the agenda header line (Open-Meteo, keyless)

(defvar my/weather-latitude 41.9227 "Latitude for weather/AQI (zip 60614).")
(defvar my/weather-longitude -87.6537 "Longitude for weather/AQI (zip 60614).")
(defvar my/weather-cache nil "Last formatted weather/AQI header string.")
(defvar my/weather-last-update nil "Time of last successful weather fetch.")
(defvar my/weather-update-interval (* 30 60) "Min seconds between weather fetches.")

(defconst my/weather-code-table
  '((0 . "clear") (1 . "mostly clear") (2 . "partly cloudy") (3 . "overcast")
    (45 . "fog") (48 . "rime fog") (51 . "drizzle") (53 . "drizzle")
    (55 . "heavy drizzle") (61 . "light rain") (63 . "rain") (65 . "heavy rain")
    (71 . "light snow") (73 . "snow") (75 . "heavy snow") (77 . "snow grains")
    (80 . "rain showers") (81 . "rain showers") (82 . "violent showers")
    (85 . "snow showers") (86 . "snow showers") (95 . "thunderstorm")
    (96 . "thunderstorm w/ hail") (99 . "thunderstorm w/ hail"))
  "Open-Meteo WMO weather code -> short description.")

(defun my/weather--emoji (code)
  (cond ((memq code '(0 1)) "☀️") ((eq code 2) "⛅")
        ((memq code '(3 45 48)) "☁️")
        ((memq code '(51 53 55 61 63 65 80 81 82)) "🌧️")
        ((memq code '(71 73 75 77 85 86)) "❄️")
        ((memq code '(95 96 99)) "⛈️") (t "🌡️")))

(defun my/weather--aqi (aqi)
  "Return (EMOJI . LABEL) for a US AQI value."
  (cond ((null aqi) '("" . ""))
        ((<= aqi 50) '("🟢" . "good")) ((<= aqi 100) '("🟡" . "moderate"))
        ((<= aqi 150) '("🟠" . "unhealthy for sensitive"))
        ((<= aqi 200) '("🔴" . "unhealthy"))
        ((<= aqi 300) '("🟣" . "very unhealthy")) (t '("⚫" . "hazardous"))))

(defun my/weather--json ()
  "Parse the JSON body in a url-retrieve result buffer."
  (goto-char (point-min))
  (when (re-search-forward "\n\n" nil t)
    (json-parse-buffer :object-type 'alist :null-object nil)))

(defun my/weather--hhmm (iso)
  "Return HH:MM from an Open-Meteo ISO time string, or nil."
  (when (and (stringp iso) (>= (length iso) 16))
    (substring iso 11 16)))

(defconst my/moon-phases
  '("🌑 new" "🌒 waxing crescent" "🌓 first quarter" "🌔 waxing gibbous"
    "🌕 full" "🌖 waning gibbous" "🌗 last quarter" "🌘 waning crescent")
  "Eight moon-phase glyphs + labels, new -> waning crescent.")

(defun my/moon-phase ()
  "Approximate current moon phase as emoji + label (computed, no network)."
  (let* ((synodic 2551442.9)            ; synodic month in seconds (29.530589 d)
         (ref 947182440.0)              ; 2000-01-06 18:14 UTC new moon (unix s)
         (frac (/ (mod (- (float-time) ref) synodic) synodic)))
    (nth (mod (floor (+ (* frac 8) 0.5)) 8) my/moon-phases)))

(defun my/weather--apply ()
  "Push the cached weather string into the agenda buffer's header line."
  (when-let ((buf (get-buffer "*Org Agenda*")))
    (with-current-buffer buf
      (setq-local header-line-format my/weather-cache)
      (force-mode-line-update))))

(defun my/weather-refresh (&optional force)
  "Fetch weather + AQI from Open-Meteo, cache a header string, update the agenda."
  (interactive (list t))
  (when (or force (null my/weather-last-update)
            (> (- (float-time) my/weather-last-update) my/weather-update-interval))
    (setq my/weather-last-update (float-time))
    (let ((wurl (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current=temperature_2m,weather_code&daily=sunrise,sunset&timezone=auto&temperature_unit=fahrenheit"
                        my/weather-latitude my/weather-longitude))
          (aurl (format "https://air-quality-api.open-meteo.com/v1/air-quality?latitude=%s&longitude=%s&current=us_aqi"
                        my/weather-latitude my/weather-longitude)))
      (url-retrieve
       wurl
       (lambda (_s)
         (let* ((wj (ignore-errors (my/weather--json)))
                (cur (and wj (alist-get 'current wj)))
                (daily (and wj (alist-get 'daily wj)))
                (temp (and cur (alist-get 'temperature_2m cur)))
                (code (and cur (alist-get 'weather_code cur)))
                (desc (or (cdr (assoc code my/weather-code-table)) ""))
                (sr (my/weather--hhmm
                     (let ((v (and daily (alist-get 'sunrise daily))))
                       (and (vectorp v) (> (length v) 0) (aref v 0)))))
                (ss (my/weather--hhmm
                     (let ((v (and daily (alist-get 'sunset daily))))
                       (and (vectorp v) (> (length v) 0) (aref v 0))))))
           (url-retrieve
            aurl
            (lambda (_s2)
              (let* ((aj (ignore-errors (my/weather--json)))
                     (aqi (and aj (alist-get 'us_aqi (alist-get 'current aj))))
                     (a (my/weather--aqi aqi)))
                (setq my/weather-cache
                      (format " %s %s°F %s   ·   %s AQI %s %s   ·   🌅 %s  🌇 %s   ·   %s"
                              (my/weather--emoji code) (if temp (round temp) "?") desc
                              (car a) (or aqi "?") (cdr a)
                              (or sr "?") (or ss "?") (my/moon-phase)))
                (my/weather--apply)))
            nil t)))
       nil t))))

;; Pin the cached line as the agenda's header (no buffer text -> can't duplicate);
;; refresh in the background when stale.
(add-hook 'org-agenda-finalize-hook
          (lambda ()
            (my/weather-refresh)
            (setq-local header-line-format my/weather-cache)))

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
