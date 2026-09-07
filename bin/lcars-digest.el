;;; lcars-digest.el --- Render the `SPC o A h' agenda as LCARS HTML -*- lexical-binding: t; -*-

;; Headless generator: `emacs --batch -Q -l lcars-digest.el -- OUT.html'.
;; Builds the same "Habits & Unscheduled" agenda used in-editor and renders it
;; as one standalone LCARS-themed HTML page: all CSS inline, no external assets
;; the page fetches at view time, so it works offline / in email / on an e-ink
;; browser. Ambient data (weather via wttr.in) is fetched once at generation and
;; baked in; moon phase and stardate are computed locally.
;;
;; Org paths/diary/habit settings below MIRROR config-local.el and config.el.
;; Keep them in sync if the real agenda config changes.

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'diary-lib)
(require 'json)
(require 'url-util)

;;;; Org config (mirror of config-local.el / config.el)

(setq org-directory "~/dropbox/org/")
(setq org-agenda-files
      (mapcar (lambda (f) (expand-file-name f "~/dropbox/org/"))
              '("journal.org" "tasks.org" "habits.org" "book-list.org")))
(setq org-log-into-drawer "LOGBOOK")
;; Float all-day / untimed items to the top of each day (mirrors config.el).
(setq org-sort-agenda-notime-is-late nil)

;; Diary (ICS calendar) integration
(setq org-agenda-include-diary t)
(setq diary-file (expand-file-name "~/.doom.d/diary"))
(setq diary-number-of-entries 14)

;; Habits — mirror config-local.el: org-window-habit's window-based graph (it
;; wraps org-habit). Load the straight-built package so the digest's habit graph
;; matches the editor rather than falling back to stock org-habit.
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-show-habits-only-for-today nil
      org-habit-graph-column 70)

(let ((dir (car (file-expand-wildcards
                 "~/.config/emacs/.local/straight/build-*/org-window-habit"))))
  (unless dir (error "org-window-habit not found under straight build dir"))
  (add-to-list 'load-path dir))
(require 'org-window-habit)
(setq org-window-habit-preceding-intervals 14
      org-window-habit-following-days 3
      org-window-habit-completed-glyph ?\s
      org-window-habit-completion-needed-today-glyph ?o
      ;; Display-only: both flags nil so OWH never rewrites SCHEDULED; it only
      ;; draws the consistency graph from the LOGBOOK (mirrors config-local.el).
      org-window-habit-repeat-to-deadline nil
      org-window-habit-repeat-to-scheduled nil)
(org-window-habit-mode +1)

;; The "h" custom command, verbatim from config.el.
(setq org-agenda-custom-commands
      '(("h" "Habits & Unscheduled"
         ((agenda "" ((org-agenda-span 3)
                      (org-agenda-show-all-dates nil)
                      (org-agenda-time-grid nil)
                      (org-agenda-start-day nil)
                      (org-agenda-overriding-header "")))
          (alltodo "" ((org-agenda-overriding-header "Unscheduled Tasks")
                       (org-agenda-todo-ignore-scheduled 'all)
                       (org-agenda-todo-ignore-deadlines 'all)))))))

;;;; Stardate (flavor; epoch chosen so present day lands in the ~79000s)

(defvar lcars-stardate-epoch 1947
  "Year mapped to stardate 0. Adjust to taste.")

(defun lcars--leap-year-p (y)
  (and (zerop (mod y 4)) (or (not (zerop (mod y 100))) (zerop (mod y 400)))))

(defun lcars--stardate (&optional time)
  (let* ((time (or time (current-time)))
         (year (nth 5 (decode-time time)))
         (doy  (string-to-number (format-time-string "%j" time)))
         (days (if (lcars--leap-year-p year) 366 365)))
    (+ (* 1000 (- year lcars-stardate-epoch))
       (/ (* 1000.0 (1- doy)) days))))

;;;; Ambient data — moon (local), weather (network), ship's log

(defvar lcars-weather-location (getenv "LCARS_WEATHER_LOCATION")
  "Location for weather, e.g. \"Chicago\" or \"SFO\". nil / empty makes wttr.in
auto-detect from the requesting IP (so it follows you when run on your laptop).")

(defvar lcars-weather-units
  (if (member (downcase (or (getenv "LCARS_WEATHER_UNITS") "f")) '("c" "celsius"))
      'c 'f)
  "Temperature units, 'f or 'c. Override with LCARS_WEATHER_UNITS=c.")

(defvar lcars-log-quotes
  '("SPACE: THE FINAL FRONTIER."
    "MAKE IT SO."
    "TEA. EARL GREY. HOT."
    "TODAY IS A GOOD DAY TO SHIP."
    "ENGAGE."
    "RESISTANCE IS FUTILE."
    "THERE ARE FOUR LIGHTS."
    "LIVE LONG AND PROSPER."
    "THE LINE MUST BE DRAWN HERE."
    "WITH THE FIRST LINK, THE CHAIN IS FORGED.")
  "Ship's-log flavor lines; one is chosen deterministically per day.")

(defun lcars--quote (time)
  (nth (mod (string-to-number (format-time-string "%j" time))
            (length lcars-log-quotes))
       lcars-log-quotes))

;; Moon phase — computed locally, no network needed.
(defun lcars--julian-day (time)
  (+ (/ (float-time time) 86400.0) 2440587.5))

(defun lcars--moon (time)
  "Return (NAME GLYPH ILLUM%) for the moon phase at TIME."
  (let* ((synodic 29.530588853)
         ;; Reference new moon 2000-01-06 18:14 UT = JD 2451550.1
         (age (mod (- (lcars--julian-day time) 2451550.1) synodic))
         (frac (/ age synodic))
         (illum (round (* 100 (/ (- 1 (cos (* 2 float-pi frac))) 2))))
         (i (mod (floor (+ 0.5 (* 8 frac))) 8))
         (names ["New Moon" "Waxing Crescent" "First Quarter" "Waxing Gibbous"
                 "Full Moon" "Waning Gibbous" "Last Quarter" "Waning Crescent"])
         (glyphs ["\U0001F311" "\U0001F312" "\U0001F313" "\U0001F314"
                  "\U0001F315" "\U0001F316" "\U0001F317" "\U0001F318"]))
    (list (aref names i) (aref glyphs i) illum)))

(defun lcars--fetch-json (url)
  "GET URL with curl and parse the JSON body (alist objects), or nil on failure.
All network bubbles route through here so a missing curl / no network simply
drops the bubble rather than erroring."
  (ignore-errors
    (with-temp-buffer
      (when (zerop (call-process "curl" nil t nil "-s" "--max-time" "8" url))
        (goto-char (point-min))
        (json-parse-buffer :object-type 'alist)))))

(defun lcars--weather (location units)
  "Fetch weather via wttr.in as a plist, or nil on any failure. Runs once at
generation time; keys: :temp :feels :desc :unit :hi :lo :sunrise :sunset."
  (ignore-errors
    (let* ((loc (and location (not (string-empty-p location))
                     (url-hexify-string location)))
           (url (format "https://wttr.in/%s?format=j1" (or loc "")))
           (json (lcars--fetch-json url)))
      (when json
        (let* ((cur (elt (alist-get 'current_condition json) 0))
               (today (elt (alist-get 'weather json) 0))
               (astro (elt (alist-get 'astronomy today) 0))
               (cel (eq units 'c)))
          (list :temp    (alist-get (if cel 'temp_C 'temp_F) cur)
                :feels   (alist-get (if cel 'FeelsLikeC 'FeelsLikeF) cur)
                :desc    (alist-get 'value (elt (alist-get 'weatherDesc cur) 0))
                :unit    (if cel "C" "F")
                :hi      (alist-get (if cel 'maxtempC 'maxtempF) today)
                :lo      (alist-get (if cel 'mintempC 'mintempF) today)
                :sunrise (alist-get 'sunrise astro)
                :sunset  (alist-get 'sunset astro)))))))

(defun lcars--stats (items)
  "Summarize collected ITEMS into a plist (:tasks :events :habits)."
  (let ((section nil) (tasks 0) (events 0) habits)
    (dolist (it items)
      (pcase (car it)
        (:section (setq section (cdr it)))
        (:entry
         (let ((e (cdr it)))
           (cond
            ((plist-get e :habit)
             (cl-pushnew (plist-get e :text) habits :test #'equal))
            ((and section (string-match-p "UNSCHEDULED" (upcase section)))
             (setq tasks (1+ tasks)))
            (t (setq events (1+ events))))))))
    (list :tasks tasks :events events :habits (length habits))))

(defun lcars--humans-in-space ()
  "Return (COUNT . CRAFTS-STRING) of people currently in orbit, or nil."
  (let ((json (lcars--fetch-json "http://api.open-notify.org/astros.json")))
    (when (and json (equal (alist-get 'message json) "success"))
      (let ((crafts (delete-dups
                     (mapcar (lambda (p) (alist-get 'craft p))
                             (append (alist-get 'people json) nil)))))
        (cons (alist-get 'number json)
              (mapconcat #'identity crafts " · "))))))

(defun lcars--kp-index ()
  "Return (KP . LABEL) for the latest NOAA planetary K-index, or nil. The feed's
rows may be JSON objects or arrays depending on the endpoint, so handle both."
  (ignore-errors
    (let ((rows (lcars--fetch-json
                 "https://services.swpc.noaa.gov/products/noaa-planetary-k-index.json")))
      (when (and (vectorp rows) (> (length rows) 0))
        (let* ((last (aref rows (1- (length rows))))
               (raw (if (listp last) (alist-get 'Kp last) (aref last 1)))
               (kp (round (if (numberp raw) raw (string-to-number raw)))))
          (cons kp (cond ((<= kp 2) "QUIET")
                         ((= kp 3) "UNSETTLED")
                         ((= kp 4) "ACTIVE")
                         ((= kp 5) "MINOR STORM")
                         ((= kp 6) "MODERATE STORM")
                         ((= kp 7) "STRONG STORM")
                         (t "SEVERE STORM"))))))))

;;;; Agenda extraction

(defun lcars--first-prop (beg end prop)
  "First non-nil value of text PROP between BEG and END."
  (let ((p beg) val)
    (while (and (< p end) (not val))
      (setq val (get-text-property p prop))
      (setq p (next-single-property-change p prop nil end)))
    val))

(defun lcars--face-bg (face)
  "Resolve a background color for FACE (symbol, plist, or list of faces)."
  (cond
   ((null face) nil)
   ((stringp face) nil)
   ((and (listp face) (plist-member face :background)) (plist-get face :background))
   ((and (listp face) (not (keywordp (car face)))) (cl-some #'lcars--face-bg face))
   ((facep face) (let ((bg (face-background face nil 'default)))
                   (and (stringp bg) (not (string= bg "unspecified-bg")) bg)))
   (t nil)))

(defun lcars--habit-graph (bol eol)
  "Return the consistency graph on a habit line as a list of (CHAR . COLOR),
with non-colored padding (spacing / trailing cookies outside the graph proper)
trimmed from both ends."
  (let ((p (min eol (+ bol org-habit-graph-column)))
        cells any)
    (while (< p eol)
      (let* ((ch (char-after p))
             (color (lcars--face-bg (get-text-property p 'face))))
        (when color (setq any t))
        (push (cons ch color) cells))
      (setq p (1+ p)))
    (when any
      (setq cells (seq-drop-while (lambda (c) (null (cdr c))) (nreverse cells)))
      (nreverse (seq-drop-while (lambda (c) (null (cdr c))) (nreverse cells))))))

(defun lcars--parse-entry (bol eol)
  "Parse an agenda entry line between BOL and EOL into a plist."
  (let* ((type (lcars--first-prop bol eol 'type))
         (tod  (lcars--first-prop bol eol 'time-of-day))
         (txt  (lcars--first-prop bol eol 'txt))
         (cat  (lcars--first-prop bol eol 'org-category))
         (habit (lcars--first-prop bol eol 'org-habit-p))
         (line (string-trim (buffer-substring-no-properties bol eol))))
    (list :type (or type "todo")
          :time (and (numberp tod) (format "%02d:%02d" (/ tod 100) (mod tod 100)))
          :text (string-trim (if (and (stringp txt) (> (length txt) 0)) txt line))
          :cat  (and cat (format "%s" cat))
          :habit (and habit t)
          :graph (and habit (lcars--habit-graph bol eol)))))

(defun lcars--collect ()
  "Walk the current org-agenda buffer into a flat tagged item list.
Items: (:section . TITLE), (:date . LABEL), (:entry . PLIST)."
  (let (items)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (plain (string-trim (buffer-substring-no-properties bol eol))))
        (cond
         ((get-text-property bol 'org-agenda-structural-header)
          (push (cons :section (if (string-empty-p plain) "AGENDA" plain)) items))
         ((get-text-property bol 'org-agenda-date-header)
          (push (cons :date plain) items))
         ((or (get-text-property bol 'org-marker)
              (lcars--first-prop bol eol 'txt)
              (lcars--first-prop bol eol 'type))
          (when (> (length plain) 0)
            (push (cons :entry (lcars--parse-entry bol eol)) items)))))
      (forward-line 1))
    (nreverse items)))

(defun lcars--build-agenda ()
  "Run the `h' agenda in batch and return collected items."
  (let ((org-agenda-sticky nil)
        (org-agenda-buffer-name "*lcars-agenda*")
        (org-agenda-window-setup 'current-window))
    (org-agenda nil "h")
    (with-current-buffer org-agenda-buffer-name
      (lcars--collect))))

;;;; HTML rendering

(defun lcars--esc (s)
  (if (not s) ""
    (thread-last s
                 (replace-regexp-in-string "&" "&amp;")
                 (replace-regexp-in-string "<" "&lt;")
                 (replace-regexp-in-string ">" "&gt;"))))

(defun lcars--entry-color (type)
  "LCARS accent color for an entry TYPE (luminance-distinct for e-ink)."
  (cond
   ((member type '("diary" "block" "timestamp")) "#6699ff")   ; blue  — calendar
   ((member type '("scheduled" "past-scheduled")) "#ff9900")  ; orange— scheduled
   ((member type '("deadline" "upcoming-deadline")) "#cc6666"); red   — deadline
   (t "#ffcc66")))                                            ; amber — todo

(defvar lcars-todo-keywords
  '("TODO" "NEXT" "STRT" "WAITING" "HOLD" "PROJ" "SOMEDAY"
    "READING" "DONE" "CANCELLED" "KILL")
  "Leading tokens rendered as an LCARS state tag instead of body text.")

(defun lcars--split-keyword (text)
  "Split a leading org state keyword off TEXT: return (KEYWORD . REST), or
(nil . TEXT) when the first token is not a known keyword."
  (if (string-match "\\`\\([A-Z][A-Z]+\\)[ \t]+\\(.*\\)\\'" text)
      (let ((kw (match-string 1 text)))
        (if (member kw lcars-todo-keywords)
            (cons kw (match-string 2 text))
          (cons nil text)))
    (cons nil text)))

(defun lcars--render-graph (cells)
  "Render habit CELLS (from org-window-habit's graph) as inline colored blocks.
The face background colors are org-window-habit's own conforming/not-conforming
gradient, kept as-is so the streak intensity survives."
  (mapconcat
   (lambda (c)
     (let ((ch (car c)) (color (cdr c)))
       (if color
           (format "<span class=\"hb\" style=\"background:%s\">%s</span>"
                   color (if (eq ch ?\s) "&nbsp;" (lcars--esc (char-to-string ch))))
         (format "<span class=\"hb hb-empty\">%s</span>"
                 (if (eq ch ?\s) "&nbsp;" (lcars--esc (char-to-string ch)))))))
   cells ""))

(defun lcars--render-entry (e)
  (let* ((type (plist-get e :type))
         (color (lcars--entry-color type))
         (time (plist-get e :time))
         (cat  (plist-get e :cat))
         (graph (plist-get e :graph))
         (split (lcars--split-keyword (plist-get e :text)))
         (kw (car split)))
    (concat
     "<div class=\"row\">"
     (format "<span class=\"tick\" style=\"background:%s\"></span>" color)
     (if time
         (format "<span class=\"time\">%s</span>" (lcars--esc time))
       "<span class=\"time time-none\">&middot;&middot;&middot;&middot;</span>")
     (if kw (format "<span class=\"kw\">%s</span>" (lcars--esc kw)) "")
     (format "<span class=\"txt\">%s</span>" (lcars--esc (cdr split)))
     (if cat (format "<span class=\"cat\">%s</span>" (lcars--esc cat)) "")
     (if graph (format "<span class=\"graph\">%s</span>" (lcars--render-graph graph)) "")
     "</div>")))

(defun lcars--bubble (accent label value &optional sub)
  "One TNG-style data bubble. VALUE and SUB may contain HTML."
  (format (concat "<div class=\"bubble\" style=\"border-color:%s\">"
                  "<div class=\"blab\" style=\"color:%s\">%s</div>"
                  "<div class=\"bval\">%s</div>%s</div>")
          accent accent (lcars--esc label) value
          (if sub (format "<div class=\"bsub\">%s</div>" sub) "")))

(defun lcars--clock (s)
  "Shorten a wttr.in time like \"05:19 AM\" to \"5:19\" (the ↑/↓ arrows already
imply morning / evening)."
  (when s
    (replace-regexp-in-string
     "\\`0" "" (replace-regexp-in-string " ?[AP]M\\'" "" (string-trim s)))))

(defun lcars--gather (items now)
  "Fetch/compute every ambient datum once, returning a plist. Keeps the two
layouts from firing off duplicate network calls."
  (list :wx    (lcars--weather lcars-weather-location lcars-weather-units)
        :moon  (lcars--moon now)
        :stats (lcars--stats items)
        :space (lcars--humans-in-space)
        :kp    (lcars--kp-index)))

(defun lcars--weather-bubble (wx)
  "One merged weather bubble: temp + condition, with hi/lo and sun times below."
  (if wx
      (lcars--bubble
       "#ff9900" "Weather"
       (format "%s&deg;%s &middot; %s"
               (lcars--esc (plist-get wx :temp)) (plist-get wx :unit)
               (lcars--esc (upcase (or (plist-get wx :desc) ""))))
       (format "H %s&deg; L %s&deg; &nbsp; &uarr; %s &darr; %s"
               (lcars--esc (plist-get wx :hi)) (lcars--esc (plist-get wx :lo))
               (lcars--esc (lcars--clock (plist-get wx :sunrise)))
               (lcars--esc (lcars--clock (plist-get wx :sunset)))))
    (lcars--bubble "#ff9900" "Weather" "OFFLINE" "SENSORS UNAVAILABLE")))

(defun lcars--mobile-infoline (data)
  "One dense, wrapping header line for the mobile view: weather · sun · moon ·
souls-in-space, mirroring the condensed header line in the Emacs agenda."
  (let* ((wx (plist-get data :wx))
         (moon (plist-get data :moon))
         (space (plist-get data :space)))
    (mapconcat
     #'identity
     (delq nil
           (list
            (when wx (format "%s&deg;%s %s"
                             (lcars--esc (plist-get wx :temp)) (plist-get wx :unit)
                             (lcars--esc (upcase (or (plist-get wx :desc) "")))))
            (when wx (format "&uarr; %s &darr; %s"
                             (lcars--esc (lcars--clock (plist-get wx :sunrise)))
                             (lcars--esc (lcars--clock (plist-get wx :sunset)))))
            (format "<span class=\"glyph\">%s</span> %d%%" (nth 1 moon) (nth 2 moon))
            (when space (format "%d IN ORBIT" (car space)))))
     " &middot; ")))

(defun lcars--other-bubbles (data)
  "Non-weather bubbles from gathered DATA: moon, mission, souls, geomagnetic Kp."
  (let ((moon  (plist-get data :moon))
        (stats (plist-get data :stats))
        (space (plist-get data :space))
        (kp    (plist-get data :kp)))
    (mapconcat
     #'identity
     (delq nil
           (list
            (lcars--bubble
             "#cc99cc" "Lunar Cycle"
             (format "<span class=\"glyph\">%s</span> %s" (nth 1 moon) (lcars--esc (nth 0 moon)))
             (format "%d%% ILLUMINATED" (nth 2 moon)))
            (lcars--bubble
             "#66cc66" "Mission Status"
             (format "%d OPEN TASKS" (plist-get stats :tasks))
             (format "%d EVENTS &middot; %d HABITS"
                     (plist-get stats :events) (plist-get stats :habits)))
            (when space
              (lcars--bubble "#cc6666" "Souls Off-World"
                             (format "%d IN ORBIT" (car space))
                             (lcars--esc (upcase (cdr space)))))
            (when kp
              (lcars--bubble "#9999ff" "Subspace / Kp"
                             (format "Kp %d" (car kp)) (cdr kp)))))
     "")))

(defun lcars--today-p (label)
  "Non-nil when agenda date-header LABEL names today (whitespace/case-loose),
so we can drop today's redundant heading — the page header already shows it."
  (cl-flet ((norm (s) (downcase (replace-regexp-in-string " +" " " (string-trim s)))))
    (string= (norm label) (norm (format-time-string "%A %e %B %Y")))))

(defun lcars--render-body (items)
  "Render the agenda ITEMS into the shared stack of LCARS panels (used by both
layouts). Day ordering (all-day items first) comes from org's own agenda sort
via `org-sort-agenda-notime-is-late'. Today's date heading is omitted since the
page header already carries it."
  (let ((body "") (open-section nil) (idx 0)
        (rail-colors ["#ff9900" "#cc99cc" "#6699ff" "#ffcc66"]))
    (dolist (it items)
      (pcase (car it)
        (:section
         (when open-section (setq body (concat body "</div></section>")))
         (let ((c (aref rail-colors (mod idx (length rail-colors)))))
           (setq idx (1+ idx))
           (setq body (concat body
                              "<section class=\"panel\">"
                              (format "<div class=\"rail\" style=\"background:%s\"></div>" c)
                              "<div class=\"panel-body\">"
                              (format "<h2 class=\"pill\" style=\"background:%s\">%s</h2>"
                                      c (lcars--esc (upcase (cdr it)))))))
         (setq open-section t))
        (:date
         (unless (lcars--today-p (cdr it))
           (setq body (concat body (format "<h3 class=\"day\">%s</h3>" (lcars--esc (cdr it)))))))
        (:entry
         ;; The `h' agenda block uses an empty overriding-header, so its entries
         ;; arrive before any :section. Give them a synthesized AGENDA panel.
         (unless open-section
           (setq body (concat body
                              "<section class=\"panel\">"
                              "<div class=\"rail\" style=\"background:#ff9900\"></div>"
                              "<div class=\"panel-body\">"
                              "<h2 class=\"pill\" style=\"background:#ff9900\">AGENDA</h2>"))
           (setq idx (1+ idx))
           (setq open-section t))
         (setq body (concat body (lcars--render-entry (cdr it)))))))
    (when open-section (setq body (concat body "</div></section>")))
    body))

(defun lcars--render (items)
  "Render collected ITEMS into the full-width LCARS console HTML document
(for e-ink / desktop)."
  (let* ((now (current-time))
         (data (lcars--gather items now))
         (stardate (format "%.1f" (lcars--stardate now)))
         (datestr (format-time-string "%A %e %B %Y" now))
         (timestr (format-time-string "%H:%M" now)))
    (concat
     "<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
     "<title>LCARS Daily</title>\n<style>\n" (lcars--css) "\n</style></head><body>\n"
     "<div class=\"lcars\">"
     ;; LCARS header: the elbow (bearing the LCARS wordmark) sweeps into the
     ;; horizontal bar; the left rail below continues the elbow's vertical stroke.
     "<div class=\"header\">"
     "<div class=\"elbow\"><span class=\"lcars-tag\">LCARS</span></div>"
     "<div class=\"hbar\">"
     (format "<span class=\"sd\">STARDATE %s</span>" stardate)
     (format "<span class=\"dt\">%s</span>" (lcars--esc datestr))
     (format "<span class=\"clock\">%s</span>" timestr)
     "</div></div>"
     "<div class=\"cols\">"
     "<aside class=\"sidebar\">"
     "<div class=\"railcap\"></div>"
     (lcars--weather-bubble (plist-get data :wx))
     (lcars--other-bubbles data)
     "<div class=\"sfill\"></div>"
     "</aside>"
     "<main class=\"content\">"
     "<div class=\"greet\">GOOD MORNING, CAPTAIN.</div>"
     (lcars--render-body items)
     (format "<footer class=\"logline\"><span class=\"logtag\">SHIP&rsquo;S LOG</span> %s</footer>"
             (lcars--esc (lcars--quote now)))
     "</main>"
     "</div></div></body></html>\n")))

(defun lcars--render-mobile (items)
  "Render collected ITEMS into a phone-first, single-column LCARS email document
(a thin full-height accent rail + stacked sections + a 2-up bubble grid). Styled
via a scoped <style> block, which Fastmail / iOS Mail (WebKit) render faithfully."
  (let* ((now (current-time))
         (data (lcars--gather items now))
         (datestr (replace-regexp-in-string
                   " +" " " (format-time-string "%A %e %B %Y" now)))
         (timestr (format-time-string "%H:%M" now)))
    (concat
     "<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
     "<title>LCARS Daily</title>\n<style>\n" (lcars--css-mobile) "\n</style></head><body>\n"
     "<div class=\"lcm\">"
     ;; Condensed header: LCARS + full date + clock, then one dense info line
     ;; carrying the stats (like the Emacs agenda header). Agenda is the main body.
     "<div class=\"lcm-head\">"
     "<span class=\"lcm-tag\">LCARS</span>"
     (format "<span class=\"lcm-date\">%s</span>" (lcars--esc datestr))
     (format "<span class=\"lcm-clock\">%s</span>" timestr)
     "</div>"
     (format "<div class=\"lcm-info\">%s</div>" (lcars--mobile-infoline data))
     (lcars--render-body items)
     (format "<div class=\"lcm-log\"><span class=\"logtag\">SHIP&rsquo;S LOG</span> %s</div>"
             (lcars--esc (lcars--quote now)))
     "</div></body></html>\n")))

(defun lcars--css ()
  "Inline LCARS stylesheet. Colors are luminance-distinct so the layout still
reads on grayscale e-ink; a monochrome media query hardens contrast further."
  (concat
"* { box-sizing: border-box; }
body { margin: 0; background: #000; color: #f4e3c8;
  font-family: 'Antonio','Oswald','HelveticaNeue-CondensedBold','Arial Narrow',
    'Helvetica Neue',Arial,sans-serif;
  font-stretch: condensed; letter-spacing: .04em; }
.lcars { max-width: 1000px; margin: 0 auto; padding: 16px; }

/* LCARS header: elbow bearing the wordmark + horizontal bar */
.header { display: flex; align-items: flex-start; gap: 10px; }
.elbow { width: 214px; flex: none; height: 80px; background: #ff9900;
  border-radius: 40px 0 0 0; display: flex; align-items: flex-start;
  padding: 15px 0 0 24px; }
.lcars-tag { color: #000; font-weight: 700; font-size: 27px; letter-spacing: .16em; }
.hbar { flex: 1; height: 46px; background: #ff9900; border-radius: 0 24px 0 0;
  display: flex; align-items: center; gap: 14px; padding: 0 12px 0 20px; }
.sd { color: #5a2d00; font-weight: 700; font-size: 20px; }
.dt { color: #5a2d00; text-transform: uppercase; font-size: 14px; letter-spacing: .08em; }
.clock { margin-left: auto; background: #000; color: #ff9900; font-weight: 700;
  font-size: 19px; padding: 6px 16px; border-radius: 18px; }

/* Body: left data rail + wide content */
.cols { display: flex; gap: 14px; align-items: stretch; }
.sidebar { width: 214px; flex: none; display: flex; flex-direction: column; gap: 8px; }
.railcap { height: 16px; background: #ff9900; border-radius: 0 0 16px 0; }
.sfill { flex: 1; min-height: 40px; background: #241a08; border-radius: 0 0 0 16px; }
.content { flex: 1; min-width: 0; }
.greet { color: #ff9900; text-transform: uppercase; font-weight: 700;
  font-size: 17px; margin: 2px 2px 14px; letter-spacing: .2em; }

/* Live data bubbles (stacked in the left rail) */
.bubble { background: #0d0d0d; border-left: 7px solid #ff9900;
  border-radius: 4px 16px 16px 4px; padding: 8px 14px; }
.blab { font-size: 11px; letter-spacing: .15em; text-transform: uppercase; }
.bval { color: #f4e3c8; font-size: 20px; font-weight: 700; line-height: 1.15;
  margin-top: 2px; }
.bsub { color: #9a8b70; font-size: 12px; letter-spacing: .05em;
  text-transform: uppercase; margin-top: 2px; }
.glyph { font-family: 'Apple Color Emoji','Segoe UI Emoji',sans-serif;
  font-stretch: normal; }

/* Agenda panels */
.panel { display: flex; margin: 10px 0; }
.rail { width: 24px; border-radius: 20px 0 0 20px; margin-right: 12px; flex: none; }
.panel-body { flex: 1; min-width: 0; }
.pill { display: inline-block; color: #000; font-weight: 700; font-size: 16px;
  margin: 0 0 8px; padding: 5px 22px; border-radius: 4px 18px 18px 4px;
  text-transform: uppercase; letter-spacing: .1em; }
.day { color: #cc99cc; text-transform: uppercase; font-weight: 700;
  font-size: 13px; margin: 12px 0 4px; letter-spacing: .12em;
  border-bottom: 1px solid #443; padding-bottom: 2px; }
.row { display: flex; align-items: baseline; gap: 10px; padding: 3px 0; }
.tick { width: 10px; height: 10px; border-radius: 3px; flex: none; align-self: center; }
.time { color: #ffcc66; font-weight: 700; min-width: 46px;
  font-variant-numeric: tabular-nums; }
.time-none { color: #665; }
.kw { background: #ffcc66; color: #000; font-size: 10px; font-weight: 700;
  padding: 1px 8px; border-radius: 9px; letter-spacing: .08em; flex: none;
  align-self: center; }
.txt { color: #f4e3c8; flex: 1; min-width: 0; }
.cat { color: #6699ff; text-transform: uppercase; font-size: 11px;
  letter-spacing: .08em; opacity: .85; }
.graph { font-family: monospace; letter-spacing: 0; white-space: nowrap; }
.hb { display: inline-block; width: 8px; text-align: center; color: #000; }
.hb-empty { color: #443; }

/* Ship's log footer */
.logline { margin: 20px 0 4px; padding-top: 10px; border-top: 3px solid #6699ff;
  color: #cc99cc; text-transform: uppercase; letter-spacing: .12em; font-size: 14px; }
.logtag { background: #6699ff; color: #000; font-weight: 700; padding: 3px 12px;
  border-radius: 12px; margin-right: 10px; }

@media (max-width: 820px) {
  .header { flex-direction: column; }
  .elbow { width: 100%; height: 54px; border-radius: 24px 24px 0 0;
    align-items: center; padding: 0 0 0 22px; }
  .hbar { width: 100%; border-radius: 0 0 8px 8px; }
  .cols { flex-direction: column; }
  .sidebar { width: 100%; flex-direction: row; flex-wrap: wrap; }
  .sidebar .bubble { flex: 1 1 160px; }
  .railcap, .sfill { display: none; }
}
@media (monochrome) {
  body { background: #fff; color: #000; }
  .txt, .bval, .day, .sd, .time, .dt, .logline { color: #000; }
  .day { border-color: #000; }
  .bubble { background: #fff; border-color: #000; }
  .sfill { background: #ddd; }
}"))

(defun lcars--css-mobile ()
  "Phone-first LCARS stylesheet: single column, thin full-height accent rail, a
condensed header + one dense info line, then tap-friendly agenda rows. Scoped
under .lcm, avoiding body/*-only selectors so email sanitizers leave it alone
(Fastmail / iOS render it faithfully)."
  (concat
"body { margin: 0; background: #000; }
.lcm { max-width: 600px; margin: 0 auto; background: #000; color: #f4e3c8;
  border-left: 12px solid #ff9900; padding: 12px 14px 24px 16px;
  font-family: 'Antonio','Oswald','Arial Narrow','Helvetica Neue',Arial,sans-serif;
  letter-spacing: .03em; }
.lcm-head { display: flex; align-items: center; gap: 12px; }
.lcm-tag { background: #ff9900; color: #000; font-weight: 700; font-size: 18px;
  padding: 4px 14px; border-radius: 4px 12px 12px 4px; letter-spacing: .14em; }
.lcm-date { color: #ffcc66; font-weight: 700; font-size: 15px;
  text-transform: uppercase; letter-spacing: .06em; }
.lcm-clock { margin-left: auto; color: #ff9900; font-weight: 700; font-size: 15px; }
.lcm-info { color: #cc99cc; font-size: 12.5px; line-height: 1.5; letter-spacing: .04em;
  text-transform: uppercase; margin: 8px 0 6px; }
.glyph { font-family: 'Apple Color Emoji','Segoe UI Emoji',sans-serif;
  text-transform: none; }

/* No per-panel rail on mobile — the outer .lcm rail is the only left bar, and
   the section pill already carries the color. Avoids a doubled-up left edge. */
.panel { display: block; margin: 12px 0; }
.rail { display: none; }
.panel-body { min-width: 0; }
.pill { display: inline-block; color: #000; font-weight: 700; font-size: 15px;
  padding: 4px 18px; border-radius: 4px 14px 14px 4px; text-transform: uppercase;
  letter-spacing: .1em; margin: 0 0 6px; }
.day { color: #cc99cc; text-transform: uppercase; font-weight: 700; font-size: 12px;
  letter-spacing: .1em; margin: 10px 0 4px; border-bottom: 1px solid #443; padding-bottom: 2px; }
.row { display: flex; align-items: baseline; gap: 8px; padding: 5px 0; font-size: 15px; }
.tick { width: 9px; height: 9px; border-radius: 2px; flex: none; align-self: center; }
.time { color: #ffcc66; font-weight: 700; min-width: 44px; }
.time-none { color: #665; }
.kw { background: #ffcc66; color: #000; font-size: 10px; font-weight: 700; padding: 1px 7px;
  border-radius: 8px; flex: none; align-self: center; }
.txt { color: #f4e3c8; flex: 1; min-width: 0; }
.cat { color: #6699ff; text-transform: uppercase; font-size: 10px; opacity: .8; }
.graph { font-family: monospace; letter-spacing: 0; white-space: nowrap; }
.hb { display: inline-block; width: 6px; text-align: center; color: #000; font-size: 11px; }
.hb-empty { color: #443; }

.lcm-log { margin-top: 18px; padding-top: 10px; border-top: 2px solid #6699ff;
  color: #cc99cc; text-transform: uppercase; letter-spacing: .1em; font-size: 13px; }
.logtag { background: #6699ff; color: #000; font-weight: 700; padding: 2px 10px;
  border-radius: 10px; margin-right: 8px; }"))

;;;; Entry point

(let* ((args (cl-remove-if (lambda (s) (string-prefix-p "-" s)) command-line-args-left))
       (mobile (or (member "--mobile" command-line-args-left)
                   (member (downcase (or (getenv "LCARS_LAYOUT") "")) '("mobile" "phone"))))
       (out (or (car args) (expand-file-name "~/lcars-digest.html")))
       (items (lcars--build-agenda))
       (html (if mobile (lcars--render-mobile items) (lcars--render items))))
  (with-temp-file out
    (insert html))
  (princ (format "wrote %s [%s] (%d agenda items)\n"
                 out (if mobile "mobile" "console")
                 (cl-count-if (lambda (i) (eq (car i) :entry)) items))))
