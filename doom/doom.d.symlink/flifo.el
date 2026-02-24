;;; united-flifo.el --- Show United WiFi flight remaining time in mode line -*- lexical-binding: t; -*-

(require 'url)
(require 'json)

(defgroup united-flifo nil
  "Show remaining flight time from unitedwifi.com FLIFO in the mode line."
  :group 'convenience)

(defcustom united-flifo-url "https://www.unitedwifi.com/api/flight/portal/v1/flifo"
  "FLIFO endpoint URL."
  :type 'string)

(defcustom united-flifo-refresh-seconds 60
  "How often to refresh the FLIFO data (seconds)."
  :type 'integer)

(defcustom united-flifo-mode-line-format "✈ %s→%s %dm"
  "Format string for the mode line.
Arguments are originAirportCode, destinationAirportCode, timeRemainingToDestination (minutes)."
  :type 'string)

(defvar united-flifo--timer nil)
(defvar united-flifo--mode-line-string "✈ --")

(defun united-flifo--set-mode-line (s)
  (setq united-flifo--mode-line-string s)
  (force-mode-line-update t))

(defun united-flifo--fetch ()
  (let ((url-request-method "GET"))
    (url-retrieve united-flifo-url #'united-flifo--on-response nil t t)))

(defun united-flifo--on-response (_status)
  (unwind-protect
      (progn
        (goto-char (point-min))
        (when (re-search-forward "\n\n" nil t)
          (let* ((json-object-type 'alist)
                 (json-array-type  'list)
                 (json-key-type    'symbol)
                 (data (condition-case _e
                           (json-read)
                         (error nil))))
            (if (not (and (listp data) (assq 'timeRemainingToDestination data)))
                (united-flifo--set-mode-line "✈ --")
              (let* ((mins (alist-get 'timeRemainingToDestination data))
                     (orig (or (alist-get 'originAirportCode data) "?"))
                     (dest (or (alist-get 'destinationAirportCode data) "?")))
                (united-flifo--set-mode-line
                 (format united-flifo-mode-line-format orig dest mins)))))))
    (kill-buffer (current-buffer))))

;;;###autoload
(define-minor-mode united-flifo-mode
  "Toggle showing United flight remaining time in the mode line."
  :global t
  :lighter ""
  (if united-flifo-mode
      (progn
        (add-to-list 'global-mode-string '(:eval united-flifo--mode-line-string) t)
        (united-flifo--fetch)
        (setq united-flifo--timer
              (run-at-time united-flifo-refresh-seconds
                           united-flifo-refresh-seconds
                           #'united-flifo--fetch)))
    (setq global-mode-string
          (delq '(:eval united-flifo--mode-line-string) global-mode-string))
    (when (timerp united-flifo--timer)
      (cancel-timer united-flifo--timer))
    (setq united-flifo--timer nil)
    (united-flifo--set-mode-line "✈ --")))

(provide 'united-flifo)
;;; united-flifo.el ends here
