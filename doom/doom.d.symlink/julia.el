(defun dpzmick/run-julia-file ()
  (interactive)
  (let
      ((starting-buffer (current-buffer))
       (filename        (buffer-file-name)))
    (switch-to-buffer "*julia-output*")
    (erase-buffer)
    ;; FIXME should run async
    ;; should try to capture the colors too
    ;; julia startup time is huge, should try keeping persistent process open
    (insert-shell-command-output (format "julia %s" filename))
    (switch-to-buffer starting-buffer)))

;; FIXME don't hardcode the pane
(defun dpzmick/run-julia-in-tmux ()
  (interactive)
  (shell-command (format "tmux send-keys -t 2 'include(\"%s\")\n'"
                         (buffer-file-name))))

;; FIXME don't hardcode the pane
(defun run-line-in-tmux ()
  (interactive)
  (let
      ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (shell-command (format "tmux send-keys -t 2 '%s\n'" line))))
