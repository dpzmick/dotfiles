(defun dpzmick/insert-shell-command-output (command)
  (insert (shell-command-to-string command)))
