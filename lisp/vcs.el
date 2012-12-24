(defun vcs-co-buffer ()
  "Check out current buffer"
  (interactive "")
  (setq cmdline (concat "vcs co " buffer-file-name))
  (shell-command cmdline)
  (revert-buffer t t)
  (kill-buffer "*Shell Command Output*")
  (message (concat "Checked out file" buffer-file-name)))
