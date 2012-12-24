;;
;; Copied from the net. Have to write some code around this to make sure
;; we have a reasonable set of functions (And get a set of one-liners from
;; somewhere.)
(defun jmv-insert-random-line (file)
  "Select a random line from the FILE and insert it into the text at the 
current point.  Does not check for missing FILE."
  (interactive "fFilename: ")
  (save-excursion
    (let ((buf (current-buffer))
	  total)
      (set-buffer (find-file-noselect file))
      (setq buffer-read-only t)
      (setq total (count-lines (point-min) (point-max)))
      (cond ((< total 1) 
	     (set-buffer buf)
	     (insert-string "Nothing"))
	    (t 
	     (goto-line (random total))
	     (beginning-of-line)
	     (let ((beg (point)))
	       (forward-line)
	       (copy-region-as-kill beg (point)))
	     (set-buffer buf)
	     (yank))
	    )
      )))


