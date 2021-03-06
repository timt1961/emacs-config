(defun increment (n)
  "Increment a number at point.  With prefix-argument, add it to the number."
  (interactive "p")
  (save-excursion
    (let ((val (char-before (point)))
	  start)
      (while (and (not (bobp))
		  (< 47 val)
		  (> 58 val))
	(backward-char)
	(setq val (char-before (point))))
      (setq start (point))
      (setq val (char-after (point)))
      (while (and (not (eobp))
		  (< 47 val)
		  (> 58 val))
	(forward-char)
	(setq val (char-after (point))))
      (setq val (buffer-substring start (point)))
      (if (zerop (length val))
	  (error "No number around point.")
	(delete-region start (point))
	(insert (int-to-string (+ (string-to-int val) n)))))))
(defun decrement (n)
  (interactive "p")
  (increment (- n)))
(provide 'increment)
(provide 'decrement)

