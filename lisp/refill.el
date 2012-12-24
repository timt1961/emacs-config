;;;;
;;;; Eric Eide (eeide)
;;;; September 21, 1992
;;;;
;;;; File refill.el
;;;;

(defvar fill-paragraph-by-first-line-marker (make-marker))

(defun fill-paragraph-by-first-line (arg)
  "Fill paragraph, using first line to decide what the fill-prefix is."
  (interactive "P")
  (let (beginning end)
    (set-marker fill-paragraph-by-first-line-marker (point))
    (forward-paragraph 1)
    (skip-chars-backward "\n")
    (setq end (point))
    (backward-paragraph 1)
    (skip-chars-forward "\n")
    (subst-char-in-region (point) end ?\n ?\ )
    (fill-paragraph arg)
    (goto-char (marker-position fill-paragraph-by-first-line-marker))
    (set-marker fill-paragraph-by-first-line-marker nil)))
