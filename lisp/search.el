;;
;; search.el : code to search for the word the point is on, and to check 
;;             for occurences of a word
;;
;; Code posted to the net July 3, 1995 by David Biesack
;;
;;
(defun re-word-search ()
  "Start an incremental search for the word at point.
Use \\[isearch-repeat-forward] to continue searching
for more occurrences."
  (interactive)
  (let (word regexp point end)
    (setq point (point))
    (while (looking-at "\\w")
      (forward-char 1))
    (setq end (point))
    (goto-char point)
    (while (and (not (bobp)) (looking-at "\\w"))
      (backward-char 1))
    (or (looking-at "\\w")
        (forward-char 1))
    (and (= (point) end)
         (error "not looking at a word"))
    (setq word (buffer-substring (point) end))
    (setq regexp (concat "\\b" (regexp-quote word) "\\b"))
    (setq regexp-search-ring (cons regexp regexp-search-ring))
    (re-search-forward regexp nil t)
    (setq this-command 'isearch-repeat-forward)
    (isearch-forward t)))
(defun word-occur ()
  "Perform \\[occur] on word at point"
  (interactive)
  (let (word regexp point end)
    (setq point (point))
    (while (looking-at "\\w")
      (forward-char 1))
    (setq end (point))
    (goto-char point)
    (while (and (not (bobp)) (looking-at "\\w"))
      (backward-char 1))
    (or (looking-at "\\w")
        (forward-char 1))
    (and (= (point) end)
         (error "not looking at a word"))
    (setq word (buffer-substring (point) end))
    (setq regexp (concat "\\b" (regexp-quote word) "\\b"))
    (occur regexp)))

