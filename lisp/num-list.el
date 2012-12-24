;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Numbered list renumbering utility
;;  Copyright (C) 1992, Steve Koren
;;
;;  16-Sep-94: Modified by Fritz Knabe <knabe@acm.org>
;;	Reimplemented renumber-column
;;
;;  In accordance with the GNU license, this file may be freely
;;  distributed and copied, provided that further distribution is not
;;  restricted.
;;
;;  There is no warranty on this software; it is distrubted freely and
;;  therefore 'as is'.
;;
;;  These routines allow renumbering of some types of numbered lists.
;;  For example, if you have a list like this:
;;
;;    1.  first item
;;    2.  second item
;;        second item continued
;;    3.  third item
;;    4.  fourth item
;;
;;  You can insert a new item after "2", and then renumber the list from
;;  one to 5.  This is helpful for long numbered lists.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar renum-number-regexp "\\(^\\s-*\\)\\([0-9]+\\)\."
  "*Regular expression which matches numbered lines.")

(defun renumber-lines-region (start end &optional start-num step)
  "Renumbers a list of numbered items in a region.
For example, this list:

   1.  mumble
       mumble line two
   2.  blah
   2.  foo
   3.  bar
  11.  test

is renumbered as follows:

   1.  mumble
       mumble line two
   2.  blah
   3.  foo
   4.  bar
   5.  test

This is useful when you have numbered lists or paragraphs, and need to
insert a new item in the middle of your old list.  Instead of
renumbering all the subsequent items by hand, use renumber-lines-region
on the region containing the list.

renumber-lines-region looks for any line matching renum-number-regexp,
and assigns numbers incrementally.  If the old number is replaced by a
shorter one (such as in the 11 -> 5 example above) it is padded on the
left with whitespace.  If the old number is replaced by a longer one,
the new number is moved left if there is whitespace available for it.
Lines which do not match renum-number-regexp are ignored.

Called non-interactively, the parameters are:

     (start end &optional start-num step)

Where start is the beginning position of the region, end is the end,
start-num is the number to assign to the first line (defaults to 1), and
step is the amount to increment the count between lines (also defaults
to 1).
"

  (interactive "r")
  (let ((num (or (and (numberp start-num) start-num) 1))
        (step-size (or step 1))
        length
        insert-len)

    (save-excursion
      (save-restriction
        (goto-char start)           ;; goto beginning of region
        (narrow-to-region start end)
        (while (re-search-forward renum-number-regexp (point-max) t)
          (beginning-of-line)
          ;; go to the start of the number part
          (goto-char (match-beginning 2))
  
          ;; find the length of the number
          (setq length (- (match-end 2) (match-beginning 2)))
          
          ;; delete that many characters
          (delete-char length)
          
          ;; find out how many characters we are about to insert
          (setq insert-len (length (int-to-string num)))
          
          ;; if we are not inserting enough, pad ourself with
          ;; spaces.  If we are inserting too many, then try
          ;; moving left until we have enough room.  If there is
          ;; no more whitespace to the left, just insert the
          ;; number anyway and don't worry about it.
  
          (if (<= insert-len length)
              (insert (make-string (- length insert-len) ? ))
            (while (and (or (= (preceding-char) ? )
                            (= (preceding-char) ?\t))
                        (> insert-len length)
                   )
                (backward-delete-char-untabify 1)
                (setq length (+ length 1))
             )
          )
  
          ;; insert the number here, and increment it.
          (insert (int-to-string num))
          (setq num (+ num step-size))
          (end-of-line)  ;; so we don't see what we just inserted again
        )
      )
    )
  )
)

(defun renumber-column (&optional right-justify start-num step)
  "Renumber a column of numbers starting at the cursor position.
The numbers are left-justified to the starting cusor position, and renumbering
is started at 1 with a step of 1. Providing a prefix argument allows these
defaults to be changed."
  (interactive
   (if current-prefix-arg
       (list (y-or-n-p "Right justify? ")
	     (string-to-int (read-from-minibuffer "Starting index: " "1"))
	     (string-to-int (read-from-minibuffer "Step: " "1")))))

  (let ((num (or start-num 1))
        (step-size (or step 1))
	(middle (current-column))
	start end startp insert-len)

    (save-excursion
      (while (and (looking-at "[0-9]") (eq (current-column) middle))
	
	;; find the left edge of the <whitespace><number><whitespace> field
	(skip-chars-backward "0-9")
	;; just stop with the left edge of the numbers for left-justification,
	;; since we don't need to remove whitespace
	(if right-justify
	    (progn
	      (skip-chars-backward " \t")
	      (setq start (- (current-column) 1)))
	  (setq start (current-column)))
	(setq startp (point))

	;; now find the right edge
	(move-to-column middle)
	(skip-chars-forward "0-9")
	(skip-chars-forward " \t")
	(setq end (current-column))

	;; delete the whole field
	(delete-region startp (point))

	;; find out how many characters we are about to insert
	(setq insert-len (length (int-to-string num)))
	
	(if right-justify
	    (progn
	      ;; put in the padding (none if the number overflows the
	      ;; left side of the field)
	      (insert (make-string (max 0 (- middle start insert-len)) ? ))
	      ;; insert the number
	      (insert (int-to-string num))
	      ;; put in the padding on the right
	      (insert (make-string (max 0 (- end (current-column))) ? )))

	  (insert (make-string (- middle start) ? ))
	  (insert (int-to-string num))
	  (insert (make-string (max 0 (- end (current-column))) ? )))

	;; increment and move
	(setq num (+ num step-size))
	(move-to-column middle)
	(next-line 1)))))
