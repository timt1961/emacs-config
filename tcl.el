;; Tcl code editing commands for Emacs
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(defvar tcl-mode-abbrev-table nil
  "Abbrev table in use in Tcl-mode buffers.")
(define-abbrev-table 'tcl-mode-abbrev-table ())

(defvar tcl-mode-map ()
  "Keymap used in Tcl mode.")

(if tcl-mode-map
    ()
  (setq tcl-mode-map (make-sparse-keymap))
  (define-key tcl-mode-map "{" 'electric-tcl-brace)
  (define-key tcl-mode-map "}" 'electric-tcl-brace)
  (define-key tcl-mode-map "[" 'electric-tcl-brace)
  (define-key tcl-mode-map "]" 'electric-tcl-brace)
  (define-key tcl-mode-map "\e\C-a" 'tcl-beginning-of-defun)
  (define-key tcl-mode-map "\e\C-e" 'tcl-end-of-defun)
  (define-key tcl-mode-map "\e\C-q" 'indent-tcl-exp)
  (define-key tcl-mode-map "\177" 'backward-delete-char-untabify)
  (define-key tcl-mode-map "\t" 'tcl-indent-command))

(defvar tcl-mode-syntax-table nil
  "Syntax table in use in Tcl-mode buffers.")

(if tcl-mode-syntax-table
    ()
  (setq tcl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?%  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?&  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?*  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?+  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?-  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?/  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?<  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?=  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?>  "." tcl-mode-syntax-table)
  (modify-syntax-entry ?|  "." tcl-mode-syntax-table))

(defvar tcl-indent-level 4
  "*Indentation of Tcl statements with respect to containing block.")

(defvar tcl-auto-newline nil
  "*Non-nil means automatically newline before and after braces,
and after colons and semicolons, inserted in Tcl code.")

(defvar tcl-tab-always-indent t
  "*Non-nil means TAB in Tcl mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defun tcl-mode ()
  "Major mode for editing Tcl code.
Expression and list commands understand all Tcl brackets.
Tab indents for Tcl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{tcl-mode-map}
Variables controlling indentation style:
 tcl-tab-always-indent
    Non-nil means TAB in Tcl mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 tcl-auto-newline
    Non-nil means automatically newline before and after braces,
    inserted in Tcl code.
 tcl-indent-level
    Indentation of Tcl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.

Turning on Tcl mode calls the value of the variable tcl-mode-hook with no args,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map tcl-mode-map)
  (setq major-mode 'tcl-mode)
  (setq mode-name "Tcl")
  (setq local-abbrev-table tcl-mode-abbrev-table)
  (set-syntax-table tcl-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'tcl-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "\n")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'tcl-mode-hook))


(defun electric-tcl-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if (and tcl-auto-newline 
			  (or (= last-command-char ?{)
			      (= last-command-char ?{)))
		     (progn
		       (tcl-indent-line)
		       (newline) 
		       t)
		   nil)))
	(progn
	  (insert last-command-char)
	  (tcl-indent-line)
	  (if tcl-auto-newline
	      (progn
		(newline)
		(setq insertpos (- (point) 2))
		(tcl-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))


(defun tcl-indent-command (&optional ignore)
  "Indent current line as Tcl code, or in some cases insert a tab character.
If tcl-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab."
  (interactive "P")
  (if (and (not tcl-tab-always-indent)
	   (save-excursion
	     (skip-chars-backward " \t")
	     (not (bolp))))
      (insert-tab)
    (tcl-indent-line)))

(defun tcl-indent-line ()
  "Indent current line as Tcl code.
Return the amount the indentation changed by."
  (let ((indent (calculate-tcl-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((= (following-char) ?})
		  (setq indent (- indent tcl-indent-level)))
		 ((= (following-char) ?\])
		  (setq indent (- indent 1))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun calculate-tcl-indent (&optional parse-start)
  "Return appropriate indentation for current line as Tcl code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  (continued-line 
	   (save-excursion
	     (if (bobp)
		 nil
	       (backward-char)
	       (= (preceding-char) ?\\))))
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.
	     (if continued-line tcl-indent-level 0))
	    (t
	     ;; Find the first statement in the block and indent like it.
	     (goto-char containing-sexp)
	     (forward-line)
	     ;; Skip to first non-empty line in block.
	     (while (and (< (point) indent-point)
			 (eolp))
	       (forward-line))
	     ;; Count it if it exists
	     (if (< (point) indent-point)
		 (if continued-line 
		     (+ (current-indentation) tcl-indent-level)
		   (current-indentation))
	       ;; If no previous statement, indent it relative to line
	       ;; brace is on.
	       (goto-char containing-sexp)
	       (if (/= (following-char) ?{)
		   (+ (current-column) 1)
		 (beginning-of-line)
		 (+ (current-indentation) tcl-indent-level))))))))


(defun mark-tcl-function ()
  "Put mark at end of Tcl function, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point))
  (beginning-of-defun)
  (backward-paragraph))


(defun indent-tcl-exp ()
  "Indent each line of the Tcl grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	at-else at-brace continued-line
	(opoint (point))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq inner-loop-done nil)
	(while (and (not inner-loop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (tcl-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq inner-loop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack 
		      (or (car (cdr state))
			  (save-excursion
			    (forward-sexp -1)
			    (point)))))
	  (forward-line 1)
	  (setq continued-line 
		(save-excursion
		  (backward-char)
		  (= (preceding-char) ?\\)))
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		(setq this-indent (car indent-stack))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-tcl-indent
			  (if (car indent-stack)
			      (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))
		(setq continued-line nil)))
	    (cond ((not (numberp this-indent)))
		  ((= (following-char) ?})
		   (setq this-indent (- this-indent tcl-indent-level)))
		  ((= (following-char) ?\])
		   (setq this-indent (- this-indent 1))))
	    ;; Put chosen indentation into effect.
	    (or (null this-indent)
		(= (current-column) 
		   (if continued-line 
		       (+ this-indent tcl-indent-level)
		     this-indent))
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to 
		   (if continued-line 
		       (+ this-indent tcl-indent-level)
		     this-indent)))))))))
  )

(defun tcl-beginning-of-defun (&optional arg)
  "Move backward to next beginning-of-defun.
With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (let (success)
    (while (and (>= (setq arg (- arg 1)) 0)
		(setq success (re-search-backward "^\\S " nil 'move 1)))
      (while (and (looking-at "[]#}]")
		  (setq success (re-search-backward "^\\S " nil 'move 1)))))
    (beginning-of-line)
    (not (null success))))

(defun tcl-end-of-defun (&optional arg)
  "Move forward to next end of defun.
An end of a defun is found by moving forward from the beginning of one."
  (interactive "p")
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (let ((start (point)))
    (forward-char)
    (tcl-beginning-of-defun)
    (while (> arg 0)
      (while (and (re-search-forward "^\\S " nil 'move 1)
		  (progn (beginning-of-line) t)
		  (looking-at "[]#}]")
		  (progn (forward-line) t)))
      (let ((next-line (save-excursion 
			 (forward-line)
			 (point))))
	(while (< (point) next-line)
	  (forward-sexp)))
      (forward-line)
      (if (> (point) start) (setq arg (1- arg))))))
