;;; cycle-mini.el --- Cycle through possible minibuffer completions
;; Copyright (C) 1994 Joseph W. Reiss

;; Author:   Joe Reiss <jreiss@magnus.acs.ohio-state.edu>
;; Created:  26 Aug 1994
;; Version:  1.02
;; Keywords: minibuffer, completion
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or 
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; cycle-mini|Joe Reiss|jreiss@magnus.acs.ohio-state.edu|
;; Make arrow keys cycle through completions in minibuffer.|
;; 11-Mar-1995|1.02|~/misc/cycle-mini.el.Z|

;;; Commentary:

;; This is an extension to the completing-read commands in the
;; minibuffer.  It allows you to cycle through the current list of
;; completions.  This works when changing buffers, with any command
;; which reads a function or variable name, or with a programmer 
;; specified completion list.  It even works with functions which
;; read a file name!  In addition, if you have part of a name already
;; typed in, cycle-mini will use that string to narrow down the matches
;; and will only cycle through the completions which contain that 
;; initial substring.

;;; Default bindings:

;; ^P,[up]	Display previous matching completion.
;; ^N,[down]	Display next matching completion.
;; TAB		Accept currently displayed completion and move cursor
;;		to end of line.  If no completion is displayed, call
;;		minibuffer-complete as usual.
;;
;; Typing any other key will clear the currently displayed completion
;; and allow you to edit as normal.

;;; Installation:

;; Byte-compile, then put this file in one of your elisp load
;; directories and add the following line to your .emacs file
;;
;; (load "cycle-mini")
;;
;; If, for some strange reason, you use different commands to exit the
;; minibuffer than the standard commands normally bound to RET, you
;; can modify the cycle-mini-exit-commands list to include the names
;; of any functions for which the current completion should *NOT* be
;; deleted prior to their execution.
;;
;; (setq cycle-mini-exit-commands
;;       '(exit-minibuffer minibuffer-complete-and-exit
;;         my-exit-minibuffer-command))

;;; History:

;; This code was inspired by elec-mini.el, found on the elisp-archive
;; but without any credit given to the original author.  I think any
;; code originally borrowed from that file has since been rewritten
;; beyond all recognition.  Some portions of this code are also
;; borrowed from Ken Manheimer's icomplete.el in the GNU Emacs
;; distribution.
;;
;; v1.02
;;  - added variables cycle-mini-sort-buffers, cycle-mini-wrap,
;;    cycle-mini-allow-edit, and cycle-mini-disable-bindings,
;;    which toggle several new options.  See variable docstrings
;;    for details
;;  - renamed several functions to make them more manageable
;; v1.01
;;  - fixed bug with file completions under Xemacs
;;  - added bindings for ^N and ^P
;;  - removed old code from elec-mini
;; v1.00
;;  - first public release

;;; Known bugs: (?)

;; Not really a bug, but an ugly kludge.  In order to make file
;; completions work, cycle-mini has to make some assumptions about
;; minibuffer-completion-predicate, namely that if it's a string and
;; minibuffer-completion-table is a symbol, then we're performing file
;; name completion.  Currently, of all the elisp code and all the
;; source distributed with GNU Emacs, at least for versions 19.25 and
;; above, and with Xemacs, only file completions use the predicate
;; this strange way.  But if that use should change, or if someone
;; else should use a string for a predicate in some other way, then...
;;
;; Also note that this has been tested with FSF Emacs 19.25 and
;; above and Xemacs 19.11 and above.  I don't think it should be too
;; difficult to make cycle-mini work with other variations, but I just
;; don't have access to them.  Anyone who wants to try is welcome to,
;; but please send me your modifications.

(provide 'cycle-mini)

;; User modifiable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cycle-mini-exit-commands
  '(exit-minibuffer minibuffer-complete-and-exit)
  "* List of functions that exit the minibuffer.
cycle-mini will not delete the current completion if one of these commands
is called, thus allowing the displayed string to be used as the completion.")

(defvar cycle-mini-sort-buffers t
  "* Sort buffer names lexiographically during completion cycling if non-nil.
Otherwise, leave buffers sorted in natural order.")

(defvar cycle-mini-wrap t
  "* Wrap around when we reach either end of the completion list, if non-nil.
Otherwise, stop and ring the bell.")

(defvar cycle-mini-allow-edit nil
  "* Allow editing of completions, if non-nil.
Otherwise, clear completion when an editing command is used.")

(defvar cycle-mini-disable-bindings nil
  "* Don't set up any bindings for cycle-mini functions, if non-nil.
Otherwise, create some nice initial bindings.

MUST be set before cycle-mini loads.")
  

;; Internal variables.  Modified during execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cycle-mini-last-default nil
  "Indicates where we are in the list of possible completions.")
(defvar cycle-mini-eoinput 1
  "Indicates where actual user input ends.")

(defvar cycle-mini-completion-type 'other
  "The type of completion we are doing.")

;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if cycle-mini-disable-bindings
    ()
  (define-key minibuffer-local-completion-map [down]
    'cycle-mini-next-completion)
  (define-key minibuffer-local-completion-map [up]
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-completion-map "\C-n"
    'cycle-mini-next-completion)
  (define-key minibuffer-local-completion-map "\C-p"
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-completion-map "\C-i"
    'cycle-mini-accept-completion)

  (define-key minibuffer-local-must-match-map [down]
    'cycle-mini-next-completion)
  (define-key minibuffer-local-must-match-map [up]
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-must-match-map "\C-n"
    'cycle-mini-next-completion)
  (define-key minibuffer-local-must-match-map "\C-p"
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-must-match-map "\C-i"
    'cycle-mini-accept-completion)
  )

(defun cycle-mini-accept-completion ()
  "Treat completed string as if it were part of the user input.
If there is no completed string, call minibuffer-complete."
  (interactive)
  (if (null cycle-mini-last-default)	; Don't have completion displayed
      (call-interactively 'minibuffer-complete)
    (goto-char (point-max))
    (setq cycle-mini-eoinput (point)
	  cycle-mini-last-default nil)))

(defun cycle-mini-next-completion (&optional incr)
  "Replace input by next possible completion."
  (interactive)
  (or incr (setq incr 1))
  (let* ((input (buffer-substring (point-min)(point-max)))
	 (inlen (length input))
	 (comps (all-completions input minibuffer-completion-table
				 minibuffer-completion-predicate))
	 (complen (length comps))
	 (filecomp (eq cycle-mini-completion-type 'file)))
    (if (or (not (eq cycle-mini-completion-type 'buffer))
	    cycle-mini-sort-buffers)
	(setq comps (sort comps 'string<)))
    (cond
     ((null comps)			; No matches
      (save-excursion
	(goto-char (point-max))
	(ding)
	(insert-string " [No match]")
	(goto-char (+ (point-min) inlen))
	(sit-for 2)
	(delete-region (point) (point-max))
	(setq cycle-mini-last-default nil)))
     ((= 1 complen)			; Only one exact match
      (erase-buffer)
      (insert-string
       (if filecomp
	   (file-name-directory input) "")
       (car comps) " [Sole completion]")
      (goto-char (+ (point-min) inlen))
      (sit-for 2)
      (delete-region (+ (point-min) (length (car comps))
			(if filecomp
			    (length (file-name-directory input)) 0))
		     (point-max))
      (setq cycle-mini-last-default
	    (if (string= input (car comps)) nil 0)))
     (t
      (erase-buffer)
      (if (not (and (numberp cycle-mini-last-default)
		    (>= cycle-mini-last-default 0)
		    (< cycle-mini-last-default complen)))
	  (setq cycle-mini-last-default
		(cond
		 ;; If doing filename completion, start after dot files
		 ((and filecomp (string-match "/$" input))
		  (if (< incr 0) (1- complen)
		    (let ((i 0))
		      (while (<= (aref (nth i comps) 0) ?.)
			(setq i (1+ i)))
		      i)))
		 ;; If we have exact match, start with *next* match
		 ((and (string= input
				(concat
				 (if filecomp (file-name-directory input)
				   "")
				 (car comps)))
		       (> complen 1))
		  (if (> incr 0) 1 (1- complen)))
		 ;; Otherwise, start at beginning (or end if going up)
		 (t (if (> incr 0) 0 (1- complen)))))
	(setq cycle-mini-last-default (+ cycle-mini-last-default incr))
	(if (eq cycle-mini-last-default -1)
	    (setq cycle-mini-last-default
		  (if cycle-mini-wrap (1- complen)
		    (ding) 0)))
	(if (eq cycle-mini-last-default complen)
	    (setq cycle-mini-last-default
		  (if cycle-mini-wrap 0
		    (ding) (1- complen)))))
      (insert-string
       (if filecomp
	   (file-name-directory input) "")
       (nth cycle-mini-last-default comps))
      (goto-char (+ (point-min) inlen))))
    (setq cycle-mini-eoinput (+ (point-min) inlen))))

(defun cycle-mini-previous-completion ()
  "Replace input by previous possible completion."
  (interactive)
  (cycle-mini-next-completion -1))

(defun cycle-mini-pre-command-hook ()
  "Do all necessary setup before a command runs in the minibuffer."
  (if (if cycle-mini-allow-edit
	  (not (memq this-command
		     '(cycle-mini-next-completion
		       cycle-mini-previous-completion)))
	(or (memq this-command cycle-mini-exit-commands)
	    (eq this-command 'cycle-mini-accept-completion)))
      ()
    (if (> cycle-mini-eoinput (point-max))
	;; Oops, got rug pulled out from under us - reinit:
	(setq cycle-mini-eoinput (point-max))
      (let ((buffer-undo-list buffer-undo-list)) ; prevent entry
	(delete-region cycle-mini-eoinput (point-max))))))

(defun cycle-mini-post-command-hook ()
  "Do all necessary cleanup after a command runs in the minibuffer."
  (if (or (eq this-command 'cycle-mini-next-completion)
	  (eq this-command 'cycle-mini-previous-completion))
      ()
    (setq cycle-mini-eoinput (point-max)
	  cycle-mini-last-default nil)))

(defun cycle-mini-reset ()
  "Reset minibuffer completion list to the beginning before we begin."
  (make-local-variable 'pre-command-hook)
  (make-local-variable 'post-command-hook)
  (add-hook 'pre-command-hook 'cycle-mini-pre-command-hook)
  (add-hook 'post-command-hook 'cycle-mini-post-command-hook)
  (make-local-variable 'cycle-mini-eoinput)
  (make-local-variable 'cycle-mini-completion-type)
  (setq cycle-mini-completion-type
	(cond
	 ((and (symbolp minibuffer-completion-table)
	       (stringp minibuffer-completion-predicate))
	  'file)
	 ((and (listp minibuffer-completion-table)
	       (bufferp (cdr (car minibuffer-completion-table))))
	  'buffer)
	 (t
	  'other)))
  (setq cycle-mini-eoinput (point-max)
	cycle-mini-last-default nil))

(add-hook 'minibuffer-setup-hook 'cycle-mini-reset)


