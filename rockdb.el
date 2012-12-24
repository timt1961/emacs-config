;;; rockdb.el : Quote random Rocky-isms
;;; Hacked from yow.el/cookie1.el

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: games

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Important pinheadery for GNU Emacs.
;;
;; See cookie1.el for implementation.  Note --- the `n' argument of yow
;; from the 18.xx implementation is no longer; we only support *random*
;; random access now.

;;; Code:

;; following is lifted from cookie1.el
(random t)

(defconst rock-delimiter "\n"
  "Delimiter used to separate rock file entries.")

(defvar rock-cache (make-vector 511 0)
  "Cache of rock files that have already been snarfed.")

;;;###autoload
(defun rock (phrase-file startmsg endmsg)
  "Return a random phrase from PHRASE-FILE.  When the phrase file
is read in, display STARTMSG at beginning of load, ENDMSG at end."
  (let ((rock-vector (rock-snarf phrase-file startmsg endmsg)))
    (shuffle-vector rock-vector)
    (aref rock-vector 1)))

;;;###autoload
(defun rock-insert (phrase-file &optional count startmsg endmsg)
  "Insert random phrases from PHRASE-FILE; COUNT of them.  When the phrase file
is read in, display STARTMSG at beginning of load, ENDMSG at end."
  (let ((rock-vector (rock-snarf phrase-file startmsg endmsg)))
    (shuffle-vector rock-vector)
    (let ((start (point)))
      (insert ?\n)
      (rock1 (min (- (length rock-vector) 1) (or count 1)) rock-vector)
      (insert ?\n)
      (fill-region-as-paragraph start (point) nil))))

(defun rock1 (arg rock-vec)
  "Inserts a rock phrase ARG times."
  (cond ((zerop arg) t)
	(t (insert (aref rock-vec arg))
	   (insert " ")
	   (rock1 (1- arg) rock-vec))))

;;;###autoload
(defun rock-snarf (phrase-file startmsg endmsg)
  "Reads in the PHRASE-FILE, returns it as a vector of strings.
Emit STARTMSG and ENDMSG before and after.  Caches the result; second
and subsequent calls on the same file won't go to disk."
  (let ((sym (intern-soft phrase-file rock-cache)))
    (and sym (not (equal (symbol-function sym)
			 (nth 5 (file-attributes phrase-file))))
	 (yes-or-no-p (concat phrase-file
			      " has changed.  Read new contents? "))
	 (setq sym nil))
    (if sym
	(symbol-value sym)
      (setq sym (intern phrase-file rock-cache))
      (message startmsg)
      (save-excursion
	(let ((buf (generate-new-buffer "*rock*"))
	      (result nil))
	  (set-buffer buf)
	  (fset sym (nth 5 (file-attributes phrase-file)))
	  (insert-file-contents (expand-file-name phrase-file))
	  (re-search-forward rock-delimiter)
	  (while (progn (skip-chars-forward " \t\n\r\f") (not (eobp)))
	    (let ((beg (point)))
	      (re-search-forward rock-delimiter)
	      (setq result (cons (buffer-substring beg (1- (point)))
				 result))))
	  (kill-buffer buf)
	  (message endmsg)
	  (set sym (apply 'vector result)))))))

(defun read-rock (prompt phrase-file startmsg endmsg &optional require-match)
  "Prompt with PROMPT and read with completion among rocks in PHRASE-FILE.
STARTMSG and ENDMSG are passed along to `rock-snarf'.
Optional fifth arg REQUIRE-MATCH non-nil forces a matching rock."
  ;; Make sure the rocks are in the cache.
  (or (intern-soft phrase-file rock-cache)
      (rock-snarf phrase-file startmsg endmsg))
  (completing-read prompt
		   (let ((sym (intern phrase-file rock-cache)))
		     ;; We cache the alist form of the rock in a property.
		     (or (get sym 'completion-alist)
			 (let* ((alist nil)
				(vec (rock-snarf phrase-file
						   startmsg endmsg))
				(i (length vec)))
			   (while (> (setq i (1- i)) 0)
			     (setq alist (cons (list (aref vec i)) alist)))
			   (put sym 'completion-alist alist))))
		   nil require-match nil nil))

; Thanks to Ian G Batten <BattenIG@CS.BHAM.AC.UK>
; [of the University of Birmingham Computer Science Department]
; for the iterative version of this shuffle.
;
;;;###autoload
(defun shuffle-vector (vector)
  "Randomly permute the elements of VECTOR (all permutations equally likely)"
  (let ((i 0)
	j
	temp
	(len (length vector)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (aref vector i))
      (aset vector i (aref vector j))
      (aset vector j temp)
      (setq i (1+ i))))
  vector)

(defvar rock-file "/home/utt/emacs/rock.db"
   "Pertinent pinhead phrases.")

;;;###autoload
(defun quote-rock (&optional interactive)
  "Return or display a random .sig line. "
  (interactive "p")
  (let ((quote-rock (rock
	      rock-file "Am I CONSING yet?..." "I have SEEN the CONSING!!")))
    (cond ((not interactive)
	   quote-rock)
	  ((not (string-match "\n" quote-rock))
	   (delete-windows-on (get-buffer-create "*Help*"))
	   (message "%s" quote-rock))
	  (t
	   (message "Yow!")
	   (with-output-to-temp-buffer "*Help*"
	     (princ quote-yow))))))

(defsubst read-zippyism (prompt &optional require-match)
  "Read a Zippyism from the minibuffer with completion, prompting with PROMPT.
If optional second arg is non-nil, require input to match a completion."
  (read-rock prompt rock-file
	       "Am I CONSING yet?..." "I have SEEN the CONSING!!"
	       require-match))

(provide 'quote-rock)

;;; yow.el ends here


;; use interactive for debugging purposes... leave it of otherwise: interactive 
;; functions don't seem to work from the hook.
(defun rock-sign () 
  "Generate a signature at the end of the buffer"
;;  (interactive
   (progn
     (end-of-buffer)
     (insert "\n----------------------------------------------------------------------------")
     (insert "\n tim.timmerman@asml.nl                          Tel: (Int+031)-(0)40-2683613")
     (insert "\n timt@dibbler.iaehv.nl        Voodoo Programmer/Keeper of the Rubber Chicken")
     (insert "\n" (quote-rock))
     (insert "\n----------------------------------------------------------------------------")
     )
;;   )
)
	     