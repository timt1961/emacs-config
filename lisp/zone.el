;;; ID: $Id: zone.el,v 1.14 2000/04/05 00:00:21 ttn Exp $
;;;
;;; Copyright (C) 2000 Thien-Thi Nguyen
;;; This file is part of ttn's personal elisp library, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Description: Emacs zones out.

;;; Based on code posted to gnu.emacs.sources by Victor Zandy.
;;; Here is the original header:
;;;    From: Victor Zandy <zandy@krusty.cs.wisc.edu>
;;;    To: gnu-emacs-sources@gnu.org
;;;    Subject: Emacs Zones Out
;;;    Date: 08 Jun 1998 19:12:51 -0500

;;; Author: Victor Zandy <zandy@cs.wisc.edu>
;;; Created: June 6, 1998

;;; Commentary:

;; Don't zone out in front of Emacs!  Try M-x zone.
;; If it eventually irritates you, try M-x zone-leave-me-alone.

;; Bored by the zone pyrotechnics?  Write your own!  Add it to
;; `zone-programs'.

;; WARNING: Not appropriate for Emacs sessions over modems or
;; computers as slow as mine.

;;; Code:

(require 'timer)
(require 'cl)
(require 'tabify)

(defvar zone-timer nil)

(defvar zone-idle 20
  "*Seconds to idle before zoning out.")

;;; Vector of functions that zone out.  `zone' will execute one of
;;; these functions, randomly chosen.  The chosen function is invoked
;;; in the *zone* buffer, which contains the text of the selected
;;; window.  If the function loops, it *must* periodically check and
;;; halt if `input-pending-p' is t (because quitting is disabled when
;;; Emacs idle timers are run).
(defvar zone-programs
  (apply 'vector
	 '(
	   zone-pgm-jitter
	   zone-pgm-putz-with-case
	   zone-pgm-dissolve
;	   zone-pgm-explode
	   zone-pgm-whack-chars
	   zone-pgm-rotate
	   zone-pgm-rotate-LR-lockstep
	   zone-pgm-rotate-RL-lockstep
	   zone-pgm-rotate-LR-variable
	   zone-pgm-rotate-RL-variable
	   zone-pgm-drip
	   zone-pgm-drip-fretfully
           zone-pgm-five-oclock-swan-dive
           zone-pgm-martini-swan-dive
           zone-pgm-paragraph-spaz
           zone-pgm-stress
	   )))

(defmacro zone-orig (&rest body)
  `(with-current-buffer (get 'zone 'orig-buffer)
     ,@body))

;;;###autoload
(defun zone ()
  "Zone out, completely."
  (interactive)
  (and (timerp zone-timer) (cancel-timer zone-timer))
  (setq zone-timer nil)
  (let ((f (and window-system (selected-frame)))
        (outbuf (get-buffer-create "*zone*"))
	(text (buffer-substring (window-start) (window-end)))
	(wp (1+ (- (window-point (selected-window))
		   (window-start)))))
    (put 'zone 'orig-buffer (current-buffer))
    (set-buffer outbuf)
    (setq mode-name "Zone")
    (erase-buffer)
    (insert text)
    (switch-to-buffer outbuf)
    (untabify (point-min) (point-max))
    (set-window-start (selected-window) (point-min))
    (set-window-point (selected-window) wp)
    (sit-for 0 500)
    (let ((pgm (elt zone-programs (random (length zone-programs))))
          (ct (and f (frame-parameter f 'cursor-type))))
      (when ct (modify-frame-parameters f '((cursor-type . (bar . 0)))))
      (condition-case nil
	  (progn
            (message "Zoning... (%s)" pgm)
	    (garbage-collect)
	    (funcall pgm)
	    (message "Zoning...sorry"))
	(error
	 (while (not (input-pending-p))
	   (message (format "Vic was zoning when he wrote %s..." pgm))
	   (sit-for 3)
	   (message "...here's hoping he didn't hose your buffer!")
	   (sit-for 3)))
	(quit (ding) (message "Zoning...sorry")))
      (when ct (modify-frame-parameters f (list (cons 'cursor-type ct)))))
    (kill-buffer outbuf)
    (zone-when-idle zone-idle)))

;;;; Zone when idle, or not.

(defvar zone-timer nil
  "Timer that zone sets to triggle idle zoning out.  If t, zone won't
zone out.")

(defun zone-when-idle (secs)
  "Zone out when Emacs has been idle for SECS seconds."
  (interactive "nHow long before I start zoning (seconds): ")
  (or (<= secs 0)
      (eq zone-timer t)
      (timerp zone-timer)
      (setq zone-timer (run-with-idle-timer secs t 'zone))))

(defun zone-leave-me-alone ()
  "Don't zone out when Emacs is idle."
  (interactive)
  (and (timerp zone-timer) (cancel-timer zone-timer))
  (setq zone-timer t)
  (message "I won't zone out any more"))


;;;; zone-pgm-jitter

(defun zone-shift-up ()
  (let* ((b (point))
	 (e (progn
	      (end-of-line)
	      (if (looking-at "\n") (1+ (point)) (point))))
	 (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-max))
    (insert s)))

(defun zone-shift-down ()
  (goto-char (point-max))
  (forward-line -1)
  (beginning-of-line)
  (let* ((b (point))
	 (e (progn
	      (end-of-line)
	      (if (looking-at "\n") (1+ (point)) (point))))
	 (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-min))
    (insert s)))

(defun zone-shift-left ()
  (while (not (eobp))
    (or (eolp)
	(let ((c (following-char)))
	  (delete-char 1)
	  (end-of-line)
	  (insert c)))
    (forward-line 1)))

(defun zone-shift-right ()
  (while (not (eobp))
    (end-of-line)
    (or (bolp)
	(let ((c (preceding-char)))
	  (delete-backward-char 1)
	  (beginning-of-line)
	  (insert c)))
    (forward-line 1)))

(defun zone-pgm-jitter ()
  (let ((ops (apply 'vector
		    '(
		      zone-shift-left
		      zone-shift-left
		      zone-shift-left
		      zone-shift-left
		      zone-shift-right
		      zone-shift-down
		      zone-shift-down
		      zone-shift-down
		      zone-shift-down
		      zone-shift-down
		      zone-shift-up
		      ))))
    (goto-char (point-min))
    (while (not (input-pending-p))
      (funcall (elt ops (random (length ops))))
      (goto-char (point-min))
      (sit-for 0 10))))


;;;; zone-pgm-whack-chars

(defvar zone-wc-tbl
  (let ((tbl (make-string 128 ?x))
	(i 0))
    (while (< i 128)
      (aset tbl i i)
      (setq i (1+ i)))
    tbl))

(defun zone-pgm-whack-chars ()
  (let ((tbl (copy-sequence zone-wc-tbl)))
    (while (not (input-pending-p))
      (let ((i 48))
	(while (< i 122)
	  (aset tbl i (+ 48 (random (- 123 48))))
	  (setq i (1+ i)))
	(translate-region (point-min) (point-max) tbl)
	(sit-for 0 2)))))


;;;; zone-pgm-dissolve

(defun zone-remove-text ()
  (let ((working t))
    (while working
      (setq working nil)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "[^(){}\n\t ]")
	      (let ((n (random 5)))
		(if (not (= n 0))
		    (progn
		      (setq working t)
		      (forward-char 1))
		  (delete-char 1)
		  (insert " ")))
	    (forward-char 1))))
      (sit-for 0 2))))

(defun zone-pgm-dissolve ()
  (zone-remove-text)
  (zone-pgm-jitter))


;;;; zone-pgm-explode

(defun zone-exploding-remove ()
  (let ((i 0))
    (while (< i 20)
      (save-excursion
	(goto-char (point-min))
	(while (not (eobp))
	  (if (looking-at "[^*\n\t ]")
	      (let ((n (random 5)))
		(if (not (= n 0))
		    (forward-char 1))
		  (insert " ")))
	    (forward-char 1)))
      (setq i (1+ i))
      (sit-for 0 2)))
  (zone-pgm-jitter))

(defun zone-pgm-explode ()
  (zone-exploding-remove)
  (zone-pgm-jitter))


;;;; zone-pgm-putz-with-case

;;; Faster than `zone-pgm-putz-with-case', but not as good: all
;;; instances of the same letter have the same case, which produces a
;;; less interesting effect than you might imagine.
(defun zone-pgm-2nd-putz-with-case ()
  (let ((tbl (make-string 128 ?x))
	(i 0))
    (while (< i 128)
      (aset tbl i i)
      (setq i (1+ i)))
    (while (not (input-pending-p))
      (setq i ?a)
      (while (<= i ?z)
	(aset tbl i
	      (if (zerop (random 5))
		  (upcase i)
		(downcase i)))
	(setq i (+ i (1+ (random 5)))))
      (setq i ?A)
      (while (<= i ?z)
	(aset tbl i
	      (if (zerop (random 5))
		  (downcase i)
		(upcase i)))
	(setq i (+ i (1+ (random 5)))))
      (translate-region (point-min) (point-max) tbl)
      (sit-for 0 2))))

(defun zone-pgm-putz-with-case ()
  (goto-char (point-min))
  (while (not (input-pending-p))
    (let ((np (+ 2 (random 5)))
	  (pm (point-max)))
      (while (< np pm)
	(goto-char np)
        (let ((prec (preceding-char))
              (props (text-properties-at (1- (point)))))
          (insert (if (zerop (random 2))
                      (upcase prec)
                    (downcase prec)))
          (set-text-properties (1- (point)) (point) props))
	(backward-char 2)
	(delete-char 1)
	(setq np (+ np (1+ (random 5))))))
    (goto-char (point-min))
    (sit-for 0 2)))


;;;; zone-pgm-rotate

(defun zone-line-specs ()
  (let (ret)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) (window-end))
	(when (looking-at "[\t ]*\\([^\n]+\\)")
	  (push (cons (match-beginning 1) (match-end 1)) ret))
	(forward-line 1)))
    ret))

(defun zone-pgm-rotate (&optional random-style)
  (let* ((specs (apply
		 'vector
		 (remove-if
		  (lambda (ent) (= 0 (aref ent 0)))
		  (mapcar (lambda (ent)
			    (let* ((beg (car ent))
				   (end (cdr ent))
				   (amt (if random-style
					    (funcall random-style)
					  (- (random 7) 3))))
			      (when (< (- end (abs amt)) beg)
				(setq amt (random (- end beg))))
			      (vector amt beg (- end (abs amt)))))
			  (zone-line-specs)))))
	 (n (length specs))
	 amt aamt cut paste txt i ent)
    (while (not (input-pending-p))
      (setq i 0)
      (while (< i n)
	(setq ent (aref specs i))
	(setq amt (aref ent 0) aamt (abs amt))
	(if (> 0 amt)
	    (setq cut 1 paste 2)
	  (setq cut 2 paste 1))
	(goto-char (aref ent cut))
	(setq txt (buffer-substring (point) (+ (point) aamt)))
	(delete-char aamt)
	(goto-char (aref ent paste))
	(insert txt)
	(setq i (1+ i)))
      (sit-for 0.04))))

(defun zone-pgm-rotate-LR-lockstep ()
  (zone-pgm-rotate (lambda () 1)))

(defun zone-pgm-rotate-RL-lockstep ()
  (zone-pgm-rotate (lambda () -1)))

(defun zone-pgm-rotate-LR-variable ()
  (zone-pgm-rotate (lambda () (1+ (random 3)))))

(defun zone-pgm-rotate-RL-variable ()
  (zone-pgm-rotate (lambda () (1- (- (random 3))))))


;;;; zone-pgm-drip

(defun zone-pgm-drip (&optional fret-p pancake-p)
  (flet ((cpos (pos) (buffer-substring pos (1+ pos)))
	 (fret
	  (pos)
	  (let* ((case-fold-search nil)
		 (c-string (cpos pos))
		 (hmm (cond
		       ((string-match "[a-z]" c-string) (upcase c-string))
		       ((string-match "[A-Z]" c-string) (downcase c-string))
		       (t " "))))
	    (do ((i 0 (1+ i))
		 (wait 0.5 (* wait 0.8)))
		((= i 20))
	      (goto-char pos)
	      (delete-char 1)
	      (insert (if (= 0 (% i 2)) hmm c-string))
	      (sit-for wait))
	    (delete-char -1) (insert c-string)))
	 (fall-through-ws
	  (c col wend)
	  (let ((fall-p nil)            ; todo: move outward
                (wait 0.15)
		(o (point))             ; for terminals w/o cursor hiding
		(p (point)))
	    (while (progn
		     (forward-line 1)
		     (move-to-column col)
		     (looking-at " "))
              (setq fall-p t)
	      (delete-char 1)
	      (insert (if (< (point) wend) c " "))
	      (save-excursion
		(goto-char p)
		(delete-char 1)
		(insert " ")
		(goto-char o)
		(sit-for (setq wait (* wait 0.8))))
	      (setq p (1- (point))))
	    fall-p))
	 ;; Add new funcs here.
	 )
    (let* ((ww (1- (window-width)))
	   (wh (window-height))
	   (mc 0)                       ; miss count
	   (total (* ww wh))
           (fall-p nil))
      (goto-char (point-min))
      ;; fill out rectangular ws block
      (while (not (eobp))
	(end-of-line)
	(let ((cc (current-column)))
	  (if (< cc ww)
	      (insert (make-string (- ww cc) ? ))
	    (delete-char (- ww cc))))
	(unless (eobp)
	  (forward-char 1)))
      ;; what the hell is going on here?
      (let ((nl (- wh (count-lines (point-min) (point)))))
	(when (> nl 0)
	  (let ((line (concat (make-string (1- ww) ? ) "\n")))
	    (do ((i 0 (1+ i)))
		((= i nl))
	      (insert line)))))
      ;;
      (catch 'done			; ugh
	(while (not (input-pending-p))
	  (goto-char (point-min))
	  (sit-for 0)
	  (let ((wbeg (window-start))
                (wend (window-end)))
            (setq mc 0)
            ;; select non-ws character, but don't miss too much
            (goto-char (+ wbeg (random (- wend wbeg))))
            (while (looking-at "[ \n\f]")
	      (if (= total (setq mc (1+ mc)))
                  (throw 'done 'sel)
                (goto-char (+ wbeg (random (- wend wbeg))))))
            ;; character animation sequence
            (let ((p (point)))
	      (when fret-p (fret p))
	      (goto-char p)
              (setq fall-p
                    (fall-through-ws (cpos p) (current-column) wend))))
          ;; assuming current-column has not changed...
          (when (and pancake-p
                     fall-p
                     (< (count-lines (point-min) (point))
                        wh))
            (previous-line 1)
            (forward-char 1)
            (sit-for 0.137)
            (delete-char -1)
            (insert "@")
            (sit-for 0.137)
            (delete-char -1)
            (insert "*")
            (sit-for 0.137)
            (delete-char -1)
            (insert "_")))))))

(defun zone-pgm-drip-fretfully ()
  (zone-pgm-drip t))

(defun zone-pgm-five-oclock-swan-dive ()
  (zone-pgm-drip nil t))

(defun zone-pgm-martini-swan-dive ()
  (zone-pgm-drip t t))


;;;; zone-pgm-paragraph-spaz

(defun zone-pgm-paragraph-spaz ()
  (if (memq (zone-orig major-mode) '(text-mode fundamental-mode))
      (let ((fill-column fill-column)
            (fc-min fill-column)
            (fc-max fill-column)
            (max-fc (1- (frame-width))))
        (while (sit-for 0.1)
          (fill-paragraph 1)
          (setq fill-column (+ fill-column (- (random 5) 2)))
          (when (< fill-column fc-min)
            (setq fc-min fill-column))
          (when (> fill-column max-fc)
            (setq fill-column max-fc))
          (when (> fill-column fc-max)
            (setq fc-max fill-column))))
    (message "Zoning... (zone-pgm-rotate)")
    (zone-pgm-rotate)))


;;;; zone-pgm-stress

(defun zone-pgm-stress ()
  (goto-char (point-min))
  (let (lines bg m-fg m-bg)
    (while (< (point) (point-max))
      (let ((p (point)))
        (forward-line 1)
        (push (buffer-substring p (point)) lines)))
    (sit-for 5)
    (when window-system
      (setq bg (frame-parameter (selected-frame) 'background-color)
            m-fg (face-foreground 'modeline)
            m-bg (face-background 'modeline))
      (set-face-foreground 'modeline bg)
      (set-face-background 'modeline bg))
    (let ((msg "Zoning... (zone-pgm-stress)"))
      (while (not (string= msg ""))
        (message (setq msg (substring msg 1)))
        (sit-for 0.05)))
    (while (not (input-pending-p))
      (when (< 50 (random 100))
        (goto-char (point-max))
        (forward-line -1)
        (let ((kill-whole-line t))
          (kill-line))
        (goto-char (point-min))
        (insert (nth (random (length lines)) lines)))
      (message (concat (make-string (random (- (frame-width) 5)) ? ) "grrr"))
      (sit-for 0.1))
    (when window-system
      (set-face-foreground 'modeline m-fg)
      (set-face-background 'modeline m-bg))))

;;; $RCSfile: zone.el,v $$Revision: 1.14 $ ends here

