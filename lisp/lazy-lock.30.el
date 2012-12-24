;;; lazy-lock.el --- Lazy demand-driven fontification for fast Font Lock mode.

;; Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.

;; Author: Simon Marshall <simon@gnu.ai.mit.edu>
;; Keywords: faces files
;; Version: 2.03

;;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Purpose:
;;
;; To make visiting buffers in `font-lock-mode' faster by making fontification
;; be demand-driven, deferred and stealthy.
;; Fontification only occurs when, and where, necessary.
;;
;; See caveats and feedback below.
;; See also the fast-lock package.  (But don't use them at the same time!)

;; Installation:
;; 
;; Put this file somewhere where Emacs can find it (i.e., in one of the paths
;; in your `load-path'), `byte-compile-file' it, and put in your ~/.emacs:
;;
;; (autoload 'turn-on-lazy-lock "lazy-lock"
;;   "Unconditionally turn on Lazy Lock mode.")
;;
;; (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
;;
;; Start up a new Emacs and use font-lock as usual (except that you can use the
;; so-called "gaudier" fontification regexps on big files without frustration).
;;
;; In a buffer (which has `font-lock-mode' enabled) which is at least
;; `lazy-lock-minimum-size' characters long, buffer fontification will not
;; occur and only the visible portion of the buffer will be fontified.  Motion
;; around the buffer will fontify those visible portions not previously
;; fontified.  If stealth fontification is enabled, buffer fontification will
;; occur in invisible parts of the buffer after `lazy-lock-stealth-time'
;; seconds of idle time.  If on-the-fly fontification is deferred, on-the-fly
;; fontification will occur after `lazy-lock-defer-time' seconds of idle time.

;; Differences with version 1:
;;
;; Version 2 can defer on-the-fly fontification.  Therefore you need not, and
;; should not, use defer-lock.el with this version of lazy-lock.el.
;;
;; A number of variables have changed meaning:
;;
;; A value of nil for the variable `lazy-lock-minimum-size' now means never
;; turn on demand-driven fontification.  This is the opposite of version 1.
;; If you really want to have demand-driven fontification regardless of buffer
;; size, set this variable to 0.
;;
;; The variable `lazy-lock-stealth-lines' cannot have a nil value.  Previously,
;; this meant use `window-height' as the maximum number of lines to fontify as
;; a stealth chunk.  This makes no sense; stealth fontification is of a buffer,
;; not a window.

;; Caveats:
;;
;; Lazy Lock mode does not work efficiently with Outline mode.  This is because
;; when in Outline mode, although text may be hidden (not visible in the
;; window), the text is visible to Emacs Lisp code (not surprisingly) and Lazy
;; Lock fontifies it mercilessly.  Hopefully this will be fixed one day.
;;
;; Because buffer text is not necessarily fontified, other packages that expect
;; buffer text to be fontified in Font Lock mode either might not work as
;; expected, or might not display buffer text as expected.  An example of the
;; latter is `occur', which copies lines of buffer text into another buffer.
;;
;; In Emacs 19.30, Lazy Lock mode does not ensure that a newly visible buffer
;; is fontified if it is made visible via a minibuffer-less command that
;; replaces an old window with a new one displaying the new buffer (e.g., via
;; the Buffers menu or mouse-2 in Dired mode).  Upgrade!
;;
;; Currently XEmacs does not have the features to support lazy-lock.el.  Maybe
;; it will one day.

;; Feedback:
;;
;; Feedback is welcome.
;; To submit a bug report (or make comments) please use the mechanism provided:
;;
;; M-x lazy-lock-submit-bug-report RET
;;;;;^L
;; History:
;;
;; 1.14--2.00:
;; - Rewrite for Emacs 19.30 and the features rms added to support lazy-lock.el
;;   so that it could work correctly and efficiently.
;; - Many thanks to those who reported bugs, fixed bugs, made suggestions or
;;   otherwise contributed in the version 1 cycle; Jari Aalto, Kevin Broadey,
;;   Ulrik Dickow, Bill Dubuque, Bob Glickstein, Boris Goldowsky,
;;   Jonas Jarnestrom, David Karr, Michael Kifer, Erik Naggum, Rick Sladkey,
;;   Jim Thompson, Ben Wing, Ilya Zakharevich, and Richard Stallman.
;; 2.00--2.01:
;; - Made `lazy-lock-fontify-after-command' always `sit-for' and so redisplay
;; - Use `buffer-name' not `buffer-live-p' (Bill Dubuque hint)
;; - Made `lazy-lock-install' do `add-to-list' not `setq' of `current-buffer'
;; - Made `lazy-lock-fontify-after-install' loop over buffer list
;; - Made `lazy-lock-arrange-before-change' to arrange `window-end' triggering
;; - Made `lazy-lock-let-buffer-state' wrap both `befter-change-functions'
;; - Made `lazy-lock-fontify-region' do `condition-case' (Hyman Rosen report)
;; 2.01--2.02:
;; - Made `buffer-live-p' as `buffer-name' can barf (Richard Stanton report)
;; - Made `lazy-lock-install' set `font-lock-fontified' (Kevin Davidson report)
;; - Made `lazy-lock-install' add hooks only if needed
;; - Made `lazy-lock-unstall' add `font-lock-after-change-function' if needed
;; 2.02--2.03:
;; - Made `lazy-lock-fontify-region' do `condition-case' for `quit' too
;; - Made `lazy-lock-mode' respect the value of `font-lock-inhibit-thing-lock'
;; - Added `lazy-lock-after-unfontify-buffer'
;; - Removed `lazy-lock-fontify-after-install' hack
;; - Made `lazy-lock-fontify-after-scroll' not `set-buffer' to `window-buffer'
;; - Made `lazy-lock-fontify-after-trigger' not `set-buffer' to `window-buffer'
;; - Made `lazy-lock-fontify-after-idle' be interruptible (Scott Burson hint)
;;;;;^L
(require 'font-lock)

;; Make sure lazy-lock.el is supported.
(if (if (save-match-data (string-match "Lucid\\|XEmacs" (emacs-version)))
	t
      (or (not (boundp 'emacs-minor-version)) (< emacs-minor-version 30)))
    (error "`lazy-lock' was written for Emacs 19.30 or later"))

;; Flush out those lusers who didn't read all of the Commentary.
(if (or (memq 'turn-on-defer-lock font-lock-mode-hook)
	(memq 'defer-lock-mode font-lock-mode-hook))
    (error "`lazy-lock' was written for use without `defer-lock'"))
  
(eval-when-compile
  ;; Well, shouldn't Lazy Lock mode be as lazy as possible?
  (setq byte-compile-dynamic t byte-compile-dynamic-docstrings t)
  ;; But, we make sure that the code is as zippy as can be.
  (setq byte-optimize t))

(eval-and-compile
  ;; We use this to preserve or protect things when modifying text properties.
  (defmacro lazy-lock-let-buffer-state (varlist &rest body)
    "Bind variables according to VARLIST and eval BODY restoring buffer state."
    (` (let* ((,@ (append varlist
		   '((modified (buffer-modified-p))
		     (inhibit-read-only t) (buffer-undo-list t)
		     before-change-functions after-change-functions
		     deactivate-mark buffer-file-name buffer-file-truename))))
	 (,@ body)
	 (and (not modified) (buffer-modified-p)
	      (set-buffer-modified-p nil)))))
  (put 'lazy-lock-let-buffer-state 'lisp-indent-function 1))

;; We use this to get all windows showing a buffer we have to fontify.
(defun lazy-lock-buffer-windows (buffer &optional frame)
  "Return windows currently displaying BUFFER, or nil if none."
  (let (windows)
    (walk-windows (function (lambda (window)
			      (if (eq (window-buffer window) buffer)
				  (setq windows (cons window windows)))))
		  'no-minibuf frame)
    windows))

(or (boundp 'font-lock-inhibit-thing-lock)
    ;; Font Lock mode uses this to direct Lazy and Fast Lock modes to stay off.
    (defvar font-lock-inhibit-thing-lock nil
      "List of Font Lock mode related modes that should not be turned on."))

(or (fboundp 'buffer-live-p)
    ;; We use this to test if a cached object is a buffer we have to fontify.
    (defun buffer-live-p (object)
      "Return non-nil if OBJECT is an editor buffer that has not been deleted."
      (and (bufferp object) (buffer-name object))))
;;;;;^L
;; User Variables:

(defvar lazy-lock-minimum-size (* 25 1024)
  "*If non-nil, the minimum size of a buffer for on-demand fontification.
On-demand fontification occurs if the buffer size is greater than this value.
If nil, means on-demand fontification is never performed.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c++-mode . 25600) (c-mode . 25600) (rmail-mode . 1048576))
means that the minimum size is 25K for buffers in `c++-mode' or `c-mode', one
megabyte for buffers in `rmail-mode', and size is irrelevant otherwise.")

(defvar lazy-lock-defer-time (if (featurep 'lisp-float-type) 0.25 1)
  "*Time in seconds to delay before beginning deferred fontification.
Deferred fontification occurs if there is no input within this time.
If nil, means on-the-fly fontification is never deferred.")

(defvar lazy-lock-stealth-time 30
  "*Time in seconds to delay before beginning stealth fontification.
Stealth fontification occurs if there is no input within this time.
If nil, means stealth fontification is never performed.")

(defvar lazy-lock-stealth-lines (if font-lock-maximum-decoration 100 200)
  "*If non-nil, the maximum size of a chunk of stealth fontification.
Each iteration of stealth fontification can fontify this number of lines.
To speed up input response during stealth fontification, at the cost of stealth
taking longer to fontify, you could reduce the value of this variable.")

(defvar lazy-lock-stealth-nice (if (featurep 'lisp-float-type) 0.125 1)
  "*Time in seconds to pause between chunks of stealth fontification.
Each iteration of stealth fontification is separated by this amount of time.
To reduce machine load during stealth fontification, at the cost of stealth
taking longer to fontify, you could increase the value of this variable.")

(defvar lazy-lock-stealth-verbose
  (and (featurep 'lisp-float-type) font-lock-verbose)
  "*If non-nil, means stealth fontification should show status messages.")

(defvar lazy-lock-mode nil)		; for modeline
(defvar lazy-lock-cache-deferred nil)	; for deferral
;;;;;^L
;; User Functions:

;;;###autoload
(defun lazy-lock-mode (&optional arg)
  "Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive.  Enable it
automatically in your `~/.emacs' by:

 (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)

When Lazy Lock mode is enabled, fontification can be lazy in a number of ways:

 - Demand-driven buffer fontification if `lazy-lock-minimum-size' is non-nil.
   This means that initial fontification does not occur if the buffer is
   greater than `lazy-lock-minimum-size' characters in length.  Instead,
   fontification occurs when necessary, such as when scrolling through the
   buffer would otherwise reveal unfontified areas.  This is useful if buffer
   fontification is too slow for large buffers.

 - Stealthy buffer fontification if `lazy-lock-stealth-time' is non-nil.
   This means that remaining unfontified areas of buffers are fontified if
   Emacs has been idle for `lazy-lock-stealth-time' seconds, while Emacs
   remains idle.  This is useful if any buffer has demand-driven fontification.

 - Deferred on-the-fly fontification if `lazy-lock-defer-time' is non-nil.
   This means that on-the-fly fontification does not occur as you type.
   Instead, fontification is deferred until Emacs has been idle for
   `lazy-lock-defer-time' seconds, while Emacs remains idle.  This is useful if
   on-the-fly fontification is too slow to keep up with your typing.

See also variables `lazy-lock-stealth-lines', `lazy-lock-stealth-nice' and
`lazy-lock-stealth-verbose' for stealth fontification.

Use \\[lazy-lock-submit-bug-report] to send bug reports or feedback."
  (interactive "P")
  (set (make-local-variable 'lazy-lock-mode)
       (and (not (memq 'lazy-lock-mode font-lock-inhibit-thing-lock))
	    (if arg (> (prefix-numeric-value arg) 0) (not lazy-lock-mode))))
  (cond ((and lazy-lock-mode (not font-lock-mode))
	 ;; Turned on `lazy-lock-mode' rather than using `font-lock-mode-hook'.
	 (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)
	 (turn-on-font-lock))
	(lazy-lock-mode
	 ;; Turn ourselves on.
	 (lazy-lock-install))
	(t
	 ;; Turn ourselves off.
	 (lazy-lock-unstall))))

(defun lazy-lock-submit-bug-report ()
  "Submit via mail a bug report on lazy-lock.el."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report "simon@gnu.ai.mit.edu" "lazy-lock 2.03"
     '(lazy-lock-minimum-size lazy-lock-defer-time lazy-lock-stealth-time
       lazy-lock-stealth-nice lazy-lock-stealth-lines
       lazy-lock-stealth-verbose)
     nil nil
     (concat "Hi Si.,

I want to report a bug.  I've read the `Bugs' section of `Info' on Emacs, so I
know how to make a clear and unambiguous report.  To reproduce the bug:

Start a fresh Emacs via `" invocation-name " -no-init-file -no-site-file'.
In the `*scratch*' buffer, evaluate:"))))

;;;###autoload
(defun turn-on-lazy-lock ()
  "Unconditionally turn on Lazy Lock mode."
  (lazy-lock-mode t))

(defun lazy-lock-install ()
  ;; Add the text properties and hooks.
  (let ((min-size (if (not (consp lazy-lock-minimum-size))
		      lazy-lock-minimum-size
		    (cdr (or (assq major-mode lazy-lock-minimum-size)
			     (assq t lazy-lock-minimum-size))))))
    (make-local-variable 'font-lock-fontified)
    (setq font-lock-fontified (and min-size (>= (buffer-size) min-size)))
    (if (not font-lock-fontified)
	(lazy-lock-after-fontify-buffer)
      (lazy-lock-after-unfontify-buffer)
      ;; Only needed if lazy-lock.el is fontifying on scrolling.
      (make-local-hook 'window-scroll-functions)
      (add-hook 'window-scroll-functions 'lazy-lock-fontify-after-scroll nil t)
      ;; Make sure we fontify in any existing windows showing the buffer.
      (let ((windows (lazy-lock-buffer-windows (current-buffer) t)))
	(while windows
	  (lazy-lock-fontify-conservatively (car windows))
	  (setq windows (cdr windows))))))
  ;; Only needed if lazy-lock.el is not deferring and is fontifying.
  (if (or lazy-lock-defer-time (not font-lock-fontified))
      nil
    (make-local-hook 'before-change-functions)
    (add-hook 'before-change-functions 'lazy-lock-arrange-before-change nil t))
  ;; Only needed if lazy-lock.el is deferring.
  (if (not lazy-lock-defer-time)
      nil
    (remove-hook 'after-change-functions 'font-lock-after-change-function t)
    (add-hook 'after-change-functions 'lazy-lock-fontify-after-change nil t))
  ;; Package-specific.
  (make-local-hook 'outline-view-change-hook)
  (add-hook 'outline-view-change-hook 'lazy-lock-fontify-after-outline nil t))

(defun lazy-lock-unstall ()
  ;; Remove the text properties.
  (lazy-lock-after-unfontify-buffer)
  ;; Remove the hooks.
  (remove-hook 'window-scroll-functions 'lazy-lock-fontify-after-scroll t)
  (remove-hook 'before-change-functions 'lazy-lock-arrange-before-change t)
  ;; If Font Lock mode is still enabled, reinstall its hook.
  (if (not font-lock-mode)
      (remove-hook 'after-change-functions 'lazy-lock-fontify-after-change t)
    (remove-hook 'after-change-functions 'lazy-lock-fontify-after-change t)
    (add-hook 'after-change-functions 'font-lock-after-change-function nil t))
  ;; Package-specific.
  (remove-hook 'outline-view-change-hook 'lazy-lock-fontify-after-outline t))
;;;;;^L
;; Hook functions.

(defun lazy-lock-fontify-after-scroll (window window-start)
  ;; Called from `window-scroll-functions'.
  ;; Fontify WINDOW from WINDOW-START.  We cannot use `window-end' so we work
  ;; out what it would be via `vertical-motion'.
  (save-excursion
    (goto-char window-start)
    (vertical-motion (window-height window) window)
    (lazy-lock-fontify-region window-start (point)))
  ;; A prior deletion that did not cause scrolling, followed by a scroll, would
  ;; result in an unnecessary trigger after this if we did not cancel it now.
  (set-window-redisplay-end-trigger window nil))

(defun lazy-lock-fontify-after-trigger (window trigger-point)
  ;; Called from `redisplay-end-trigger-functions'.
  ;; Fontify WINDOW from TRIGGER-POINT.  We cannot use `window-end' so we work
  ;; out what it would be via `vertical-motion'.
  ;; We could probably just use `lazy-lock-fontify-after-scroll' without loss:
  ;;  (lazy-lock-fontify-after-scroll window (window-start window))
  (save-excursion
    (goto-char (window-start window))
    (vertical-motion (window-height window) window)
    (lazy-lock-fontify-region trigger-point (point))))

(defun lazy-lock-fontify-after-resize (frame)
  ;; Called from `window-size-change-functions'.
  ;; Fontify windows in FRAME.  We cannot use `window-start' or `window-end' so
  ;; we fontify conservatively.
  (save-excursion
    (save-selected-window
      (select-frame frame)
      (walk-windows (function (lambda (window)
		       (set-buffer (window-buffer window))
		       (if lazy-lock-mode
			   (lazy-lock-fontify-conservatively window))
		       (set-window-redisplay-end-trigger window nil)))
		    'no-minibuf frame))))

(defun lazy-lock-arrange-before-change (beg end)
  ;; Called from `before-change-functions'.
  ;; Arrange that if text becomes visible it will be fontified (if a deletion
  ;; is pending, text might become visible at the bottom).
  (if (eq beg end)
      nil
    (let ((windows (lazy-lock-buffer-windows (current-buffer) t)) window)
      (while windows
	(setq window (car windows))
	(or (markerp (window-redisplay-end-trigger window))
	    (set-window-redisplay-end-trigger window (make-marker)))
	(set-marker (window-redisplay-end-trigger window) (window-end window))
	(setq windows (cdr windows))))))

(defun lazy-lock-fontify-after-change (beg end old-len)
  ;; Called from `after-change-functions'.
  ;; Defer fontification of the current line.  Save the selected window in
  ;; which the change took place (if we are inserting in the selected window's
  ;; buffer) or the current buffer (so we subsequently fontify in all windows
  ;; showing the buffer).
  (lazy-lock-let-buffer-state nil
    (add-to-list 'lazy-lock-cache-deferred
		 (if (and (eq old-len 0) (eq (current-buffer) (window-buffer)))
		     (selected-window)
		   (current-buffer)))
    (remove-text-properties (max (1- beg) (point-min))
			    (min (1+ end) (point-max))
			    '(lazy-lock nil))))

(defun lazy-lock-fontify-after-command ()
  ;; Called from `post-command-idle-hook'.
  ;; Fontify all windows where deferral has occurred for its buffer.
  (if (and lazy-lock-cache-deferred
	   (not executing-kbd-macro)
	   (progn
	     ;; Do this so the mark is deactivated now, rather than after this
	     ;; function (and its `sit-for') finishes.  Is doing this worse?
	     (if deactivate-mark (deactivate-mark))
	     (sit-for (or lazy-lock-defer-time 0))))
      ;;
      ;; Loop over all objects, fontify windows for each if necessary.
      (while (and lazy-lock-cache-deferred (not (input-pending-p)))
	(let ((object (car lazy-lock-cache-deferred)))
	  ;; If the deferred object is a window, fontify it.  If it is a
	  ;; buffer, fontify all windows currently showing that buffer.
	  (cond ((window-live-p object)
		 (lazy-lock-fontify-window object))
		((buffer-live-p object)
		 (let ((windows (lazy-lock-buffer-windows object t)))
		   (while windows
		     (lazy-lock-fontify-window (car windows))
		     (setq windows (cdr windows))))))
	  (setq lazy-lock-cache-deferred (cdr lazy-lock-cache-deferred))))))

(defun lazy-lock-fontify-after-idle ()
  ;; Called from `post-command-idle-hook'.
  ;; Fontify all buffers that need it, stealthily while idle.
  (if (and lazy-lock-stealth-time
	   (not executing-kbd-macro)
	   (not (window-minibuffer-p (selected-window)))
	   (progn
	     ;; Do this so the mark is deactivated now, rather than after this
	     ;; function (and its `sit-for') finishes.  Is doing this worse?
	     (if deactivate-mark (deactivate-mark))
	     (sit-for lazy-lock-stealth-time)))
      ;;
      ;; Loop over all buffers, fontify stealthily for each if necessary.
      (let ((buffers (buffer-list)) (continue t) fontified message-log-max)
	(save-excursion
	  (while (and buffers continue)
	    (set-buffer (car buffers))
	    (if (not (and lazy-lock-mode (lazy-lock-unfontified-p)))
		(setq continue (not (input-pending-p)))
	      ;; Fontify regions in this buffer while there is no input.
	      (let ((bufname (buffer-name)))
		(if (and lazy-lock-stealth-verbose (not fontified))
		    (message "Fontifying stealthily..."))
		;; Implement a `do-while' loop for message purposes.
		(lazy-lock-fontify-chunk)
		(while (and (lazy-lock-unfontified-p)
			    (setq continue (sit-for lazy-lock-stealth-nice)))
		  (if lazy-lock-stealth-verbose
		      (message "Fontifying stealthily... %2d%% of %s"
			       (lazy-lock-percent-fontified) bufname))
		  (lazy-lock-fontify-chunk))
		;; Note that fontification occurred for message purposes.
		(setq fontified t)))
	    (setq buffers (cdr buffers))))
	(if (and lazy-lock-stealth-verbose fontified)
	    (message "Fontifying stealthily... %s."
		     (if continue "done" "quit"))))))

(defun lazy-lock-fontify-after-outline ()
  ;; Called from `outline-view-change-hook'.
  ;; Fontify windows showing the current buffer, as its visibility has changed.
  ;; This is a conspiracy hack between lazy-lock.el and noutline.el.
  (let ((windows (lazy-lock-buffer-windows (current-buffer) t)))
    (while windows
      (lazy-lock-fontify-conservatively (car windows))
      (setq windows (cdr windows)))))

(defun lazy-lock-after-fontify-buffer ()
  ;; Called from `font-lock-after-fontify-buffer'.
  ;; Mark the current buffer as fontified.
  ;; This is a conspiracy hack between lazy-lock.el and font-lock.el.
  (lazy-lock-let-buffer-state nil
    (add-text-properties (point-min) (point-max) '(lazy-lock t))))

(defun lazy-lock-after-unfontify-buffer ()
  ;; Called from `font-lock-after-unfontify-buffer'.
  ;; Mark the current buffer as unfontified.
  ;; This is a conspiracy hack between lazy-lock.el and font-lock.el.
  (lazy-lock-let-buffer-state nil
    (remove-text-properties (point-min) (point-max) '(lazy-lock nil))))
;;;;;^L
;; Fontification functions.

;; If packages want to ensure that some region of the buffer is fontified, they
;; should use this function.  For an example, see ps-print.el.
(defun lazy-lock-fontify-region (beg end)
  ;; Fontify between BEG and END, where necessary, in the current buffer.
  (if (setq beg (text-property-any beg end 'lazy-lock nil))
      (save-excursion
	(save-match-data
	  (lazy-lock-let-buffer-state
	      ;; Ensure syntactic fontification is always correct.
	      (font-lock-beginning-of-syntax-function next)
	    ;; Find successive unfontified regions between BEG and END.
	    (while beg
	      (setq next (or (text-property-any beg end 'lazy-lock t) end))
	      ;; Make sure the region end points are at beginning of line.
	      (goto-char beg)
	      (or (bolp) (progn (beginning-of-line) (setq beg (point))))
	      (goto-char next)
	      (or (bolp) (progn (forward-line) (setq next (point))))
	      ;; Fontify the region and flag it as fontified.
	      (condition-case error-data
		  (font-lock-fontify-region beg next)
		((error quit) (message "Fontifying region... %s" error-data)))
	      (add-text-properties beg next '(lazy-lock t))
	      (setq beg (text-property-any next end 'lazy-lock nil))))))))

(defun lazy-lock-fontify-chunk ()
  ;; Fontify the nearest chunk, for stealth, in the current buffer.
  (save-excursion
    (save-restriction
      (widen)
      ;; Move to end of line in case the character at point is not fontified.
      (end-of-line)
      ;; Find where the previous, and next, unfontified regions end, and begin.
      (let ((prev (previous-single-property-change (point) 'lazy-lock))
	    (next (text-property-any (point) (point-max) 'lazy-lock nil)))
	;; Fontify from the nearest unfontified position.
	(if (or (null prev) (and next (< (- next (point)) (- (point) prev))))
	    ;; The next, or neither, region is the nearest not fontified.
	    (lazy-lock-fontify-region
	     (progn (goto-char (or next (point-min)))
		    (beginning-of-line)
		    (point))
	     (progn (goto-char (or next (point-min)))
		    (forward-line lazy-lock-stealth-lines)
		    (point)))
	  ;; The previous region is the nearest not fontified.
	  (lazy-lock-fontify-region
	   (progn (goto-char prev)
		  (forward-line (- lazy-lock-stealth-lines))
		  (point))
	   (progn (goto-char prev)
		  (forward-line)
		  (point))))))))

(defun lazy-lock-fontify-window (window)
  ;; Fontify in WINDOW between `window-start' and `window-end'.
  ;; We can only do this when we can use `window-start' and `window-end'.
  (save-excursion
    (set-buffer (window-buffer window))
    (lazy-lock-fontify-region (window-start window) (window-end window))))

(defun lazy-lock-fontify-conservatively (window)
  ;; Fontify in WINDOW conservatively around point.
  ;; Where we cannot use `window-start' and `window-end' we do `window-height'
  ;; lines around point.  That way we guarantee to have done enough.
  (save-excursion
    (set-buffer (window-buffer window))
    (lazy-lock-fontify-region
     (save-excursion
       (vertical-motion (- (window-height window)) window) (point))
     (save-excursion
       (vertical-motion (window-height window) window) (point)))))

(defun lazy-lock-unfontified-p ()
  ;; Return non-nil if there is anywhere still to be fontified.
  (save-restriction
    (widen)
    (text-property-any (point-min) (point-max) 'lazy-lock nil)))

(defun lazy-lock-percent-fontified ()
  ;; Return the percentage (of characters) of the buffer that are fontified.
  (save-restriction
    (widen)
    (let ((beg (point-min)) (end (point-max)) (size 0) next)
      ;; Find where the next fontified region begins.
      (while (setq beg (text-property-any beg end 'lazy-lock t))
	(setq next (or (text-property-any beg end 'lazy-lock nil) end)
	      size (+ size (- next beg))
	      beg next))
      ;; Saying "99% done" is probably better than "100% done" when it isn't.
      (truncate (/ (* size 100.0) (buffer-size))))))
;;;;;^L
;; Install ourselves:

;; We don't install ourselves on `font-lock-mode-hook' as other packages can be
;; used with font-lock.el, and lazy-lock.el should be dumpable without forcing
;; people to get lazy or making it difficult for people to use alternatives.

;; Deferral and stealth.
(add-hook 'post-command-idle-hook 'lazy-lock-fontify-after-command t)
(add-hook 'post-command-idle-hook 'lazy-lock-fontify-after-idle t)
;; Window scrolling.
(add-hook 'window-size-change-functions 'lazy-lock-fontify-after-resize)
(add-hook 'redisplay-end-trigger-functions 'lazy-lock-fontify-after-trigger)

(or (assq 'lazy-lock-mode minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist '((lazy-lock-mode nil)))))

;; Provide ourselves:

(provide 'lazy-lock)

;;; lazy-lock.el ends here

