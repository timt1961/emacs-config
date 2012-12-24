;;; cps.el --- display characters per second input

;; Copyright (C) 2000 Raffi Krikorian <raffik@mit.edu>

;; cps.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; cps.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(defgroup display-cps nil
  "Display cps in mode line of Emacs."
  :group 'modeline)

(defcustom display-cps-mode nil
  "Toggle the display of the cps in the modeline."
  :set (lambda (symbol value)
	 (display-cps-mode (or value 0)))
  :initialize 'custom-initialize-default
  :type 'boolean
  :group 'display-cps
  :require 'cps
  :version "20.3")


(defcustom display-cps-interval 5
  "Seconds between updates of cps in the mode line."
  :type 'integer
  :group 'display-cps)

(defvar display-cps-string " 0cps ")
(defvar display-cps-timer nil)

;; the last time we did an update of the cps
(defvar display-cps-last-update-time nil)

;; the last number of keystrokes that we had processed
(defvar display-cps-last-update-keystrokes nil)


;;;###autoload
(defun display-cps ()
  "Enable display of cps input in mode line."
  (interactive)
  (display-cps-mode 1))

;;;###autoload
(defun display-cps-mode (arg)
  "Toggle display of cps input in mode line.
With a numeric arg, enable this display if arg is positive."
  (interactive "P")
  (let ((on (if (null arg)
		(not display-cps-timer)
	      (> (prefix-numeric-value arg) 0))))
    (setq display-cps-mode on)
    (and display-cps-timer (cancel-timer display-cps-timer))
    (setq display-cps-timer nil)
    (setq display-cps-string " 0cps ")
    (setq display-cps-last-update-time (current-time))
    (setq display-cps-last-update-keystrokes num-input-keys)
    (or global-mode-string (setq global-mode-string '("")))
    (if on
	(progn
	  (or (memq 'display-cps-string global-mode-string)
	      (setq global-mode-string
		    (append global-mode-string '(display-cps-string))))
	  (setq display-cps-timer
		(run-at-time t
			     display-cps-interval
			     'display-cps-event-handler))
	  (display-cps-update)))))

(defun display-cps-event-handler ()
  (display-cps-update)
  (sit-for 0)
  (let* ((current (current-time))
	 (timer display-cps-timer)
	 ;; compute the time when this timer will run again, next.
	 (next-time (timer-relative-time
		     (list
		      (aref timer 1)
		      (aref timer 2)
		      (aref timer 3))
		     (* 5 (aref timer 4)) 0)))
    ;; if the activation time is far in the past, skip executions
    ;; until we reach a time in the future.  this avoids a long pause
    ;; if emacs has been suspended for hours.  hopefully -- i haven't
    ;; taken a good look at this piece of code -- it is ripped out of
    ;; time.el
    (or (> (nth 0 next-time) (nth 0 current))
	(and (= (nth 0 next-time) (nth 0 current))
	     (> (nth 1 next-time) (nth 1 current)))
	(and (= (nth 0 next-time) (nth 0 current))
	     (= (nth 1 next-time) (nth 1 current))
	     (> (nth 2 next-time) (nth 2 current)))
	(progn
	  (timer-set-time timer (timer-next-integral-multiple-of-time
				 current
				 display-cps-interval)
			  display-cps-interval)
	  (timer-activate timer)))))

(defun display-cps-update ()
  (let ((now (current-time)))
    ;; first make sure that we are not going to get a division by zero
    ;; error
    (if (not (= (nth 1 now)
		(nth 1 display-cps-last-update-time)))
	;; calculate the cps -- the past time is stored and not simply
	;; subtracted in case of suspension and stuff like that.
	;; there is definately a bug in the way that the time is
	;; subtracted.  haven't given much though to the handling of
	;; the 32 bit number
	(let* ((keys num-input-keys)
	       (cps (/ (- keys display-cps-last-update-keystrokes)
		       (- (nth 1 now) (nth 1 display-cps-last-update-time)))))
	  ;; update our variables to make sure that we have everything
	  ;; set
	  (setq display-cps-last-update-keystrokes keys)
	  (setq display-cps-last-update-time now)
	  (setq display-cps-string
		(format " %dcps " cps)))))
  ;; the force update happens outside the let so that the characters
  ;; per second will update itself even when nobody is typing
  (force-mode-line-update))


(if display-cps-mode
    (display-cps-mode t))

(provide 'cps)

;;; cps.el ends here
