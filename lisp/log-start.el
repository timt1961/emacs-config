;;
;; File          : log-start.el
;; Creation date : 15-Apr-1994
;; Description   : 
;;  Start up code for the log emacs I usually have turned on..
;; Author        : TimT.
;;============================================================================
;; log mode:
;;  calendar at bottom
;;  starts with a diary window..
;; Some of the stuff from my default .emacs file is included here as well
;; start emacs with -q -l /home/utt/emacs/lisp/log-start.el
;;
(load-file "/home/utt/.emacs") ;; Yess.. I need everything in there, anyway

;; Useful stuff for calendar
(if emacs-19
    (progn (setq calendar-latitude  51.40)
	   (setq calendar-longitude 5.45)
	   (setq calendar-location-name "Eindhoven")
	   (require 'calendar)
	   (setq diary-display-hook 'appt-make-list)
	   (setq view-calendar-holidays-initially t)
	   (setq holidays-in-diary-buffer t)
	   (setq mark-holidays-in-calendar t)
	   (setq calendar-week-start-day 1)))
(calendar)
(diary)