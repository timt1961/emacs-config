
;;; cal-html.el -- Generate an HTML calendar
;;; Copyright (C) 1996,1997 Greenwich Capital Markets, Inc.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Send bug reports and improvements to dcw@gcm.com

;;; $Revision: 1.2 $
;;; $Log: cal-html.el,v $
;;; Revision 1.2  1997/03/31 16:39:46  dcw
;;; Works pretty nicely
;;;
;;; Revision 1.1  1996/10/15  21:26:19  dcw
;;; initial

(defvar cal-html-title "My Calendar"
  "*Title of HTML Calendar")

(defvar cal-html-doc-header (concat "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">
<html> <head>
<title>" cal-html-title "</title>
</head>

<body>
<center><h1>" cal-html-title "</h1></center>
<br>
"))

(defvar cal-html-doc-footer "<hr>
<address></address>
</body> </html>")


(defun cal-html (date)
  "Produce an HTML calendar for MONTH, YEAR on the Gregorian calendar."
  (interactive (list (calendar-read-date t)))
  (let ((month (car date))
		(year  (nth 2 date)))
	(goto-char (point-max))
	(insert cal-html-doc-header)
	(newline)
	(insert "<table width=100% border nowrap>\n")
	(cal-html-header month year)
	(cal-html-body month year)
	(insert "</table>\n")
	(insert cal-html-doc-footer)))

(defun cal-html-header (month year)
  (insert (format "  <caption>Calendar for %s, %d</caption>"
				  (calendar-month-name month)
				  year))
  (newline)
  (insert "  <tr class=header valign=top>\n")
  (calendar-for-loop i from 0 to 6 do
					 (insert (format "    <td>%s</td>\n"
									 (substring (aref calendar-day-name-array 
													  (mod (+ calendar-week-start-day i) 7)) 
												0 2))))
  (insert "  </tr>\n"))

(defun cal-html-body (month year)
  (let ((blank-days;; at start of month
		 (mod
		  (- (calendar-day-of-week (list month 1 year))
			 calendar-week-start-day)
		  7))
		(last (calendar-last-day-of-month month year))
		(curr-buffer (current-buffer))
		(fancy-diary-buffer (get-buffer-create "*cal-html-diary-buffer*")))
	(set-buffer fancy-diary-buffer)
	(erase-buffer fancy-diary-buffer)
	(list-diary-entries (list month 1 year) last)
	(setq buffer-read-only nil)
	(set-buffer curr-buffer)
	(cal-html-start-line)
	(if (> blank-days 0)
		(progn
		  (insert "<td>\n")
		  (cal-html-quick-month (1- month) year)
		  (insert "</td>\n")
		  (if (> blank-days 1)
			  (progn
				(insert "<td>\n")
				(cal-html-quick-month (1+ month) year)
				(insert "</td>\n")
				(if (> blank-days 2)
					(progn
					  ;; Add blank days before the first of the month
					  (insert (format "<td colspan=%d></td>\n" (- blank-days 2)))))))))
	;; Put in the days of the month
	(calendar-for-loop i from 1 to last do
					   (cal-html-day (list month i year) (= i last))
					   (and (zerop (mod (+ i blank-days) 7))
							(/= i last)
							(cal-html-new-line)))
	(cal-html-end-line)
	(kill-buffer fancy-diary-buffer)))

(defun cal-html-quick-month (month year)
  (let* ((blank-days;; at start of month
          (mod
           (- (calendar-day-of-week (list month 1 year))
              calendar-week-start-day)
           7))
		 (last (calendar-last-day-of-month month year)))
	(insert "<font size=1><pre>\n")
	(newline)
    (insert (calendar-string-spread
			 (list "" (format "%s %d" (calendar-month-name month) year) "") ?  20))
	(newline)
	(calendar-for-loop i from 0 to 6 do
					   (insert (substring (aref calendar-day-name-array 
												(mod (+ calendar-week-start-day i) 7))
										  0 2))
					   (insert " "))
	(newline)
	;; Add blank days before the first of the month
   (calendar-for-loop i from 1 to blank-days do (insert "   "))
   ;; Put in the days of the month
   (calendar-for-loop i from 1 to last do
					  (insert (format "%2d " i))
					  (if (zerop (mod (+ i blank-days) 7))
						  (progn
							(/= i last)
							(newline)))))
  (insert "</pre></font>\n")
  (newline))

(defun cal-html-start-line ()
  (insert "<tr class=body valign=top>\n"))

(defun cal-html-end-line ()
  (insert "</tr>\n"))

(defun cal-html-new-line ()
  (cal-html-start-line)
  (cal-html-end-line))

(defun cal-html-day (date &optional last)
  (let ((curr-buff (current-buffer))
		header
		body
		hebrew-date
		end)
	(set-buffer fancy-diary-buffer)
	(goto-char (point-min))
	(while (looking-at "^$")
		(kill-line))
	(if (looking-at (concat "^\\(Sun\\|Mon\\|Tues\\|Wednes\\|Thurs\\|Fri\\|Satur\\)day, "
							(calendar-month-name (car date))
							" "
							(nth 1 date)
							", "
							(nth 2 date)
							"\\(: *\\)?"))
		(kill-region (match-beginning 0) (match-end 0)))
	(if (looking-at "^$")
		(kill-line))
	(while (not (looking-at "^========"))
	  (if (looking-at "^ *\\([^ ].*\\)$")
		  (progn
			(setq header (append header (list (buffer-substring (match-beginning 1) (match-end 1)))))))
	  (kill-line 1))
	(kill-line 1)
	(if last
		(goto-char (point-max))
	  (re-search-forward "^========")
	  (beginning-of-line)
	  (re-search-backward "^$"))
	(setq end (point-marker))
	(goto-char (point-min))
	(while (< (point) end)
	  (if (looking-at "^$")
		  (kill-line)
		(if (looking-at "^Hebrew date (until sunset): \\(.*\\)$")
			(setq hebrew-date (buffer-substring (match-beginning 1) (match-end 1)))
		  (if (looking-at "^.+$")
			  (setq body (append body (list (buffer-substring (match-beginning 0)
															  (match-end 0)))))))
		(kill-line 1)))
	(if (< (point) (point-max))
		(kill-line))
	(set-buffer curr-buff)
	(insert (concat "<td><table width=100% noborder nowrap>\n"
					"<tr class=header valign=top>\n"))
	(if hebrew-date
		(insert (concat "<td align=left><font size=2><strong>" hebrew-date "</strong></font></td>\n")))
	(insert (concat "<td align=right><strong>"
					(nth 1 date)
					"<strong></td></tr>\n"))
	(while header
	  (insert "<tr class=header valign=top><td align=right"
			  (if hebrew-date " colspan=2")
			  "><font size=1>"
			  (car header)
			  "</font></td></tr>\n")
	  (setq header (cdr header)))
	(while body
	  (insert "<tr class=body valign=bottom><td align=left"
			  (if hebrew-date " colspan=2")
			  "><font size=1>"
			  (car body)
			  "</font></td></tr>\n")
	  (setq body (cdr body)))
	(insert "</table></td>\n")))



(defun cal-html-foo (date &optional filename)
  (interactive (list (calendar-read-date t)))
  (let* ((month (car date))
		 (year  (nth 2 date))
		 (last (calendar-last-day-of-month month year))
		 (fancy-diary-buffer (get-buffer-create "*Temp Fancy Buffer*")))
	(set-buffer fancy-diary-buffer)
	(setq buffer-file-name (or filename "/tmp/fancy-buffer"))
	(erase-buffer fancy-diary-buffer)
	(list-diary-entries (list month 1 year) last)
	(set-buffer-modified-p t)
	(save-buffer)
	(kill-buffer fancy-diary-buffer)))

