Return-Path: <help-gnu-emacs-request@prep.ai.mit.edu>
To: help-gnu-emacs@prep.ai.mit.edu
Date: Wed, 4 Jan 1995 21:22:02 GMT
Organization: ml.com
From: tg@etsd.ml.com ( tg )
Sender: help-gnu-emacs-request@prep.ai.mit.edu
Reply-To: tg@etsd.ml.com
Subject: Automatic pop-mail for Emacs
Content-Type: text
Content-Length: 5391

I have Emacs 19.19.2.
The following hack for automatically popping e-mail in an emacs buffer as e-mail
arrives used to work for earlier versions.

Could someone please fix it for version 19.19.2 and send me the fix?
 
My email is tg@etsd.ml.com.

Thanx



-------------------------------
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;	File:     popmail.el						     ;;
;;	Author:   Wolfgang S. Rupprecht <wolfgang@wsrcc.com>                 ;;
;;	Created:  Thu Sep 17 14:55:45 EDT 1987				     ;;
;;	Contents: pop up the mail buffer and incorportate any new mail       ;;
;;                whenever new mail arrives  	                             ;;
;;									     ;;
;;	Copyright (c) 1994, 1991, 1987 Wolfgang Rupprecht.		     ;;
;; 	Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.        ;;
;;									     ;;
;;	$Log$								     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This version is for FSF v.19.  It has been tested with 19.27

;; GNU Emacs and this file popmail.el, are distributed in the hope
;; that they will be useful, but WITHOUT ANY WARRANTY.  No author or
;; distributor accepts responsibility to anyone for the consequences of
;; using them or for whether they serve any particular purpose or work at
;; all, unless he says so in writing.  Refer to the GNU Emacs General
;; Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs and popmail.el, but only under the conditions described in the
;; GNU Emacs General Public License.  A copy of this license is supposed
;; to have been given to you along with GNU Emacs so you can know your
;; rights and responsibilities.  It should be in a file named COPYING.
;; Among other things, the copyright notice and this notice must be
;; preserved on all copies.

;; don't let two pop-mails be active at once.
(defvar pop-mail-lockfile (expand-file-name "~/.pop-mail.lock")
  "The pop-mail lock file.")

(defvar inhibit-pop-mail-start nil "If non-nil don't start up pop-mail.")
(defvar pop-mail nil "*If t display new mail in a pop up mail buffer.
	files listed in rmail-primary-inbox-list are watched")
(defvar pop-mail-ding t "*If non-nil ding bell, when new mail pops up.")
(defvar pop-mail-hook nil "*Optional hook to run when new mail arrives.")

(defvar pop-mail-file "~/RMAIL"
  "*The file into which mail will be popped.
Use ~/INBOX for the vm mail-reader.")

(defvar pop-mail-command 'rmail
  "*The command to invoke when mail appears.
Either 'rmail 'vm will work nicely.")

(setq display-time-string-forms
      '((if display-time-day-and-date
	    (format "%s %s %s " dayname monthname day)
	  "")
	(format "%s:%s%s"
		(if display-time-24hr-format 24-hours 12-hours)
		minutes
		(if display-time-24hr-format "" am-pm))
	load
	(if mail (do-pop-mail) "")
	)
      )

(defun stop-pop-mail ()
  "Stop pop-mail."
  (interactive)
  (if (and pop-mail (file-exists-p pop-mail-lockfile))
      (delete-file pop-mail-lockfile))
  ;; lock file crap doesn't work.
  ;;      (save-excursion
  ;; 	(let ((buf (get-buffer-create " tmp")))
  ;; 	  (set-buffer buf)
  ;; 	  (set-visited-file-name pop-mail-lockfile)
  ;; 	  (unlock-buffer)
  ;; 	  ;; (erase-buffer)
  ;; 	  ;; (save-buffer)
  ;; 	  (delete-file pop-mail-lockfile))))
  (remove-hook 'kill-emacs-hook 'stop-pop-mail)
  (setq pop-mail nil))

(defun start-pop-mail ()
  "Start up pop-mail."
  (interactive)
  (if (file-exists-p pop-mail-lockfile)
      (message "Pop-mail NOT started.  Lock file %s exists." pop-mail-lockfile)
    (if inhibit-pop-mail-start
	(message "Pop-mail NOT requested.")
      ;; assumes real users > 100
      (if (> 100 (user-uid) )
	  (message "Pop-mail not started because uid < 100")
	(add-hook 'kill-emacs-hook 'stop-pop-mail)
	(save-excursion
	  (let ((buf (get-buffer-create " tmp")))
	    (set-buffer buf)
	    (erase-buffer)
	    (insert (system-name))
	    (write-region (point-min) (point-max) pop-mail-lockfile 
			  nil 'noprint)))

	;; lock crap doesn't work.  It queries user about file changing...
	;;   (save-excursion
	;;     (let ((buf (get-buffer-create " tmp")))
	;;       (set-buffer buf)
	;;       (lock-buffer pop-mail-lockfile)
	;;       (erase-buffer)
	;;       ;; (insert (format "%s:%s" (system-name) (getpid???)))
	;;       (insert (system-name))
	;;       (write-region (point-min) (point-max) pop-mail-lockfile nil 'noprint)
	;;       ;; Previous write clears the lock.  Set it again.
	;;       (lock-buffer pop-mail-lockfile)
	;;       ))

	(setq pop-mail t)
	(display-time)))))

(defun do-pop-mail ()
  "Incorportate new mail into user's mail file."
  ;; If we have the file active, don't play with it.
  (if (get-file-buffer pop-mail-file)
      " MoreMail"
    ;; if in minibuffer - just do Mail flag. 
    (if (string-match " \\*Minibuf-[0-9]+\\*" (buffer-name))
	" Mail"
      (save-window-excursion (funcall pop-mail-command))
      (if pop-mail-ding
	  (let ((visible-bell nil))
	    (ding)))

      ;; for popping in a separate frame:
      ;;      (let ((pop-up-frames t))
      ;;	(display-buffer (get-file-buffer pop-mail-file)))

      (display-buffer (get-file-buffer pop-mail-file))
      (if (eq (frame-visible-p (selected-frame)) 'icon)
	  (iconify-or-deiconify-frame))
      (run-hooks 'pop-mail-hook)
      " Gotit")))
; end


