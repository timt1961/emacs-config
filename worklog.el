;;; worklog.el --- keep track of stuff you do

;; Copyright (C) 1998 Kai Grossjohann

;; Author: Kai.Grossjohann@CS.Uni-Dortmund.DE
;; Maintainer: Kai.Grossjohann@CS.Uni-Dortmund.DE
;; Version: $Id: worklog.el,v 1.2 1998/10/23 14:02:22 grossjoh Exp $
;; Keywords: timetracker

;; This file is not (yet) part of GNU Emacs.

;; worklog.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation,  Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code lets you keep track of stuff you do.  It writes
;; time-stamps and some data into a file, formatted for easy parsing.
;; The format of the file is as follows: Each entry starts in the
;; beginning of a line with a date and time of the format YYYY-MM-DD
;; HH:MM (24 h clock).  Then comes whitespace and the entry itself.
;; The entry may be continued on the next line, continuation lines
;; begin with whitespace.
;;
;; The most important commands are M-x worklog RET and M-x
;; worklog-show RET (the former automatically adds an entry).  Type
;; C-h m when looking at the worklog file for information what you can
;; do.

;;; History:

;; No history yet.

;;; Code:

(defgroup worklog nil
  "Keep track of what you do.")

;;; User customizable variables:

(defcustom worklog-file "~/.worklog"
  "Where worklog info should be put."
  :type 'file
  :group 'worklog)

(defcustom worklog-fill-prefix "   "
  "Fill prefix to use for worklog mode."
  :type 'string
  :group 'worklog)

(defcustom worklog-mode-hook nil
  "Standard mode hook."
  :type 'hook
  :group 'worklog)

;;; Other variables:

(defvar worklog-mode-map
  (let ((m (copy-keymap text-mode-map)))
    (define-key m "\C-c\C-c" 'worklog-finish)
    (define-key m "\C-c\C-a" 'worklog-add-entry)
    (define-key m "\C-c\C-d" 'worklog-kill-entry)
    (define-key m "\C-c\C-k" 'worklog-kill-entry)
    (define-key m "\C-c\C-l\C-i" 'worklog-add-entry-login)
    (define-key m "\C-c\C-l\C-o" 'worklog-add-entry-logout)
    (define-key m "\C-c\C-n" 'worklog-forward-entry)
    (define-key m "\C-c\C-p" 'worklog-backward-entry)
    (define-key m "\M-n" 'worklog-forward-entry)
    (define-key m "\M-p" 'worklog-backward-entry)
    m)
  "Keymap for worlog mode.")

(defvar worklog-font-lock-defaults
  (list 'worklog-font-lock-keywords nil nil nil nil)
  "Font lock defaults for worklog mode.")

(defvar worklog-font-lock-keywords
  (list "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]")
  "Font lock keywords for worklog mode.")

(defvar worklog-target-column 17
  "Where the text of an entry starts.")

;;; Entry points:

(defun worklog ()
  "Enter worklog file, add a new entry."
  (interactive)
  (find-file worklog-file)
  (worklog-mode)
  (worklog-add-entry))

(defun worklog-show ()
  "Enter worklog file."
  (interactive)
  (find-file worklog-file)
  (worklog-mode))

(defun worklog-mode ()
  "Mode for editing worklog sheets.
\\{worklog-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'worklog-mode)
  (setq mode-name "WorkLog")
  (use-local-map worklog-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults worklog-font-lock-defaults)
  (setq fill-prefix worklog-fill-prefix)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat paragraph-separate "\\|[0-9][0-9][0-9][0-9]"))
  (turn-on-auto-fill)
  (run-hooks 'text-mode-hook)
  (run-hooks 'worklog-mode-hook))

;;; Interactive commands:

(defun worklog-finish ()
  "Save and bury worklog buffer."
  (interactive)
  (save-buffer)
  (bury-buffer))

(defun worklog-add-entry ()
  (interactive)
  (end-of-buffer)
  (unless (bolp)
    (newline))
  (insert (worklog-make-date-time) "  "))

(defun worklog-add-entry-login ()
  (interactive)
  (worklog-add-entry)
  (insert "login\n"))

(defun worklog-add-entry-logout ()
  (interactive)
  (worklog-add-entry)
  (insert "logout\n"))

(defun worklog-kill-entry (n)
  (interactive "p")
  (kill-region (progn (backward-paragraph 1) (point))
               (progn (forward-paragraph n) (point))))

(defun worklog-backward-entry (n)
  "Move backward one (or N if prefix arg) entries."
  (interactive "p")
  (worklog-forward-entry (- n)))

(defun worklog-forward-entry (n)
  "Move forward one (or N if prefix arg) entries."
  (interactive "p")
  (when (< n 0) (backward-paragraph 1))
  (forward-paragraph n)
  (move-to-column worklog-target-column))

;;; Util code:

(defun worklog-make-date-time (&optional time)
  (format-time-string "%Y-%02m-%02d %H:%M" time))

;;; worklog.el ends here
