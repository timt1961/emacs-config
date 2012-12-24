;; allows you to have attachments in your (sendmail) mails

;; Author: Tom Wurgler <twurgler@goodyear.com>
;; Created: 4/04/97
;; Version: 1.1
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; attach_mail.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code just is an easy way to "attach" files to outgoing 
;; sendmail w/o making the buffer huge while you're still working
;; on it.  The attached file will be included in the mail when you
;; send the mail.

;; A little thing, but little things mean a lot...

;; This works with sendmail.el.  Just load it and maybe define a key:
;; (define-key mail-mode-map [tab] 'attach-file-to-mail)

;; To do: make this (optionally) not include the attachments to
;; addresses in the CC list.  Anybody have any ideas on how to do that?

;;; Code:

;; need to save where you started mail to provide a better default
;; dir for read-file-name than ~/. 

(defvar attach-mail-default-dir default-directory
  "Directory where you where when you started `mail'.")

(defadvice mail (before save-the-directory-for-attaching-mail activate)
  (setq attach-mail-default-dir default-directory))

(defun attach-file-to-mail ()
  "Prompts for a file that will be attached to outgoing mail when it is sent."
  (interactive)
  (let ((file "")
	(default-directory attach-mail-default-dir))
    (setq file (read-file-name "Enter the file to attach: "))
    (save-excursion 
      (goto-char (point-max))
      (insert "\n===File Attachment==========================================\n")
      (insert file)
      (insert "\n============================================================\n\n"))))

;; need to change the banner around the file too in case of resends...

(defun add-the-attached-file ()
  "Inserts the file attached to the mail with `attach-file-to-mail'."
  (goto-char (point-min))
  (let ((file ""))
    (while (search-forward "\n===File Attachment===" nil t)
      (replace-match "\n===Attached File=====")
      (forward-line 1)
      (setq file (buffer-substring (point) (progn (end-of-line) (point))))
      (beginning-of-line)
      (kill-line 1)
      (insert (file-name-nondirectory file) "\n")
      (forward-line 1)
      (insert-file file))))

(add-hook 'mail-send-hook 'add-the-attached-file)



