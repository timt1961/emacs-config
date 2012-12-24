;;; mail-alias.el --- Create a mail alias popup menu

;; Author: Dr. Gene De Lisa <blustone!gene@uunet.uu.net>
;; Created: July 1994
;; Version: 1.0
;; Last-modified: 19 Jul 1994
;; Keywords: mail

;; LCD Archive Entry:
;; mail alias|Dr. Gene De Lisa|blustone!gene@uunet.uu.net
;; |19 Jul 1994|1.0|

;; This file is distributed under the same conditions as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:
;; When you're about to send mail it is sometimes helpful to jog your memory
;; for some of your lesser used mail aliases. This will pop up a menu with all
;; of your mail aliases when you're in mail mode. If you select one from the
;; menu, it will be inserted on the To: line and will advance to the Subject:
;; line. If you change your mind about the alias, you can pop up the menu
;; again and select a different one.
;;
;; There may be a slicker way to do this but I don't have time. If you can
;; improve it, send me a copy.
;;
;; To do:
;; If the list is really long, write a segmenter to make pull right menus.
;;
;; How about parsing /etc/passwd?
;;
;; Sort the list.
;;
;; Configuration:
;; put this in your .emacs file:
;; (setq mail-mode-hook
;;      '(lambda ()
;;     (require 'mail-alias)
;;     (define-key mail-mode-map [M-C-down-mouse-3] 'make-mail-alias-menu)
;;     ))


(defun make-mail-alias-menu (event)
  "Make a popup of mail aliases."
  (interactive "e")

  (setq mail-alias-menu
	(cons "foo"
	      (list (cons "title"
			  (mapcar (lambda (cell)
				    (let ((str (car cell)))
				      (cons str (list str) )))
				  mail-aliases)))))

  (setq selection (car (x-popup-menu event mail-alias-menu)))
  (if (eq nil selection)
      (message "Nothing selected")
    (progn (message "Mail to %s" (cdr(assq selection mail-aliases)) )
	   (goto-char (point-min))
	   (if (search-forward-regexp "^To:" (point-max) t)
	       (progn
		 (kill-line 1)		;so you can change your mind and
					;reselect from the list
		 (insert (format "%s\n" selection))
		 (end-of-line nil)	; move to the end of the subject line
		 )
	     (error "No To: line"))
	   ))
  )
(provide 'mail-alias)

