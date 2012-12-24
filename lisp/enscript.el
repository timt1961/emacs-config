;;; enscript.el - GNU Emacs interface to print with the enscript command.
;;; bugs/comments to quinlan@spectrum.cs.bucknell.edu
;;; Version: 1.3
;;; Last modified: 9/19/93
;;; this file contains code from lpr.el
;;;
;;; Copyright (C) 1993  Daniel Quinlan
;;;
;;; INSTRUCTIONS:
;;;
;;; To add enscript capability to you Emacs setup, you only have to add
;;; the following lines (without the ";;;") to your .emacs file.
;;;
;;;   (require 'enscript)
;;;
;;; APPLICATION NOTES:
;;;
;;; Here are examples of setting enscript-switches from your .emacs:
;;;   (setq enscript-switches (list "-2rh"))
;;;   (setq enscript-switches (cons (concat "-P" (getenv "PRINTER")) nil))

(defvar enscript-switches nil
  "*List of strings to pass as extra switch arguments to enscript when it
is invoked.  The environment variable ENSCRIPT may be used to specify
printing defaults.  The environment variable PRINTER indicates the
printer to be used if no -P option is specified by ENSCRIPT or
`enscript-switches'.")

(defvar enscript-command "enscript"
  "Shell command for converting a text file to Postscript format for printing")

(defun enscript-buffer ()
  "Print buffer contents as with Unix command `enscript'.
`enscript-switches' is a list of extra switches (strings) to pass to enscript."
  (interactive)
  (enscript-region-1 (point-min) (point-max) enscript-switches))

(defun enscript-region (start end)
  "Print region contents as with Unix command `enscript'.
`enscript-switches' is a list of extra switches (strings) to pass to enscript."
  (interactive "r")
  (enscript-region-1 start end enscript-switches))

(defun enscript-buffer-query ()
  "Print buffer contents as with Unix command `enscript'.
Prompt for switches."
  (interactive)
  (let ((switches (cons (read-from-minibuffer "Enscript switches: ") nil )))
    (enscript-region-1 (point-min) (point-max) switches)))

(defun enscript-region-query (start end)
  "Print region contents as with Unix command `enscript'.
Prompt for switches."
  (interactive "r")
  (let ((switches (cons (read-from-minibuffer "Enscript switches: ") nil )))
    (enscript-region-1 start end switches)))

(defun enscript-region-1 (start end switches)
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
      (message "Formatting...")
      (if (/= tab-width 8)
	  (let ((oldbuf (current-buffer)))
	    (set-buffer (get-buffer-create " *spool temp*"))
	    (widen) (erase-buffer)
	    (insert-buffer-substring oldbuf start end)
	    (setq tab-width width)
	    (untabify (point-min) (point-max))
	    (setq start (point-min) end (point-max))))
      (apply 'call-process-region
             (nconc (list start end enscript-command
                          nil nil nil)
                    (nconc (list "-b" name)
			   switches)))
      (message "Spooling...done"))))

(provide 'enscript)
