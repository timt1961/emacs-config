;;; journal-mode.el -- A major mode for simple journal files.
;;; Copyright (c) 1997 by Stefan Braun <braun32@ibm.net>

;;; This file is NOT part of GNU Emacs.

;;; Version: $Id: journal-mode.el,v 1.7 1998/04/26 16:49:20 SB Exp $  

;;;;; Commentary:

;;; Major mode for simple journal files.

; Copy these lines into your .emacs
;(setq auto-mode-alist (cons '("\\.jrn$" . journal-mode) auto-mode-alist))
;(autoload 'journal-mode "journal-mode")
;
; Change the font-lock keywords in journal-font-lock-keywords (replace "Thema:"
; and "Ort:")


;;; Code:

(require 'font-lock)
(require 'time-stamp)

(defconst journal-font-lock-keywords
  (let* ((keywords '("Thema:" "Ort:" ))
	 (kwregex (mapconcat 'identity keywords "\\|")))
    (list
     ;; keywords not at beginning of line
     (cons (concat "\\s-\\(" kwregex "\\)[ \n\t(]") 1)
     ;; keywords at beginning of line.  i don't think regexps are
     ;; powerful enough to handle these two cases in one regexp.
     ;; prove me wrong!
     (cons (concat "^\\(" kwregex "\\)[ \n\t(]") 1)
     ;; time stamps
     (cons "[0-9]+\\.[0-9]+\\.[0-9]+ [0-9]+:[0-9]+ -" font-lock-function-name-face)
     ))
  "Additional expressions to highlight in journal mode.")


(defvar journal-mode-syntax-table nil
  "Syntax table used while in journal mode.")

(if journal-mode-syntax-table
    ()
  (setq journal-mode-syntax-table (make-syntax-table))
  (mapcar (function
	   (lambda (x) (modify-syntax-entry
			(car x) (cdr x) journal-mode-syntax-table)))
	  '(( ?\( . "()" ) ( ?\) . ")(" )
	    ( ?\[ . "(]" ) ( ?\] . ")[" )
	    ( ?\{ . "(}" ) ( ?\} . "){" )
	    ( ?\< . "(>" ) ( ?\> . ")<" )
            )))

;(defvar journal-mode-abbrev-table nil
;  "Abbrev table used while in journal mode.")
;(define-abbrev-table 'journal-mode-abbrev-table ())

(defvar journal-mode-map nil
  "Keymap for Journal mode.")

(if journal-mode-map
    ()
  (setq journal-mode-map (make-sparse-keymap))
  (define-key journal-mode-map "\C-cn" 'journal-new-entry)
  )


(defun journal-mode ()
  " This is a major mode for a journal.
Special commands:
\\{journal-mode-map}
Turning on Journal mode calls the value of the variable `journal-mode-hook',
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map journal-mode-map)
  (set-syntax-table journal-mode-syntax-table)
  (setq mode-name "Journal")
  (setq major-mode 'journal-mode)
  (setq fill-column 78)
  (setq fill-prefix "\t")
  (turn-on-auto-fill)
  (add-hook 'write-file-hooks 'time-stamp)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(journal-font-lock-keywords t))
  (run-hooks 'journal-mode-hook))


;;; special commands


(defun journal-new-entry()
  "Insert a new journal entry w/ timestamp at the end of the journal."
  (interactive)
  (progn
    (goto-char (point-max))               ;; position to end of buffer
    (delete-blank-lines)                  ; once leaves one line
    (delete-blank-lines)                  ; twice kills 'em all
    (if (bobp)                            ; should work in an empty buffer too
        (progn
          (insert "Time-stamp: <>\n")
          (insert "Thema: "))
      (bolp) (forward-char -1))
    (insert "\n\n")
    (journal-time-stamp)))

(defun journal-time-stamp ()
  "Print a time stamp at point."
  (insert (time-stamp-strftime "%y.%02m.%02d %02H:%02M - ")))

(setq debug-on-error t)

;; $Log: journal-mode.el,v $
; Revision 1.7  1998/04/26  16:49:20  SB
; requires time-stamp
;
; Revision 1.6  1998/04/26  16:43:47  SB
; font lock support.
;
; Revision 1.5  1998/04/26  10:52:58  SB
; Put a time stamp for the last update into the journal file.
;
; Revision 1.4  1998/04/26  10:13:57  SB
; Correct comments
;
; Revision 1.3  1998/04/26  09:14:24  SB
; Format of timestamp changed.
;

