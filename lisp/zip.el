;;;
;;; Copyright (C) 1992 Eric M. Ludlam
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu
;;;
;;; $Id: zip.el,v 1.1 1995/04/04 00:26:22 zappo Exp $
;;;
;;; ZIP mode ----------------------------------------------
;;;
;;; Some of the procedure names are a little innocuous, like
;;; zip-buffer really means to unzip the buffer... cest la vie'
;;; Anyways, this was designed not to manipulate zip files, but
;;; instead, to browse them.  Making manipulatory functions would, of
;;; course, be moderatly easy I suppose, but that wasn't what I wanted
;;; this program for. 
;;;
;;; This program was specifically designed for use with ange-ftp.
;;; When auto-mode is used, zip-buffer is called, but all it does is
;;; zorch the current buffer and replace it with an unzip -v of the
;;; file you loaded in.  If the buffer is an ange-ftp'ed file, it
;;; saves it to a temporary file and works from there.  On quit, it
;;; deletes the temp file.  Thus the command "m" to move the tmp file
;;; to a safer location in case you don't feel like doing another get
;;; on the file.
;;; 
;;; The only other really usefull command is "v" which loads a file
;;; under the cursor into a view buffer from the zip file using 
;;; "unzip -p zipfile extractfile" and using view to look at the file.
;;; 
;;; As you might have noticed.. you need unzip to use this on your
;;; machine.

;;; 21-Oct-93  Kevin Broadey, EDS-Scicon <KevinB@bartley.demon.co.uk>
;;; Get zip-view working under emacs-19 (don't know if it worked under emacs-18
;;; either!)
;;; Add zip-find.  Bound to "e" and "f".

;;; 4/3/95 Eric Ludlam <zappo@choochoo.ultranet.com>
;;; Quick debug for release due to a request.
 
(provide 'zip)
 
;;; To Do:
;;; if this isn't already in the list (done by hand) then add it.
(setq auto-mode-alist (nconc '(("\\.zip$" . zip-buffer)	; normal
			       ("\\.ZIP$" . zip-buffer)) ; DOSafied name
			     auto-mode-alist))
 
(defvar zip-dos-doc-files '(".txt" ".doc" "readme" ".1st" ".me" ".bat"
			    ".c" ".cpp" ".h" ".TXT" ".DOC" "README"
			    ".1ST" ".ME" ".BAT" ".C" ".CPP" ".H" )
  "list of strings signifying dos text files which need character
conversions when running on 7 bit non-dos terminals")
 
(defvar zip-command-binary "unzip"
  "Zip binary program.")
(defvar zip-list-flag "-v"
  "Zip flag for listing a zip file.")
(defvar zip-extract-flag "-p"
  "Zip flag for extracting to stdout.")
 
(defvar zip-mode-map nil
  "Keymap used in zip-mode")
(if zip-mode-map
    ()
  (setq zip-mode-map (make-sparse-keymap))
  (define-key zip-mode-map " "   'zip-next-line)
  (define-key zip-mode-map "e"   'zip-find)
  (define-key zip-mode-map "f"   'zip-find)
  (define-key zip-mode-map "g"   'zip-re-zip)
  (define-key zip-mode-map "h"   'describe-mode)
  (define-key zip-mode-map "m"   'zip-move-tmp-file)
  (define-key zip-mode-map "n"   'zip-next-line)
  (define-key zip-mode-map "p"   'zip-prev-line)
  (define-key zip-mode-map "q"   'zip-quit)
  (define-key zip-mode-map "r"   'isearch-backward)
  (define-key zip-mode-map "s"   'isearch-forward)
  (define-key zip-mode-map "v"   'zip-edit-rdonly)
  (define-key zip-mode-map "x"   'zip-quit)
  (define-key zip-mode-map "\177" 'zip-prev-line))
 
(defvar zip-clear-before-zip t
  "Non-nil means delete the zip buffer before conducting a zip.")
 
(defun zip (zfile)
  "Major mode for looking at zip information."
 
  (interactive (cons (save-excursion
		       (setq onewindow (one-window-p t))
		       (setq fw (zip-blurb-buffer))
		       (read-file-name "Zip what file: "))
		     '()))
  (if (setq tmp (zip-make-list zfile))
      (progn
	(switch-to-buffer tmp)
	(zip-mode)
	))
  (setq buffer-read-only t)
  (if onewindow 
      (delete-window fw))
  (if (get-buffer "Your Rights with zip.el")
      (kill-buffer "Your Rights with zip.el")))
 
(defun zip-buffer ()
  "Unzip current buffer into the list thing.  I guess the name is a
little incongruous since we are not zipping the buffer, but unzipping
it."
 
  (interactive)
  (zip-mode)
  (if (or (string-match "@[a-zA-Z0-9.]+:" (buffer-file-name)) ;ange-ftp?
	  (not (buffer-file-name))) ; has no file name to reference
      (progn
	(setq zip-zorch-on-quit t)
	;; Fret not demacs users... /tmp will never be used cause you
	;; can't run asychronous processes.
	(setq rf (concat "/tmp/" (getenv "USER") "+" (buffer-name)))
	(write-region (point-min) (point-max) rf)
	(setq tmp (zip-make-list rf (buffer-file-name))))
    (setq tmp (zip-make-list (buffer-file-name))))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (switch-to-buffer tmp))
 
(defun zip-mode ()
  "Defines a major mode for processing zip information.
   n,SPC - next
   p,DEL - prev
   h     - help
   v     - view file from zip archive
   m     - move the zip file (useful for ange-ftp use)
   q,x   - quit.
"
 
  (interactive)
  (use-local-map zip-mode-map)
  (set-buffer-modified-p nil)
  (setq mode-name "Ziped")
  (setq major-mode 'zip-mode)
  (make-local-variable 'zip-zorch-on-quit)
  (setq zip-zorch-on-quit nil)
  (run-hooks 'zip-mode-hooks))
 
(defun zip-edit-rdonly (&optional file)
  "unzip a file into a view buffer...."
 
  (interactive)
  (require 'view)
  (let ((buff (if file
		  (zip-unzip-to-buffer file)
		(zip-unzip-to-buffer (zip-file-this-line)))))
    (view-buffer buff)
    (setq view-exit-action 'kill-buffer)))

(defun zip-find (&optional file)
  "unzip a file into an edit buffer..."
  (interactive)
  (switch-to-buffer (if file
			(zip-unzip-to-buffer file)
		      (zip-unzip-to-buffer (zip-file-this-line))))
  (set-buffer-modified-p nil)
  (toggle-read-only)
  )

(defun zip-unzip-to-buffer (file)
  "Unzip a file from in the predefined zip file (in zip mode) and put
it into a buffer."
  
  (setq tf zip-tmp-file)
  (set-buffer (get-buffer-create (concat tf "->" file)))
  (call-process zip-command-binary nil (current-buffer) nil zip-extract-flag
		tf file)
  (goto-char 1)
  (setq match-list zip-dos-doc-files)
  (while match-list
    (if (and (not (boundp 'dos-machine-type)) 
	     (string-match (car match-list) file))
	(progn
	  (subst-char-in-region (point-min) (point-max) 175 ?#)
	  (subst-char-in-region (point-min) (point-max) 176 ?#)
	  (subst-char-in-region (point-min) (point-max) 177 ?#)
	  (subst-char-in-region (point-min) (point-max) 196 ?-)
	  (subst-char-in-region (point-min) (point-max) 205 ?-)
	  (subst-char-in-region (point-min) (point-max) 179 ?|)
	  (subst-char-in-region (point-min) (point-max) 186 ?|)
	  (replace-regexp "[\251-\332]" "+")
	  (read-msdos)
	  (goto-char 1)))
    (setq match-list (cdr match-list)))
  (set-visited-file-name (buffer-name))
  (normal-mode)   ; to do things to buffer like re-unzip
  (set-visited-file-name nil)
  (current-buffer))
 
(defun zip-move-tmp-file (newname)
  "Move the ange-ftp temporary file into a new location.  Will work on
non-anged files as well."
 
  (interactive "FRename to: ")
  (setq zip-zorch-on-quit nil)
  (rename-file zip-tmp-file  newname)
  (setq zip-tmp-file newname))
 
(defun zip-file-this-line ()
  "Get the file from this line..."
  (beginning-of-line)
  (forward-char 58)
  (buffer-substring (point) (save-excursion (end-of-line) (point))))
 
(defun zip-re-zip ()
  "Update the current zip list."
  (interactive)
  (zip-make-list (buffer-name))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))
 
(defun zip-quit ()
  "Quit the current zip buffer."
 
  (interactive)
  (if zip-zorch-on-quit
      (if (y-or-n-p (format "Zorch %s on exit?" zip-tmp-file))
	  (delete-file zip-tmp-file)))
  (if (equal major-mode 'zip-mode)
      (kill-buffer (current-buffer))))
 
(defun zip-next-line ()
  "Move zip cursor to next line."
  (interactive)
  (next-line 1)
  (beginning-of-line)
  (forward-char 58))
 
(defun zip-prev-line ()
  "Move zip cursor to prev line."
  (interactive)
  (previous-line 1)
  (beginning-of-line)
  (forward-char 58))
 
(defun zip-make-list (zfile &optional realfile)
  "Generate a zip directory listing."
 
  (interactive "fFile: ")
 
  (message "Checking %s for zip information." zfile)
 
  (make-local-variable 'zip-tmp-file)
  (setq zip-tmp-file zfile)
 
  (setq buffer-read-only nil)
  (if zip-clear-before-zip
      (delete-region (point-min) (point-max)))
  (call-process zip-command-binary nil (current-buffer) nil zip-list-flag
		zfile)
  (goto-char 1)
  ;;  Only do substitutions if you are on a whimpy 7-bit terminal.
  (or (boundp 'dos-machine-type)
      (subst-char-in-region (point-min) (point-max) 175 ?#)
      (subst-char-in-region (point-min) (point-max) 176 ?#)
      (subst-char-in-region (point-min) (point-max) 177 ?#)
      (subst-char-in-region (point-min) (point-max) 219 ?#)
      (subst-char-in-region (point-min) (point-max) 196 ?-)
      (subst-char-in-region (point-min) (point-max) 205 ?-)
      (subst-char-in-region (point-min) (point-max) 179 ?|)
      (subst-char-in-region (point-min) (point-max) 186 ?|)
      (replace-regexp "[\251-\332]" "+")
      (goto-char 1))
  (current-buffer))
 
  
(defun zip-blurb-buffer ()
  "You have the right to remain silent, anything you say can and will
be typecast against you."
 
  (save-excursion
    (get-buffer-create "Your Rights with zip.el")
    (set-buffer (get-buffer "Your Rights with zip.el"))
    (delete-region (point-min) (point-max))
    (insert "
     Thanks for using Emacs Zip!
 
     This program is free under the GNU general public license.
     See the GNU COPYING file for more details.
 
     Please report Bugs/Problems/Suggestions to:
                                                zappo@gnu.ai.mit.edu")
    (display-buffer "Your Rights with zip.el")))
 
;;; The following DOS/UNIX stuff was written by somebody else!  Here
;;; it is:
;;
;; read-msdos/write-msdos
;;
;; Try to find and handle PC files - Here are two funtions that are bound to
;; the read and write file hooks.  The first read-msdos checks to see if a file
;; contains a <cr> character. If so it is assumed that all lines are terminated
;; by a <cr><lf> sequence.  The <cr>'s are removed from the buffer and a 
;; buffer-local flag is set to indicate that this was done.  When the file is
;; written out, the flag is checked and if it is set, all line feeds are 
;; replaced with <cr><lf> sequences.
;;
(defvar msdos-file nil "*Variable to indicate if a file was an MS-DOS file or not")
(defun read-msdos ()
  "Function to strip <cr><lf> sequence out of a file when read"
  (interactive)
  (make-local-variable 'msdos-file)
  (save-excursion (goto-char (point-min))
		  (if (search-forward "\r" nil t)
		      (progn (setq msdos-file t)
			     (goto-char (point-min))
			     (replace-string "\r" "")
			     (setq msdos-file t)
			     (not-modified)))))
 
(defun write-msdos ()
  "Function to change <lf> to <cr><lf> for MS-DOS files"
  (if msdos-file
      (save-excursion (goto-char (point-min))
		      (replace-string "\n" "\r\n")
		      (write-region (point-min) (point-max) (buffer-file-name))
		      (goto-char (point-min))
		      (replace-string "\r" "")
		      (not-modified)
		      (clear-visited-file-modtime)
		      t)))
;; the MSDOS functions were taken from code by:
;   ___  ___  ___ ___  _  _ ___
;  /__/ /  / /__   /  /\ / /  _   Stephen Martin, Boeing Canada, Toronto.
; /__/ /__/ /__  _/_ /  / /__/             
;                                           Nuke the Raisins
;UUCP: smartin@iemisi.UUCP
;      {uunet|suncan}!jtsv16!marsal1!iemisi!smartin
