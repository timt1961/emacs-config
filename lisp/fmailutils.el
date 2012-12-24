;;; fmailutils.el -- random mail frobnication utilities

;; Copyright (C) 1992, 1993, 1995, 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: mail, extensions
;; Created: 1992

;; LCD Archive Entry:
;; fmailutils|Noah Friedman|friedman@prep.ai.mit.edu|
;; random mail frobnication utilities|
;; $Date: 1997/04/17 11:26:32 $|$Revision: 1.5 $|~/misc/fmailutils.el.gz|

;; $Id: fmailutils.el,v 1.5 1997/04/17 11:26:32 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; These functions are meant to be primitives with which to build other
;; header-parsing routines.  I have written an example of their use in
;; mail-reorder-headers.el (available separately).

;; These functions use mail-header-separator to determine the end of the
;; mail header list in a given buffer.  This string is by default set to
;; "--text follows this line--" but you can change it to something else
;; temporarily via a (let ...) and hack incoming mail messages as well (in
;; that case, mail-header-separator should be temporarily set to "").

;; I find it particular useful to put this in my .emacs:
;;    (require 'fmailutils)
;;    (add-hook 'mail-send-hook 'mail-add-fcc-related-headers)
;; So that my saved messages get threaded properly with VM and GNUS.

;; Inspiration for this package came from a couple of mail-parsing
;; functions written by Mike Williams around 1991 or so.

;;; Code:

(require 'sendmail)

(defconst fmailutils-header-name-regexp "^[^:\n\t ]+:")

(defmacro fmailutils-save-state (&rest body)
  (list 'let '((case-fold-search t))
        (cons 'save-excursion
              body)))

;; indent like save-excursion
(put 'fmailutils-save-state 'lisp-indent-function 0)

(defsubst fmailutils-make-header-regexp (header)
  (concat "^" (regexp-quote header) ":\\s-"))


(defun mail-header-separator-position ()
  "Return point of the beginning of the mail-header-separator line.
Returns nil if there isn't one."
  (fmailutils-save-state
    (goto-char (point-min))
    (and (re-search-forward
          (concat "^" (regexp-quote mail-header-separator) "$") nil t)
         (match-beginning 0))))

(defun mail-append-header-contents (header contents)
  "Append CONTENTS to existing contents of header HEADER.
Otherwise create new header and append contents to it."
  (fmailutils-save-state
    (mail-position-on-field header)
    (insert contents)))

(defun mail-current-header ()
  "Return name of mail header contents at point.
If point is not in a mail header \(e.g. it is in the body of the message\),
return nil."
  (let ((pos (mail-header-separator-position)))
    (cond ((and pos (< (point) pos))
           (fmailutils-save-state
             (end-of-line)
             (re-search-backward fmailutils-header-name-regexp nil t)
             (buffer-substring (match-beginning 0) (1- (match-end 0))))))))

(defun mail-get-beginning-of-header-line-position (&optional header)
  "Return point where header HEADER begins.
If header name is not specified, use current header.
Return nil if header doesn't exist or point isn't in a mail header."
  (or header (setq header (mail-current-header)))
  (fmailutils-save-state
    (cond ((and (mail-header-separator-position)
                (mail-position-on-field header 'soft))
           (re-search-backward (fmailutils-make-header-regexp header))
           (beginning-of-line)
           (point)))))

(defun mail-get-header-contents (header)
  "Return a list containing contents of any headers named HEADER.
If no occurrences of header exist in the current mail buffer, return nil."
  (fmailutils-save-state
    (save-restriction
      (let ((re-header (fmailutils-make-header-regexp header))
            contents-list end beg)
        (while (mail-position-on-field header 'soft)
          (setq end (point))
          (setq beg (progn
                      (re-search-backward re-header)
                      (goto-char (match-end 0))))
          (setq contents-list (cons (buffer-substring beg end) contents-list))
          (narrow-to-region (1+ end) (point-max)))
        (nreverse contents-list)))))

(defun mail-get-header-names (&optional uniquep)
  "Return a list of all existing mail headers, or nil if none.
If optional argument UNIQUEP is non-nil, only list each header name once,
even if it appears more than once in the mail headers."
  (fmailutils-save-state
    (let ((mail-headers-end (mail-header-separator-position))
          mail-header-list this-mail-header)
      (cond (mail-headers-end
             (goto-char (point-min))
             (while (re-search-forward fmailutils-header-name-regexp
                                       mail-headers-end t)
               (setq this-mail-header
                     (buffer-substring (match-beginning 0)
                                       (1- (match-end 0))))
               (or (and uniquep (member this-mail-header mail-header-list))
                   (setq mail-header-list
                         (cons this-mail-header mail-header-list))))
             (nreverse mail-header-list))))))

(defun mail-put-header (header contents)
  "Add HEADER to the current mail message, with CONTENTS.
If the header already exists in the message, place this header and contents
one below it, on a new line.  (use  mail-put-unique-header  if you want to
overwrite pre-existing headers and their contents)."
  (if (mail-position-on-field header 'soft)
      (fmailutils-save-state
        (save-restriction
          (widen)
          (let (mail-header-end)
            (goto-char (point-min))
            (re-search-forward
             (concat "^" (regexp-quote mail-header-separator) "$"))
            (setq mail-header-end (match-end 0))

            (while (mail-position-on-field header 'soft)
              (narrow-to-region (1+ (point)) mail-header-end))
            (insert header ": " contents "\n"))))
    ;; use mail-put-unique-header, which will put the header at the end of
    ;; the header list.  This is where we really want it since no previous
    ;; header of the same name exists anyway.
    (mail-put-unique-header header contents)))

(defun mail-put-unique-header (header contents &optional replace)
  "Add HEADER to the current mail message, with the given CONTENTS.
If the header already exists, the contents are left unchanged,
unless optional argument REPLACE is non-nil."
  (fmailutils-save-state
    (let ((header-exists (mail-position-on-field header))
          beg end)
      ;; Delete old contents if replace is set
      (and header-exists
           replace
           (progn
             (setq end (point))
             (re-search-backward (fmailutils-make-header-regexp header))
             (setq beg (goto-char (match-end 0)))
             (delete-region beg end)))
      ;; Add new contents if replace is set, or this is a new header.
      (and (or (not header-exists) replace)
           (insert contents)))))

(defun mail-remove-header (header &optional allp)
  "Remove first instance of HEADER (and contents) from the current
mail message.  If optional second argument ALLP is non-nil, all such
instances are removed."
  (fmailutils-save-state
    (let ((re-header (fmailutils-make-header-regexp header))
          (doit t)
          beg end)
      (while (and doit (mail-position-on-field header 'soft))
        (setq end (point))
        (setq beg (progn
                    (re-search-backward re-header)
                    (goto-char (match-beginning 0))))
        (setq doit allp)
        (delete-region beg (1+ end))))))


;; Return a string suitable for use as a Message-Id token.
;; This is useful for maintaining proper threading in messages you save to
;; folders via FCC headers.
(defun mail-make-message-id ()
  "Return a string suitable for use as a Message-Id token."
  (let ((timestr (cond
                  ((fboundp 'format-time-string)
                   (format-time-string "%Y%m%d%H%M%S"))
                  (t
                   (let ((str (current-time-string)))
                     (format "%s%02d%s%s%s%s"
                             (substring str 20 24)
                             (length (member (substring str 4 7)
                                             '("Dec" "Nov" "Oct"
                                               "Sep" "Aug" "Jul"
                                               "Jun" "May" "Apr"
                                               "Mar" "Feb" "Jan")))
                             (substring str 8 10)
                             (substring str 11 13)
                             (substring str 14 16)
                             (substring str 17 19)))))))
    (format "<%s.FMU%05d@%s>" timestr (random 99999) (system-name))))

;; This function is useful for putting dates on mail-send-hook if you want
;; to control the date header format in your outgoing messages, or just
;; provide dates in messages saved to folders via the FCC header.
;; This is prefixed with fmailutils- instead of mail- because Emacs 19.34
;; already defines mail-rfc822-date in mail-utils.el.  My implementation is
;; slightly different.
(defun fmailutils-rfc822-date (&optional time)
  "Return a string of the form \"Thu, 01 Jan 1970 00:00:00 -0000 \(UTC\)\"."
  (let ((datestr (cond
                  ((fboundp 'format-time-string)
                   (format-time-string "%a, %d %b %Y %H:%M:%S" time))
                  (t
                   (let ((str (current-time-string time)))
                     (format "%s, %s %s %s %s"
                             (substring str  0  3)
                             (substring str  8 10)
                             (substring str  4  7)
                             (substring str 20 24)
                             (substring str 11 19))))))
        (tzoff (fmailutils-rfc822-time-zone-offset time))
        (tznam (fmailutils-rfc822-time-zone-name time)))
    (cond ((and tzoff tznam)
           (format "%s %s (%s)" datestr tzoff tznam))
          ((or tzoff tznam)
           (format "%s %s" (or tzoff tznam)))
          (t datestr))))

(defun fmailutils-rfc822-time-zone-offset (&optional time)
  (and (fboundp 'current-time-zone)
       (let* ((sec (or (car (current-time-zone time)) 0))
              (absmin (/ (abs sec) 60)))
         (format "%c%02d%02d"
                 (if (< sec 0) ?- ?+)
                 (/ absmin 60)
                 (% absmin 60)))))

(defun fmailutils-rfc822-time-zone-name (&optional time)
  (cond ((fboundp 'current-time-zone)
         (nth 1 (current-time-zone time)))
        ((getenv "TZ")
         ;; This can be utterly wrong, particular for posix timezone specs,
         ;; but about as correct as is worthwhile for supporting emacs 18.
         (substring (getenv "TZ") 0 3))))


;; Useful mail mode hacks

(defvar mail-folder-directory "~/"
  "*Default directory where mail saved via FCC headers should go.")

(defun mail-fcc (file)
  "Add a new FCC field, with file name completion."
  (interactive (list (read-file-name "Folder carbon copy: "
                                     mail-folder-directory)))
  (mail-put-unique-header "Fcc" file 'force-replace))

(define-key mail-mode-map "\C-c\C-f\C-f" 'mail-fcc)

(defun mail-add-fcc-related-headers (&optional forcep)
  "Add Date and Message-Id headers to messages saved via Fcc headers.
When called from lisp, this function only adds the Date and Message-Id
headers if an Fcc header is already present or the optional argument FORCEP
is non-nil.  If called interactively, the headers are added
unconditionally.

If you put this function on mail-send-hook, this will help document when
you sent the message as well as allow for proper threading when you visit
that folder with a mail reader that supports threads."
  (interactive)
  (fmailutils-save-state
    (cond ((or forcep
               (interactive-p)
               (mail-position-on-field "Fcc" 'soft))
           (mail-put-unique-header "Date" (fmailutils-rfc822-date))
           (mail-put-unique-header "Message-Id" (mail-make-message-id))))))


(provide 'fmailutils)
(provide 'friedman-mail-utils)  ; old name prior to revision 1.4

;; fmailutils.el ends here

