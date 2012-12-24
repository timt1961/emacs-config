;; look up the thesaurus
;; $Id: thesaurus.el,v 1.9 1994/08/05 03:42:21 nickson Exp $
;;
;; Copyright (C) 1992,1994 by Ray Nickson <nickson@cs.uq.oz.au>
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'thesaurus)

(defvar thesaurus-file "~emacs/roget/roget13a.txt"
  "*File where the thesaurus is kept.")

(defvar thesaurus-last-word nil)

(defun thesaurus-word (arg)
  "Look up the word(s) before point in the thesaurus."
  (interactive "p")
  (let ((word (words-before-point arg)))
    (if (string= word thesaurus-last-word)
        (thesaurus-lookup-word word 'again)
      (thesaurus-lookup-word (setq thesaurus-last-word word) nil))))

(defun thesaurus-lookup-word (word again)
  "Look up WORD in the thesaurus.  Start from the top unless AGAIN is non-nil."
  (or word
      (error "Specify word!"))
  (set-buffer (find-file-noselect thesaurus-file))
  (setq buffer-read-only t)
  (if again nil
    (goto-char 1)
    (or (re-search-forward "^ *#1\\." nil t)
        (error "unexpected format in %s" thesaurus-file)))
  (or (search-forward word nil t)
      (error "%s: word not found" word))
  (let (start end)
    (or (re-search-backward "^ *#[0-9]+\\." nil t)
        (error "%s: can't find start of entry" word))
    (setq start (point))
    (forward-line 1)
    (or (re-search-forward "^ *#[0-9]+\\." nil t)
        (error "%s: can't find end of entry" word))
    (setq end (match-beginning 0))
    (let (col1 col2
          (buf (get-buffer-create "*Thesaurus Entry*"))
          (entry (buffer-substring start end)))
      (set-buffer buf)
      (erase-buffer)
      (insert entry)
      (goto-char 1)
      (while (search-forward word nil t)
        (setq col2 (current-column))
        (search-backward word nil t)
        (setq col1 (current-column))
        (beginning-of-line nil)
        (insert-char 32 col1)
        (insert-char ?v (- col2 col1))
        (insert "\n")
        (forward-line 1)
        (insert-char 32 col1)
        (insert-char ?^ (- col2 col1))
        (insert "\n"))
      (display-buffer buf))))

(defun words-before-point (arg)
  "Return the last ARG word(s) before point.  Zero ARG means return the
region."
  (and (>= arg 0)
       (buffer-substring
        (point)
        (if (= arg 0) (mark)
          (save-excursion (forward-word (- arg)) (point))))))

