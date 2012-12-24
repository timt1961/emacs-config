;;
;; Copyright 1997 Eric M. Ludlam
;;
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
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.org

(defvar curly-leave-cursor-in-comment t
  "If non-nil, then move the cursor back into the comment where one
would add additional information.  Otherwise, the cursor is placed at
the end of the line.")

(defun curly-brace-comment ()
  "When a curly brace is inserted, check to see what it matches under
c-mode, and comment it."
  (interactive)
  (electric-c-brace 1)
  (c-indent-command)
  (if (not (looking-at (concat "[ \t]*" (regexp-quote comment-start))))
      (let (part semi)
        (save-excursion
          (backward-sexp 2)
          (if (not (re-search-backward ";\\|\\*/\\|}\\|{\\|>\\|\\\"" nil t))
              (goto-char 0))
          (if (re-search-forward "\\([a-zA-Z0-9_]+\\)\\s *\\((\\|{\\|\"\\|[a-zA-Z0-9_]+\\s *:\\)" nil t)
              (progn
                (setq part (buffer-substring (match-beginning 1) 
                                             (match-end 1)))
                (if (string= part "case")
                    (setq part (concat part " " (buffer-substring
                                                 (match-beginning 2)
                                                 (match-end 2))))
                  (if (looking-at "\"C\"")
                      (setq part (concat part " \"C\""))
                    (if (not (string-match "if\\|for\\|else\\|do\\|while\\|switch" part))
                        (progn
                          (forward-char -1)
                          (backward-sexp 2)
                          (if (looking-at "enum\\|struct\\|union") (setq semi t))
                    )))))))
        (if part
            (cond
             ((string= part "do")
              (insert " while( );")
              (forward-char -3))
             ((and (not (string= part "if"))
                   (not (string= part "else")))
              (if semi (insert ";"))
              (insert (format " %s%s%s%s" comment-start part
                              (if (and (not semi) 
                                       (not (string-match "switch\\|for\\|while\\|:" part)))
                                  "( )" "")
                               comment-end))
              (if (and curly-leave-cursor-in-comment
                       (not semi) (string-match "for\\|while" part))
                  (backward-char (length comment-end))))
             (t (message "Not putting comment by curly.")))))))
          

(provide 'curly)
;; end of lisp
