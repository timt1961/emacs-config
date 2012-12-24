;------------------------------ match.el ---------------------------------
; match parenthesis according to syntax tables. Like % in vi.
; matches the character following dot by jumping to matching character.
;
; By Kayvan Sylvan

(defun match-it ()
  "Match character after dot to its corresponding open or close."
  (interactive)
  (let ((syntax (char-syntax (following-char))))
    (cond ((= syntax ?\() (match-it-forward))
	  ((= syntax ?\)) (match-it-backward))
	  (t (error "%c is not an open or close." (following-char))))))

(defun matching-char (char table)
  "return the matching char of CHAR from TABLE. TABLE must be a syntax-table.
returns nil if none is found"
  (cond ((and (syntax-table-p table) (char-or-string-p char))
	 (if (stringp char) (setq char (string-to-char char)))
	 (if (> (setq char (lsh (aref table char) -8)) 0) char nil))
	(t (error "Not syntax table or bad char"))))

(defun go-error (pos err)
  "Goto position POS of file, while signalling error ERROR"
  (goto-char pos)
  (error err))

(defun match-it-forward ()
  "Find match for an open."
  (let ((table (syntax-table)) (pos (dot)))
    (setq char (matching-char (following-char) table))
    (forward-sexp 1)
    (backward-char)
    (if (not (= char (following-char))) (go-error pos "mismatch"))))

(defun match-it-backward ()
  "Find match for a close."
  (let ((table (syntax-table)) (pos (dot)))
    (setq char (matching-char (following-char) table))
    (forward-char)
    (backward-sexp 1)
    (if (not (= char (following-char))) (go-error pos "mismatch"))))

;;------------------------------------------------------------------
;; goto-matching-paren-or-insert
;; Func useful in C-mode and Emacs-lisp mode, mostly.
;;------------------------------------------------------------------
(defun goto-matching-paren-or-insert (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
	((looking-at "[])}]") (forward-char) (backward-sexp 1))
	(t (self-insert-command (or arg 1)))))

;; Cause the "%" key to work just like in "vi" edit mode.  If you really
;; wish to insert a "%", you must type ``C-q %''.

;; MCN> Actually, this looks to see if cursor is on a paren-type char 
;; MCN> (unfortunately doesn't use syntax table to do so). If so, it
;; MCN> hops to the matching paren-type char (which may be of a
;; MCN> different type, such as "[...}"); if not, it inserts a "%".
;; MCN> If you really wish to insert a "%" *next to a paren-type char*,
;; MCN> you must type ``C-q %''.

;(global-set-key "%" 'goto-matching-paren-or-insert)
(global-set-key "\C-x%" 'match-it)

