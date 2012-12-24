;; ksh-hilit.el - Patterns for highlighting sh/bash/ksh scripts with hilit19
;;
;; Copyright (C) 1995 Hernan Astudillo R.
;; 
;; Author:    Hernan Astudillo R. <hernan@cc.gatech.edu>
;; Version:   1.0 (23 June 1995)
;; Keywords:  sh, ksh, bash, script, hilit19, hl319, highlight
;; 
;; Everyone is granted permission to copy, modify and redistribute this
;; file provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.
;; 
;; ----------------------------------------------------------------------------
;; Description:
;;  This package adds hilit19 highlighting patterns for ksh-mode, or any
;;  other mode for editing sh-like shell scripts.
;; 
;; Installation:
;;
;;  To use this package, add the following line to your.emacs file (without
;;  the ";;", of course):
;;    (and window-system
;;      (require 'ksh-hilit)
;;      (add-hook 'ksh-mode-hook 'ksh-hilit:add-patterns))
;;  If you use another mode for editing your scripts, replace as appropriate.
;; 
;; Comment:
;;  I suggest you also get "hilit-LaTeX", whih provides slanted fonts for
;;  strings, and so enables you (yes, you) to perceive the string as a
;;  slanted thing *and* its componentsa as properly colored
;;  You can get "hilit-LaTeX" from either:
;;  ftp://bathybius.meteo.mcgill.ca/pub/users/rhogee/elisp/hilit-LaTeX.el
;;  ftp://mixing.qc.dfo.ca/pub/emacs-add-ons/hilit-LateX.el


;;--PROLOGUE
(require 'hilit19)

;;{{{ hilit functions for string
;; define hilit functions for strings delimited with \' and \`
;; these two functions are based on "hilit-string-find-lquote", from hilit19

(defun hilit-string-find-1quote (qchar)
  "looks for a string and returns (start . end) or NIL.  The argument QCHAR
is the character that would precede a character constant single quote.
Finds strings delimited by single quotes.  The first double quote may not be
preceded by QCHAR and the closing single quote may not be preceded by an odd
number of backslashes."
  (let (st en)
    (while (and (search-forward "\'" nil t)
		(eq qchar (char-after (1- (setq st (match-beginning 0)))))))
    (while (and (search-forward "\'" nil t)
		(save-excursion
		  (setq en (point))
		  (forward-char -1)
		  (skip-chars-backward "\\\\")
		  (forward-char 1)
		  (not (zerop (% (- en (point)) 2))))))
    (and en (cons st en))))    

(defun hilit-string-find-backquote (qchar)
  "looks for a string and returns (start . end) or NIL.  The argument QCHAR
is the character that would precede a character constant reverse quote.
Finds strings delimited by reverse quotes.  The first reverse quote may not be
preceded by QCHAR and the closing reverse quote may not be preceded by an odd
number of backslashes."
  (let (st en)
    (while (and (search-forward "\`" nil t)
		(eq qchar (char-after (1- (setq st (match-beginning 0)))))))
    (while (and (search-forward "\`" nil t)
		(save-excursion
		  (setq en (point))
		  (forward-char -1)
		  (skip-chars-backward "\\\\")
		  (forward-char 1)
		  (not (zerop (% (- en (point)) 2))))))
    (and en (cons st en))))    

;;}}}

;;--MAIN FUNCTION
(defvar ksh-strings-2quotes	'((hilit-string-find ?\\ string)))
(defvar ksh-strings-1quote	'((hilit-string-find-1quote ?\\ string)))
(defvar ksh-strings-backquote	'((hilit-string-find-backquote ?\\ type)))

(defun ksh-hilit:add-patterns ()
  "Define patterns to highlight sh/bash/ksh scripts withto use with hilit19."

(hilit-set-mode-patterns
 '(ksh-mode)
 (append
  '( ;; STRATEGY: hilit \"-strings first, then keywords, then others, and
     ;; last comments and \'-strings
    ;; program name (after comments!)
    ("^#!.*$" nil include)
    ;; comments
    ("[^\\$]#.*$" nil comment)
    )
  ksh-strings-1quote
  '(
    ;; [ ... ] expressions
    ("\\[" "\\]" formula)
    ("\\$\\[" "\\]" formula)
    ;; functions names
    ("^[A-Za-z0-9_-]+[ \t]*()[ \t]*{" nil defun) ;; in col. 0
    ("function[ \t]+[A-Za-z0-9_-]+[ \t]*()[ \t]*{" nil type)
    ("^}" nil defun);; trick for end-of-function
    ;; case label
    ("^[ \t]*[a-zA-Z0-9\\*]+[ \t]*)" nil label)
    ;; things that bring in external files
    ("^\\.[ \t]+" "[;$]" include)
    ;; aliases
    ("\\(alias\\|unalias\\)[ \t]+[A-Za-z0-9#_-]+" nil crossref)
    ;; variable declaration/setting
    ("[A-Za-z0-9#_-]+=" nil decl)
    ("\\(declare\\|typeset\\|export\\|unset\\)[ \t]+[A-Za-z0-9#_-]+"
     nil decl)
    ;; variable usage (weird for ${var:-""} etc...)
    ("\\$[A-Za-z0-9_]+" nil define)
    ("\\$([A-Za-z0-9_]+[):#%]" nil define)
    ("\\${[A-Za-z0-9_]+[}:#%]" nil define)
    ("\\$[*@#?-$!]" nil define) ;; special variables
    )
  ;; next lines hilit properly `"..."`, but not "`...`"
  ksh-strings-2quotes
  ksh-strings-backquote
  '( ;; keywords
    ("\\(^\\|[; \t]\\)\\(\\!\\|for\\|in\\|do\\|done\\|case\\|esac\\|if\\|then\\|elif\\|else\\|fi\\|while\\|until\\)\\($\\|[; \t]\\)"
     nil keyword)
    ;; builtins (only those not used in other rules)
    ("\\(^\\|[; \t]\\)\\(bg\\|bind\\|break\\|builtin\\|cd\\|command\\|continue\\|dirs\\|echo\\|enable\\|eval\\|exec\\|exit\\|bye\\|fc\\|fg\\|getopts\\|hash\\|help\\|history\\|jobs\\|kill\\|let\\|local\\|logout\\|popd\\|pushd\\|pwd\\|read\\|readonly\\|return\\|set\\|shift\\|suspend\\|test\\|times\\|trap\\|type\\|ulimit\\|umask\\|wait\\|source\\)\\($\\|[; \t]\\)"
     nil keyword)
    ;; special symbols
    (";\\|&\\|&&\\|||" nil keyword) ;; list separators and terminators
    ("|\\|<\\|<<\\|>\\|>>\\|>&\\|&" nil keyword) ;; actually, |,<,>,& are enough...
    )
  ))
);;ksh-hilit:add-patterns

;;-- EPILOGUE
(provide 'ksh-hilit)

;; ksh-hilit ends here
-- 
