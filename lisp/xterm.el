;;
;; File    : xterm.el
;; Author  : TimT.
;; Purpose : Definitions and extended keymap for xterm window running emacs
;;
;; Notes   :
;;  Based on vt200.el
;;  Changed some definitions as of 9-Nov-1993 and reordered file
;;
(load-file "/usr/local/emacs/lisp/term/sun.el")
(load-file "/usr/local/emacs/lisp/term/vt200.el")
;; F1 key
(global-set-key "\eOP" 'rmail)
;; F2 key
(global-set-key "\eOQ" 'isearch-forward)
;; F3 key
(global-set-key "\eOR" 'overwrite-mode)
;; F4 key
(global-set-key "\eOS" 'spell-buffer)
;; Undo key
(global-set-key "\e[26~" 'undo)
;; Keypad 0/Ins
(global-set-key "\e[2~" 'overwrite-mode)
;; <shift> F1
(define-key suntool-map "aT" 'compile)
;; <ctrl> F1
(define-key suntool-map "a4" 'shell)
;;
;; Following keydefs for convenience's sake
(global-set-key "\e[v" 'forward-word)
(global-set-key "\e[t" 'backward-word)


