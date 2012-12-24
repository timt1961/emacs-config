;; File        : emacs18.el
;; Description : definitions necessary to run the old emacs version
;; Author      : TimT.

;;
;; Set the emacs loadpath
(setq load-path '("/home/utt/emacs/lisp" "/swuser/defaults/emacs" "/usr/local/emacs/lisp"))
;;
;; Load the keydefs for the emacs file
(if (string-match (getenv "TERM") "xterm")
    (load-file "/home/utt/emacs/lisp/xterm.el")
)
