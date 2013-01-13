;; color-settings.el: configuration of color-theme.el
;;
;; History :
;; 25-Dec-2012 Creation
;;
;;
;; Comment:
;; Apparently, color theme is no longer supported (or needed)
;; handling of color themes is different under emacs 24
;; Kept around for reference, and possible use in Solaris
;; Reference http://www.emacswiki.org/emacs/ColorTheme 
;;
;;----------------------------------------------------------------------------
;;
;; playing around with colors:
(add-to-list 'load-path "~/emacs/lib/emacs/site-lisp/color-theme-6.6.0")
(require 'color-theme)
;; Following information received on emacs wiki

;; If you find that
;; <code>M-x color-theme-select</code> fails with an error like the
;; following: <pre> face-attribute: Wrong type argument: symbolp,
;; (quote font-lock-builtin-face) </pre> try using this alternate
;; definition of <code>color-theme-face-attr-construct</code>:
(defun color-theme-face-attr-construct (face frame)
       (if (atom face)
	   (custom-face-attributes-get face frame)
	 (if (and (consp face) (eq (car face) 'quote))
	     (custom-face-attributes-get (cadr face) frame)
	   (custom-face-attributes-get (car face) frame))))

;;
;; After some playing around with color-theme-select:
;; (Whateveryou like is a nice theme too.. )
(setq color-theme-is-global t)
(color-theme-initialize)
;;(color-theme-high-contrast)
