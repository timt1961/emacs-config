;;
;; 13-Jan-2013 : Cut from .emacs
;; Programming mode support
;; Old, possibly obsolete C-code support. 
;; Review 
;; cwarn : highlight suspicious C/C++ constructs
(require 'cwarn)
(global-cwarn-mode 1)

;; Move between functions
(defun beginning-of-next-defun ()
  "Moves cursor to the beginning of the next defun."
  (interactive)
  (beginning-of-defun -1))

(global-set-key '[(meta up)] 'beginning-of-defun)
(global-set-key '[(meta down)] 'beginning-of-next-defun)
;;
;; Buffer cycling
(global-set-key "\M-z"  'bury-buffer)

;; Template code definitions.
(require 'template)
(setq template-default-directories `("/home/utt/templates"))
(template-initialize t)
(defun pad-comment ()
  "fill the current line with blanks to pos 72"
  (interactive)
  (setq padding  ( - 72 (current-column)))
  (insert-char ?\  padding))

(setq template-expansion-alist  '(("PAD" (insert-char ?\ (- 72 (current-column))))))
(require 'compile)
(setq compilation-error-regexp-alist
  (append (list
     ;; works for jikes
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
     ;; works for javac
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
  compilation-error-regexp-alist))
;;
;; Correctly parse ant output
;; Taken from ant faq
(require 'compile)
(setq compilation-error-regexp-alist
  (append (list
     ;; works for jikes
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
     ;; works for javac
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
  compilation-error-regexp-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                ;;
;;  Source: http://irreal.org/blog/?p=371         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jcs-comment-box ()
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive)
  (with-region-or-buffer (b e)
    (save-restriction
      (narrow-to-region b e)
      (goto-char b)
      (end-of-line)
      (insert-char ?  (- fill-column (current-column)))
      (comment-box b (point-max) 1)
      (goto-char (point-max)))))
