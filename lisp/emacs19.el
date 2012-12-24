;;
;; File    : emacs19.el
;; Author  : TimT.
;; Description : Setup for the new emacs version
;; Add as appropriate
;;
;; Note :
;; To get the f11/f12 function keys, 
;; use the following commands
;; xmodmap -e "keycode 16=f11"

;;(setq load-path (parse-colon-path "/home/utt/emacs/lisp:/swuser/def/emacs:/swuser/lib/emacs-19.22/lisp"))
(load-file "/swuser/share/emacs/19.34/lisp/term/sun.el")
;; r7 = f27 = Home
(defun top-of-screen ()
  (interactive)
  (move-to-window-line 0))

(defun bottom-of-screen ()
  (interactive)
  (move-to-window-line -1))

(defun justify-region ()
  (interactive)
  (exchange-point-and-mark)
  (beginning-of-line)
  (exchange-point-and-mark)
  (fill-region (mark) (point) 1))

(global-set-key [f27]  'top-of-screen)
;; r13 = f33 = End
(global-set-key [f33] 'bottom-of-screen)
;; c-left, c-right, shift-left, shift-right; move by words
(global-set-key [C-left] 'backward-word)
(global-set-key [S-f30] 'backward-word)
(global-set-key [C-right] 'forward-word)
(global-set-key [S-f32] 'forward-word)
;; r9 = f29 = PgUp 
(global-set-key [f29] 'scroll-down)
;; r15 = f35 = PgDn
(global-set-key [f35] 'scroll-up)
;; L4 = f14 = UNDO
(global-set-key [f14] 'undo)
;; r11 = f31 = num 5
(global-set-key [f31] 'recenter)
;;
;; Not having this gets irritating after a while
(global-set-key [S-delete] 'backward-delete-char-untabify)
;; ESC-space : set this to set-mark
(global-set-key "\e " 'set-mark-command) 
;;
;; Define the standard asm key map
;; Note : I use alt : Alt: meta is warp under olvwm
;;
;; F2
(global-set-key [f2] 'isearch-forward)
(global-set-key [S-f2] 'isearch-backward)
(global-set-key [C-f2] 'replace-string)
(global-set-key [A-f2] 'query-replace)   
;;
;; F3
(global-set-key [f3] 'overwrite-mode)
(global-set-key [S-f3] 'downcase-word)
(global-set-key [C-f3] 'upcase-word)
(global-set-key [A-f3] 'fundamental-mode)
;; 
;; F4
(global-set-key [f4] 'find-tag)
(global-set-key [S-f4] 'tags-search)
(global-set-key [C-f4] 'tags-loop-continue)
(global-set-key [A-f4] 'find-tag-other-window)
;;
;; F5
(global-set-key [f5] 'other-window)
(global-set-key [S-f5] 'split-window)
(global-set-key [C-f5] 'delete-window)
;;
;; F6
(global-set-key [f6] 'shrink-window)
(global-set-key [S-f6] 'enlarge-window)
(global-set-key [C-f6] 'enlarge-window-horizontally)
(global-set-key [A-f6] 'shrink-window-horizontally)
;;
;; F7
(global-set-key [f7] 'find-file)
(global-set-key [S-f7] 'view-file)
(global-set-key [C-f7] 'insert-file)
;;
;; F8
(global-set-key [f8] 'indent-new-comment-line)
(global-set-key [S-f8] 'indent-for-comment)
(global-set-key [C-f8] 'indent-region)
;;
;; F9
(global-set-key [f9] 'justify-region)
(global-set-key [S-f9] 'fill-paragraph)
(global-set-key [C-f9] 'set-fill-column)
;;
;; F10
(global-set-key [f10] 'call-last-kbd-macro)
(global-set-key [S-f10] 'start-kbd-macro)
(global-set-key [C-f10] 'end-kbd-macro)
(global-set-key [A-f10] 'name-last-kbd-macro)
;;
;; F11
(global-set-key [f11] 'apropos)
(global-set-key [S-f11] 'describe-bindings)
(global-set-key [C-f11] 'info)
(global-set-key [A-f11] 'where-is)
;;
;; F12
(global-set-key [f12] 'save-buffer)
(global-set-key [S-f12] 'compile)
(global-set-key [C-f12] 'compile)   ;; Won't work. Sorry
(global-set-key [A-f12] 'next-error)
;;
;;
;; Make the window title correspond to the current buffer
(defun set-frame-title ()
  "Set title to current's buffer \\[buffer-file-name] name
or to \\[buffer-name if it has no file"
  (let ((name (format "%s"
		      (buffer-name (current-buffer)))))
    (modify-frame-parameters (selected-frame)
			     (list (cons 'name name)))))

(setq post-command-hook (cons 'set-frame-title post-command-hook))
;;
;; Turn accents and stuff on
(standard-display-european t)
;;
;; Left side of the keyboard
(global-set-key [f16] 'copy-region-as-kill)
(global-set-key [f18] 'yank)
(global-set-key [f20] 'kill-region-and-unmark)
(global-set-key [f19] 'research-forward)
;;
;; Right side of keyboard
(global-set-key [S-f35] 'forward-page)
(global-set-key [S-f29] 'backward-page)






