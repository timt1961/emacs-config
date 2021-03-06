;;
;; File : ASML-keys.el
;; Author: TimT. 
;;
;; Original ASML keybindings; based on an old template found in an 
;; abandoned drawer.
;;
;; Most function keys commented out, since I don't use them.
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

(defun previous-error ()
  (interactive)
  (next-error -1))
;; On sun keyboard, this is home/end
;; I prefer the current beginning-of-line/end-of-line
;; Sun keyboard bindings
;; (global-set-key [f27]  'beginning-of-line)
;; r13 = f33 = End
;;(global-set-key [f33] 'end-of-line)
;;(global-set-key [S-f30] 'backward-word)
;;(global-set-key [S-f32] 'forward-word)
;; r9 = f29 = PgUp
;;(global-set-key [f29] 'scroll-down)
;; r15 = f35 = PgDn
;;(global-set-key [f35] 'scroll-up)
;; L4 = f14 = UNDO
;;(global-set-key [f14] 'undo)
;; r11 = f31 = num 5
;;(global-set-key [f31] 'recenter)
;; Left side of the keyboard
;;(global-set-key [f16] 'copy-region-as-kill)
;;(global-set-key [f18] 'yank)
;;(global-set-key [f20] 'kill-region-and-unmark)
;;(global-set-key [f19] 're-search-forward)
;;
;; Right side of keyboard
;;(global-set-key [S-f35] 'forward-page)
;;(global-set-key [S-f29] 'backward-page)


;; c-left, c-right, shift-left, shift-right; move by words
(global-set-key [C-left] 'backward-word)
(global-set-key [C-right] 'forward-word)


;; Not having this gets irritating after a while
(global-set-key [S-delete] 'backward-delete-char-untabify)
;; ESC-space : set this to set-mark
(global-set-key [M- ] 'set-mark-command)
;;
;; Emacs 21 changed the following keys
(global-set-key [S-home] 'beginning-of-buffer)
(global-set-key [S-end] 'end-of-buffer)
;;
;; Define the standard asm key map
;; Note : I use alt : Alt: meta is warp under olvwm
;;
;; F2
;;(global-set-key [f2] 'isearch-forward)
;;(global-set-key [S-f2] 'isearch-backward)
;;(global-set-key [C-f2] 'replace-string)
;;(global-set-key [A-f2] 'query-replace)
;;
;; F3
;;(global-set-key [f3] 'overwrite-mode)
;;(global-set-key [S-f3] 'downcase-word)
;;(global-set-key [C-f3] 'upcase-word)
;;(global-set-key [A-f3] 'fundamental-mode)
;;
;; F4
;;(global-set-key [f4] 'find-tag)
;;(global-set-key [S-f4] 'tags-search)
;;(global-set-key [C-f4] 'tags-loop-continue)
;;(global-set-key [A-f4] 'find-tag-other-window)
;;
;; F5
;;(global-set-key [f5] 'other-window)
;;(global-set-key [S-f5] 'split-window)
;;(global-set-key [C-f5] 'delete-window)
;;
;; F6
;;(global-set-key [f6] 'shrink-window)
;;(global-set-key [S-f6] 'enlarge-window)
;;(global-set-key [C-f6] 'enlarge-window-horizontally)
;;(global-set-key [A-f6] 'shrink-window-horizontally)
;;
;; F7
;;(global-set-key [f7] 'find-file)
;;(global-set-key [S-f7] 'view-file)
;;(global-set-key [C-f7] 'insert-file)
;;
;; F8
;;(global-set-key [f8] 'indent-new-comment-line)
;;(global-set-key [S-f8] 'indent-for-comment)
;;(global-set-key [C-f8] 'indent-region)
;;
;; F9
;;(global-set-key [f9] 'justify-region)
;;(global-set-key [S-f9] 'fill-paragraph)
;;(global-set-key [C-f9] 'set-fill-column)
;;
;; F10
(global-set-key [f10] 'call-last-kbd-macro)
(global-set-key [S-f10] 'start-kbd-macro)
(global-set-key [C-f10] 'end-kbd-macro)
(global-set-key [M-f10] 'name-last-kbd-macro)
;;
;; F11
(global-set-key [f11]   'apropos)
(global-set-key [S-f11] 'describe-bindings)
(global-set-key [C-f11] 'info)
(global-set-key [M-f11] 'where-is)
;;
;; F12
(global-set-key [f12]   'save-buffer)
(global-set-key [S-f12] 'compile)
(global-set-key [C-f12] 'previous-error)
(global-set-key [M-f12] 'next-error)
;; F12 ==
;;
;;
;;
;; Following key assignments are very specific to my set up.
;;
;; Compatibility with Dan Lawrence's microemacs: M-g becomes goto-line
;;
(global-set-key [M-g] 'goto-line)
(global-set-key [M-a] 'apropos)
