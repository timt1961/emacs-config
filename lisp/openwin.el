;;; openwin.el --- openwindows keyboard and mouse emulation

;; Copyright (C) 1994 Edward A. Seidman

;; Author: Ed Seidman <eseidman@syr.ge.com>
;; Keywords: mouse
;; Version: $Revision: 1.0 $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;;This package provides basic openwindows keyboard and mouse
;;emulation.  It requires version 19.22 or later of GNU emacs.
;;
;; --- Limitations -------------------------------------------------------
;;
;; 1)  When a selection is replaced by hilighting it and then
;; typing or pasting, it is deleted, instead of being put into the
;; CLIPBOARD This means that it cannot be pasted back in.
;;
;; 2) the keyboard Find key does not do a wrap.  It ends at the end
;; of the buffer
;;
;; 3) If two adjacent selections are "cut", they both end up in in
;; the CLIPBOARD, instead of only the last one.
;;
;; 4) moving the mouse with the keyboard after making a selection
;; extends the region that will be deleted if a character is typed.
;; Either using the mouse to move the point, or pressing the Stop key
;; will deselect the region.

;;; Code:

(provide 'openwin)

(defvar openwin-basics-only nil
  "Set this to non-nil to skip some handy extensions of keys")

(setq transient-mark-mode t)
(delete-selection-mode t)
(setq highlight-nonselected-windows nil)
(setq  mouse-sel-default-bindings 'interprogram-cut-paste)

(require 'mouse-sel)

;(global-unset-key [mouse-2])
;(global-set-key [down-mouse-2] 'mouse-extend)
;; define the R function keys
(global-set-key [f27] 'openwin-beginning-of-buffer)	; R7 Home
(global-set-key [f29] 'scroll-down)		; R9 PgUp
(global-set-key [f33] 'openwin-end-of-buffer)		; R13 End
(global-set-key [C-return] 'openwin-end-of-buffer)	; Ctrl Return - also  End
(global-set-key [f35] 'scroll-up)		; R15 PgDn

;; define the L function keys
(global-set-key [cancel] 'keyboard-quit)		; L1 - Stop
(global-set-key [redo] 'call-last-kbd-macro)	; L2 - Again
(global-set-key [f13] 'list-buffers)		; L3 - Props
(global-set-key [f14] 'undo)			; L4 - Undo
(global-set-key [f15] 'ignore-key)		; L5 - Front - X does this
(global-set-key [f16] 'copy-region-as-kill)		; L6 - Copy
(global-set-key [f17] 'ignore-key)		; L7 - Open - X does this
(global-set-key [f18] 'yank)	                ; L8 - Paste
(global-set-key [f19] 'openwin-find-key)	; L9 - Find
(global-set-key [f20] 'kill-region)    	        ; L10 - Cut
(global-set-key [help] 'help-for-help)		; HELP



;; These keys do not strictly emulate openwin, but they are handy
(if openwin-basics-only
    ()
  (global-set-key [f21] 'save-buffer)		; R1 Pause
  (global-set-key [f22] 'find-file)		; R2 PrSc
  (global-set-key [f23] 'insert-file)		; R3 ScrollLockBreak
  (global-set-key [f24] 'ignore-key)		; R4 = NOT DEFINED
  (global-set-key [f25] 'ignore-key)		; R5 / NOT DEFINED
  (global-set-key [f26] 'goto-line)		; R6 *
  (global-set-key [f31] 'recenter)		; R11 5
  (global-set-key [C-right] 'forward-char)
  (global-set-key [C-left]  'backward-char)
  (global-set-key [C-up]    'previous-line)
  (global-set-key [C-down]  'next-line)
)

(defun openwin-find-key ()
  "find the primary selection"
  (interactive)
  (let ((my-text (x-get-selection)))
    (search-forward my-text)
    (set-mark (point))
    (search-backward my-text)
    (exchange-point-and-mark)
    (x-set-selection 'PRIMARY (buffer-substring (region-beginning) (region-end)))
    ))

(defun openwin-end-of-buffer ()
  "Goto the end of the buffer without touching the mark"
  (interactive)
  (goto-char (point-max)))

(defun openwin-beginning-of-buffer ()
  "Go to the beginning of the buffer without touching the mark"
  (interactive)
  (goto-char (point-min)))

(defun openwin-x-set-clipboard (openwin-selection &optional PUSH)
  "set the clipboard selection"
  (x-set-selection 'CLIPBOARD openwin-selection ))

(setq interprogram-cut-function 'openwin-x-set-clipboard)
(setq interprogram-paste-function 'x-get-clipboard)


;; This kludge is needed to make openwindows applications properly grab the
;; selection from emacs
(add-hook 'x-sent-selection-hooks '(lambda (NAME TYPE FLAG &optional FOUR)
				    (setq eas-x-selection-name NAME)
				    (setq eas-x-selection-type TYPE)
				    (setq eas-x-selection-flag FLAG)
				    (setq eas-x-selection-four FOUR)
				    (if FLAG 
					(message "Paste request successful!")
				      (message "Paste request failed!"))))

;; openwin.el ends here.


