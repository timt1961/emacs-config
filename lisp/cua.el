;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  CUA-select "mode"
;;;
;;;  OK, this is a misnomer, since it's merely a bunch of stuff to
;;;  throw in your .emacs file.  There is a lot of code here mainly
;;;  because I'm a hack (there is probably a better way).  This is an
;;;  attempt to make emacs work with CUA-type keys (ala MSWindows and
;;;  Motif, and to an extent, Macintosh).  Most importantly, this
;;;  makes selection using the <shift> key work, and copy(<ctrl-ins>),
;;;  cut(<shift-del>), and paste(<shift-ins>) work.
;;;
;;;  Copyright (c) 1995 Rob Lanphier (robla@eskimo.com)
;;;  Freely distributable under the GNU Public License
;;;
;;;  Updates may be found at http://www.eskimo.com/~robla
;;;

(transient-mark-mode 1)
(defun CUA-shift-left ()
  "An attempt to get CUA-ish response to shift-left"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (backward-char)
  )

(defun CUA-shift-right ()
  "An attempt to get CUA-ish response to shift-right"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (forward-char)
  )

(defun CUA-shift-up ()
  "An attempt to get CUA-ish response to shift-up"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (previous-line 1)
  )

(defun CUA-shift-down ()
  "An attempt to get CUA-ish response to shift-down"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (next-line 1)
  )

(defun CUA-shift-home ()
  "An attempt to get CUA-ish response to home"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (beginning-of-line)
  )

(defun CUA-shift-end ()
  "An attempt to get CUA-ish response to end"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (end-of-line)
  )

(defun CUA-shift-ctrl-home ()
  "An attempt to get CUA-ish response to ctrl-home"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (beginning-of-buffer)
  )

(defun CUA-shift-ctrl-end ()
  "An attempt to get CUA-ish response to ctrl-end"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (end-of-buffer)
  )

(defun CUA-shift-pgup ()
  "An attempt to get CUA-ish response to page-up"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (scroll-down)
  )

(defun CUA-shift-pgdn ()
  "An attempt to get CUA-ish response to ctrl-end"
  (interactive) 
  (if (not mark-active)
      (set-mark-command 'nil)
    ())
  (scroll-up)
  )

(defun CUA-left ()
  "An attempt to get CUA-ish response to left"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (backward-char)
  )

(defun CUA-right ()
  "An attempt to get CUA-ish response to right"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (forward-char)
  )

(defun CUA-up ()
  "An attempt to get CUA-ish response to up"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (previous-line 1)
  )

(defun CUA-down ()
  "An attempt to get CUA-ish response to down"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (next-line 1)
  )

(defun CUA-home ()
  "An attempt to get CUA-ish response to home"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (beginning-of-line)
  )

(defun CUA-end ()
  "An attempt to get CUA-ish response to end"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (end-of-line)
  )

(defun CUA-ctrl-home ()
  "An attempt to get CUA-ish response to ctrl-home"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (beginning-of-buffer)
  )

(defun CUA-ctrl-end ()
  "An attempt to get CUA-ish response to ctrl-end"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (end-of-buffer)
  )

(defun CUA-pgup ()
  "An attempt to get CUA-ish response to page-up"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (scroll-down)
  )

(defun CUA-pgdn ()
  "An attempt to get CUA-ish response to ctrl-end"
  (interactive) 
  (if mark-active
      (deactivate-mark)
    ())
  (scroll-up)
  )

(defun CUA-delete ()
  "An attempt to get CUA-ish response to ctrl-end"
  (interactive) 
  (if mark-active
      (delete-region (region-beginning) (region-end))
    (delete-char 1))
  )

(global-set-key [S-left] 'CUA-shift-left)
(global-set-key [S-right] 'CUA-shift-right)
(global-set-key [S-up] 'CUA-shift-up)
(global-set-key [S-down] 'CUA-shift-down)

(global-set-key [left] 'CUA-left)
(global-set-key [right] 'CUA-right)
(global-set-key [up] 'CUA-up)
(global-set-key [down] 'CUA-down)

(global-set-key [S-delete] 'clipboard-kill-region)
(global-set-key [delete] 'CUA-delete)
(global-set-key [C-insert] 'kill-ring-save)
(global-set-key [S-insert] 'yank)

(global-set-key [home] 'CUA-home)
(global-set-key [end] 'CUA-end)
(global-set-key [C-home] 'CUA-ctrl-home)
(global-set-key [C-end] 'CUA-ctrl-end)
(global-set-key [pgup] 'CUA-pgup)
(global-set-key [pgdn] 'CUA-pgdn)

(global-set-key [S-home] 'CUA-shift-home)
(global-set-key [S-end] 'CUA-shift-end)
(global-set-key [C-S-home] 'CUA-shift-ctrl-home)
(global-set-key [C-S-end] 'CUA-shift-ctrl-end)
(global-set-key [S-pgup] 'CUA-shift-pgup)
(global-set-key [S-pgdn] 'CUA-shift-pgdn)

;;;
;;;  End of CUA-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

