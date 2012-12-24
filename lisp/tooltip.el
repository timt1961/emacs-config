;;; tooltip.el -- make tooltip windows

;; Copyright (C) 1997 Gerd Moellmann

;; Author:	  Gerd Moellmann <gerd@acm.org>
;; Maintainier:   Gerd Moellmann <gerd@acm.org>
;; Version:	  $Id: tooltip.el,v 1.6 1997/06/01 09:30:24 gerd Exp $
;; Keywords:	  help c mouse tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; I recently saw the Microsoft VC++ Debugger for the first time, and
;; I must admit that it has one feature that made me jealous the
;; longer I used it:  it shows variable values in small tool tip
;; windows just by pointing at a variable in the source text.

;; You guess it---I had to do this for Emacs...

;; The result is, more or less a general tooltip package with a
;; small fraction implementing the debugger part.  What you get is:

;; (a) a global `tooltip-hook' that is run whenever the user hasn't
;; moved the mouse (unless she was hacking on the keyboard,
;; in the meantime),

;; (b) a buffer local `tooltip-local-hook' (that can, of course,
;; only handle events over buffers).

;; (c) a function `tooltip-show' that can be called from hook
;; functions to show a tool tip with arbitrary text, at an arbitray
;; position, and 

;; (d) some predefined hook functions that show tool tips for the
;; modeline, toolbars, extents with the `help-echo' property (try
;; `dired', and---finally here we are---for printing expressions in
;; `gdbsrc' buffers. See also the doc string of `tooltip-gdbsrc-tips'
;; for what you can do with it.

;; (BTW: Yes I know of the package `ballon-help' but i preferred to
;; redo it from scratch).

;;; Platform:

;;; XEmacs 19.15p4, FreeBSD 2.2.1, fvwm95

;;; Installation:

;; Compile `tooltip.el' and `(require 'tooltip)' from your `.emacs'.
;; Depending on your window manager you should also make sure that:

;; 1. Tooltip frames get no decoration.  The name of the tooltip frames
;; is `tooltip'.  Here is an example for `fvwm95' that can be copied
;; to your `~/.fvwm95rc':

;; Style "tooltip" NoTitle, NoHandles, BorderWidth 0

;; 2. Make sure that tooltip frames require no interactive placement.

;; Style "tooltip" RandomPlacement

;;; Customization:

;; You can and probably should customize the package via the
;; `Options/Customize' menu.  You find it under `Emacs/Help'.

;;; To do, Bugs:

;; 1. Find out the height of the menubar, if present.  I have no idea how
;; this can be done from Emacs Lisp.  Currently there are fixed values
;; in the code.

;; 2. Perhaps change the tooltip position if it isn't fully visible?
;; This could be difficult for virtual desktops because we can only
;; find out the width and height of the current device but not its
;; origin...

;;; Code:

(require 'cl)
(require 'custom)
(provide 'tooltip)



;;; Version info

(defun tooltip-version ()
  "Return and print, if interactively called, the tooltip version
number (a float)."
  (interactive)
  (let ((version (string-to-number (substring "$Revision: 1.6 $" 11))))
    (when (interactive-p)
      (message "Tooltip version %g" version))
    version))



;;; Customization

(defgroup tooltip nil
  "Customization group for the `tooltip' package."
  :group 'help
  :group 'c
  :group 'mouse
  :group 'tools
  :tag "Tool Tips")


(defcustom tooltip-frame-parameters
  '(name "tooltip"
    initially-unmapped t
    unsplittable t
    minibuffer nil
    menubar-visible-p nil
    scrollbar-width 0
    scrollbar-height 0
    border-width 1
    border-color "black"
    top-toolbar-height 0
    left-toolbar-width 0
    modeline-shadow-thickness 0
    right-toolbar-width 0
    bottom-toolbar-height 0
    [default background] "gold"
    [default foreground] "black"
    has-modeline-p nil)
  "Frame parameters used to create `tooltip-frame'.  The frames
name is `tooltip'.  This name should be used to switch off any
decorations for that frame, and to prevent user interaction when
placing it.  Here is an example for `fvwm95':

Style \"tooltip\" NoTitle, NoHandles, BorderWidth 0, RandomPlacement"
  :type 'sexp
  :tag "Frame Parameters"
  :group 'tooltip)


(defcustom tooltip-delay 1.0
  "Delay in seconds for tooltip display."
  :tag "Delay"
  :type 'number
  :group 'tooltip)


(defcustom tooltip-hook nil
  "Functions to call when idle-time is detected. Each function is called
with one argument `event' which is a copy of the last mouse movement
event that occurred."
  :type 'sexp
  :tag "Hook"
  :group 'tooltip)


(defcustom tooltip-modeline-tips-p t
  "Show tooltips for the modeline?"
  :type 'boolean
  :tag "Modeline"
  :group 'tooltip)


(defcustom tooltip-toolbar-tips-p t
  "Show tooltips for toolbar buttons?"
  :type 'boolean
  :tag "Tool Bar"
  :group 'tooltip)


(defcustom tooltip-extent-tips-p t
  "Shall tooltips for extents with property `help-echo'?"
  :type 'boolean
  :tag "Extents"
  :group 'tooltip)


(defcustom tooltip-gdbsrc-tips-p t
  "Show tooltips for expressions in `gdbsrc' mode."
  :type 'boolean
  :tag "Gdbsrc"
  :group 'tooltip)


(defcustom tooltip-decoration-height 35
  "The height of the title plus menu bar in pixels.

The frame property `top' apparently returns the pixel position of the upper
left corner of the title bar (if such a decoration exists).  Other positions
are relative to the `Emacs frame' which seems to begin under the menu bar,
and includes the tool bar.  If someone knows a way to determine these
values from Lisp, please tell me."
  :type 'integer
  :tag "Menu Bar Height"
  :group 'tooltip)


(defcustom tooltip-adjust-x 0
  "Adjust the X position at which tooltips appear relative to the
mouse position.  Use this to make sure that the tooltip doesn't
hide what the mouse is pointing at."
  :type 'integer
  :tag "Adjust X"
  :group 'tooltip)


(defcustom tooltip-adjust-y 0
  "Adjust the Y position at which tooltips appear relative to the
mouse position.  Use this to make sure that the tooltip doesn't
hide what the mouse is pointing at."
  :type 'integer
  :tag "Adjust Y"
  :group 'tooltip)


(defcustom tooltip-max-width 60
  "The maximum width for tooltip-frames.  Lines longer than this
will be shown in continuation lines like in other Emacs windows
when `truncate-lines' is nil."
  :type 'integer
  :tag "Maximal Width"
  :group 'tooltips)


(defcustom tooltip-max-height 40
  "The maximum height in character lines for tooltip-frames."
  :type 'integer
  :tag "Maximal Height"
  :group 'tooltips)




;;; Variables that are not customizable.

(defvar tooltip-frame nil
  "The frame window used to display tool tips.")

(defvar tooltip-buffer nil
  "The buffer displayed in `tooltip-frame'.")

(defvar tooltip-frame-displayed-on nil
  "The frame on which the tooltip is displayed, if any.")

(defvar tooltip-timeout-id nil
  "The id of the timeout started when XEmacs becomes idle.")

(defvar tooltip-local-hook nil
  "Buffer local hook for tooltips.  Functions on this hook are passed
one parameter `event' which is a copy of the last mouse motion event
seen.  Automatically buffer-local when set.")
(make-variable-buffer-local 'tooltip-local-hook)



;;; Mouse motion

(defvar tooltip-mouse-motion-hook mouse-motion-handler
  "There is currently no global hook for mouse motions. Instead,
`mouse.el'has a variable `mouse-motion-handler' which can be set
to a function that is called when the mouse moves, and buffer local
`mode-motion-hook'.  Since we want to do somthing that is not
buffer specific, we wrap up `mouse-motion-handler' with a function
that calls a hook of our own.")

(setq mouse-motion-handler 'tooltip-mouse-motion-handler)
(add-hook 'tooltip-mouse-motion-hook 'tooltip-mouse-motion-hook)


(defvar tooltip-last-mouse-motion-event (allocate-event)
  "A copy of the last mouse motion event seen.")


(defun tooltip-mouse-motion-handler (event)
  "Runs all functions on `tooltip-mouse-motion-hook' when a mouse motion
event occurs.  Each function is passed one argument `event' which is
a copy of the last mouse motion event that has occurred."
  (run-hook-with-args 'tooltip-mouse-motion-hook event))


(defun tooltip-mouse-motion-hook (event)
  "Hook function called when the mouse is moved.  Removes a tooltip
if one is visible.  Records the mouse motion event for the next
tooltip in `tooltip-last-mouse-motion-event'.  Starts timeout for the
next tooltip display."
  (tooltip-hide)
  (copy-event event tooltip-last-mouse-motion-event)
  (tooltip-add-timeout))




;;; Timeout

(defun tooltip-disable-timeouts ()
  "Disable all timeouts  Note that for small `tooltip-delay' values
it could happen that `tooltip-hide-timeout-id' is still active."
  (when tooltip-timeout-id
    (disable-timeout tooltip-timeout-id)
    (setq tooltip-timeout-id nil))
  (when tooltip-hide-timeout-id
    (disable-timeout tooltip-hide-timeout-id)
    (setq tooltip-hide-timeout-id nil)))


(defun tooltip-add-timeout ()
  "Add a one-shot timeout to call `tooltip-timeout'."
  (setq tooltip-timeout-id
	(add-timeout tooltip-delay 'tooltip-timeout nil)))


(defun tooltip-timeout (object)
  "Function called when our timer fires.  If the last mouse motion
event was over a buffer and this buffer contains a local variable
`tooltip-local-hook' run functions from this hook.  Each function
receives one parameter---a copy of the last mouse motion event seen.
Hook functions must return nil if they don't handle the tip.
Processing stops with the first function returning a non-nil value.
If the last motion event was not associated with a buffer, has no
local variable `tooltip-local-hook' or none of the buffer local
hook function applies, then function from the global hook `tooltip-hook'.
are run with the same logic as above."
  (let ((buffer (event-buffer tooltip-last-mouse-motion-event)))
    (unless (and buffer
		 (local-variable-p 'tooltip-local-hook buffer)
		 (save-excursion
		   (set-buffer buffer)
		   (run-hook-with-args-until-success
		    'tooltip-local-hook tooltip-last-mouse-motion-event)))
      (run-hook-with-args-until-success
       'tooltip-hook tooltip-last-mouse-motion-event))))



;;; Input focus.

;; We would like to make sure that tooltips are hidden when the input
;; focus is given to another frame without using the mouse.  This is
;; a little bit tricky because window manager policies are highly
;; customizable.

(defvar tooltip-hide-timeout-id nil
  "A timeout started when the frame on which the tooltip is displayed
is deselected.  If the `tooltip-frame' isn't selected shortly after
`tooltip-frame-displayed-on' is deselected, we can assume that the
window managers policy is not `ClickToFocus' and can hide the tooltip.")


(defconst tooltip-hide-timeout 0.1
  "The time in s to wait for the selection of the `tooltip-frame' after
`tooltip-frame-displayed-on' has been deselected.")


(defun tooltip-hide-timeout (object)
  "Called when `tooltip-pending-hide-timeout-id' fires. Hide the tip."
  (setq tooltip-hide-timeout-id nil)
  (tooltip-hide))


(defun tooltip-deselect-frame-hook ()
  "Called when a frame is about to loose the input focus.  When the
`tooltip-frame' looses focus, this is an indicator for a `ClickToFocus'
input focus policy, i.e. frames get the focus when they are shown.  We
can hide the tooltip in this case.  Otherwise, if the frame on which
the tooltip is displayed looses focus, we have to wait a little to see
if the tooltip gets the focus."
  (let ((frame (selected-frame)))
    (cond
     ((eq frame tooltip-frame)
      (tooltip-hide))
     ((eq frame tooltip-frame-displayed-on)
      (setq tooltip-hide-timeout-id
	    (add-timeout tooltip-hide-timeout 'tooltip-hide-timeout nil))))))


(defun tooltip-select-frame-hook ()
  "Called just after a frame gets the input focus.  If the tooltip
frame gets the focus disable a pending hide."
  (when (and tooltip-hide-timeout-id
	     (eq (selected-frame) tooltip-frame))
    (tooltip-disable-timeouts)))


(defun tooltip-leave-frame-hook (frame)
  "Moving the mouse over the toolbar up to the menu generates
a `leave-frame'.  This must disable timeouts, otherwise we would
eventually show a tooltip for some of the toolbar buttons while
the mouse is in the menu because we don't get mouse motion events
over the menu bar."
  (tooltip-disable-timeouts))


(add-hook 'deselect-frame-hook 'tooltip-deselect-frame-hook)
(add-hook 'select-frame-hook 'tooltip-select-frame-hook)
(add-hook 'mouse-leave-frame-hook 'tooltip-leave-frame-hook)



;;; DIsplay.

(defun tooltip-initialize-tooltip-frame ()
  "Create the frame, buffer etc. for tooltip windows."
  (unless (and tooltip-frame
	       (frame-live-p tooltip-frame))
    (setq tooltip-frame (make-frame tooltip-frame-parameters))
    (setq tooltip-buffer (get-buffer-create " *tooltip*"))
    (save-excursion
      (set-buffer tooltip-buffer)
      (setq truncate-lines nil)
      (setq buffer-read-only t))
    (set-specifier text-cursor-visible-p nil tooltip-buffer)
    (set-window-buffer (frame-selected-window tooltip-frame) tooltip-buffer)))


(defun tooltip-text-dimensions (buffer max-width)
  "Return two values.  The first is the width of the longest line in
`buffer', the second is the number of lines in `buffer'.  A final
newline in the buffer is ignored."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (loop with width = 0
	  with height = 1
	  with continuation-lines = 0
	  until (eobp)
	  do (let ((columns (progn (end-of-line) (current-column))))
	       (setq width (max width columns))
	       (when (> columns max-width)
		 (incf continuation-lines
		       (+ (1- (/ columns max-width))
			  (if (plusp (mod columns max-width)) 1 0)))))
	  unless (eobp) do (forward-char)
	  unless (eobp) do (incf height)
	  finally return (values (min width max-width)
				 (+ height continuation-lines)))))


(defun tooltip-set-text (text)
  "Insert `text' into the tooltip buffer.  Return the dimension of the
text in columns and lines."
  (save-excursion
    (set-buffer tooltip-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert text))))


(defun tooltip-show (x y text)
  "Show a tooltip window at position `x/y' displaying `text'.  The window
will automatically disappear when the user moves the mouse, clicks or
issues a command."
  (setq tooltip-frame-displayed-on (selected-frame))
  (tooltip-initialize-tooltip-frame)
  (tooltip-set-text text)
  (multiple-value-bind (cx cy)
      (tooltip-text-dimensions tooltip-buffer tooltip-max-width)
    (setq cy (min cy tooltip-max-height))
    (set-frame-size tooltip-frame (1+ cx) cy)
    (decf y (frame-pixel-height tooltip-frame)) 
    (set-frame-position tooltip-frame x y)
    (make-frame-visible tooltip-frame)))
  

(defun tooltip-hide (&optional ignored-arg)
  "Cancel timeout, hide tooltip window."
  (tooltip-disable-timeouts)
  (when (and tooltip-frame
	     (frame-live-p tooltip-frame)
	     (frame-visible-p tooltip-frame))
    (setq tooltip-frame-displayed-on nil)
    (make-frame-invisible tooltip-frame)))


(defun tooltip-x (event)
  "Given a mouse event, return the x position relative to the root
desktop window where the event occurred."
  (let ((frame (event-frame event)))
    (+ (event-x-pixel event)
       tooltip-adjust-x
       (frame-property frame 'internal-border-width)
       (frame-property frame 'left))))


(defun tooltip-y (event)
  "Given a mouse event, return the y position relative to the root
desktop window where the event occurred."
  (let ((frame (event-frame event)))
    (+ (event-y-pixel event)
       tooltip-adjust-y
       (frame-property frame 'top)
       (frame-property frame 'top-toolbar-height)
       tooltip-decoration-height)))



;;; Modeline, toolbar and extent tips.

;; This uses the standard `help-char' extent property for the modeline
;; and for extents in the text area.  Toolbar buttons have an
;; associated function that can be called to obtain the help string.


(defun tooltip-help-echo (point object)
  "Return the `help-echo' extent property from `point' in `object',
if any.  This function must return nil if it doesn't handle `event'."
  (let ((extent (extent-at point object 'help-echo)))
    (and extent (extent-property extent 'help-echo))))


(defun tooltip-modeline-tips (event)
  "Show tooltips for the modeline.  This function must return nil
if it doesn't handle `event'."
  (when (and tooltip-modeline-tips-p
	     (event-over-modeline-p event))
    (let* ((point (event-modeline-position event))
	   (buffer (event-buffer event))
	   (string (symbol-value-in-buffer 'generated-modeline-string buffer))
	   (help (tooltip-help-echo point string)))
      (when help
	(tooltip-show (tooltip-x event) (tooltip-y event) help))
      help)))


(defun tooltip-toolbar-tips (event)
  "Show tooltips for the toolbar.  This function must return nil if it
 doesn't handle `event'."
  (when (and tooltip-toolbar-tips-p
	     (event-over-toolbar-p event))
    (let* ((button (event-toolbar-button event))
	   (help (and button (toolbar-button-help-string button))))
      (when help
	(tooltip-show (tooltip-x event) (tooltip-y event) help))
      help)))


(defun tooltip-extent-tips (event)
  "Display tooltips for extents with property `help-echo'.
This function must return nil if it doesn't handle `event'."
  (when (and tooltip-extent-tips-p
	     (event-over-text-area-p event)
	     (event-point event))
    (let ((help (tooltip-help-echo (event-point event) (event-buffer event))))
      (when help
	(tooltip-show (tooltip-x event) (tooltip-y event) help))
      help)))
      


;;; Display variable values in tooltips when in `gdbsrc' mode.  

;; doesn't really belong here, but as long as `tooltip' is not
;; supported by other packages, I find it easier to keep everything
;; together.

(defvar tooltip-gdb-original-filter nil
  "Process filter to restore after result of `print' gdb command has
been received.")


(defun tooltip-c-identifier-from-point (point)
  "Extract the C identifier at `point', if any.  Returns nil if no
identifier exists at point.  Syntactic context (comments, strings)
is respected.  Do not assume that we are in `c-mode' or `c++-mode'
because this would be inappropriate for `noweb' programmers using
`noweb-mode' (a mode in which different portions of a buffer have
different major modes)."
  (save-excursion
    (goto-char point)
    (unless (buffer-syntactic-context)
      (let ((start (progn (skip-chars-backward "a-zA-Z_0-9") (point))))
	(when (looking-at "[a-zA-Z]")
	  (skip-chars-forward "a-zA-Z_0-9")
	  (buffer-substring start (point)))))))


(defun tooltip-gdb-output (process output)
  "Process the `gdb' output and show it in a tooltip window.
We have to strip the `gdb' prompt at the end of the output."
  (set-process-filter process tooltip-gdb-original-filter)
  (setq output (replace-in-string output "\n*(gdb) *$" "" 'literal))
  (tooltip-show (tooltip-x tooltip-last-mouse-motion-event)
		(tooltip-y tooltip-last-mouse-motion-event)
		output))


(defun tooltip-gdb-expr-to-print (event)
  "Get the expression that should be printed.  If a region is active
and the mouse is inside the region, print the region.  Otherwise
figure out the identifier around the point where the mouse is."
    (save-excursion
      (set-buffer (event-buffer event))
      (let ((point (event-point event)))
	(if (region-active-p)
	    (when (and (<= (region-beginning) point) (<= point (region-end)))
	      (buffer-substring (region-beginning) (region-end)))
	  (tooltip-c-identifier-from-point point)))))
  

(defun tooltip-gdbsrc-tips (event)
  "Show tip for variable value in `gdbsrc' mode.  The mouse must
either point at a variable or inside a selected region for the tip
window to be shown.  If the shift key was down when the mouse was
moved, `print*' else `print' the value. This function must return nil
if it doesn't handle `event'."
  (let (gdb-buffer process buffer)
    (when (and tooltip-gdbsrc-tips-p
	       (setq gdb-buffer (and (boundp 'current-gdb-buffer)
				     (symbol-value 'current-gdb-buffer)))
	       (setq process (get-buffer-process gdb-buffer))
	       (event-over-text-area-p event)
	       (event-point event)
	       (setq buffer (event-buffer event))
	       (symbol-value-in-buffer 'gdbsrc-mode buffer))
      (let ((expr (tooltip-gdb-expr-to-print event)))
	(when expr
	  (setq tooltip-gdb-original-filter (process-filter process))
	  (set-process-filter process 'tooltip-gdb-output)
	  (process-send-string
	   process
	   (concat (if (memq 'shift (event-modifiers event))
		       "print * " "print ")
		   expr "\n")))
	expr))))



;;; Installation.

(add-hook 'tooltip-hook 'tooltip-modeline-tips)
(add-hook 'tooltip-hook 'tooltip-toolbar-tips)
(add-hook 'tooltip-hook 'tooltip-extent-tips)
(add-hook 'tooltip-hook 'tooltip-gdbsrc-tips)
(add-hook 'pre-command-hook 'tooltip-hide)
(add-hook 'activate-menubar-hook 'tooltip-hide)
(add-hook 'activate-popup-menu-hook 'tooltip-hide)

;; tooltip.el ends here

