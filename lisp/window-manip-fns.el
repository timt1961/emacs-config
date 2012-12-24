;;;; Miscellaneous window manipulation functions
;;;;
;;;; Distributed with compile2 version 2.07
;;;; Copyright Nick Duffek, 1993
;;;;
;;;; This file is not part of GNU Emacs.  However, the following applies as
;;;; if it were:
;;;;
;;;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;;;; ANY WARRANTY.  No author or distributor accepts responsibility to anyone
;;;; for the consequences of using it or for whether it serves any particular
;;;; purpose or works at all, unless he says so in writing.  Refer to the GNU
;;;; Emacs General Public License for full details.
;;;;
;;;; Everyone is granted permission to copy, modify and redistribute GNU
;;;; Emacs, but only under the conditions described in the GNU Emacs General
;;;; Public License.  A copy of this license is supposed to have been given
;;;; to you along with GNU Emacs so you can know your rights and
;;;; responsibilities.  It should be in a file named COPYING.  Among other
;;;; things, the copyright notice and this notice must be preserved on all
;;;; copies.
;;;;
;;;;===========================================================================
;;;;

;; window-list is from 18.55 distributions's saveconf.el; it's included here
;; so users don't need to load saveconf just to use other functions in this
;; file.

(defun window-list (&optional mini)
  "Returns a list of Lisp window objects for all Emacs windows.
Optional first arg MINIBUF t means include the minibuffer window
in the list, even if it is not active.  If MINIBUF is neither t
nor nil it means to not count the minibuffer window even if it is active."
  (let* ((first-window
	  (next-window (previous-window (selected-window)) mini))
	 (windows (cons first-window nil))
	 (current-cons windows)
	 (w (next-window first-window mini)))
    (while (not (eq w first-window))
      (setq current-cons (setcdr current-cons (cons w nil)))
      (setq w (next-window w mini)))
    windows))

(defun get-buffer-windows (buffer)
  "Return a list of all windows currently displaying BUFFER, which
can be a buffer or a buffer name."
  (and (stringp buffer)
       (setq buffer (or (get-buffer buffer)
			(error "No buffer named %s" buffer))))
  (let ((windows nil))
    (mapcar
     (function
      (lambda (window)
	(and (eq (window-buffer window) buffer)
	     (setq windows (cons window windows)))))
     (window-list))
    windows))

(defun top-window ()
  "Return the topmost window in Emacs' screen."
  (let (window)
    (mapcar (function (lambda (w)
			(and (= 0 (nth 1 (window-edges w)))
			     (setq window w))))
	    (window-list))
    window))

(defun display-buffer-excluding-windows (buffer &optional minibuf &rest
						excluded-windows)
  "Display but don't select BUFFER in some window other than WINDOWS
\(third and following args\).  Optional second arg MINIBUF t means
minibuffer window can be used, even if it is not active.  MINIBUF
neither t nor nil means to not use the minibuffer window even if it is
active.

If no windows other than WINDOWS exist and none of WINDOWS can be
split, the largest of WINDOWS is used.

Elements in WINDOWS can be null.

Returns the window displaying BUFFER."
  (let ((possible-windows (window-list minibuf))
	(window (get-buffer-window buffer))
	(selected-window (selected-window)))
    (or window
	;;
	;; Filter out excluded-windows from possible-windows
	;;
	(progn
	  (mapcar
	   (function
	    (lambda (w)
	      (setq possible-windows
		    (delq w possible-windows))))
	   excluded-windows)
	  (if possible-windows
	      (setq window (car possible-windows))
	    ;;
	    ;; Pick the biggest window and split it
	    ;;
	    (let ((max-size 0))
	      (mapcar
	       (function
		(lambda (w)
		  (let ((size (* (window-width w) (window-height w))))
		    (and (> size max-size)
			 (setq window w)))))
	       excluded-windows)
	      ;;
	      ;; Make sure the window is big enough to split.  If it's not, the
	      ;; request can't be satisfied; just use the biggest window.
	      ;;
	      (and (>= (window-height window) (* 2 window-min-height))
		   (setq window (split-window window)))))))
    
    (select-window window)
    (switch-to-buffer buffer)
    (select-window selected-window)
    window))

(provide 'window-manip-fns)
