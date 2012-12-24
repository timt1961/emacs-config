;;;; Generic filter wrapper
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
;;;; Generic wrapper for filters that modify buffers and perform matching.
;;;;
;;;; Functions that use generic-filter require window-manip-fns to be loaded.

(defmacro generic-filter (filter-name process output buffer inhibit
				      &rest forms)
  "Wrapper for use around code in any filter which doesn't modify any
buffer at a point preceding \(point-max\).

Args: FILTER-NAME PROCESS OUTPUT BUFFER INHIBIT &rest FORMS.

Sets buffer to \(process-buffer PROCESS\), creates variable BUFFER and sets
it to that buffer, inhibits other invocations of the filter by PROCESS while
this one is active \(e.g. if this one is waiting for user input, as during
debugging\) if INHIBIT is non-nil, and evaluates FORMS, intelligently
retaining the following context across evaluation:
 - match data
 - window points
 - buffer points

Uses FILTER-NAME and OUTPUT \(from process, passed as arg to filter
function\) internally for inhibiting other invocations if INHIBIT.

Requires process-mark to be set \(e.g. \(set-marker \(process-mark process\)
\(point-max\)\)."
  (let ((body
	 (` (let* ((inhibit-quit nil)
		   (start-buffer (current-buffer))
		   (start-buffer-point (point))
		   (proc-mark (process-mark (, process)))
		   window-start-points
		   buffer-start-point
		   start-point-max
		   finish-point-max
		   (start-proc-mark (marker-position proc-mark))
		   (match-data (match-data))
		   ((, buffer) (process-buffer (, process))))
	      (unwind-protect
		  (progn
		    (set-buffer (, buffer))
		    (setq buffer-start-point (point))
		    (setq window-start-points
			  (mapcar (function (lambda (w)
					      (cons w (window-point w))))
				  (get-buffer-windows (, buffer))))
		    (setq start-point-max (point-max))
		    (goto-char start-proc-mark)
		    
		    (,@ forms)
		    (, t)			;; avoid bug in Emacs v18
		    
		    ;; Generic filter post-insert and match-data restoring
		    (move-marker proc-mark (point)))
		(setq finish-point-max (point-max))
		(goto-char
		 (if (< buffer-start-point start-proc-mark)
		     buffer-start-point
		   (- finish-point-max (- start-point-max buffer-start-point))))
		(set-buffer start-buffer)
		(goto-char start-buffer-point)
		(mapcar (function
			 (lambda (wp)
			   (let ((window (car wp))
				 (window-start-point (cdr wp)))
			     (set-window-point
			      window
			      (if (< window-start-point start-proc-mark)
				  window-start-point
				(- finish-point-max (- start-point-max
						       window-start-point)))))))
			window-start-points)
		(store-match-data match-data))))))
    
    (if inhibit
	(` (if (get 'generic-filter-inhibited (, process))
	       
	       ;; Another filter is active -- append our output to saved output
	       ;; and return.
	       
	       (put 'generic-filter-output (, process)
		    (concat (get 'generic-filter-output (, process))
			    (, output)))
	     
	     ;; No other filters active -- install this invocation as active
	     ;; filter and evaluate body.

	     (put 'generic-filter-inhibited (, process) t)
	     (, body)
	     (put 'generic-filter-inhibited (, process) nil)

	     ;; Reinvoke self if output was collected by other invocations
	     ;; during our evaluation of body.

	     (let ((output (get 'generic-filter-output (, process))))
	       (and output
		    (progn (put 'generic-filter-output (, process) nil)
			   ((, filter-name) (, process) output))))))
      
      body)))

(provide 'generic-filter)
