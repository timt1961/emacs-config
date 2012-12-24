;;;; Prompt for directory
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
;;;; Prompt for and read a directory name, bypassing peculiar behavior of
;;;; read-file-name.

(defun request-directory (prompt)
  "Read a directory name with PROMPT, bypassing read-file-name's peculiar
behavior of returning buffer-file-name if users' response is
default-directory."
  (let ((dir (expand-file-name
	      ;; prevent read-file-name from returning buffer file name if
	      ;; user's response is default-directory
	      (let ((buffer-file-name nil))
		(read-file-name prompt default-directory nil t)))))
    (or (eq system-type 'vax-vms)
	(setq dir (file-name-as-directory dir)))
    (or (file-directory-p dir)
	(error "%s is not a directory" dir))
    dir))

(provide 'request-directory)
