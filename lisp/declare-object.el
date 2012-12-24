;;;; Efficient fixed-size objects with named fields
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
;;;; Generate macros to create fixed-size objects and efficiently access and
;;;; modify their fields.

(defun declare-object (prefix-sym name-sym field-syms)
  "Args: PREFIX \(a symbol\), NAME \(a symbol\), and FIELDS \(a list of
symbols\).  Define macro \(PREFIX-create-NAME\) to return an object with
FIELDS, and macros \(PREFIX-NAME-FIELD object\) and \(PREFIX-set-NAME-FIELD
object value\) to respectively access and modify those FIELDS."
  (let* ((prefix (symbol-name prefix-sym))
	 (name (symbol-name name-sym))
	 (fields (mapcar '(lambda (field) (symbol-name field))
			 field-syms))
	 (nfields (length fields))
	 (index 0)
	 (creation-macro-name (concat prefix "-create-" name)))
    
    ;; Object creation macro
    (eval (` (defmacro
	       (, (intern creation-macro-name)) (&rest init-list)
	       (, (concat "\
Create and return a " name " object initialized with INIT-LIST's elements if
INIT-LIST is non-nil, and with null values otherwise."))
	       (let ((create-form '(make-vector (, nfields) nil)))
		 (if (null init-list)
		     create-form
		   (and (/= (length init-list) (, nfields))
			(error
			 (, (format
			     "%s requires either zero or %d arguments"
			     creation-macro-name nfields))))
		   
		   ;; Generate list of initialization forms, return let
		   ;; construct which creates and initializes object
		   (let ((let-construct
			  (` (let ((obj (, '(, create-form)))))))
			 (i 0))
		     (mapcar
		      (function
		       (lambda (init)
			 (nconc let-construct
				(list (list 'aset 'obj i init)))
			 (setq i (1+ i))))
		      init-list)
		     (nconc let-construct (list 'obj))))))))
    
    (mapcar
     '(lambda (field)
	
	;; Object modify macros
	(eval
	 (` (defmacro
	      (, (intern (concat prefix "-set-"
				 name "-" field)))
	      (object val)
	      (` (aset (, '(, object)) (, index)
		       (, '(, val)))))))
	
	;; Object create macros
	(eval
	 (` (defmacro
	      (, (intern (concat prefix "-"
				 name "-" field)))
	      (object)
	      (` (aref (, '(, object)) (, index))))))
	
	(setq index (1+ index)))
     
     fields)))

(put 'declare-object 'lisp-indent-hook 'defun)

(provide 'declare-object)
