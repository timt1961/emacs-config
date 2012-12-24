;;;; History list data structure
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
;;;; An hlist is a data structure optimized for use as a history list, i.e. a
;;;; list whose elements are typically accessed sequentially.
;;;;
;;;; This file provides functions for creating and operating on hlists,
;;;; including hlist-create, hlist-emptyp, hlist-clear, hlist-length,
;;;; hlist-current, hlist-first, hlist-last, hlist-nth, hlist-find,
;;;; hlist-find-sorted, hlist-forward, hlist-insert, hlist-insert-last,
;;;; hlist-delete, hlist-delete-first, hlist-map, and hlist-copy.
;;;;
;;;; For efficiency of insert, previous, and next operations, a history list
;;;; is a fairly complex data structure:
;;;;
;;;;   ---------------------------------
;;;;  |      _______ _______ _______ ___|___ _______
;;;;  |     |       |       |       |       |       |
;;;;  |     |  pos  |  len  |  max  |  beg  |  end  |----------
;;;;  |     |_______|_______|_______|_______|_______|          |
;;;;  |         |                       	              |
;;;;  |          ------------------------------               |
;;;;  |      ___ ___       ___ ___             |     ___ ___  |    ___ ___
;;;;   ---->|___|___|---->|___|___|----> . . .  --->|___|___|---->|___|_/_|
;;;;          |   ^         |   ^                     |   ^         |   ^
;;;;          |   |         |   |                     |   |         |   |
;;;;          |    ----     |    ----                 |    ----     |    ----
;;;; 	     _v_ ___   |   _v_ ___   |               _v_ ___   |   _v_ ___  |
;;;; 	    |___|___|  |  |___|___|  |   . . .      |___|___|  |  |___|___| |
;;;;         |   val   |   |   val   |               |   val   |   |   val  |
;;;; 	     |         |   |         |               |         |   |        |
;;;;         |  -------    |  -------                |  -------    |  -------
;;;; 	     v_| ___       v_| ___                   v_| ___       v_| ___
;;;; 	    |___|_/_|<--  |___|___|<--   . . . <--  |___|___|<--  |___|___|
;;;;		        |       |                 |       |	 |	 |
;;;;			 ------- 		   -------	  -------
;;;;
;;;; This structure allows using the fast nth function for scanning the list
;;;; forward or backward.

(require 'declare-object)

(eval-when-compile
  (declare-object 'hlist 'hl
    '(pos len max beg end)))

;;; Private functions

(defun hlist-elt-create (val next prev)
  "Return a new hlist element with components VAL, NEXT, and PREV."
  (let* ((val-pair (cons nil val))
	 (next-pair (cons val-pair next))
	 (prev-pair (cons next-pair
			  (and prev (car (car prev))))))
    (setcar val-pair prev-pair)
    next-pair))

(defmacro hlist-elt-val (elt)
  (` (cdr (car (, elt)))))

(defmacro hlist-elt-next (elt)
  (` (cdr (, elt))))

(defmacro hlist-elt-set-next (elt next)
  (` (setcdr (, elt) (, next))))

(defmacro hlist-elt-nth-next (n elt)
  ;; Returns the nth elt after elt, or nil if there are less than n elts
  ;; after hist-elt
  (` (nthcdr (, n) (, elt))))

(defmacro hlist-elt-nth-prev (n elt)
  ;; Returns the nth elt before elt, or nil if there are less than n elts
  ;; before hist-elt
  (` (nth (, n) (car (car (, elt))))))

(defmacro hlist-elt-prev (elt)
  ;; Returns the elt before elt, or nil if none
  (` (let ((prev (cdr (car (car (, elt))))))
       (and prev (car prev)))))

(defmacro hlist-elt-set-prev (elt prev)
  ;; Sets elt's prev pointer to point to prev's prev pointer
  (` (setcdr (car (car (, elt)))
	     (car (car (, prev))))))

(defun hlist-elt-delete (hl elt)
  ;; Delete elt from hlist.
  (let ((next (hlist-elt-next elt))
	(prev (hlist-elt-prev elt)))
    (hlist-set-hl-len hl (1- (hlist-hl-len hl)))
    (and (eq (hlist-hl-pos hl) elt)
	 (hlist-set-hl-pos hl (or next prev)))
    (if prev
	(hlist-elt-set-next prev next)
      (hlist-set-hl-beg hl next))
    (if next
	(hlist-elt-set-prev next prev)
      (hlist-set-hl-end hl prev))))

;;; Public functions

(defun hlist-create (&optional max)
  "Create and return a new hlist whose length never exceeds MAX \(nil means
no limit\)."
  (and (or (not (integerp max))
	   ;; if max <= 0, deleting the last element won't work right.
	   (<= max 0))
       (setq max nil))
  (let ((hl (hlist-create-hl)))
    (hlist-clear hl)
    (hlist-set-hl-max hl max)
    hl))

(defsubst hlist-emptyp (hl)
  "Return t if HLIST is empty, nil otherwise."
  (not (hlist-hl-beg hl)))

(defun hlist-clear (hl)
  "Remove all elements from HLIST."
  (hlist-set-hl-pos hl nil)
  (hlist-set-hl-len hl 0)
  (hlist-set-hl-beg hl nil)
  (hlist-set-hl-end hl nil))

(defsubst hlist-length (hl)
  "Return the number of elements in HLIST."
  (hlist-hl-len hl))

(defsubst hlist-current (hl)
  "Return the element at the current position in HLIST."
  (hlist-elt-val (hlist-hl-pos hl)))

(defsubst hlist-first (hl &optional set-position)
  "Return the first element in HLIST.  Optional second argument SET-POSITION
non-nil sets HLIST's position to that element."
  (let ((beg (hlist-hl-beg hl)))
    (and set-position (hlist-set-hl-pos hl beg))
    (hlist-elt-val beg)))

(defsubst hlist-last (hl &optional set-position)
  "Return the last element in HLIST.  Optional second argument SET-POSITION
non-nil sets HLIST's position to that element."
  (let ((end (hlist-hl-end hl)))
    (and set-position (hlist-set-hl-pos hl end))
    (hlist-elt-val end)))

(defsubst hlist-nth (n hl &optional set-position)
  "Return the Nth element from the beginning of HLIST.  N counts from zero.
Optional third argument SET-POSITION non-nil sets HLIST's position to that
element if it exists.  If HLIST is not that long, return nil and don't modify
its position."
  (let ((nth (hlist-elt-nth-next n (hlist-hl-beg hl))))
    (and nth (progn (and set-position (hlist-set-hl-pos hl nth))
		    (hlist-elt-val nth)))))

(defsubst hlist-nth-hence (n hl &optional set-position)
  "Return the Nth element from HLIST's current position.  N counts from zero.
Optional third argument SET-POSITION non-nil sets HLIST's position to that
element if it exists.  If HLIST is not that long, return nil and don't modify
its position."
  (let ((nth (if (< n 0)
		 (hlist-elt-nth-prev (- n) (hlist-hl-pos hl))
	       (hlist-elt-nth-next n (hlist-hl-pos hl)))))
    (and nth (progn (and set-position (hlist-set-hl-pos hl nth))
		    (hlist-elt-val nth)))))

(defun hlist-find (hl predicate &optional set-position)
  "Perform a linear search in HLIST for the first element for which PREDICATE
returns non-nil, and return that element.  Optional third argument
SET-POSITION non-nil sets HLIST's position to that element if it exists."
  (let ((elt (hlist-hl-beg hl))
	(found nil))
    (while (and elt (not found))
      (and (apply predicate (hlist-elt-val elt) nil)
	   (progn (setq found (hlist-elt-val elt))
		  (and set-position (hlist-set-hl-pos hl elt))))
      (setq elt (hlist-elt-next elt)))
    found))

(defun hlist-find-sorted (hl predicate &optional set-position)
  "Perform a binary search in HLIST for the first element for which PREDICATE
returns non-nil, and return that element.  Optional third argument
SET-POSITION non-nil sets HLIST's position to that element if it exists.

Assumes elements in HLIST are sorted such that PREDICATE returns nil for all
elements preceding the desired one, and non-nil for all remaining elements."
  (let ((len (hlist-length hl))
	(cur (hlist-hl-beg hl)))
    (while (> len 1)
      
      ;; Pick an element in the middle of the current list for examination by
      ;; PREDICATE.
      (let ((middle (hlist-elt-nth-next (/ len 2) cur)))
	(if (apply predicate (hlist-elt-val middle) nil)
	    
	    ;; Set the current list to the elements preceding the middle one.
	    (setq len (/ len 2))

	  ;; Set current list to elements following the middle one.
	  (setq len (- len 1 (/ len 2)))
	  (setq cur (hlist-elt-next middle)))))
    
    ;; cur's value now satisfies one of three conditions:
    (or
     (null cur)
     ;; (1) results from the degenerate case where len in the
     ;;     above loop was 2 and the last element didn't satisfy predicate.
     
     (apply predicate (hlist-elt-val cur) nil)
      ;; (2) implies cur is the first element satisfying predicate.
     
     (setq cur (hlist-elt-next cur))
     ;; (3) cur does not satisfy predicate, which implies cur either is
     ;;     the last element or else immediately precedes an element
     ;;     satisfying predicate.
     )
    
    (and cur (progn (and set-position (hlist-set-hl-pos hl cur))
		    (hlist-elt-val cur)))))

(defun hlist-forward (n hl)
  "Return the element N elements away from current position in HLIST and set
current position to point to it \(i.e., \(nth-hlist 0 h\) returns the element
at current position in h\)."
  ;; Return the nth element in hlist, and update hlist's current
  ;; position to point to that element.
  ;; If n > 0 and hlist's position is at its end, return nil; otherwise,
  ;;     if n > 0 and there aren't n more elements beyond hlist's
  ;;     position, return hlist's end.
  ;; Similarly for n < 0.
  (let ((pos (hlist-hl-pos hl)))
    (if (null pos)
	nil
      (let ((beg (hlist-hl-beg hl))
	    (end (hlist-hl-end hl))
	    (nth-elt
	     (if (< n 0)
		 (hlist-elt-nth-prev (- n) pos)
	       (hlist-elt-nth-next n pos))))
	(and (null nth-elt)
	     (setq nth-elt
		   ;; If we're already at the limit of the list, return nil
		   (if (< n 0)
		       (if (eq pos beg) nil beg)
		     (if (eq pos end) nil end))))
	;; Update hlist position:
	(and nth-elt
	     (progn (hlist-set-hl-pos hl nth-elt)
		    (hlist-elt-val nth-elt)))))))

(defun hlist-insert (hl val &optional no-duplicates predicate)
  "Insert at HLIST's beginning VAL, and set current position to point to it.
Optional third arg NO-DUPLICATES non-nil means remove all other occurrences
of VAL from the list, determining value equivalence by optional fourth arg
PREDICATE, which defaults to eq."
  (let* ((beg (hlist-hl-beg hl))
	 (new (hlist-elt-create val beg nil))
	 (len (hlist-hl-len hl)))
    (if beg				;; empty hlist?
	(hlist-elt-set-prev beg new)
      (hlist-set-hl-end hl new))
    (hlist-set-hl-pos hl new)
    (hlist-set-hl-beg hl new)
    (setq len (1+ len))
    (hlist-set-hl-len hl len)
    (and (hlist-hl-max hl)
	 (> len (hlist-hl-max hl))
	 (hlist-elt-delete hl (hlist-hl-end hl))))
  
  (and no-duplicates
       (let ((elt (hlist-elt-next (hlist-hl-beg hl))))
	 (while elt
	   (and (apply (or predicate 'eq) (hlist-elt-val elt) val nil)
		(hlist-elt-delete hl elt))
	   (setq elt (hlist-elt-next elt)))))
  hl)

(defun hlist-insert-last (hl val &optional no-duplicates pred)
  "Insert at HLIST's end VAL, provided HLIST has not already reached its
maximum length.  Do not modify HLIST's current position unless HLIST was
empty.  Optional third arg NO-DUPLICATES non-nil means remove all other
occurrences of VAL from the list, determining value equivalence by optional
fourth arg PREDICATE, which defaults to eq."
  (let ((new-len (1+ (hlist-hl-len hl))))

    ;; Don't exceed max length:
    (or (and (hlist-hl-max hl)
	     (> new-len (hlist-hl-max hl)))

	;; Don't violate duplicate rule:
	(and no-duplicates
	     (hlist-find hl (function (lambda (v)
					(apply (or pred 'eq) v val nil)))))
	
	;; Insert val:
	(let* ((end (hlist-hl-end hl))
	       (new (hlist-elt-create val nil end)))
	  (if end			;; empty hlist?
	      (hlist-elt-set-next end new)
	    (hlist-set-hl-beg hl new)
	    (hlist-set-hl-pos hl new))
	  (hlist-set-hl-end hl new)
	  (hlist-set-hl-len hl new-len)))))

(defun hlist-delete (hl predicate)
  "Remove from HLIST all values for which PREDICATE returns non-nil."
  (let ((elt (hlist-hl-beg hl)))
    (while elt
      (if (apply predicate (hlist-elt-val elt) nil)
	  (let ((doomed elt))
	    (setq elt (hlist-elt-next elt))
	    (hlist-elt-delete hl doomed))
	(setq elt (hlist-elt-next elt))))))

(defun hlist-delete-first (hl)
  "Remove and return the first element from HLIST."
  (let ((elt (hlist-hl-beg hl)))
    (and elt
	 (progn (hlist-elt-delete hl elt)
		(hlist-elt-val elt)))))

(defun hlist-delete-current (hl)
  "Remove and return the current element from HLIST.  Leave position at
following element if there is one, and at previous element otherwise."
  (let ((elt (hlist-hl-pos hl)))
    (and elt
	 (progn (hlist-elt-delete hl elt)
		(hlist-elt-val elt)))))

(defun hlist-map (fn hl)
  "Apply FUNCTION to each element of HLIST, starting with first element.
Return nil."
  (let ((elt (hlist-hl-beg hl)))
    (while elt
      (apply fn (hlist-elt-val elt) nil)
      (setq elt (hlist-elt-next elt))))
  nil)

(defun hlist-map-reverse (fn hl)
  "Apply FUNCTION to each element of HLIST, starting with last element.
Return nil."
  (let ((elt (hlist-hl-end hl)))
    (while elt
      (apply fn (hlist-elt-val elt) nil)
      (setq elt (hlist-elt-prev elt))))
  nil)

(defun hlist-copy (hl1 hl2)
  "Copy HLIST1 to HLIST2.  Sequences in HLIST1 are copied by reference, not
by value."
  (hlist-clear hl2)
  (hlist-map
   (function
    (lambda (val)
      (hlist-insert-last hl2 val)))
   hl1)
  nil)

(provide 'hlist)
