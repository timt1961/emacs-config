;;; findlib.el - Find file in a directory list, with name completion

;; If you work with source code (or other files) arranged in several
;; directories or in a tree of subdirectories, here's a helpful utility.
;; It finds a module anywhere in a specified directory list,
;; with automatic name completion.
;; 
;; By default, it's configured to find Emacs Lisp modules on the standard
;; library search path.
;;
;; Hooks are provided to restrict the search to specified file types
;; and to generate lists of (recursive) subdirectories.
;; 
;; Author:   Bill Brodie <wbrodie@panix.com>
;; Version:  $Revision: 1.10 $
;; Date:     $Date: 1995/04/13 11:40:55 $

;; Sample key binding for .emacs:
;; (autoload 'findlib "findlib"
;;   "Find a file in a list of directories, with name completion.")
;; (global-set-key "\C-x\C-l" 'findlib)  ; overwrite downcase-region


;;; Configuration hooks

(defvar findlib-path load-path
  "Directories searched by \\[findlib].
You can reassign this to your own list of project directories.
To obtain a list of a directory and all its subdirectories (excluding those
which match a specified pattern), use the function `findlib-tree'.
Example:
 (setq findlib-path (findlib-tree \"/usr/projects/app\" \"/RCS$\"))")

(defvar findlib-file-regexp "\\.el$"
  "*Regexp for files retrieved by \\[findlib].
Example:  (setq findlib-file-regexp \"\\\\.[ch]$\")")

(defvar findlib-truncate t
  "*If T, truncate extension from names offered in completion list.
The portion truncated is the portion matching `findlib-file-regexp.'")


;;; Main entry point

(defun findlib (file)
  "Find a file in a list of directories, with name completion.
By default, searches for Lisp sources in the directories named in `load-path.'
To change the directories and file type matched, assign new values
to the variables `findlib-path' and `findlib-file-regexp'.
See also the documentation for functions `load-library' and `load.'"
  (interactive
   (list (let* ((choices (findlib-file-alist))
		(choice
		 (completing-read "Find library file: " choices nil t))
		(matching-choices
		 (findlib-matching-choices choice choices)))
	   (cond
	    ((null matching-choices)
	     (error "Library file %s not found." choice))
	    ((null (cdr matching-choices))
	     (cdr (car matching-choices)))
	    (t
	     (let* ((file-choices
		     (mapcar (function (lambda (entry)
					 (cons (cdr entry) (cdr entry))))
			     matching-choices))
		    (initial-input (findlib-initial-input file-choices)))
	       (completing-read "Please select unique pathname: "
				file-choices nil t initial-input)))))))
  (progn
    (find-file file)
    (message "Found %s"
	     (if (featurep 'listbuf)
		 (listbuf-prettify-file-name file)
	       file))))

(defun findlib-matching-choices (choice a-list)
  "List all A-LIST entries matching CHOICE."
  (let ((choices nil))
    (while a-list
      (if (equal choice (car (car a-list)))
	  (setq choices (cons (car a-list) choices)))
      (setq a-list (cdr a-list)))
    (nreverse choices)))

(defun findlib-initial-input (choices)
  "Find longest initial substring matching all elements of a-list CHOICES."
  (let* ((len 1)
	 (first-choice (car (car choices)))
	 (max-len (length first-choice))
	 (failed nil))
    (while (and (<= len max-len) (not failed))
      (let ((test (substring first-choice 0 len))
	    (others (cdr choices)))
	(while (and others (not failed))
	  (setq failed
		 (not (string-equal test (substring (car (car others)) 0 len)))
		others
		 (cdr others)))
	(or failed (setq len (1+ len)))))
    (substring first-choice 0 (1- len))))

(defun findlib-tree (path &optional regexp)
  "Return a list of PATH and its subdirectories.
Directories matching optional second argument REGEXP are excluded.
The list is constructed depth-first.
See the documentation for the variable `findlib-path.'"
  (if (file-directory-p path)
      (findlib-tree-1 (expand-file-name path) regexp)
    (error "%s is not a directory." path)))

(defun findlib-tree-1 (path regexp)
  (let ((result (list path))
	(files (directory-files path t)))
    (while files
      (let ((file (car files)))
	(if (and (not (string-match "\\(^\\|/\\)\\(\\.\\|\\.\\.\\)$" file))
		 (not (string-match regexp file))
		 (file-directory-p file))
	  (setq result (nconc result (findlib-tree-1 file regexp)))))
      (setq files (cdr files)))
    result))

(defun findlib-file-alist ()
  "Return a-list of (library-file . pathname) for completing-read.
By default, finds all Emacs Lisp files (*.el) on directories in load-path.
To change the directory and file type matched, assign new values to the
variables `findlib-path' and `findlib-file-regexp.'
See also the documentation for `findlib', `load', and `load-path'."
  (apply 'nconc
	 (mapcar
	  (function
	   (lambda (dir)
	     (mapcar (function
		      (lambda (file)
			(let* ((name (file-name-nondirectory file))
			       (module
				(if findlib-truncate
				    (substring name 0
					       (string-match
						 findlib-file-regexp
						 name))
				  name)))
			  (cons module file))))
		     ;; If no regexp provided, screen out directories
		     ;; (This is slow, so if regexp given, assume that's
		     ;;  sufficient.)
		     (let ((files
			    (directory-files dir t findlib-file-regexp t)))
		       (or findlib-file-regexp
			   (setq files (findlib-remove-directories files)))
		       files))))
	  findlib-path)))

(defun findlib-remove-directories (files)
  ;; Remove elements of FILES which name directories
  (let ((result '()))
    (while files
      (or (file-directory-p (car files))
	  (setq result (cons (car files) result)))
      (setq files (cdr files)))
    (nreverse result)))

(provide 'findlib)

;;; findlib.el ends here

