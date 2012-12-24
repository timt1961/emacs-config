
;;;
;;; notes-mode.el
;;; $Id: notes-mode.el,v 1.38 1996/07/15 17:21:38 johnh Exp $
;;;
;;; Copyright (C) 1994-1996 by John Heidemann
;;; Comments to <johnh@isi.edu>.
;;;
;;; This file is under the Gnu Public License.
;;;

(require 'notes-variables)
(require 'notes-aux)

(defvar notes-mode-hooks nil
  "Hooks to run when entering notes-mode.")
(defvar notes-load-mode-hooks nil
  "Hooks to run when entering notes-mode is loaded.")

(defconst notes-beginning-of-defun-regexp "^\\* .*\n\\-"
  "Regexp matching the beginning of notes section.")

(defvar notes-default-tab-binding nil
  "Saved tab binding for notes-complete-subject.")
(defvar notes-default-return-binding nil
  "Saved return binding for notes-electric-return.")


(defun notes-beginning-of-defun ()
  "Go to the beginning of a notes ``section''."
  (interactive)
  (let
      ((old-point (point)))
    (beginning-of-line)
    ;; handle starting on a title
    (if (and (looking-at notes-beginning-of-defun-regexp)
	     (/= (point) old-point))
	nil
      (goto-char old-point)
      (if (looking-at "^-")  ;; handle starting on the underline under a title
	  (forward-char 1))
      (generic-beginning-of-defun notes-beginning-of-defun-regexp))))

(defun notes-end-of-defun ()
  "Go to the end of a notes ``section''."
  (interactive)
  (generic-end-of-defun notes-beginning-of-defun-regexp))

(defun notes-follow-link (which)
  "Go to the WHICH link for this topic.
WHICH is either \"next\" or \"prev\".
If there are no links for the current note,
we go to the last note based upon the index file."
  (let
      (beginning-of-note
       end-of-note
       (start-buffer (current-buffer))
       ;; We have to handle links in the same buffer,
       ;; so the following code figure out where we go
       ;; and returns it out of the save-excursion.
       ;; If we end up in another buffer, we let the save-excursion
       ;; leave the original buffer unchanged.  If we end up in
       ;; the same buffer, we need to go wherever we end up.
       ;; Can anyone suggest a better way?
       (end-buffer-and-point
	(save-excursion
	  (notes-end-of-defun)
	  (setq end-of-note (point))
	  (notes-beginning-of-defun)
	  (setq beginning-of-note (point))
	  (if (re-search-forward (concat "^"
					 (if (eq which 'next) "next" "prev")
					 ":[ ]+<") end-of-note t)
	      (progn ; link exists, just take it
		(beginning-of-line)
		(notes-w3-follow-link (point))
		(cons (current-buffer) (point)))
	    ;; No link; go through the index file.
	    (if (notes-goto-index-entry which)
		(let ((index-buffer (current-buffer)))
		  (notes-index-follow-link (point))
		  (bury-buffer index-buffer))
	      (error "No known notes in that direction.")
	      (bury-buffer (current-buffer)))
	    (cons (current-buffer) (point))))))
    ;; Check for going to the same buffer (and the save-excursion
    ;; undoing our work).
    (if (eq start-buffer (car end-buffer-and-point))
	(goto-char (cdr end-buffer-and-point)))))
       

(defun notes-follow-next-link ()
  "Go to the next link for this topic."
  (interactive)
  (notes-follow-link 'next))

(defun notes-follow-prev-link ()
  "Go to the previous link for this topic."
  (interactive)
  (notes-follow-link 'prev))

(defun notes-complete-subject ()
  "Complete the notes subject under point."
  (interactive)
  (let
      ((subject (save-excursion 
		  (beginning-of-line) 
		  (notes-extract-subject t)))
       old-completion-ignore-case
       full-subject)
    (if (not (and notes-mode-complete-subjects subject))
	(call-interactively notes-default-tab-binding)
      ;; Complete the title.
      (if (null notes-subject-table)
	  (save-excursion
	    (find-file-noselect (concat notes-dir "/index"))))
      ;; Do completion.
      ;; Run completer if it's loaded,
      ;; otherwise do our own thing.
      (setq completion-ignore-case t)
      (cond
       ((fboundp 'completer-complete-goto)
	(completer-complete-goto "^ \t\n\"" " " notes-subject-table nil))
       ;; NEEDSWORK:  should try other completers, too.
       (t   ;; Do our own completion.
	(setq full-subject (try-completion subject notes-subject-table)
	      subject (completing-read "Subject: "
				       notes-subject-table nil nil 
				       (if (stringp full-subject)
					   full-subject
					 subject)))
	(delete-region (get-beginning-of-line) (get-end-of-line))
	(insert "* " subject)))
      	(setq completion-ignore-case old-completion-ignore-case))))

(defun notes-electric-return (arg)
  "* Return, underlining if we're on a subject."
  (interactive "*P")
  (if (let ((cur-point (point)))
	 (save-excursion
	   (beginning-of-line)
	   (and (not (eq cur-point (point)))  ;; normal return if at b-o-ln
		(notes-extract-subject t))))
      (notes-underline-line)
    (call-interactively notes-default-return-binding)))

(defun notes-current-url ()
  "* Returns the notes-URL of the current entry around the current point."
  (let ((subject (notes-extract-subject nil t))
	(date (file-name-nondirectory buffer-file-name)))
    (concat "<file:///"
	    (abbreviate-file-name (buffer-file-name))
	    (if subject (concat "#* " subject) "")
	    ">")))

(defun notes-current-url-as-kill ()
  "* Put the notes-URL of the current entry into the kill ring."
  (interactive)
  (kill-new (notes-current-url)))

(defun notes-goto-index-entry (&optional direction)
  "* Jump to the index entry corresponding to our current note entry.
If we're not in an entry, we leave you in the index file.
If the current date doesn't exist, error in DIRECTION.
Returns nil if on errors (no index; no date in DIRECTION),
otherwise the point of the hit."
  (interactive) 
  (let ((start-buffer (current-buffer))
	(subject (notes-extract-subject))  ; get subject if on it
	(date (file-name-nondirectory buffer-file-name)))
    ;; Try and get the subject, either forward...
    (if (not subject)
	(save-excursion 
	  (notes-beginning-of-defun)
	  (setq subject (notes-extract-subject))))
    ;;    ...or backwards.
    (if (not subject)
	(save-excursion 
	  (notes-end-of-defun)
	  (setq subject (notes-extract-subject))))
    ;; Form and jump to the url for the index-entry.
    (if (and (notes-w3-url (concat notes-url-prefix
				   "index"
				   (if subject (concat "#" subject) ""))
			   nil t)
	     ;; Go to the current date, if any.
	     (notes-index-goto-date date direction))
	t
      nil)))

(defun notes-extract-subject (&optional relaxed search)
  "Extract the subject under the point in the current buffer.
If RELAXED, then accept non-underlined subjects.
If SEARCH we'll search back in the buffer for the nearest
subject title.

Returns nil if we're not on as subject."
  (save-match-data
    (cond
     ;; directly on a note
     ((or (looking-at notes-beginning-of-defun-regexp)
	  (and relaxed
	       (looking-at "^\\* ")))
      (save-excursion
	(let
	    ((start (+ (point) 2))
	     (end (progn (end-of-line) (point))))
	  (buffer-substring start end))))
     (search
      (save-excursion
	(notes-beginning-of-defun)
	(notes-extract-subject relaxed nil)))
     (t
      nil))))


;;;###autoload
(defun notes-underline-line ()
  "* Create a row of dashes as long as this line, or adjust the current underline."
  (interactive)
  ;; check to see if it's already underlined
  (if (save-excursion
	(forward-line 1)
	(looking-at "^[ \t]*-"))
      (notes-old-underline-line)
    (progn
      (notes-new-underline-line)
      (insert "\n\n"))))

(defun notes-new-underline-line ()
  "Underline a line with a row of dashes.  Moves the point after the dashes.
\\[notes-new-underline-line] reproduces leading spaces."
  (interactive)
  (let*
      ((bol (progn (beginning-of-line)
		   (point)))
       (bospaces (progn (skip-chars-forward " \t")
			(point)))
       (nospaces (- bospaces bol))
       (eol (progn (end-of-line)
		   (point))))
    (insert "\n" (buffer-substring bol bospaces))
    (insert-char ?- (- eol bospaces))))

(defun notes-old-underline-line ()
  "Replace the following line with a row of dashes.  Leave the point unchanged."
  (save-excursion
    (save-excursion
      (forward-line 1)
      (kill-line 1))
    (notes-new-underline-line)))

(defun notes-mode-initialize-note-from-cache ()
  "Build a new note from the cache.  Returns valid cache contents or nil."
  (save-excursion
    (let*
	((new-buffer (current-buffer))
	 (cache-file (concat notes-dir "/mknew.cache"))
	 (buf (find-file cache-file))
	 magic-line
	 prev-file
	 this-file
	 cache-contents
	 m)
      (if (and buf
	       (>= (count-lines (point-min) (point-max)) 3))
	  (progn
	    ;; If you know a more elegant way to extact the first
	    ;; three lines of a file, please let me know.
	    (goto-char (point-min))
	    (setq m (point))
	    (forward-line 1)
	    (setq magic-line (buffer-substring m (- (point) 1)))
	    (setq m (point))
	    (forward-line 1)
	    (setq prev-file (buffer-substring m (- (point) 1)))
	    (setq m (point))
	    (forward-line 1)
	    (setq this-file (buffer-substring m (- (point) 1)))
	    (setq cache-contents (buffer-substring (point) (point-max)))
	    (bury-buffer buf)
	    ;; is cache valid?
	    (if
		(and
		 (string-equal magic-line "mknew.cache 830494922")
		 (file-newer-than-file-p cache-file prev-file)
		 (string-equal (file-name-nondirectory this-file)
			       (file-name-nondirectory (buffer-file-name
							new-buffer))))
		cache-contents
	      nil))
	nil))))

(defun notes-mode-initialize-note ()
  "Fill in an empty new note.
Create any directories as necessary.
Use the mknew cache if possible."
  (interactive)
  (let
      ((dir (directory-file-name (file-name-directory (buffer-file-name)))))
    (if (file-exists-p dir)
	t
      (make-directory dir t)
      (message "New intermediate directory created.")))
  (if notes-mode-initialization-program
      (let
	  ((cache-contents (notes-mode-initialize-note-from-cache)))
	(if cache-contents
	    (insert cache-contents)
	  (shell-command-on-region
	   (point-min)
	   (point-max)
	   (concat notes-bin-dir "/" notes-mode-initialization-program " "
		   (buffer-file-name)) 't)))))
  

;;;
;;; encryption
;;; requires "PEM - PGP Enhanced Messaging for GNU Emacs"
;;; from Roy Frederick Busdiecker, III (Rick)
;;;

(defvar notes-encryption-userid nil
  "PGP key for the current user.")

(defun notes-encryption-userid ()
  "Return notes-encryption-userid, initializing it if necessary."
  (if notes-encryption-userid
      notes-encryption-userid
    (setq notes-encryption-userid
	  (list
	   (if notes-encryption-key-id
	      (npgp:get-key-by-key-id notes-encryption-key-id)
	     (pam:read-name-key (user-full-name)))))))

(defun notes-encrypt-note (prefix)
  "Encrypt the current note for the current user.  With PREFIX, start from point."
  (interactive "P")
  (require 'pam)
  (require 'npgp)
  (save-excursion
    (let (start end)
      ;; Unless a prefix arg, start at beginning-of-note.
      (if prefix
	  nil
	(if (not (looking-at notes-beginning-of-defun-regexp))
	    (notes-beginning-of-defun))
	;; skip over the header
	(while (or (looking-at notes-beginning-of-defun-regexp)
		   (looking-at "^-+$")
		   (looking-at "^\\(prev\\|next\\): ")
		   (looking-at "^[ \t]*$"))
	  (forward-line 1)))
      (setq start (point))
      ;; sanity check
      (if (or (looking-at pam:begin-pgp-message-regexp)
	      (looking-at pam:begin-pgp-signed-message-regexp))
	  (error "Note is already encrypted."))
      ;; find the end
      (notes-end-of-defun)
      (while (or (looking-at notes-beginning-of-defun-regexp)
		 (looking-at "^[ \t]*$"))
	(forward-line -1))
      (forward-line 1)
      (setq end (point))
      (npgp:encrypt-region (notes-encryption-userid)
			   start end))))

(defun notes-decrypt-note ()
  "Decrypt the current note for the current user."
  (interactive)
  (require 'pam)
  (require 'npgp)
  (save-excursion
    (if (not (looking-at notes-beginning-of-defun-regexp))
	(notes-beginning-of-defun))
    (let ((start (point)))
      (notes-end-of-defun)
      (save-restriction
	(narrow-to-region start (point))
	(pam:message-decrypt nil t)))))


;;;
;;; notes or notes-index?
;;;
(defun notes-summarize-subject (recursive &optional subject)
  "* Collect all of a subject."
  (interactive "P")
  (require 'notes-index-mode)
  (cond
   ((eq major-mode 'notes-mode)
    (setq subject (notes-extract-subject nil t)))
   ((eq major-mode 'notes-index-mode)
    (setq subject (notes-index-extract-subject))))
  (if (null subject)
   (error "notes-summarize-subject:  no subject specified or inferable."))
  (let
      ((buf (get-buffer-create (concat "*notes on " subject "*"))))
    (pop-to-buffer buf)
    (erase-buffer)
    (apply 'call-process (concat notes-bin-dir "/catsubject") nil buf t
	   (if recursive
	       (list "-r" subject)
	     (list subject)))
    (notes-mode)))


;;;
;;; notes-mode
;;;

(define-derived-mode notes-mode text-mode "Notes"
  "Enable notes-mode for a buffer.

Inside a notes buffer one can click on URLs and follow them to
other notes files.

Notes are fontified if notes-use-font-lock is set.
See the file notes-variables.el for all customization options.
To change options, (require 'notes-variables) in your .emacs
and then change things.

Subjects in notes mode are lines beginning with an asterisk
and underlined with dashes.  Subjects can be completed 
with \\[notes-complete-subject] and are automatically underlined.

You may wish to add this code to your .emacs file:
    (setq auto-mode-alist
  	(cons (cons \"/9[0-9][0-9][0-9][0-9][0-9].?$\" 'notes-mode)
  	      auto-mode-alist))
    (define-key global-map \"\C-cn\" 'notes-index-todays-link)
to automatically enter notes mode.

I have two suggestions for how to organize your notes files.
First, I collect my notes into a separate file per day.  (If you have
fewer notes, you may find once-per-week or month more suitable.)
Second, at the beginning of each file I have a subject \"* Today\".
Since every file has this subject, I can use its prev and next links
to easily move around the collection of files.

The key-bindings of this mode are:
\\{notes-mode-map}"
  ;; (interactive)

  ;; bug workaround:
  ;; Emacs-19.30's define-derived-mode sets up a bogus syntax-table.
  ;; (Evidence for the error is ``Wrong type argument: consp, nil''
  ;; when typing in the buffer.)
  (if (>= emacs-minor-version 30)
      (set-syntax-table (setq notes-mode-syntax-table text-mode-syntax-table)))

  ;; now set up the mode
  (auto-fill-mode 1)

  ;; random key-bindings
  (define-key notes-mode-map "\M-\C-a" 'notes-beginning-of-defun)
  (define-key notes-mode-map "\M-\C-e" 'notes-end-of-defun)
  (define-key notes-mode-map "\C-c\C-d" 'notes-decrypt-note)
  (define-key notes-mode-map "\C-c\C-e" 'notes-encrypt-note)
  (define-key notes-mode-map "\C-c\r" 'notes-w3-follow-link)
  (define-key notes-mode-map "\C-c\C-p" 'notes-follow-prev-link)
  (define-key notes-mode-map "\C-c\C-n" 'notes-follow-next-link)
  (define-key notes-mode-map "\C-c\C-i" 'notes-goto-index-entry)
  (define-key notes-mode-map "\C-c\C-k" 'notes-current-url-as-kill)
  (define-key notes-mode-map "\C-c\C-s" 'notes-summarize-subject)
  (define-key notes-mode-map "\C-c-" 'notes-underline-line)
  (if (null notes-default-tab-binding)
      (setq notes-default-tab-binding (key-binding "\t")))
  (define-key notes-mode-map "\t" 'notes-complete-subject)
  (if (null notes-default-return-binding)
      (setq notes-default-return-binding (key-binding "\r")))
  (define-key notes-mode-map "\r" 'notes-electric-return)
  (define-key notes-mode-map [S-mouse-2] 'notes-w3-follow-link-mouse)

  ;; imenu stuff 
  (make-variable-buffer-local 'imenu-prev-index-position-function)
  (make-variable-buffer-local 'imenu-extract-index-name-function)
  (setq imenu-prev-index-position-function 'notes-beginning-of-defun)
  (setq imenu-extract-index-name-function 'notes-extract-subject)

  (if (and notes-use-font-lock window-system)
      (progn
	(require 'font-lock)
	;; (require 'font-lock-stuff)
	(make-local-variable 'font-lock-no-comments)
	(setq font-lock-no-comments t)
	(make-local-variable 'font-lock-keywords)
	(setq font-lock-keywords notes-font-lock-keywords)
	(font-lock-mode 1)))

  ;; finally, try to fill in an empty note
  (if (eq (point-min) (point-max))
      (notes-mode-initialize-note))

  (run-hooks 'notes-mode-hooks))




;;;

(run-hooks 'notes-mode-load-hooks)
(provide 'notes-mode)

