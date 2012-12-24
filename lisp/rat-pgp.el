;;; -*-Emacs-Lisp-*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:          rat-pgp.el v 1.4.3um
;;; Description:   PGP Public Key system front-end for GNU Emacs
;;; Author:        Richard Pieri, ratinox@ccs.neu.edu
;;;		   Some additional code Dan Rich, drich@lerc.nasa.gov
;;;                 Ulf Moeller, 3umoelle@informatik.uni-hamburg.de
;;; Created:       Fri Dec 25 12:25:42 1992
;;; Last modified: Tue Dec  6 19:56:26 MET 1994
;;; FTP:           The original version of rat-pgp.el can be anonymously FTP'ed
;;;                from ftp.ccs.neu.edu:/pub/ratinox/emacs-lisp/rat-pgp.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Caveat: it is inherently insecure to use PGP or any other encryption
;;; system on a multi-user system. There are just too many ways for someone
;;; to spy on what you are doing. It is highly recommended that you keep
;;; your private keys (secring.pgp) on write-protected mountable floppies
;;; and you keep these disks in a secure place.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Known Bugs:
;;; + There is no checking to see if you have entered an invalid pass
;;;   phrase in pgp-decrypt-message. If you do, then everything will seem
;;;   to freeze as PGP awaits a valid pass phrase. Typing C-g will unlock
;;;   things, and you can check the *PGP-Log* buffer for any errors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation. 

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a copy of the GNU General Public License write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Installation:
;;;
;;; Make sure that the PGP executable is in your PATH, then byte-compile
;;; this file, put it in your load-path. Add the command:
;;;  (autoload 'pgp-insinuate-keys "pgp" "Add PGP key bindings to a mode" nil)
;;; then update your approprate setup hooks (ie, mail-setup-hook) to call
;;; pgp-insinuate-keys.
;;;
;;; You will probably also want to configure config.txt to do things like
;;; automatically add keys to your keyrings and such.
;;;
;;; description with "M-x pgp-help"
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History:
;;; * Richard Pieri, Feb 25, 1993: rewrote the decryption code based on
;;;   suggestions and code written by Robert Anderson
;;;   <bs891@cleveland.Freenet.Edu>.
;;; * Richard Pieri, Jun 7, 1993: incorporated Dan Rich's code, made
;;;   clearing temporary files a bit more reasonable.
;;; * Richard Pieri, Jun 18, 1993: changed the name to "rat-pgp" to avoid
;;;   confusion with other PGP front-ends for GNU Emacs. Output from PGP
;;;   commands now is kept in the buffer *PGP-Log*, so you can see what
;;;   went right or wrong. Re-wrote the passphrase handling code. Made lots
;;;   of improvements.
;;; * Richard Pieri, June 22, 1993: fixed a bug in pgp-set-passphrase.
;;; * Richard Pieri, June 22, 1993: fixed all the problems created by the
;;;   last edit. Maybe that will teach me not to code when caffeine sober.
;;; * Richard Pieri, June 25, 1993: added pgp-validate-signature.
;;; * um, Mar 28, 1994: code restructured, minor bugfixes,
;;;   added pgp-encrypt-region, pgp-encrypt-current-buffer,
;;;   pgp-sign-and-encrypt-region, pgp-sign-and-encrypt-current-buffer
;;;   pgp-sign-region, pgp-sign-current-buffer.
;;; * um, Dec 6, 1994: pgp-decrypt-message: PGP output redirected to PGP Log,
;;;   notify user of signature.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pgp-program "pgp"
  "PGP program. This should be in your PATH environment variable somewhere.")

(defvar pgp-path (getenv "PGPPATH")
  "This should match your PGPPATH environment variable.")

(defvar pgp-tmp (concat pgp-path "/pgptmp.pgp")
  "Scratch file used by pgp -f.")

(defvar pgp-passphrase nil
  "PGP passphrase.")

(defvar pgp-always-clear-passphrase nil
  "If t, clear the pass phrase from memory every time PGP finishes using it.
This is the secure, but inconvenient option.
Anything else will cause the current pass to remain in memory. This is the
less secure, but more convenient option.")

(defvar pgp-user ((lambda (u) (if u u (getenv "LOGNAME"))) (getenv "USER")))

(defconst pgp-flags nil
  "Flags to be used with all PGP commands.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pgp-log-buffer-create ()
  "Create PGP Log buffer."
  (get-buffer-create "*PGP-Log*")
  (let ((this-buffer (current-buffer))
	(pgp-log-buffer (get-buffer-create "*PGP-Log*")))
    (set-buffer pgp-log-buffer)
    (goto-char (point-max))
    (newline 1)
    (insert-char ?= 70)
    (newline 1)
    (set-buffer this-buffer)
    pgp-log-buffer
    ))

(defun pgp-delete-files ()
  "Delete pgp-tmp if it exists. Smart enough to check for
temporary files in whatever directory you are currently in."
  (if (file-exists-p pgp-tmp)
      (delete-file pgp-tmp))
  (if (file-exists-p "pgptemp.pgp")
      (delete-file "pgptemp.pgp"))
  )

(defun pgp-insinuate-keys ()
  "Call from various mode setup hooks to bind PGP keys."
  (global-set-key "\C-cpc" 'pgp-clear-passphrase)
  (global-set-key "\C-cpd" 'pgp-decrypt-message)
  (global-set-key "\C-cpe" 'pgp-encrypt-region)
  (global-set-key "\C-cpE" 'pgp-encrypt-current-buffer)
  (global-set-key "\C-cph" 'pgp-help)
  (global-set-key "\C-cpi" 'pgp-insert-public-key-block)
  (global-set-key "\C-cpp" 'pgp-set-passphrase)
  (global-set-key "\C-cps" 'pgp-sign-region)
  (global-set-key "\C-cpS" 'pgp-sign-current-buffer)
  (global-set-key "\C-cpa" 'pgp-sign-and-encrypt-region)
  (global-set-key "\C-cpA" 'pgp-sign-and-encrypt-current-buffer)
  (global-set-key "\C-cpv" 'pgp-validate-signature)
  (global-set-key "\C-cpx" 'pgp-extract-public-key)
  )

(defun pgp-help ()
  "Describe the rat-pgp key bindings.

Key      Command Name                  Description
=======  ============================  ========================================
C-c p c  pgp-clear-passphrase          Clears the current PGP passphrase from
                                       memory (see security note below).
C-c p d  pgp-decrypt-message           Decrypts the PGP encrypted message in
                                       the current buffer. Asks for passphrase.
C-c p e  pgp-encrypt-region            Encrypts the region/buffer.
C-c o E  pgp-encrypt-current-buffer    Asks for recipient.

C-c p h  pgp-help                      What you are reading right now.
                                       
C-c p i  pgp-insert-public-key-block   Inserts your PGP Public Key Block at
                                       point.
C-c p p  pgp-set-passphrase            Sets your PGP passphrase (see security
                                       note below).
C-c p s  pgp-sign-region               Signs the region/buffer.
C-c p S  pgp-sign-current-buffer       Uses CLEARSIG, asks for passphrase.

C-c p v  pgp-validate-signature        Checks the validity of the signature on
                                       the message in the current message
C-c p a  pgp-sign-and-encrypt-region   Signs and encrypts the region/buffer.
C-c p A  pgp-sign-and-encrypt-current-buffer

C-c p x  pgp-extract-public-key        Attempts to add the PGP Public Key Block
                                       in the current buffer to your keyring.

WARNING! Security Holes:
People can see your PGP passphrase if:

* You set pgp-passphrase via a setq.

* Emacs crashes and leaves a core file; anyone with even a partial
  understanding of hash tables can extract your pass phrase from the core.

* Plus all the other normal Unix and/or X-Windows security holes.
"
  (interactive)
  (describe-function 'pgp-help))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Passphrase support. Some of this is blatantly taken from ange-ftp.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pgp-read-passphrase (prompt &optional default)
  "Read a password from the user. Echos a . for each character typed.
End with RET, LFD, or ESC. DEL or C-h rubs out.  ^U kills line.
Optional DEFAULT is password to start with."
  (let ((pass (if default default ""))
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t))
    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
      (message "%s%s"
	       prompt
	       (make-string (length pass) ?.))
      (setq c (read-char))
      (if (= c ?\C-u)
	  (setq pass "")
	(if (and (/= c ?\b) (/= c ?\177))
	    (setq pass (concat pass (char-to-string c)))
	  (if (> (length pass) 0)
	      (setq pass (substring pass 0 -1))))))
    (pgp-repaint-minibuffer)
    (substring pass 0 -1)))

(defun pgp-repaint-minibuffer ()
  "Gross hack to set minibuf_message = 0, so that the contents of the
minibuffer will show."
  (if (eq (selected-window) (minibuffer-window))
      (if (fboundp 'allocate-event)
	  ;; lemacs
	  (let ((unread-command-event (character-to-event ?\C-m
							  (allocate-event)))
		(enable-recursive-minibuffers t))
	    (read-from-minibuffer "" nil pgp-tmp-keymap nil))
	;; v18 GNU Emacs
	(let ((unread-command-char ?\C-m)
	      (enable-recursive-minibuffers t))
	  (read-from-minibuffer "" nil pgp-tmp-keymap nil)))))

(defun stripstrlist (l str)
  "Strip from list-of-strings L any string which matches STR."
  (cond (l (cond ((string-match str (car l))
		  (stripstrlist (cdr l) str))
		 (t (cons (car l) (stripstrlist (cdr l) str)))))))

(defun pgp-set-passphrase (arg)
  "Set PGPPASS environment variable from argument."
  (interactive)
  (setq arg
	(pgp-read-passphrase "Enter pass phrase: " pgp-passphrase))
  (setq process-environment
	(cons (concat "PGPPASS=" arg)
	      (stripstrlist process-environment "^PGPPASS=")))
  (setq pgp-passphrase arg)
  )

(defun pgp-clear-passphrase ()
  "Clear PGPPASS environment variable."
  (interactive)
  (setq process-environment (stripstrlist process-environment "^PGPPASS="))
  (setq pgp-passphrase nil)
  )

;;;
;;; internal functions
;;;

(defun pgp-sign (start end)
  (pgp-delete-files)
  (save-window-excursion
    (save-excursion
      (pgp-set-passphrase pgp-passphrase)
      (let ((buffer-status buffer-read-only))
	(setq buffer-read-only nil)
	(let ((this-buffer (current-buffer))
	      (pgp-log-buffer (pgp-log-buffer-create)))
	  (kill-region start end)
	  (set-buffer pgp-log-buffer)
	  (yank)
	  (message "PGP: signing message...")
	  (shell-command-on-region (point) (mark)
				   (concat pgp-program
					   " -fast +clearsig=on") t)
	  (search-backward "-----BEGIN PGP SIGNED MESSAGE-----")
	  (kill-region (point) (point-max))
	  (goto-char (point-max))
	  (set-buffer this-buffer)
	  (yank)
	  (setq buffer-read-only buffer-status))
	)))
  (if pgp-always-clear-passphrase
      (pgp-clear-passphrase))
  (pgp-delete-files)
  (message "PGP: signing message... done.")
  )

(defun pgp-encrypt (userid start end)
  (pgp-delete-files)
  (save-window-excursion
    (save-excursion
      (let ((this-buffer (current-buffer))
	    (pgp-log-buffer (pgp-log-buffer-create)))
	(kill-region start end)
	(set-buffer pgp-log-buffer)
	(yank)
	(message "PGP: encrypting message...")
	(shell-command-on-region
	 (point) (mark) (concat pgp-program " -fea " userid) t)
	(search-backward "-----BEGIN PGP MESSAGE-----")
	(push-mark)
	(search-forward "-----END PGP MESSAGE-----")
	(forward-char 1)
	(kill-region (point) (mark))
	(goto-char (point-max))
	(set-buffer this-buffer)
	(yank)
	)))
  (pgp-delete-files)
  (message "PGP: encrypting message... done.")
  )

(defun pgp-sign-and-encrypt (userid start end)
  (pgp-delete-files)
  (save-window-excursion
    (save-excursion
      (pgp-set-passphrase pgp-passphrase)
      (let ((buffer-status buffer-read-only))
	(setq buffer-read-only nil)
	(let ((this-buffer (current-buffer))
	      (pgp-log-buffer (pgp-log-buffer-create)))
	  (kill-region start end)
	  (set-buffer pgp-log-buffer)
	  (yank)
	  (message "PGP: signing and encrypting message...")
	  (shell-command-on-region (point) (mark)
				   (concat pgp-program
					   " -safe " userid) t)
	  (search-backward "-----BEGIN PGP MESSAGE-----")
	  (kill-region (point) (point-max))
	  (goto-char (point-max))
	  (set-buffer this-buffer)
	  (yank)
	  (setq buffer-read-only buffer-status))
	)))
  (if pgp-always-clear-passphrase
      (pgp-clear-passphrase))
  (pgp-delete-files)
  (message "PGP: signing and encrypting message... done.")
  )

(defun pgp-decrypt (start end)
  (let ((pgp-message "PGP: decrypting message... done."))
    (pgp-delete-files)
    (save-window-excursion
      (save-excursion
	(pgp-set-passphrase pgp-passphrase)
	(let ((buffer-status buffer-read-only))
	  (setq buffer-read-only nil)
	  (let ((this-buffer (current-buffer))
		(pgp-log-buffer (pgp-log-buffer-create)))
	    (message "PGP: decrypting message...")
	    (shell-command-on-region
	     start end (concat pgp-program " -f 2>" pgp-tmp) t)
	    (set-buffer pgp-log-buffer)
	    (insert-file pgp-tmp)
	    (cond ((not (search-forward "Just a moment.." nil t))
		   (setq pgp-message "PGP: cannot decrypt message..."))
		  ((search-forward "Good signature" nil t)
		   (beginning-of-line)
		   (kill-line)
		   (setq pgp-message (car kill-ring-yank-pointer))
		   (yank)))
	    (set-buffer this-buffer)
	    (setq buffer-read-only buffer-status)
	    ))))
  (if (eq pgp-always-clear-passphrase t)
      (pgp-clear-passphrase))
  (pgp-delete-files)
  (message pgp-message)))

(defun pgp-validate (start end)
  (let ((pgp-message "PGP: cannot validate signature."))
    (message "PGP: validating signature...")
    (shell-command-on-region
     start end (concat pgp-program " -f "))
    (set-buffer "*Shell Command Output*")
    (cond ((search-backward "Good signature" nil t)
	   (beginning-of-line)
	   (kill-line)
	   (setq pgp-message (car kill-ring-yank-pointer))
	   (yank)
	   (next-line 2)))
  (message pgp-message)))

;;;
;;; interactive functions
;;;

(defun pgp-sign-message ()
  "Sign the message at point."
  (interactive)
  (goto-char (point-min))
  (search-forward mail-header-separator)
  (forward-char 1)
  (pgp-sign (point) (point-max))
  )

(defun pgp-encrypt-message (userid)
  "Encrypt from mail-header-separator to (point-max), replacing clear text
with cyphertext and the Public Key message delimiters."
  (interactive "sRecipient's userid: ")
  (goto-char (point-min))
  (search-forward mail-header-separator)
  (forward-char 1)
  (pgp-encrypt userid (point) (point-max))
  )

(defun pgp-sign-and-encrypt-message (userid)
  "Sign the message at point."
  (interactive "sRecipient's userid: ")
  (goto-char (point-min))
  (search-forward mail-header-separator)
  (forward-char 1)
  (pgp-sign-and-encrypt userid (point) (point-max))
  )

(defun pgp-decrypt-message ()
  "Decrypt the PGP message between the BEGIN/END PGP MESSAGE delimiters,
replacing cyphertext with clear text in the current buffer.

Note that this function may be a security hole. If a pass phrase is in
memory when GNU Emacs crashes, it will appear in the core file. Anyone with
a half-decent grasp of hash tables will be able to extract your pass phrase
from the core file."
  (interactive)
  (goto-char (point-min))
  (search-forward "-----BEGIN PGP ")
  (beginning-of-line)
  (push-mark)
  (search-forward "-----END PGP ")
  (forward-char 1)
  (pgp-decrypt (mark) (point)))

(defun pgp-sign-current-buffer ()
  "Sign the current buffer."
  (interactive)
  (pgp-sign (point-min) (point-max))
  )

(defun pgp-encrypt-current-buffer (userid)
  "Encrypt the current buffer, replacing clear text with cyphertext and
the Public Key message delimiters."
  (interactive "sRecipient's userid: ")
  (pgp-encrypt userid (point-min) (point-max))
  )

(defun pgp-sign-and-encrypt-current-buffer (userid)
  "Sign and encrypt the current buffer."
  (interactive "sRecipient's userid: ")
  (pgp-sign-and-encrypt userid (point-min) (point-max))
  )

(defun pgp-sign-region ()
  "Sign region."
  (interactive)
  (pgp-sign (point) (mark))
  )

(defun pgp-encrypt-region (userid)
  "Encrypt region, replacing clear text with cyphertext and the Public Key message delimiters."
  (interactive "sRecipient's userid: ")
  (pgp-encrypt userid (point) (mark))
  )

(defun pgp-sign-and-encrypt-region (userid)
  "Sign and encrypt region."
  (interactive "sRecipient's userid: ")
  (pgp-sign-and-encrypt userid (point) (mark))
  )

(defun pgp-validate-signature ()
  "Validate the signature on the current message. An error will occour if the
public key from the sender does not exist on your key ring."
  (interactive)
  (pgp-validate (point-min) (point-max)))

;;;
;;; public key handling
;;;

(defun pgp-insert-public-key-block ()
  "Insert your PGP Public Key Block at point."
  (interactive)
  (pgp-delete-files)
  (save-window-excursion
    (let ((this-buffer (current-buffer))
	  (pgp-log-buffer (pgp-log-buffer-create)))
      (set-buffer pgp-log-buffer)
      (message "PGP: inserting public-key block...")
      (shell-command (concat pgp-program " -kxaf " pgp-user " > " pgp-tmp ) t)
      ))
  (if (> 0 (current-column)) (newline 1))
  (insert-file pgp-tmp)
  (pgp-delete-files)
  (message "PGP: inserting public-key block... done.")
  )

(defun pgp-extract-public-key ()
  "Extract the public key from a message and put it into your public keyring."
  (interactive)
  (pgp-delete-files)
  (save-window-excursion
    (save-excursion
      (let ((buffer-status buffer-read-only))
	(setq buffer-read-only nil)
	(goto-char (point-min))
	(search-forward "-----BEGIN PGP PUBLIC KEY BLOCK-----")
	(beginning-of-line)
	(push-mark)
	(search-forward "-----END PGP PUBLIC KEY BLOCK-----")
	(forward-char 1)
	(let ((this-buffer (current-buffer))
	      (pgp-log-buffer (pgp-log-buffer-create)))
	  (kill-region (point) (mark))
	  (yank)
	  (set-buffer pgp-log-buffer)
	  (yank)
	  (write-region (point) (mark) pgp-tmp)
	  (message "PGP: extracting public-key block...")
	  (shell-command (concat pgp-program " -ka " pgp-tmp) t)
	  (goto-char (point-max))
	  (setq buffer-read-only buffer-status))
	)))
  (pgp-delete-files)
  (message "PGP: extracting public-key block... done.")
  )
