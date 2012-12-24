;;;; dired-tar.el - extensions to dired to create and unpack tar files.
;;;; Jim Blandy <jimb@cyclic.com> --- June 1995

;; Author: Jim Blandy <jimb@cyclic.com>
;; Maintainer: Jim Blandy <jimb@cyclic.com>
;; Created: 6 Sep 1993
;; Updated: 6 Jun 1995
;; Version: 1.4
;; Keywords: unix

;;; Commentary:

;;; dired-tar adds a command to dired-mode for creating and unpacking
;;; tar files.  When using this package, typing `T' on a tar file in a
;;; dired listing unpacks the tar file, uncompressing it if necessary.
;;; Typing `T' on a directory packs up that directory into a gzipped
;;; tar file named DIRNAME.tar.gz.
;;;
;;; To use this package, just place it in a directory in your Emacs
;;; lisp load path, byte-compile it, and put the line
;;;    (require 'dired-tar)
;;; in your .emacs.
;;;
;;; This file defines the following function:
;;;
;;; dired-tar-pack-unpack - If the file on the current line is a tar
;;;    file, or a gzipped or compressed tar file, unpack it.  If the
;;;    file on the current line is a directory, build a tar file for
;;;    it, and gzip it.
;;;
;;; It also declares the following variables:
;;;
;;; dired-tar-should-gzip - if t, dired-tar-pack gzips the tar files
;;;	it creates.  If nil, dired-tar-pack leaves the tars alone.
;;;
;;; dired-tar-gzip-command - a shell command which gzips its
;;;     standard input to its standard output.
;;;
;;; dired-tar-ungzip-command - a shell command which ungzips
;;;	its standard input to its standard output.

;;; Code:

(require 'compile)


;;;; Variables.
(defvar dired-tar-should-gzip t
  "*t if dired-tar-pack-unpack should gzip the files it creates, by default.
nil if it should leave them alone.")

(defvar dired-tar-gzip-command "gzip --best --stdout"
  "*A shell command which gzips its standard input to its standard output.")

(defvar dired-tar-ungzip-command "gzip --decompress --stdout"
  "*A shell command which ungzips its standard input to its standard output.")

(defvar dired-tar-result nil
  "For internal use by dired-tar functions.
This variable is made local to the buffer in which we run the tar
process, and holds the name of the file created or affected.  The
process-termination sentinal uses this to update the dired listing
when the process completes its work, or dies.")


;;;; Internal functions.

(defun dired-tar-run-command (command directory result)
  "Internal function for use by the dired-tar package.
Run COMMAND asynchronously in its own window, like a compilation.
Use DIRECTORY as the default directory for the command's execution.
RESULT is the name of the tar file which will be created, or the
name of the directory into which the tar file was unpacked."
  (let ((buf (dired-tar-get-buffer)))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (widen)
      (erase-buffer)
      (goto-char (point-min))
      (insert "cd " directory)
      (newline)
      (insert command)
      (newline)

      (setq buffer-read-only t
	    mode-name "Tar-Output"
	    default-directory directory)

      (set (make-local-variable 'dired-tar-result)
	   result)
      (set (make-local-variable 'mode-line-process)
	   '(": %s"))
      (set (make-local-variable 'compilation-finish-function)
	   'dired-tar-operation-done)

      (let ((process
	     ;; Chris Moore <Chris.Moore@src.bae.co.uk> says that the tar
	     ;; commands barf using his version of the zsh.  We don't need
	     ;; anything but the Bourne shell here.
	     (let ((shell-file-name "/bin/sh"))
	       (start-process-shell-command "*Tar*" buf command))))
	(set-process-sentinel process 'compilation-sentinel))
      (display-buffer buf))))

(defun dired-tar-get-buffer ()
  "Choose a buffer to run a tar process in.
Tar output buffers have names like *Tar*, *Tar*<2>, *Tar*<3>, ...
We return the lowest-numbered buffer that doesn't have a live tar
process in it.  We delete any other buffers whose processes have
deleted."

  ;; Kill all completed tar buffers.
  (let ((number 1))
    (while number
      (let* ((name (if (<= number 1) "*Tar*"
		     (format "*Tar*<%d>" number)))
	     (buf (get-buffer name)))
	(if (null buf) (setq number nil)
	  (save-excursion
	    (set-buffer buf)
	    (if (let ((process (get-buffer-process buf)))
		  (not (and process (eq (process-status process) 'run))))
		(kill-buffer buf)))
	  (setq number (1+ number))))))

  ;; Make us a fresh buffer.
  (generate-new-buffer "*Tar*"))
	

(defun dired-tar-operation-done (buf message)
  "Internal function for use by the dired-tar package.
This function is run when the tar operation completes.  It tries to
update the dired listing by looking at dired-tar-result."
  (cond
   ((null dired-tar-result))

   ((file-directory-p dired-tar-result)
    (save-excursion
      (mapcar
       (function (lambda (buf)
		   (set-buffer buf)
		   (dired-revert)))
       (dired-buffers-for-dir dired-tar-result))))

   ((file-exists-p dired-tar-result)
    (dired-relist-file dired-tar-result))

   ;; Otherwise, I guess the tar operation must have failed somehow.
   ))

(defun dired-tar-pack (directory prefix-arg)
  "Internal function for use by the dired-tar package.
Create a gzipped tar file from the contents of DIRECTORY.
The archive is named after the directory, and the files are stored in
the archive with names relative to DIRECTORY's parent.

For example, (dired-tar-pack \"/home/blandy/womble/\") would produce a
tar file named \"/home/blandy/womble.tar.gz\", whose contents had
names like \"womble/foo\", \"womble/bar\", etcetera.

The second argument PREFIX-ARG is ignored."
  (let* ((dir-file (directory-file-name directory))
	 (tar-file-name (format "%s.tar.gz" dir-file))
	 (parent-name (file-name-directory dir-file))
	 (content-name (file-name-nondirectory dir-file)))
    (dired-tar-run-command (if dired-tar-should-gzip
			       (format "tar cvf - %s | %s > %s"
				       content-name
				       dired-tar-gzip-command
				       tar-file-name)
			     (format "tar cvf %s %s"
				     content-name
				     tar-file-name))
			   parent-name
			   tar-file-name)))

(defun dired-tar-unpack (tar-file prefix-arg)
  "Internal function for use by the dired-tar package.
Unpack TAR-FILE into the directory containing it.
If PREFIX-ARG is non-nil, just list the archive's contents without
unpacking it."

  (let ((tar-file-dir (file-name-directory tar-file))
	(action (if prefix-arg "t" "x")))
    (dired-tar-run-command
     (cond

      ;; Does this look like a tar file at all?
      ((not (string-match "\\.tar\\(\\.z\\|\\.gz\\|\\.Z\\)?\\'" tar-file))
       (error
	"bug: dired-tar-unpack should only be passed tar file names."))

      ;; Does it look like a compressed tar file?
      ((match-beginning 1)
       (format "%s < %s | tar %svf -"
	       dired-tar-ungzip-command
	       tar-file
	       action))

      ;; Okay, then it must look like an uncompressed tar file.
      (t
       (format "tar %svf %s" action tar-file)))
     tar-file-dir

     ;; If we're just unpacking the archive, don't bother updating the
     ;; dired listing.
     (if prefix-arg nil tar-file-dir))))


;;;; User-visible functions.

;;;###autoload
(defun dired-tar-pack-unpack (prefix-arg)
  "Create or unpack a tar archive for the file on the current line.

If the file on the current line is a directory, make a gzipped tar
file out of its contents.

If the file on the current line is a tar archive, unpack it.  If the
archive appears to be gzipped or compressed, expand it first.  With a
prefix argument, just list the tar archive's contents, and don't
unpack it.  The file's name must end in \".tar\", \".tar.gz\", or
\".tar.Z\" or else this command will assume it's not a tar file."
  (interactive "P")

  (let ((filename (dired-get-filename)))
    (cond
     ((file-directory-p filename)
      (dired-tar-pack filename prefix-arg))

     ((string-match "\\.tar\\(\\.z\\|\\.gz\\|\\.Z\\)?\\'" filename)
      (dired-tar-unpack filename prefix-arg))

     (t
      (error "%s is neither a tar file nor a directory" filename)))))


;;;; Hooking this into dired mode.

;;;###autoload
(add-hook 'dired-mode-hook
	  (function
	   (lambda ()
	     (define-key dired-mode-map "T" 'dired-tar-pack-unpack))))	    


(provide 'dired-tar)

;;; dired-tar.el ends here
