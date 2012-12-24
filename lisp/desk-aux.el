;;;; desk-aux.el - Auxiliary functions for use with desktop

;; This file contains enhancement code for the Emacs desktop facility
;; (desktop.el) which permits you to:
;; 
;;   - Save arbitrary buffer-local variables, such as the mark ring and state,
;;     to the desktop file
;; 
;;   - Specify regexps for additional file-visiting buffers, such as
;;     tags and other automatically-loaded data files, to omit from the desktop
;; 
;;   - Via a separate interface function, kill immediately any file-visiting
;;     buffers which will not be saved to the desktop
;; 
;; To use these enhancements, place the following in your .emacs file:
;;    (eval-after-load "desktop" "desk-aux")
;;
;; To customize, modify variables `desktop-locals-to-save' and
;; `desktop-data-buffers'.
;;
;; (To use the desktop facility, place these lines as close as possible to
;;  the end of your .emacs file:
;;     (require 'desktop)
;;     (desktop-load-default)
;;     (desktop-read)
;;  Activate the desktop with desktop-save; deactivate with desktop-remove.
;;  See lisp/desktop.el for more instructions.)
;;
;; Author:  Bill Brodie <wbrodie@panix.com>
;; Last modification (1.8):  Tue Apr  4 00:35:26 1995

(require 'desktop)

;;;; Save my favorite locals along with FSF's

(defvar desktop-locals nil
  "Association list of local variables to save on desktop.
Of the form ((buffer (var . value) ...) ...).")

(defvar desktop-locals-to-save 
  '((mark-active identity identity)
    (mark-ring
        (lambda (ring) (mapcar 'marker-position ring))
        (lambda (ring)
          (mapcar (function (lambda (pos)
                              (set-marker (make-marker) pos)))
                  ring))))
  "Association list of additional local variables to save on desktop.
Each entry contains three values:
   NAME  of the local variable
   EXTRACT-FN    function to call to extract value to be saved
   RESTORE-FN    function to call to obtain value to be restored")

(defun desktop-buffers-to-be-saved ()
  "Buffers which will be saved on desktop."
  (let ((buffers (buffer-list))
        (buffers-to-save '()))
    (while buffers
      (if (desktop-save-buffer-p
              (buffer-file-name (car buffers))
              (buffer-name (car buffers))
              (cdr (assoc 'major-mode (buffer-local-variables (car buffers)))))
          (setq buffers-to-save (cons (car buffers) buffers-to-save)))
      (setq buffers (cdr buffers)))
    (nreverse buffers-to-save)))

(defun desktop-save-locals ()
  "Save local variables to DESKTOP-LOCALS.
Variables to save are specified in variable DESKTOP-LOCALS-TO-SAVE."
  (setq desktop-locals
        (mapcar
         (function
          (lambda (buffer)
            (let ((locals (buffer-local-variables buffer)))
              (cons (buffer-name buffer)
                    (mapcar (function
                             (lambda (entry)
                               (let ((sym (nth 0 entry))
                                     (local-binding nil))
                                 (and (setq local-binding (assoc sym locals))
                                      (cons sym
                                            (funcall
                                             (nth 1 entry)
                                             (cdr local-binding)))))))
                            desktop-locals-to-save)))))
         (desktop-buffers-to-be-saved))))

(defun desktop-restore-locals ()
  "Restore local variables from desktop.  See variable DESKTOP-LOCALS."
  (mapcar
   (lambda (buffer-entry)
     (if (get-buffer (car buffer-entry))
         (progn
           (set-buffer (car buffer-entry))
           (mapcar (lambda (pair)
                     (and pair
                          (progn
                            (make-local-variable (car pair))
                            (set (car pair)
                                 (funcall
                                  (or (nth 2 (assoc (car pair)
                                                    desktop-locals-to-save))
                                      'identity)
                                  (cdr pair))))))
                   (cdr buffer-entry)))))
   desktop-locals))

;;; Put globals which record local-variable values on the desktop
(add-hook 'desktop-globals-to-save 'desktop-locals-to-save)
(add-hook 'desktop-globals-to-save 'desktop-locals)

;;; Before saving the desktop, record local-variable values.
(defadvice desktop-save (before desktop-save-advise-locals activate)
  (desktop-save-locals))

;;; After reading the desktop, restore saved local variables.
(defadvice desktop-read (after desktop-read-advise-locals activate)
  (desktop-restore-locals))

;;;; Other desktop helper functions and interfaces

;;; Avoid reloading tags unless perform a tags operation.
;;; Similarly, avoid reloading Emacs Lisp Archive data file from desktop.
(defvar desktop-tags-patched nil)
(defvar desktop-data-buffers '("tags" "lcd-data")
  "Names of additional buffers to omit from the desktop")
(if (not desktop-tags-patched)
    (progn
      (setq desktop-buffers-not-to-save
            (concat "\\(" desktop-buffers-not-to-save
                    "\\)\\|^\\("
                    (mapconcat 'identity desktop-data-buffers "\\|")
                    "\\)\\(<[0-9]+>\\|\\)$")
            desktop-tags-patched t)))

(defun clear-extra-buffers ()
  "Clear data-file buffers from desktop.
Kill file-visiting buffers which will not be saved to desktop file:
tags tables and the Emacs Lisp Archive data file."
  (interactive)
  (let ((buffers (buffer-list)))
    (while buffers
      (let ((buffer (car buffers)))
        (if (and (buffer-file-name buffer)
                 (string-match desktop-buffers-not-to-save
                               (buffer-name buffer)))
            (kill-buffer buffer))
        (setq buffers (cdr buffers))))))

;;; desk-aux.el ends here



