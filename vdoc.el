(defun vdoc (docname)
"Find an ASM Software document from directory /pas5500/doc
and switch to VDOC-mode"
  (interactive 
    (list 
     (expand-file-name (read-file-name "Vdoc file: " "/pas5500/doc/" nil t))))
  (switch-to-buffer (find-file-noselect docname))
  (vdoc-mode))

(global-set-key "\C-xv"  (quote vdoc-mode))

(defvar vdoc-mode-abbrev-table nil
  "Abbrev table used while in vdoc mode.")
(define-abbrev-table 'vdoc-mode-abbrev-table ())

(defvar vdoc-mode-map nil "")
(if vdoc-mode-map
    ()
  (setq vdoc-mode-map (make-sparse-keymap))
  (define-key vdoc-mode-map "g"  'goto-page)
  (define-key vdoc-mode-map "f"  'first-page)
  (define-key vdoc-mode-map "l"  'last-page)
  (define-key vdoc-mode-map "t"  'table-of-contents)
  (define-key vdoc-mode-map "n"  'next-page)
  (define-key vdoc-mode-map " "  'next-page)
  (define-key vdoc-mode-map "" 'next-page)
  (define-key vdoc-mode-map "p"  'previous-page))

(defun vdoc-mode ()
  "Major mode for viewing documents.
   
   Buffers in vdoc-mode are set to read-only mode.

   Key definitions :
    SPC, RET, n : next-page
              p : previous-page
              g : goto-page
              f : first-page
              l : last-page
              t : goto table-of-contents"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (use-local-map vdoc-mode-map)
  (setq mode-name "VDOC")
  (setq major-mode 'vdoc-mode)
  (setq local-abbrev-table vdoc-mode-abbrev-table)
  (run-hooks 'vdoc-mode-hook)
  (beginning-of-buffer)
  (recenter 0))

(defun next-page ()
  (interactive "")
  (if (not (search-forward "" (point-max) t))
      (message "End of Document")
    (next-line 1)
    (beginning-of-line)
    (recenter 0)))

(defun previous-page ()
   (interactive "")
   (beginning-of-line)
   (search-backward "" (point-min) t)
   (if (not (search-backward "" (point-min) t))
       (progn (beginning-of-buffer)
              (recenter 0)
              (message "Beginning of document"))
       (next-line 1)
       (recenter 0)))

(defun goto-page (page)
  (interactive "NGoto page : ")
  (setq possav (point))
  (beginning-of-buffer)
  (setq searchstr (concat "Page *: *" page))
  (if (not (re-search-forward searchstr (point-max) t))
      (progn (message "Page not found")
             (goto-char possav))
    (if (not (search-backward "" (point-min) t))
	(progn (beginning-of-buffer)
	       (recenter 0))
      (next-line 1)
      (recenter 0))))

(defun table-of-contents ()
  (interactive "")
  (setq possav (point))
  (beginning-of-buffer)
  (setq searchstr "TABLE OF CONTENTS")
  (if (not (search-forward searchstr (point-max) t))
      (progn (message "Table of contents not found")
             (goto-char possav))
    (if (not (search-backward "" (point-min) t))
	(progn (beginning-of-buffer)
	       (recenter 0))
      (next-line 1)
      (recenter 0))))

(defun first-page ()
  (interactive "")
  (beginning-of-buffer)
  (recenter 0)
  (message "Beginning of document"))

(defun last-page ()
  (interactive "")
  (end-of-buffer)
  (search-backward "" (point-min) t)
  (message "End of document")
  (next-line 1)
  (recenter 0))
