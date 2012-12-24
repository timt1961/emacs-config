;;
;; newdotemacs: replacement .emacs
;; 18-Aug-2009 Creation
;;
;; Speed up startup
(modify-frame-parameters nil '((wait-for-wm . nil)))
;; Don't load the default library
;;
(setq inhibit-default-init t)
;;
;; Expand tabs to spaces
(setq-default indent-tabs-mode nil)
;; indent-tabs-mode is buffer local
(setq indent-tabs-mode nil)
;; Eliminate splash screen
(setq inhibit-splash-screen t)
;;
;; Allow emacs to operate on regions only
(put 'narrow-to-region 'disabled nil)
;;
;; Extend the load path with my personal libs
(add-to-list 'load-path "~/emacs/lisp")
(add-to-list 'load-path "~/emacs/tramp-2.1.17")
(add-to-list 'load-path "~/emacs/lib/emacs/site-lisp")
(add-to-list 'load-path "~/emacs/lib/emacs/site-lisp/color-theme-6.6.0")
;;
;; Load my standard keys
(load-file "~/emacs/lib/emacs/site-lisp/asml-keys.el")
;;
;; turn on time and mail flag in the modeline
(display-time)
(setq display-time-24hr-format t)
;;
;; Turn on font-lock for high lighting
(require 'font-lock)
;; and turn font-lock on for all buffers
(global-font-lock-mode 1 )

;; Found in a post by Shyamal Prasad (exushml@s10b06.exu.ericsson.se)
;; If on a parenthesis, typing %  takes you to the matching parenthesis.
;; Otherwise, it just inserts a %.
;; I ext
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
"Go to the matching parenthesis if on parenthesis otherwise insert %."
      (interactive "p")
      (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	    ((looking-at "\\s\{") (forward-list 1) (backward-char 1))
	    ((looking-at "\\s\}") (forward-char 1) (backward-list 1))
	    ((looking-at "\\s\[") (forward-list 1) (backward-char 1))
	    ((looking-at "\\s\]") (forward-char 1) (backward-list 1))
	    (t (self-insert-command (or arg 1)))))

;; Ediff configuration.
;; Needs review
(autoload 'ediff-buffers "ediff" "Visual interface to diff" t)
(autoload 'ediff  "ediff"  "Visual interface to diff" t)
(autoload 'ediff-files "ediff" "Visual interface to diff" t)
(autoload 'ediff-windows "ediff" "Visual interface to diff" t)
(autoload 'ediff-regions "ediff" "Visual interface to diff" t)
(autoload 'epatch  "ediff"  "Visual interface to patch" t)
(autoload 'ediff-patch-file "ediff" "Visual interface to patch" t)
(autoload 'ediff-patch-buffer "ediff" "Visual interface to patch" t)
(autoload 'epatch-buffer "ediff" "Visual interface to patch" t)
(autoload 'ediff-revision "ediff"
			"Interface to diff & version control" t)
;; Following customizes ediff the way I like it.
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-use-last-dir t)
(require 'vc)
(unless (fboundp 'make-temp-file)
  (defun make-temp-file (p)
    (make-temp-name (expand-file-name p temporary-file-directory))))
(defun vc-ediff (rev)
  (interactive "sVersion to diff (default is BASE): ")
  (vc-ensure-vc-buffer)
  (let* ((version (if (string-equal rev "")
		      (vc-latest-version buffer-file-name)
		    rev))
	 (filename (make-temp-file "vc-ediff")))
    (vc-backend-checkout buffer-file-name nil version filename)
    (let ((buf (find-file-noselect filename)))
      (ediff-buffers buf (current-buffer)))))
;;
;; Set up the print environment.
(setq lpr-command "lp")
;; ps-print: post script generator
(require 'ps-print)
(setq ps-paper-type 'a4)
;; Make sure the buffer name is unique, and reflects the location of its
;; contents
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on crypt++ mode: this also handles tar zip etc.
(require 'crypt++)

;; Time stamp functionality
(defun date ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%d-%b-%Y"))
)
(defun timestamp()
  "insert the current date/timestamp."
  (interactive)
  (insert (format-time-string "%d-%b-%YT%H:%M:%S"))
)

;;
;; Programming mode support
;; cwarn : highlight suspicious C/C++ constructs
(require 'cwarn)
(global-cwarn-mode 1)

;; Move between functions
(defun beginning-of-next-defun ()
  "Moves cursor to the beginning of the next defun."
  (interactive)
  (beginning-of-defun -1))

(global-set-key '[(meta up)] 'beginning-of-defun)
(global-set-key '[(meta down)] 'beginning-of-next-defun)
;;
;; Buffer cycling
(global-set-key "\M-z"  'bury-buffer)

;; Template code definitions.
(require 'template)
(setq template-default-directories `("/home/utt/templates"))
(template-initialize t)
(defun pad-comment ()
  "fill the current line with blanks to pos 72"
  (interactive)
  (setq padding  ( - 72 (current-column)))
  (insert-char ?\  padding))

(setq template-expansion-alist  '(("PAD" (insert-char ?\ (- 72 (current-column))))))
(require 'compile)
(setq compilation-error-regexp-alist
  (append (list
     ;; works for jikes
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
     ;; works for javac
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
  compilation-error-regexp-alist))
;;
;; Correctly parse ant output
;; Taken from ant faq
(require 'compile)
(setq compilation-error-regexp-alist
  (append (list
     ;; works for jikes
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
     ;; works for javac
     '("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2))
  compilation-error-regexp-alist))

;;
(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
	 (if buffer-read-only "black"
	   (if overwrite-mode "red"
	     "blue"))))
    (unless (and
	     (string= color hcz-set-cursor-color-color)
	     (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)
;;
;; Planner.el support: load paths
(add-to-list 'load-path "~/emacs/lib/emacs/site-lisp/muse-3.12/lisp")
(add-to-list 'load-path "~/emacs/lib/emacs/site-lisp/planner-3.42/")
(add-to-list 'load-path "~/emacs/lib/emacs/site-lisp/remember-2.0/")
;;
;; Planner.el : Set up remember
(require 'remember-planner)
    (setq remember-handler-functions '(remember-planner-append))
    (setq remember-annotation-functions planner-annotation-functions)

;; Planner set up from emacs wiki
(setq planner-project "WikiPlanner")
     (setq muse-project-alist
	   '(("WikiPlanner"
	      ("~/Plans"   ;; Or wherever you want your planner files to be
	       :default "index"
	       :major-mode planner-mode
	       :visit-link planner-visit-link))))

(require 'planner)
;;; Planner key bindings. (This is how I like them)
(global-set-key [f1] 'plan)
(global-set-key [f2] 'planner-create-task-from-buffer)
(global-set-key [S-f2] 'planner-task-done)
(global-set-key [C-f2] 'planner-delete-task)
(require 'remember-planner)
(global-set-key [f3] 'remember)

;;Old planner setup
;;(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)
;; Publishing Plans
(planner-option-customized 'planner-publishing-directory "/home/utt/WWW/Plans")
;;
;; diary mode
(european-calendar)
(setq calendar-week-start-day 1)
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
(setq diary-file "~/Plans/diary")

;; Appointments
(require 'planner-appt)
(planner-appt-use-tasks-and-schedule)
(planner-appt-insinuate)
;;
;; Temporary planner-diary code (move to final location later)
(require 'planner-diary)
(add-hook 'diary-display-hook 'fancy-diary-display)
(setq planner-diary-use-diary t)
(planner-diary-insinuate)
(setq planner-day-page-template
      "* Tasks\n\n\n* Schedule\n\n\n* Timeclock\n\n\n* Diary\n\n\n* Notes")

;; global task ids
(require 'planner-id)

;; planner reports
(require 'planner-report)

;;
;; Project as keywords
(require 'muse-wiki)    ;;; Allow wiki-links
(setq muse-wiki-allow-nonexistent-wikiword t)
;;
;; Auto fill on in text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; playing around with colors:
(require 'color-theme)
;; Following information received on emacs wiki
;; http://www.emacswiki.org/emacs/ColorTheme
(defun color-theme-face-attr-construct (face frame)
       (if (atom face)
	   (custom-face-attributes-get face frame)
	 (if (and (consp face) (eq (car face) 'quote))
	     (custom-face-attributes-get (cadr face) frame)
	   (custom-face-attributes-get (car face) frame))))

;;
;; After some playing around with color-theme-select:
;; (Whateveryou like is a nice theme too.. )
(setq color-theme-is-global t)
(color-theme-initialize)
;;(color-theme-high-contrast)
;;
;; jdee installation (Experimental)
;;(add-to-list 'load-path (expand-file-name "/scratch/emacs/jde-2.3.5.1/lisp"))
;;(add-to-list 'load-path (expand-file-name "/scratch/emacs/cedet-1.0pre6/common"))
;;(load-file (expand-file-name "/scratch/emacs/cedet-1.0pre6/common/cedet.el"))
;;(add-to-list 'load-path (expand-file-name "/scratch/emacs/elib-1.0"))
;; (require 'jde)
;;
;; tramp mode
(require 'tramp)

;; Orgmode
;; Following based on tutorial 
;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                ;;
;;  Source: http://irreal.org/blog/?p=371         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jcs-comment-box ()
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive)
  (with-region-or-buffer (b e)
    (save-restriction
      (narrow-to-region b e)
      (goto-char b)
      (end-of-line)
      (insert-char ?  (- fill-column (current-column)))
      (comment-box b (point-max) 1)
      (goto-char (point-max)))))

;;
;; set up flymake for python using pylint
;;(when (load "flymake" t)
;;  (defun flymake-pylint-init ()
;;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                       'flymake-create-temp-inplace))
;;           (local-file (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;      (list "epylint" (list local-file))))
  
;;  (add-to-list 'flymake-allowed-file-name-masks
;;               '("\\.py\\'" flymake-pylint-init)))
(load-file "/home/timt/emacs/python/epy-init.el")
(epy-setup-checker "epylint %f")
(epy-django-snippets)

