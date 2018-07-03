;;
;; .emacs: emacs startup
;; 18-Aug-2009 Creation
;; 25-Dec-2012 Updates
;;
;;
;;----------------------------------------------------------------------
;; Collect probably dead code here.. may want it for solaris emacs later
;;
;; Speed up startup
;; No noticable effect on my linux laptop
;; (modify-frame-parameters nil '((wait-for-wm . nil)))
;; Don't load the default library
;;
;;(setq inhibit-default-init t)
;;
;; fontlock mode seems to be automatic now
;; Turn on font-lock for high lighting
;;(require 'font-lock)
;; and turn font-lock on for all buffers
;;(global-font-lock-mode 1 )

;; Original version.
;; Found in a post by Shyamal Prasad (exushml@s10b06.exu.ericsson.se)
;; If on a parenthesis, typing %  takes you to the matching parenthesis.
;; Otherwise, it just inserts a %.
;; I ext
;;(defun match-paren (arg)
;;"Go to the matching parenthesis if on parenthesis otherwise insert %."
;;      (interactive "p")
;;      (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;	    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;	    ((looking-at "\\s\{") (forward-list 1) (backward-char 1))
;;	    ((looking-at "\\s\}") (forward-char 1) (backward-list 1))
;;	    ((looking-at "\\s\[") (forward-list 1) (backward-char 1))
;;	    ((looking-at "\\s\]") (forward-char 1) (backward-list 1))
;;	    (t (self-insert-command (or arg 1)))))

;; End dead code
;;-----------------------------------------------------------------------
;;
;; Expand tabs to spaces when using indent-region
(setq-default indent-tabs-mode nil)

;; Eliminate splash screen
(setq inhibit-splash-screen t)
;;
;; Allow emacs to operate on regions only
(put 'narrow-to-region 'disabled nil)
;;
;; Extend the load path with my personal libs
(add-to-list 'load-path "~/emacs/lisp")
(add-to-list 'load-path "~/emacs/lib/emacs/site-lisp")
;;
;; Load my standard keys
(load-file "~/emacs/lib/emacs/site-lisp/asml-keys.el")
;; ediff configuration
(load-file "~/emacs/ediff-config.el")
;;
;; turn on time and mail flag in the modeline
(display-time)
(setq display-time-24hr-format t)


(global-set-key "%" 'goto-match-paren)
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis AND last command is a movement command, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (message "%s" last-command)
  (if (not (memq last-command '(
                                set-mark
                                cua-set-mark
                                goto-match-paren
                                down-list
                                up-list
                                end-of-defun
                                beginning-of-defun
                                backward-sexp
                                forward-sexp
                                backward-up-list
                                forward-paragraph
                                backward-paragraph
                                end-of-buffer
                                beginning-of-buffer
                                backward-word
                                forward-word
                                mwheel-scroll
                                backward-word
                                forward-word
                                mouse-start-secondary
                                mouse-yank-secondary
                                mouse-secondary-save-then-kill
                                move-end-of-line
                                move-beginning-of-line
                                backward-char
                                forward-char
                                scroll-up
                                scroll-down
                                scroll-left
                                scroll-right
                                mouse-set-point
                                next-buffer
                                previous-buffer
                                )
                 ))
      (self-insert-command (or arg 1))
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1))))))


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

;;
;; tramp mode
(require 'tramp)

;; Orgmode
;; Following based on tutorial 
;; http://orgmode.org/worg/org-tutorials/orgtutorial_dto.php
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;
;; python tooling
;;(load-file "~/emacs/python/emacs-for-python/epy-init.el")
;;(epy-setup-checker "epylint %f")
;;(epy-django-snippets)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes (quote ("dd43c9f997208c61ce0f4855932cc20a57ae2f37fe2ced218dace5c8c321d1e8" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
