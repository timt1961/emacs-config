;;
;; File        : emacs19.old.el
;; Description : classic (emacstool) macro definitions for emacs
;; Author      : Michiel Kupers/ TimT.
;;
;; 
(setq load-path (parse-colon-path "/home/utt/emacs/lisp:/swuser/defaults/emacs:/swuser/local/src/emacs-19.21/lisp"))
(load-file "/swuser/local/src/emacs-19.21/lisp/term/sun.el")
;; r7 = f27 = Home
(fset 'top-of-screen
   "0�move-to-window-line
")

(fset 'bottom-of-screen
   "-1�move-to-window-line
")

(fset 'sun-mouse-handler
   (lambda (&optional hit) "Evaluates the function or list associated with a mouse hit.
Expecting to read a hit, which is a list: (button x y delta).  
A form bound to button by define-mouse is found by mouse-lookup. 
The variables: *mouse-window*, *mouse-x*, *mouse-y* are bound.  
If the form is a symbol (symbolp), it is funcall'ed with *mouse-window*,
*mouse-x*, and *mouse-y* as arguments; if the form is a list (listp),
the form is eval'ed; if the form is neither of these, it is an error.
Returns nil." (interactive) (byte-code "̈?� � ����8�8\"�	8�	8�	8�	\"pӎ�
!q��!))?�V ���8\"!??�S ����!!\"�� 9�l �	��
$�� <�� @�	��!�� ��\"),)�	�=�� �	�̇" [hit loc *mouse-window* *mouse-x* *mouse-y* mouse-code form StartBuffer sm::UpBits this-command t last-command nil sm::combined-hits sm::window-xy 1 2 0 mouse-event-code ((byte-code "q�" [StartBuffer] 1)) window-buffer mouse-lookup zerop logand error "Undefined mouse event: %s" prin1-to-string mouse-code-to-mouse-list funcall eval "Mouse action must be symbol or list, but was: %s" sun-mouse-handler] 15)))

(fset 'sun-mouse-once
   (lambda nil "Converts to emacstool and sun-mouse-handler on first mouse hit." (interactive) (emacstool-init) (sun-mouse-handler)))

(fset 'sun-select-region
   (lambda (beg end) "Set the sunwindows selection to the region in the current buffer." (interactive "r") (byte-code "��	\"!�" [beg end nil sun-set-selection buffer-substring] 4)))

;;(fset 'sun-set-selection
;;   <subr sun-set-selection>)

(fset 'sun-yank-selection
   (lambda nil "Set mark and yank the contents of the current sunwindows selection
into the current buffer at point." (interactive "*") (byte-code "����!��� !�" [nil set-mark-command insert-string sun-get-selection] 4)))

(fset 'justify-region
   "2�fill-region
")

(global-set-key [f27] 'top-of-screen)
;; r13 = f33 = End
(global-set-key [f33] 'bottom-of-screen)
;; c-left, c-right, shift-left, shift-right; move by words
(global-set-key [C-left] 'backward-word)
(global-set-key [S-f30] 'backward-word)
(global-set-key [C-right] 'forward-word)
(global-set-key [S-f32] 'forward-word)
;; r9 = f29 = PgUp 
(global-set-key [f29] 'scroll-down)
;; r15 = f35 = PgDn
(global-set-key [f35] 'scroll-up)
(global-set-key [f1] 'rmail)
(set-default-font "fixed")
;; L4 = f14 = UNDO
(global-set-key [f14] 'undo)
;; r11 = f31 = num 5
(global-set-key [f31] 'recenter)
;;
;; Not having this gets irritating after a while
(global-set-key [S-delete] 'backward-delete-char-untabify)
;; ESC-space : set this to set-mark
(global-set-key "\e " 'set-mark-command) 
;;
;; Define the standard asm key map
;; Note : I use alt : Alt: meta is warp under olvwm
;;
;; F2
(global-set-key [f2] 'isearch-forward)
(global-set-key [S-f2] 'isearch-backward)
(global-set-key [C-f2] 'replace-string)
(global-set-key [A-f2] 'query-replace)   
;;
;; F3
(global-set-key [f3] 'overwrite-mode)
(global-set-key [S-f3] 'downcase-word)
(global-set-key [C-f3] 'upcase-word)
(global-set-key [A-f3] 'fundamental-mode)
;; 
;; F4
(global-set-key [f4] 'spell-buffer)
(global-set-key [S-f4] 'spell-region)
;;
;; F5
(global-set-key [f5] 'other-window)
(global-set-key [S-f5] 'split-window)
(global-set-key [C-f5] 'delete-window)
;;
;; F6
(global-set-key [f6] 'shrink-window)
(global-set-key [S-f6] 'enlarge-window)
(global-set-key [C-f6] 'enlarge-horizontal)
(global-set-key [A-f6] 'shrink-horizontal)
;;
;; F7
(global-set-key [f7] 'find-file)
(global-set-key [S-f7] 'view-file)
(global-set-key [C-f7] 'insert-file)
;;
;; F8
(global-set-key [f8] 'indent-new-comment-line)
(global-set-key [S-f8] 'indent-for-comment)
(global-set-key [C-f8] 'indent-region)
;;
;; F9
(global-set-key [f9] 'justify-region)
(global-set-key [S-f9] 'fill-paragraph)
(global-set-key [C-f9] 'set-fill-column)
;;
;; F10
(global-set-key [f10] 'call-last-kbd-macro)
(global-set-key [S-f10] 'start-kbd-macro)
(global-set-key [C-f10] 'end-kbd-macro)
(global-set-key [A-f10] 'name-last-kbd-macro)
;;
;; F11
(global-set-key [f11] 'apropos)
(global-set-key [S-f11] 'describe-bindings)
(global-set-key [C-f11] 'info)
(global-set-key [A-f11] 'where-is)
;;
;; F12
(global-set-key [f12] 'save-buffer)
(global-set-key [S-f12] 'compile)
(global-set-key [C-f12] 'compile)   ;; Won't work. Sorry
(global-set-key [A-f12] 'next-error)
;;
;;
;; Make the window title correspond to the current buffer
(defun set-frame-title ()
  "Set title to current's buffer \\[buffer-file-name] name
or to \\[buffer-name if it has no file"
  (let ((name (format "%s"
		      (buffer-name (current-buffer)))))
    (modify-frame-parameters (selected-frame)
			     (list (cons 'name name)))))

(setq post-command-hook (cons 'set-frame-title post-command-hook))
;;
;; Turn accents and stuff on
(standard-display-european t)
;;
;; Left side of the keyboard
(global-set-key [f16] 'sun-select-region)
(global-set-key [f18] 'sun-yank-selection)
(global-set-key [f20] 'kill-region-and-unmark)
(global-set-key [f19] 'research-forward)
;;
;; Right side of keyboard
(global-set-key [S-f35] 'forward-page)
(global-set-key [S-f29] 'backward-page)

(global-set-key [f18] 'sun-yank-selection)
