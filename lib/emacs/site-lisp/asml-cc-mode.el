;;
;; File    : asml-cc-mode.el
;; Author  : TimT.
;; Purpose : ASML specific customizations of CC-Mode
;; Date    : 12-Nov-2001
;; 
;; Based on work by Erick Branderhorst. 
;;(setq-default c-basic-offset 3)
(setq c-auto-newline t)
;;(setq c-tab-always-indent 'other)

(fmakunbound 'c-mode)
(makunbound  'c-mode-map)
(fmakunbound 'c++-mode)
(makunbound  'c++-mode-map)
(makunbound  'c-style-alist)

(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'objc-mode "cc-mode" "Objective-C Editing Mode" t)
(autoload 'java-mode "cc-mode" "Java Editing Mode" t)

(setq auto-mode-alist
      (append
       '(("\\.C$"    . c++-mode)
	 ("\\.H$"    . c++-mode)
	 ("\\.cc$"   . c++-mode)
	 ("\\.hh$"   . c++-mode)
	 ("\\.c$"    . c-mode)
	 ("\\.h$"    . c-mode)
	 ("\\.m$"    . objc-mode)
	 ("\\.java$" . java-mode)
	 ) auto-mode-alist))

(defun my-c-mode-hook ()
  ;; use Ellemtel style for all C, C++, and Objective-C code
  (c-set-style "ellemtel")
  ;; other customizations can go here
  (c-set-offset 'case-label 0)
  (c-set-offset 'statement-case-intro c-basic-offset)
  (c-set-offset 'statement-case-open c-basic-offset)
  (setq c-cleanup-list '(defun-close-semi scope-operator))
  (setq tab-width 4
	;; this will make sure spaces are used instead of tabs
	indent-tabs-mode nil)
;; Emacs additions for Qt
  (define-key c++-mode-map [(f5)] 'kdab-insert-header)
  (define-key c++-mode-map [(shift f5)] 'kdab-insert-forward-declaration)
  (define-key c++-mode-map [(control f5)] 'kdab-lookup-qt-documentation)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)

;;(defun my-java-mode-hook ()
;; 
;;  (setq tab-width 4
;;	indent-tabs-mode nil)
;;  (setq c-hanging-braces-alist '((class-open after)
;;				 (defun-open after)
;;				 (inline-open after)
;;				 (defun-block-intro after)
;;				 )
;;	)
;;
;;)
;;(add-hook 'java-mode-hook 'my-java-mode-hook)
