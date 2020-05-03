
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (elpy flycheck-pycheckers flymake-python-pyflakes format-all company-org-roam company org-roam org-journal deft ansible go-mode python org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(eval-when-compile (require 'use-package))
(require 'bind-key)

(use-package org
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 )
  :custom
  (org-agenda-files (list "~/stack/Plans"))
  (org-agenda-include-diary t)
  (org-log-done t)
  :ensure t
  )
(require 'org)

(use-package deft
  :after org
  :bind
   ("<f7>" . deft)
  :init
   (setq deft-file-naming-rules
      '((noslash . "-")
        (nospace . "-")
         (case-fn . downcase)))
   :custom
   (deft-recursive t)
   (deft-use-filename-as-title nil)
   (deft-use-filter-string-for-filename t)
   (deft-extensions '("org" "txt" "md"))
   (deft-default-extension "org")
   (deft-directory (expand-file-name "~/stack/Plans/"))
   :ensure t
   )
(require 'deft)

;; org-roam
(use-package org-roam
  :after deft org
  :bind (("C-c r t" . org-roam-today)
          :map org-mode-map
          (("<C-f7>" . org-roam)
          ("C-c r u" . org-roam-update-buffer)
          ("C-c r l" . org-roam-get-linked-files)
          ("C-c r i" . org-roam-insert)))
  :custom
  (org-roam-directory (expand-file-name "~/stack/Plans/"))
  :ensure t
  )
(require 'org-roam)

;; org-journal config
(use-package org-journal
  :after deft org
  :custom
  (org-journal-dir (expand-file-name "~/stack/Plans/"))
  (org-journal-file-type "weekly")
  (org-journal-date-format "%d-%b-%Y")
  :ensure t
  )
(require 'org-journal)


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
;; Ediff - Configuration
;; Following customizes ediff the way I like it.
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

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-use-last-dir t)

(setq inhibit-splash-screen t)
;; turn on time and mail flag in the modeline
(setq display-time-24hr-format t)
(display-time)





