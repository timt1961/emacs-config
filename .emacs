(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages (quote (ansible go-mode python org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
