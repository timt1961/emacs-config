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
