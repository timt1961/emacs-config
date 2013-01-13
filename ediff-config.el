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
