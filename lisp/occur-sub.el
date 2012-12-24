;;; occur-sub.el - sub-select/reject in occurs buffer
(defun occur-copy-to-sub (cnct regexp)
  "Copy current buffer (must be in `occur-mode') to `*Occur-sub*'
and position on 2nd line (1st occurence line)"
       (or (eq major-mode 'occur-mode)
           (error "Buffer must be in occur mode"))
       (let ((cbf (buffer-substring (point-min) (point-max)))  
             (nbf (get-buffer-create "*Occur-sub*"))
             (str " in buffer ")
              buffer-read-only)
           (occur-copy-local-vars nbf)
           (switch-to-buffer nbf)
           (delete-region (point-min) (point-max))
           (insert cbf)
           (goto-char (point-min))
           (forward-line 1)
           (and (search-backward str nil t)
                (replace-match (concat " " cnct " \""
                                       (regexp-quote regexp)
                                       "\"" str) 'FIX))
           (forward-line 1)))


(defun occur-copy-local-vars (NEW-BUF)
  "Copy all current buffer local vars to NEW-BUF killing all its previuos
local vars (if any). Use `get-buffer-create' to ensure its existence."
       (let ((blv (buffer-local-variables))
             (lmp (current-local-map))
             v-nm v-va)                        ;var name, var-value
           (set-buffer NEW-BUF)
           (kill-all-local-variables)
           (use-local-map lmp)
           (while blv
               (setq v-va (car blv))
               (setq v-nm (car v-va))
               (setq v-va (cdr-safe v-va))
               (condition-case ()
                   (set (make-local-variable v-nm) v-va)
                   (error nil))
               (setq blv (cdr blv)))))


(defun occur-sub-select (regexp)
  "Sub-select occurence from an `occurs' buffer by using `keep-lines'"
       (interactive "sKeep Occurs matching regexp: ")
       (occur-copy-to-sub "and" regexp)
       (let (buffer-read-only)
           (keep-lines regexp)))


(defun occur-sub-reject (regexp)
  "Sub-select occurence from an `occurs' buffer by using `flush-lines'"    
       (interactive "sKeep Occurs matching regexp: ")
       (occur-copy-to-sub "and not" regexp)
       (let (buffer-read-only)
           (flush-lines regexp)))


(define-key occur-mode-map "r" 'occur-sub-reject)  ;reject occurs lines by `r'
(define-key occur-mode-map "s" 'occur-sub-select)  ;select occurs lines by `s'

;;; occur-sub.el ends here
