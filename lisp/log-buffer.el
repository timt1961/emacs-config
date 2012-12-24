;
;log the time spent typing to a particular buffer 
;grabbed from a message in one of the emacs groups.
;
(defvar log-buffer-name (buffer-name)
  "Name of current buffer at end of last command")
(defvar log-last-command-time (current-time)
  "Time at which last command took place")
(defvar log-current-time-block 0
  "Accumulated unlogged unidle time")
(defvar log-unidle nil
  "Is user at terminal?")

(defvar log-idle-threshold 180
  "in seconds")

(defvar log-stat-file "~/emacs.stats"
  "File in which usage statistics are maintained")

(defvar log-stats nil
  "association list (buffername . time-spent)")

(defun time-difference (a b)
  (let ((hi (- (car a) (car b)))
	(lo (- (car (cdr a)) (car (cdr b)))))
    (+ (lsh hi 16) lo)))


(defun log-stamp-time ()
  (let* ((ct (current-time))
	 (d (time-difference ct log-last-command-time)))
    (if (setq log-unidle (< d log-idle-threshold))
	(setq log-current-time-block (+ d log-current-time-block)))
    (setq log-last-command-time ct)))



;; Record buffer name and time entered, if new
(defun log-hook ()
  (log-stamp-time)
  (if (not (and log-unidle (eql (buffer-name) log-buffer-name)))
      (log-record))
  (setq log-buffer-name (buffer-name)))

(defun log-record ()
  ;; Switching out of log-buffer -- do bookkeeping for it
  (let ((entry (assoc log-buffer-name log-stats)))
    (if (not entry)
	(setq log-stats (cons (setq entry (cons log-buffer-name 0))
			      log-stats)))
    (setcdr entry
        (+ (cdr entry) log-current-time-block))
    (setq log-current-time-block 0)))


(defun log-write ()
  (log-stamp-time)
  (log-record)
  (find-file log-stat-file)
  (let ((old-log (and (> (buffer-size) 0)
		      (car (read-from-string (buffer-string))))))
    (erase-buffer)
    (insert (format "(\n%s \n)"
		    (mapconcat 	     
		     (function (lambda (x) (format "%S" x)))
		     (sort
		      (log-merge-logs old-log log-stats)
		      (function (lambda (a b) (> (cdr a) (cdr b)))))
		     "\n")))
    (save-buffer)))
		     

(defun log-merge-logs (old-list new-list)
  (mapcar (function (lambda (new-rec) 
		      (and (> (cdr new-rec) 60) 
			   (setcdr new-rec (/ (cdr new-rec) 60))
			   (let ((old-rec (assoc (car new-rec) old-list)))
			     (if old-rec 
				 (setcdr old-rec (+ (cdr old-rec) 
						    (cdr new-rec)))
			       (setq old-list (cons new-rec old-list)))))))
	  new-list)
   old-list)
			     

(add-hook 'post-command-hook 'log-hook)
(add-hook 'kill-emacs-hook 'log-write)


