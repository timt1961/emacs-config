;; log.el -- Functions to manage the log tool.
;; Copyright (c) 1993 Hare, Hatter and Dormouse
;;
;; 
(defvar log-hook nil
  "List of functions called after the log has been loaded")
;; where do we keep our log.
(defvar logdir (getenv("LOGDIR")))

(defun mylogname()
  "This function determines the name of the file in which to store the log."
  (let*
      (
       (date (current-time-string))
;;       (garbage (string-match
;;		 " \\([A-Z][a-z][a-z]\\) *\\([0-9]*\\) .* \\([0-9]*\\)$"
;;		 date))
;;       (day (string-to-int
;;	     (substring date (match-beginning 2) (match-end 2))))
       
       (setq month
	     '(substring date (match-beginning 1) (match-end 1)))
;;       (year
;;	(string-to-int ( substring( data (match-beginning 3) match-end 3))))
       (log (concat day year))
       )
    )
  )
