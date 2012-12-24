;;; uniquify-simple.el  Simplified uniquify -- 

;; Young-il Choo 1989-10-29  choo-young-il@yale.edu

;; A simplification of someone's uniquify.el so that we only add enough of
;; the path to the current name to make the buffer name unique, and we do not
;; modify other buffer names.

;; Example: When two files named ".../x/file" and ".../y/file" are opened in
;; sequence, the buffers will be name "file" and "y/file" respectively.

(defun yic-uniquify (filename)
  "Returns a unique buffer name for filename."
  (let ((lastname (file-name-nondirectory filename))
        (buffer-name-list (mapcar 'buffer-name (buffer-list)))
        (dir-list (reverse (yic-path-list filename))))
    (if (string= lastname "") (setq lastname filename))
    (while (and (yic-mem-string-equal lastname buffer-name-list)
                (not (null dir-list)))
      (setq lastname (concat (car dir-list) lastname))
      (setq dir-list (cdr dir-list)))
    lastname))

(defun yic-mem-string-equal (string list)
  "Predicate for testing membership of STRING in LIST, by chasing the tail."
  (if list                              ; list is not nil
      (if (string= string (car list)) t
        (yic-mem-string-equal string (cdr list)))
     nil))

(defun yic-path-list (filename)
  "Takes a full path name and returns a list of the subdirectories."
  (let* ((beg (string-match "[^/]*/" filename))
         (end (match-end 0)))
    (if beg                             ; there are subdirectories
        (cons (substring filename beg end)
              (yic-path-list (substring filename end nil)))
      nil)))

(defun create-file-buffer (filename)    ;from files.el
  "Creates a suitably named buffer for visiting FILENAME, adding parts of the 
directory if necessary to make the name unique.."
  (generate-new-buffer (yic-uniquify filename)))

;;;;--
;;;;--  Young-il Choo
;;;;
;;;;    Yale University  Computer Science  New Haven  CT 06520-2158  USA
;;;;    choo-young-il@cs.yale.edu  1-203-432-6828

