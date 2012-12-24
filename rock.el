;; rock.el : extract a random quote from the database
;;
;; 
 
(random t)

(defvar rock-database-file "/home/utt/emacs/rock.db" 
  "Database of funny and not so funny quotes.")

(defconst rock-delimiter "\n"
  "Delimiter used to separate Database file entires")

(defvar rock-cache (make-vector 511 0)
  "Cache for the Rock database.")

(defun rock (database)
  "Return a random phrase from the database."
  (let ((sym (intern-soft database rock-cache))))
