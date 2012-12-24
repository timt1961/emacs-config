;;
;; Rock.el : a yow hack to access the database of Rocky Frisco's current 
;;           tag lines.
;;
;; 
;;(require 'cookie1)

;; Randomize the random number generator
(random t)

(defconst cookie-delimiter "\n"
  "Delimiter used to separate cookie file entries.")

(require 'cookie1)
(defconst rock-file "/home/utt/emacs/rock.db"
   "Pertinent pinhead phrases.")

;;;###autoload
(defun rock (&optional interactive)
  "Return or display a random tagline for inclusion in your .sig"
  (interactive "p")
  (let ((yow (cookie
	      rock-file "Am I CONSING yet?..." "I have SEEN the CONSING!!")))
    (cond ((not interactive)
	   yow)
	  ((not (string-match "\n" yow))
	   (delete-windows-on (get-buffer-create "*Help*"))
	   (message "%s" yow))
	  (t
	   (message "Yow!")
	   (with-output-to-temp-buffer "*Help*"
	     (princ yow))))))

(defsubst read-zippyism (prompt &optional require-match)
  "Read a Zippyism from the minibuffer with completion, prompting with PROMPT.
If optional second arg is non-nil, require input to match a completion."
  (read-cookie prompt yow-file
	       "Am I CONSING yet?..." "I have SEEN the CONSING!!"
	       require-match))



