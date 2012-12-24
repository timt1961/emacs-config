--------------------------[format.el]-------------------------
;; format.el
;; Copyright 1991,1994 Ronald Florence <ron@mlfarm.com>
;;
;; modified for groff & postscript, 5 Feb 1991
;; pageview proofing, 10 Jul 1991
;; groff-1.06 changes, 25 Dec 1992
;; menu for emacs-19, 2 Nov 1994
;;
;; Provides simple commands for printing, proofing, and entering
;; special characters when formatting files with troff or groff in an
;; emacs buffer.  The formatting commands work asychronously.
;;
;; Installation: 
;;   The format strings are set up for groff and its various auxiliary
;;   programs, pageview as the screen proofer for Postscript, lpr as
;;   the print spooler, and a2ps to format ascii files into
;;   Postscript.  They can be changed here, in a site-wide defaults.el
;;   file, or in .emacs to suit your needs.  Format will be invoked
;;   automatically if you  use something like the following in .emacs
;;   or default.el:
;;
;;   (setq nroff-mode-hook '(lambda () (abbrev-mode 1) 
;;			 (auto-fill-mode 1)
;;			 (require 'format)
;;			 (setq mode-name "Troff" )))
;;
;;   If you are using emacs-19+, you will need an eight-bit font for the
;;   pop-up menus (Emacs.shell.menu.popup.font:) to display special
;;   characters.  The following works well for Sun OW3:
;;      *-lucida sans-medium-r-*-*-14-*-*-*-*-*-iso8859-1
;;   On X11R6, the default font will work, or you can try something like:
;;      *-lucida-medium-r-normal-sans-14-*-*-*-*-*-iso8859-1
;;
;; Commands:
;;   All of the formatting, font and macro change commands, and
;;   special character commands are available in the pop-up menus.
;;   There are also some short-cut keys for some functions:
;;
;;	(troff-buffer)		C-c C-t		format and print buffer
;;	(proof-buffer)		C-c C-p		format and proof buffer
;;						  (Postscript preview)
;;	(quick-proof-buffer)	C-c C-q		quick proof buffer
;;						  (groff -Tps -X)
;;	(aproof-buffer)				rough (ascii) proof
;;	(kill-proof)		C-C C-k		kills proof process
;;	(kill-print)		C-c C-i		kills print process
;;	(te-buffer)				format (pic,eqn,tbl) 
;;						  and print
;;	(te-proof-buffer)			format (pic,eqn,tbl)
;;						  and preview Postscript
;;	(pr-buffer)				print with a2ps
;;	(pr2-buffer)				print landscape with a2ps
;;
;;   There are -region versions of the proof and print commands.  The
;;   print commands accept a prefix argument as the number of copies
;;   to print.
;;

(require 'easymenu)
(provide 'format)

(defvar roff-macro "-mm"
  "*Default macro package to use with troff.")

(defvar roff-options "-rN2"
  "*Default options for troff.")

(defvar roff-font ""
  "*Default font for troff.")

;; The roff-macro, roff-options, roff-font, temporary-file-name, and
;; number of copies are substituted into the various format strings.

(defvar troff-format-string "gtroff -mps %s %s %s %s | grops -c%d | lpr"
  "*Command string to format with troff for the printer.")

(defvar proof-string "gtroff -mps %s %s %s %s | grops | pageview -"
  "*Command string to format with troff for the Postscript screen previewer.")

(defvar quick-proof-string "groff -Tps -X %s %s %s %s"
  "*Command string for quick screen previews.")

(defvar qte-proof-string "groff -etp -Tps -X %s %s %s %s"
  "*Command string for quick screen previews with pic, tbl, and eqn.")

(defvar aproof-string "gtroff -a -Tps %s %s %s  -mps %s"
  "*Command string for rough (ascii) proofing with troff.")

;; The substitution order is changed for these two; the temporary
;; file-name is first, then the roff-options, roff-macro, and roff-font.

(defvar te-format-string "gpic %s | gtbl - | geqn | gtroff -mps %s %s %s - | grops -c%d | lpr"
  "*Command string to format with pic, tbl, eqn, and troff for the printer.")

(defvar te-proof-string "gpic %s | gtbl - | geqn | gtroff -mps %s %s %s - | grops | pageview -"
  "*Command string to format with pic, tbl, eqn, and troff for the Postscript
screen previewer.")

;; These two aren't really troff.  The substitutions are buffer-name
;; (subject), number of copies, temporary-file-name.

(defvar pr-format-string "a2ps -ns -p -S%s -#%d %s | lpr"
  "*Command string to format ascii into Postscript for the printer.")

(defvar pr2-format-string "a2ps -S%s -#%d %s | lpr"
  "*Command string to format ascii into Postscript (landscape, two-up)
for the printer.")

;; end of configuration

;; (or (not 'nroff-mode) (nroff-mode))

(progn
  (define-key nroff-mode-map "\C-c\C-t" 'troff-buffer)
  (define-key nroff-mode-map "\C-c\C-p" 'proof-buffer)
  (define-key nroff-mode-map "\C-c\C-q" 'quick-proof-buffer)
  (define-key nroff-mode-map "\C-c\C-k" 'kill-proof)
  (define-key nroff-mode-map "\C-c\C-i" 'kill-print))

(easy-menu-define nroff-mode-menu
    nroff-mode-map
    "Menu used in troff mode."
  (list "Troff"
	(list "Insert Char (a - i)"
	      ["`"	(insert "\\(`a") "\\('a"]
	      ["a"	(insert "\\('a") "\\('a"]
	      ["b"	(insert "\\(^a") "\\(^a"]
	      ["c"	(insert "\\(~a") "\\(~a"]
	      ["d"	(insert "\\(:a") "\\(:a"]
	      ["@"	(insert "\\(`A") "\\(`A"]
	      ["A"	(insert "\\('A") "\\('A"]
	      ["B"	(insert "\\(^A") "\\(^A"]
	      ["C"	(insert "\\(~A") "\\(~A"]
	      ["D"	(insert "\\(:A") "\\(:A"]
	      ["E"	(insert "\\(oA") "\\(oA"]
	      ["F"	(insert "\\(AE") "\\(AE"]
	      ["e"	(insert "\\(oa") "\\(oa"]
	      ["f"	(insert "\\(ae") "\\(ae"]
	      ["h"	(insert "\\(`e") "\\(`e"]
	      ["i"	(insert "\\('e") "\\('e"]
	      ["j"	(insert "\\(^e") "\\(^e"]
	      ["k"	(insert "\\(:e") "\\(:e"]
	      ["H"	(insert "\\(`E") "\\(`E"]
	      ["I"	(insert "\\('E") "\\('E"]
	      ["J"	(insert "\\(^E") "\\(^E"]
	      ["K"	(insert "\\(:E") "\\(:E"]
	      ["l"	(insert "\\(`i") "\\(`i"]
	      ["m"	(insert "\\('i") "\\('i"]
	      ["n"	(insert "\\(^i") "\\(^i"]
	      ["o"	(insert "\\(:i") "\\(:i"]
	      ["L"	(insert "\\(`I") "\\(`I"]
	      ["M"	(insert "\\('I") "\\('I"]
	      ["N"	(insert "\\(^I") "\\(^I"]
	      ["O"	(insert "\\(:I") "\\(:I"])
	(list "Insert Char (o - u)"
	      ["r"	(insert "\\(`o") "\\(`o"]
	      ["s"	(insert "\\('o") "\\('o"]
	      ["t"	(insert "\\(^o") "\\(^o"]
	      ["u"	(insert "\\(~o") "\\(~o"]
	      ["v"	(insert "\\(:o") "\\(:o"]
	      ["R"	(insert "\\(`O") "\\(`O"]
	      ["S"	(insert "\\('O") "\\('O"]
	      ["T"	(insert "\\(^O") "\\(^O"]
	      ["U"	(insert "\\(~O") "\\(~O"]
	      ["V"	(insert "\\(:O") "\\(:O"]
	      ["X"	(insert "\\(/O") "\\(/O"]
	      ["x"	(insert "\\(/o") "\\(/o"]
	      ["y"	(insert "\\(`u") "\\(`u"]
	      ["z"	(insert "\\('u") "\\('u"]
	      ["{"	(insert "\\(^u") "\\(^u"]
	      ["|"	(insert "\\(:u") "\\(:u"]
	      ["Y"	(insert "\\(`U") "\\(`U"]
	      ["Z"	(insert "\\('U") "\\('U"]
	      ["["	(insert "\\(^U") "\\(^U"]
	      ["\"	(insert "\\(:U") "\\(:U"])
	(list "Insert Char (specials)"
	      ["_"	(insert "\\(ss") "\\(ss"]
	      ["!"	(insert "\\(r!") "\\(r!"]
	      ["?"	(insert "\\(r?") "\\(r?"]
	      ["G"	(insert "\\(,C") "\\(,C"]
	      ["g"	(insert "\\(,c") "\\(,c"]
	      ["Q"	(insert "\\(~N") "\\(~N"]
	      ["q"	(insert "\\(~n") "\\(~n"]
	      ["""	(insert "\\(ct") "\\(ct"]
	      ["#"	(insert "\\(Po") "\\(Po"]
	      ["%"	(insert "\\(Ye") "\\(Ye"]
	      ["'"	(insert "\\(sc") "\\(sc"]
	      [")"	(insert "\\(co") "\\(co"]
	      ["."	(insert "\\(rg") "\\(rg"]
	      ["+"	(insert "\\(Fo") "\\(Fo"]
	      [";"	(insert "\\(Fc") "\\(Fc"]
	      ["6"	(insert "\\(ps") "\\(ps"]
	      ["0"	(insert "\\(de") "\\(de"]
	      ["<"	(insert "\\(14") "\\(14"]
	      ["="	(insert "\\(12") "\\(12"]
	      [">"	(insert "\\(34") "\\(34"]
	      ["1"	(insert "\\(+-") "\\(+-"]
	      ["w"	(insert "\\(di") "\\(di"])
	(list "Operations"
	      ["spell check" (ispell-buffer) t]
	      ["proof" (proof-buffer) t]
	      ["tbl-eqn-pic proof" (te-proof-buffer) t]
	      ["print" (troff-buffer) t]
	      ["tbl-eqn-pic print" (te-buffer) t]
	      ["ascii print" (pr-buffer) t]
	      ["landscape (ascii) print" (pr2-buffer) t]
	      ["quick proof" (quick-proof-buffer) t]
	      ["tbl-eqn-pic quick proof" (qte-proof-buffer) t]
	      ["rough (ascii) proof" (aproof-buffer) t]
	      ["kill print" (kill-print) t]
	      ["kill proof" (kill-proof) t])
	(list "Default Font"
	      ["Palatino" (setq roff-font "-fP") t]
	      ["New Century Schoolbook" (setq roff-font "-fN") t]
	      ["Times" (setq roff-font "-fT") t]
	      ["Avant-Garde" (setq roff-font "-fA") t]
	      ["Helvetica" (setq roff-font "-fH") t]
	      ["Courier" (setq roff-font "-fC") t]
	      ["Bookman" (setq roff-font "-fB") t]
	      ["Helvetica Narrow" (setq roff-font "-fHN") t]
	      ["Zapf Chancery" (setq roff-font "-fZCMI") t])
	(list "Macro Sets"
	      ["mm" (setq roff-macro "-mm") t]
	      ["ms" (setq roff-macro "-ms") t]
	      ["me" (setq roff-macro "-me") t]
	      ["man" (setq roff-macro "-man") t]
	      ["mgm" (setq roff-macro "-mgm") t]
	      ["doc" (setq roff-macro "-mdoc") t]
	      ["mn" (setq roff-macro "-mn") t])))


(setq proof-tmp-file nil 
      print-tmp-file nil
      proof-process nil 
      print-process nil
      proof-file nil)

(defun te-buffer (&optional copies)
  "Typeset buffer after formatting with pic, tbl, eqn, and troff.
Optional prefix argument specifies number of copies."
  (interactive "p")
  (format-to-printer-region (point-min) (point-max) "te" copies))

(defun te-region (start end &optional copies)
  "Typeset region after formatting with pic, tbl, eqn, and troff.
Optional prefix argument specifies number of copies."
  (interactive "r\np")
  (format-to-printer-region start end "te" copies))

(defun troff-buffer (&optional copies)
  "Typeset buffer after formatting with troff.
Optional prefix argument specifies number of copies."
  (interactive "p")
  (format-to-printer-region (point-min) (point-max) "troff" copies))

(defun troff-region (start end &optional copies)
  "Typeset region contents after formatting with troff.
Optional prefix argument specifies number of copies."
  (interactive "r\np")
  (format-to-printer-region start end "troff" copies))

(defun pr-buffer (&optional copies)
  "Print buffer contents in portrait mode.
Optional prefix argument specifies number of copies."
  (interactive "p")
  (format-to-printer-region (point-min) (point-max) "pr" copies))

(defun pr-region (start end &optional copies)
  "Print region contents in portrait mode.
Optional prefix argument specifies number of copies."
  (interactive "r\np")
  (format-to-printer-region start end "pr" copies))

(defun pr2-buffer (&optional copies)
  "Print buffer contents in two columns, landscape mode. 
Optional prefix argument specifies number of copies."
  (interactive "p")
  (format-to-printer-region (point-min) (point-max) "pr2" copies))

(defun pr2-region (start end &optional copies)
  "Print region contents in two columns, landscape mode.
Optional prefix argument specifies number of copies."
  (interactive "r\np")
  (format-to-printer-region start end "pr2" copies))

(defun proof-region (start end)
  "Proof region using troff & pageview."
  (interactive "r")
  (proof-region-to-buffer start end "troff"))

(defun proof-buffer ()
  "Proof buffer using troff & pageview."
  (interactive)
  (proof-region-to-buffer (point-min) (point-max) "troff"))

(defun quick-proof-region (start end)
  "Quick proof region using troff & xditview."
  (interactive "r")
  (proof-region-to-buffer start end "qroff"))

(defun quick-proof-buffer ()
  "Quick proof buffer using troff & xditview."
  (interactive)
  (proof-region-to-buffer (point-min) (point-max) "qroff"))

(defun aproof-region (start end)
  "Rough (ascii) proof region using troff."
  (interactive "r")
  (proof-region-to-buffer start end "ascii"))

(defun aproof-buffer ()
  "Rough (ascii) proof buffer using troff."
  (interactive)
  (proof-region-to-buffer (point-min) (point-max) "ascii"))

(defun te-proof-region (start end)
  "Proof region using troff, pic, eqn, tbl & pageview."
  (interactive "r")
  (proof-region-to-buffer start end "te-proof"))

(defun te-proof-buffer ()
  "Proof buffer using troff, pic, eqn, tbl & pageview."
  (interactive)
  (proof-region-to-buffer (point-min) (point-max) "te-proof"))

(defun qte-proof-region (start end)
  "Quick proof region using troff, pic, eqn, tbl & xditview."
  (interactive "r")
  (proof-region-to-buffer start end "te-proof"))

(defun qte-proof-buffer ()
  "Quick proof buffer using troff, pic, eqn, tbl & xditview."
  (interactive)
  (proof-region-to-buffer (point-min) (point-max) "qte-proof"))

(defun kill-print ()
  "Kill format-to-printer process."
  (interactive)
  (if print-process
      (interrupt-process print-process)))

(defun kill-proof ()
  "Kill proof process."
  (interactive)
  (if proof-process
      (interrupt-process proof-process)))

(defun format-to-printer-region (start end formatter &optional copies)
  (if print-process
      (if (or 
	   (not (eq (process-status print-process) 'run))
	   (yes-or-no-p "A format-to-printer process is running; kill it? "))
	  (condition-case ()
	      (let ((print-proc print-process))
		(interrupt-process print-proc)
		(sit-for 1)
		(delete-process print-proc))
	    (error nil))
	(error "One format-to-printer process at a time.")))
  (save-excursion
    (setq printer-output-buffer " *printer output*")
    (get-buffer-create printer-output-buffer)
    (set-buffer printer-output-buffer)
    (erase-buffer))
  (if (null copies) (setq copies 1))
  (setq print-tmp-file (concat "/tmp/" (make-temp-name "#pr#")))
  (write-region start end print-tmp-file nil 'nomsg)
  (setq print-command 
	(cond ((string= formatter "troff")
	       (format troff-format-string
		       roff-options roff-macro roff-font
		       print-tmp-file copies))
	      ((string= formatter "te")
	       (format te-format-string
		       print-tmp-file roff-options roff-macro
		       roff-font
		       copies))
	      ((string= formatter "pr")
	       (format pr-format-string (buffer-name)
		       copies print-tmp-file))
	      ((string= formatter "pr2")
	       (format pr2-format-string (buffer-name)
		       copies print-tmp-file))))
  (setq print-process
	(start-process formatter printer-output-buffer "sh" "-c"
		       print-command))
  (set-process-sentinel print-process 'print-sentinel))

(defun print-sentinel (process msg)
  (delete-file print-tmp-file)
  (save-excursion
    (set-buffer (process-buffer process))
    (if (> (buffer-size) 0)
	(progn
	  (goto-char (point-min))
	  (end-of-line)
	  (message "%s: %s" (process-name process) 
		   (buffer-substring 1 (point))))
      (if (string-match "^exited" msg)
	  (message "%s: killed" (process-name process))))
  (setq print-process nil)
  (kill-buffer (process-buffer process))))

(defvar proof-file)

(defun proof-region-to-buffer (start end formatter)
  (if proof-process
      (if (or (not (eq (process-status proof-process) 'run))
	      (yes-or-no-p "A proof process is running; kill it? "))
	  (condition-case ()
	      (let ((proof-proc proof-process))
		(interrupt-process proof-proc)
		(sit-for 1)
		(delete-process proof-proc))
	    (error nil))
	(error "One proof process at a time.")))
  (setq proof-tmp-file (concat "/tmp/" (make-temp-name "#p#")))
  (save-excursion
    (setq proof-file (buffer-name))
    (setq proof-buffer "*proof*")
    (get-buffer-create proof-buffer)
    (set-buffer proof-buffer)
    (erase-buffer))
  (write-region start end proof-tmp-file nil 'nomsg)
  (setq proof-command 
	(cond ((string= formatter "troff") 
	       (format proof-string roff-options roff-macro roff-font
		       proof-tmp-file ))
	      ((string= formatter "te-proof")
	       (format te-proof-string proof-tmp-file roff-options
		       roff-macro roff-font))
	      ((string= formatter "qroff")
	       (format quick-proof-string roff-options roff-macro roff-font
		       proof-tmp-file ))
	      ((string= formatter "qte-proof")
	       (format qte-proof-string roff-options roff-macro roff-font
		       proof-tmp-file ))
	      ((string= formatter "ascii")
	       (format aproof-string roff-options roff-macro roff-font
		       proof-tmp-file ))))
  (setq proof-process
	(start-process formatter proof-buffer "sh" "-c" proof-command))
  (set-process-sentinel proof-process 'proof-sentinel))


(defun proof-sentinel (process msg)
  (delete-file proof-tmp-file)
  (if (string-match "^exited" msg)
      (message "%s: killed" (process-name process))
    (progn
      (set-buffer (process-buffer process))
      (if (> (buffer-size) 0)
	  (progn
	    (text-mode)
	    (setq mode-name (format "%s:%s"
				    (process-name proof-process) proof-file))
     ;      (if (string= (process-name process) "nroff")
     ;      (zap-nroff-crap))
	    (goto-char (point-min))
	    (display-buffer (process-buffer process)))
	  (set-buffer proof-file))))
  (setq proof-process nil))
	
(defun zap-nroff-crap ()
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let* ((preceding (char-after (- (point) 2)))
	   (following (following-char)))
      	    ;; x\bx
      (cond ((= preceding following)	
	     (delete-char -2))
	    ;; _\b
	    ((= preceding ?\_)		
	     (delete-char -2))
	    ;; \b_
	    ((= following ?\_)		
	     (delete-region (1- (point)) (1+ (point)))))))
  ;; expand ^G lines
  (goto-char (point-min))
  (while (search-forward "\C-g" nil t)	
    (delete-char -2)
    (while (not (eolp))
      (insert " ")
      (forward-char 1)))
  ;; zap Esc-8 & Esc-9 vertical motions
  (goto-char (point-min))
  (while (search-forward "\e" nil t)
    (if (or (= (following-char) ?8) (= (following-char) ?9))
	    (delete-region (1+ (point)) (1- (point))))))

-----------------------------[eof]----------------------------
