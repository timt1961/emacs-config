;;
;; Automatic signature generator. Users the Rocky Frisco database of one-liners
;;
(require 'quote-rock)
(defun make-signature () 
 "Generate a signature"
  (progn(
	 (insert "\n----------------------------------------------------------------------------")
	 (insert "\n tim.timmerman@asml.nl                          tel: (Int+031)-(0)40-2580613");
	 (insert "\n timt@dibbler.iaehv.nl        Voodoo Programmer/Keeper of the Rubber Chicken")
	 (insert "\n" (quote-rock))
	 (insert "\n----------------------------------------------------------------------------")
)))

(provide 'make-signature)

(make-signature)
