;;; latin-9-pre.el --- Latin-9 Quail input method  -*-coding: iso-8859-15;-=
*-

;; Copyright (C) 1999  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: i18n

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is extracted from what I did in quail/latin-pre.el for Emacs
;; 21.1.  It's not necessarily ideal, but does provide a way of
;; entering latin-iso8859-15.

;;; Code:

(require 'quail)

(quail-define-package
 "latin-9-prefix" "Latin-9" "0>" t
 "Latin-9 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> =E1
    grave    |   `    | `a -> =E0
  circumflex |   ^    | ^a -> =E2
  diaeresis  |   \"    | \"a -> =E4, \"Y -> =BE
    tilde    |   ~    | ~a -> =E3
    caron    |   ~    | ~z -> =B8
   cedilla   |   ~    | ~c -> =E7
    misc     | \" ~ /  | \"s -> =DF  ~d -> =F0  ~t -> =FE  /a -> =E5  /e ->=
 =E6  /o -> =F8
             | \" ~ /  | /o -> =BD
   symbol    |   ~    | ~> -> =BB  ~< -> =AB  ~! -> =A1  ~? -> =BF  ~~ -> =
=B8
             |   ~    | ~s -> =A7  ~e -> =A4  ~. -> =B7  ~$ -> =A3  ~u -> =
=B5
             |   ~    | ~- -> =AD  ~=3D -> =AF
   symbol    |  _ /   | _o -> =BA  _a -> =AA  // -> =B0  /\\ -> =D7  _y -> =
=A5
             |  _ /   | _: -> =F7  /c -> =A2  ~p -> =B6
             |  _ /   | /=3D -> =AC
   symbol    |   ^    | ^r -> =AE  ^c -> =A9  ^1 -> =B9  ^2 -> =B2  ^3 -> =
=B3  _a -> =AA
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?=C1)
 ("'E" ?=C9)
 ("'I" ?=CD)
 ("'O" ?=D3)
 ("'U" ?=DA)
 ("'Y" ?=DD)
 ("'a" ?=E1)
 ("'e" ?=E9)
 ("'i" ?=ED)
 ("'o" ?=F3)
 ("'u" ?=FA)
 ("'y" ?=FD)
 ("' " ?')
 ("`A" ?=C0)
 ("`E" ?=C8)
 ("`I" ?=CC)
 ("`O" ?=D2)
 ("`U" ?=D9)
 ("`a" ?=E0)
 ("`e" ?=E8)
 ("`i" ?=EC)
 ("`o" ?=F2)
 ("`u" ?=F9)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?=C2)
 ("^E" ?=CA)
 ("^I" ?=CE)
 ("^O" ?=D4)
 ("^U" ?=DB)
 ("^a" ?=E2)
 ("^e" ?=EA)
 ("^i" ?=EE)
 ("^o" ?=F4)
 ("^u" ?=FB)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?=C4)
 ("\"E" ?=CB)
 ("\"I" ?=CF)
 ("\"O" ?=D6)
 ("\"U" ?=DC)
 ("\"a" ?=E4)
 ("\"e" ?=EB)
 ("\"i" ?=EF)
 ("\"o" ?=F6)
 ("\"s" ?=DF)
 ("\"u" ?=FC)
 ("\"y" ?=FF)
 ("\" " ?\")
 ("~A" ?=C3)
 ("~C" ?=C7)
 ("~D" ?=D0)
 ("~N" ?=D1)
 ("~O" ?=D5)
 ("~S" ?=A6)
 ("~T" ?=DE)
 ("~Z" ?=B4)
 ("~a" ?=E3)
 ("~c" ?=E7)
 ("~d" ?=F0)
 ("~n" ?=F1)
 ("~o" ?=F5)
 ("~s" ?=A8)
 ("~t" ?=FE)
 ("~z" ?=B8)
 ("~>" ?\=BB)
 ("~<" ?\=AB)
 ("~!" ?=A1)
 ("~?" ?=BF)
 ("~ " ?~)
 ("/A" ?=C5)
 ("/E" ?=C6)
 ("/O" ?=D8)
 ("/a" ?=E5)
 ("/e" ?=E6)
 ("/o" ?=F8)
 ("//" ?=B0)
 ("/ " ?/)
 ("_o" ?=BA)
 ("_a" ?=AA)
 ("_+" ?=B1)
 ("_y" ?=A5)
 ("_:" ?=F7)
 ("/c" ?=A2)
 ("/\\" ?=D7)
 ("/o" ?=BD)				; clash with =F8, but =E6 uses /
 ("/O" ?=BC)
 ("\"Y" ?=BE)
 ("~s" ?=A7)
 ("~p" ?=B6)
 ;; Is this the best option for Euro entry?
 ("~e" ?=A4)
 ("~." ?=B7)
 ("~$" ?=A3)
 ("~u" ?=B5)
 ("^r" ?=AE)
 ("^c" ?=A9)
 ("^1" ?=B9)
 ("^2" ?=B2)
 ("^3" ?=B3)
 ("~-" ?=AD)
 ("~=3D" ?=AF)
 ("/=3D" ?=AC))

(provide 'latin-9-pre)
;;; latin-9-pre.el ends here
