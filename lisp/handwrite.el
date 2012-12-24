;; handwrite.el --- turns your emacs-buffer into a handwritten document. 


;; Author: danny@tvs.kun.nl
;; Created: September 5 1996
;; Keywords: cursive writing

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;
;; Installation: 
;;
;; type at your prompt "emacs -l handwrite.el" or put this file on your 
;; Emacs-Lisp load path,  add the following into your ~/.emacs startup file
;;
;;                (require 'handwrite)
;;
;; "M-x pshand"  or "Write by hand"  in the edit menu should work now.
;;
;; I tried to make it `iso_8859_1'-friendly, but there are some exotic 
;; characters missing.
;;
;;


(defvar pshand-numlines 60 
  "The number of line on a page created by `pshand'")
(defvar pshand-pageh 782)
(defvar pshand-linel 494)
(defvar pshand-fontsize 11 
  "The size of the font for the command `pshand'")
(defvar pshand-linespace 12 
  "The spacing for the command `pshand'")
(defvar pshand-ntel  0)
(defvar pshand-ipage 0)
(defvar pshand-itel 0)
(defvar pshand-cbuffer "*scratch*")
(defvar pshand-last-point 1)
(defvar pshand-bname " ")
(defvar pshand-index 0)
(defvar pshand-psbuffer " ")
(defvar pshand-thispoint 1)
(defvar pshand-style "fnt" 
	"The style that is used for the entire document
          Options are `fnt' for plain text
                       `mb' for bold
                       `mo' for outlined")

(defun  pshand ()
  "Turn the buffer into a handwritten document. 
   Variables: pshand-linespace (default 12)
              pshand-fontsize  (default 11)
              pshand-numlines  (default 60), the number of lines on a page"
  (interactive)
  (setq pshand-cbuffer (current-buffer))
  (setq pshand-thispoint (point))
  (setq pshand-bname (buffer-name))
  (goto-char (point-min))		;start at beginning
  (setq pshand-ntel 63)
  (setq pshand-itel 0)
  (setq pshand-ipage 1)
  (setq pshand-index (1+ pshand-index))
  (setq pshand-psbuffer (format "file%d.ps" pshand-index))
  (switch-to-buffer pshand-psbuffer)
  (insert-header)
  (insert-procset)
  (insert "\n%%EndProlog\n")
  (insert-document-setup)
  (insert "%%Page: 1 1
op
0 0 xl
1 1 pen
63 44 gm\n")
  (insert "(nc 42 44 " (number-to-string pshand-pageh) " " (number-to-string pshand-linel) " 6 rc)kp\n")
  (insert "F 1 setTxMode
0 fs\n")
  (insert-font)
  (insert (number-to-string pshand-fontsize) " fz\n")
  (insert "bu fc\n")
  (insert "2 F /|______Joepie " pshand-style)
  (insert "\nbn
0.52490 0. 32 0.05249 0.( )ashow")
  (backward-char 7)
  (switch-to-buffer pshand-cbuffer)
  (goto-char (point-min))		;start at beginning
  (save-excursion
    (while (re-search-forward "\n" nil t)
      (previous-line 1)
      (beginning-of-line)
      (push-mark (point) t)
      (search-forward "\n" nil t)
      (backward-char 1)
      (copy-region-as-kill (point) (mark))
      (forward-char 1)
      (switch-to-buffer pshand-psbuffer)
      (yank)
      (message "write write write...")
      (search-forward ")ashow" nil t)
      (backward-char 6)
      (setq pshand-last-point (point))
      (beginning-of-line)
      (search-forward "(" nil t)
      (while (re-search-forward "[()\\]" pshand-last-point t)
	(save-excursion
	  (setq pshand-last-point (+ pshand-last-point 1))
	  (forward-char -1)
	  (insert "\\")))
      (setq pshand-ntel (+ pshand-ntel pshand-linespace))
      (end-of-line)
      (insert "\n")
      (setq pshand-itel (+ pshand-itel 1))
      (cond ( (eq pshand-itel pshand-numlines)
	      (setq pshand-ipage (+ pshand-ipage 1))
	      (insert "0 0 gm\n")
	      (insert "F T cp\n")
	      (insert "%%Page: " (number-to-string pshand-ipage) " " (number-to-string pshand-ipage) "\n")
	      (insert "op                          \n")
	      (insert "0 0 xl                      \n")
	      (insert "1 1 pen                     \n")
	      (insert "51 44 gm                    \n")
					;(insert "(nc 42 44 782 494 6 rc)kp   \n")
	      (insert "(nc 42 44 " (number-to-string pshand-pageh) " " (number-to-string pshand-linel) " 6 rc)kp\n")
	      (insert "F 1 setTxMode               \n")
	      (insert "0 fs                        \n")
	      (insert (number-to-string pshand-fontsize) " fz\n")
	      (insert "bu fc                       \n")
	      (insert "2 F /|______Joepie " pshand-style)
	      (insert "\nbn                          \n")
	      (setq pshand-ntel 63)
	      (setq pshand-itel 0)
	      ))
      (insert (number-to-string pshand-ntel))
      (insert  " 44 gm\n")
      (insert "0.52490 0. 32 0.05249 0.( )ashow")
      (backward-char 7)
      (switch-to-buffer pshand-cbuffer)
      ))
  (switch-to-buffer pshand-psbuffer)
  (next-line 1)
  (insert "\n0 0 gm\n")
;  (insert-dict)
  (insert-ending)
  (insert "%%Pages " (number-to-string pshand-ipage) " 0\n")
  (insert "%%EOF\n")
  (goto-char (point-min))		; start at beginning
  (while (search-forward "ÿ" nil t)  (replace-match "\\" nil t) (insert "264"))
  (goto-char (point-min))
  (while (search-forward "á" nil t)  (replace-match "\\" nil t) (insert "207"))
  (goto-char (point-min))
  (while (search-forward "à" nil t)  (replace-match "\\" nil t) (insert "210"))
  (goto-char (point-min))
  (while (search-forward "â" nil t)  (replace-match "\\" nil t) (insert "211"))
  (goto-char (point-min))
  (while (search-forward "ä" nil t)  (replace-match "\\" nil t) (insert "212"))
  (goto-char (point-min))
  (while (search-forward "ã" nil t)  (replace-match "\\" nil t) (insert "213"))
  (goto-char (point-min))
  (while (search-forward "å" nil t)  (replace-match "\\" nil t) (insert "214"))
  (goto-char (point-min))
  (while (search-forward "é" nil t)  (replace-match "\\" nil t) (insert "216"))
  (goto-char (point-min))
  (while (search-forward "è" nil t)  (replace-match "\\" nil t) (insert "217"))
  (goto-char (point-min))
  (while (search-forward "ê" nil t)  (replace-match "\\" nil t) (insert "220"))
  (goto-char (point-min))
  (while (search-forward "ë" nil t)  (replace-match "\\" nil t) (insert "221"))
  (goto-char (point-min))
  (while (search-forward "í" nil t)  (replace-match "\\" nil t) (insert "222"))
  (goto-char (point-min))
  (while (search-forward "ì" nil t)  (replace-match "\\" nil t) (insert "223"))
  (goto-char (point-min))
  (while (search-forward "î" nil t)  (replace-match "\\" nil t) (insert "224"))
  (goto-char (point-min))
  (while (search-forward "ï" nil t)  (replace-match "\\" nil t) (insert "225"))
  (goto-char (point-min))
  (while (search-forward "ó" nil t)  (replace-match "\\" nil t) (insert "227"))
  (goto-char (point-min))
  (while (search-forward "ò" nil t)  (replace-match "\\" nil t) (insert "230"))
  (goto-char (point-min))
  (while (search-forward "ö" nil t)  (replace-match "\\" nil t) (insert "232"))
  (goto-char (point-min))
  (while (search-forward "õ" nil t)  (replace-match "\\" nil t) (insert "233"))
  (goto-char (point-min))
  (while (search-forward "ú" nil t)  (replace-match "\\" nil t) (insert "234"))
  (goto-char (point-min))
  (while (search-forward "ù" nil t)  (replace-match "\\" nil t) (insert "235"))
  (goto-char (point-min))
  (while (search-forward "û" nil t)  (replace-match "\\" nil t) (insert "236"))
  (goto-char (point-min))
  (while (search-forward "ü" nil t)  (replace-match "\\" nil t) (insert "237"))
  (goto-char (point-min))
  (while (search-forward "ß" nil t)  (replace-match "\\" nil t) (insert "247"))
  (goto-char (point-min))
  (while (search-forward "°" nil t)  (replace-match "\\" nil t) (insert "241"))
  (goto-char (point-min))
  (while (search-forward "®" nil t)  (replace-match "\\" nil t) (insert "250"))
  (goto-char (point-min))
  (while (search-forward "æ" nil t)  (replace-match "ae" nil t) )
  (goto-char (point-min))
  (while (search-forward "ij" nil t) (replace-match "\\" nil t) (insert "264"))
  (goto-char (point-min))
  (while (search-forward "ç" nil t)  (replace-match "\\" nil t) (insert "215"))
  (goto-char (point-min))
  (while (search-forward "§" nil t)  (replace-match "\\" nil t) (insert "244"))
  (if (y-or-n-p "Send this to the printer? ")  (call-process-region (point-min) (point-max) lpr-command nil nil nil))
  (switch-to-buffer pshand-cbuffer)
  (goto-char pshand-thispoint)
  )



(defun insert-header ()
  (insert "%!PS-Adobe-2.0\n")
  (insert "%%Title: " pshand-bname "\n")
  (insert "%%CreationDate: " (current-time-string) )
  (insert "\n%%Pages: (atend)")
;;;%%BoundingBox: ? ? ? ?
;;;%%PageBoundingBox: 28 30 566 811
  (insert "\n%%For: "    user-mail-address)            
  (insert "\n%%DocumentProcSets: \"\(AppleDict md\)\" 71 0\n%%EndComments\n"))

(defun insert-procset ()
  (insert "%%BeginProcSet: \"\(AppleDict md\)\" 71 0
userdict/LW{save statusdict/product get(LaserWriter)anchorsearch
exch pop{dup length 0 eq{pop 1}{( Plus)eq{2}{3}ifelse}ifelse}{0}ifelse exch restore}bind put
userdict/patchOK known not{save LW dup 1 ne exch 2 ne and false<1861AEDAE118A9F95F1629C0137F8FE656811DD93DFBEA65E947502E78BA12284B8A58EF0A3
2E272778DAA2ABEC72A84102D591E11D96BA61F57877B895A752D9BEAAC3DFD7D3220E2BDE7C036467464E0E836748F1DE7AB6216866F130CE7CFCEC8CE050B870C11881EE3E9D70919>{eexec}stopped{dup type/stringtype eq{pop}if}if and exch restore userdict/patchOK 3 -1 roll put} if
userdict/downloadOK known not{userdict/downloadOK{ vmstatus exch sub exch pop 120000 gt patchOK and}bind put}if
userdict/type42known known not{userdict/type42known systemdict/resourcestatus known{42/FontType resourcestatus{pop pop true}{false}ifelse }{false}ifelse put}if
type42known not downloadOK and {userdict begin /*charpath /charpath load def/charpathflag false def/charpath{userdict/charpathflag true put userdict/*charpath get exec userdict/charpathflag false put}bind def end}if
userdict/checkload known not{userdict/checkload{{pop exec} {save 3 dict begin/mystring 6050 string def
exch/endstring exch def{currentfile mystring readline not{stop}if endstring eq{exit}if}loop end restore pop}ifelse}bind put}if
userdict/LW+{LW 2 eq}bind put
userdict/ok known not{userdict/ok{systemdict/statusdict known dup{LW 0 gt and}if}bind put}if
systemdict/setobjectformat known{0 setobjectformat}if
systemdict/currentpacking known{currentpacking true setpacking}if
/md 270 dict def md begin
/av 71 def
/T true def/F false def/mtx matrix def/s75 75 string def/sa8 8 string def/sb8 8 string def
/sc8 8 string def/sd8 8 string def/s1 ( ) def/pxs 1 def/pys 1 def
/ns false def
1 0 mtx defaultmatrix dtransform exch atan/pa exch def/nlw .24 def/ppr [-32 -29.52 762 582.48] def
/pgr [0 0 0 0] def
/pgs 1 def/por true def/xb 500 array def/so true def/tso true def/fillflag false def/pnm 1 def/fmv true def
/sfl false def/ma 0 def/invertflag false def/dbinvertflag false def/xflip false def/yflip false def/noflips true def/scaleby96 false def/fNote true def/fBitStretch true def
/4colors false def/fg (Rvd\\001\\001\\000\\000\\177) def
/bdf{bind def}bind def
/xdf{exch def}bdf
/xl{neg exch neg translate}bdf
/fp{pnsh 0 ne pnsv 0 ne and}bdf
/nop{}bdf/lnop[/nop load]cvx bdf
/vrb[
{fp{fg 6 get 0 ne{gsave stroke grestore}{gsave 1 setlinewidth pnsh pnsv scale stroke grestore}ifelse}if newpath}bind
/eofill load
dup
/newpath load
2 index
dup
{clip newpath}bind
{}bind
dup
2 copy
]def
/sgd systemdict/setpagedevice known{{2 dict begin/PreRenderingEnhance exch def/Policies 1 dict dup/PreRenderingEnhance 1 put def currentdict end setpagedevice}}{{pop}}ifelse bdf
/svsc systemdict/currentcolorscreen known{{currentcolorscreen/dkspf xdf/dkrot xdf/dkfreq xdf/dyspf xdf/dyrot xdf/dyfreq xdf/dmspf xdf/dmrot xdf/dmfreq xdf
/dcspf xdf/dcrot xdf/dcfreq xdf}}{{currentscreen/spf xdf/rot xdf/freq xdf}}ifelse bdf
/doop{vrb exch get exec}bdf
/psu{/udf xdf/tso xdf /fNote xdf/fBitStretch xdf/scaleby96 xdf/yflip xdf/xflip xdf
/invertflag xdf/dbinvertflag invertflag statusdict begin version cvr 47.0 ge product (LaserWriter) eq not and end invertflag and {not}if def
xflip yflip or{/noflips false def}if
/pgs xdf 2 index .72 mul exch div/pys xdf div .72 mul/pxs xdf ppr astore pop pgr astore pop/por xdf sn and/so xdf}bdf
/tab{userdict /11x17 known{userdict begin /11x17 load exec end}{statusdict /setpage known{statusdict begin 792 1224 1 setpage end}{statusdict /setpageparams known{statusdict begin 792 1224 0 1 setpageparams end}if}ifelse}ifelse}bdf
/a3Size{userdict /a3 known{userdict begin /a3 load exec end}{statusdict /setpageparams known{statusdict begin 842 1191 0 1 setpageparams end}if}ifelse}bdf
/txpose{fNote{smalls}{bigs}ifelse pgs get exec pxs pys scale ppr aload pop por{noflips{pop exch neg exch translate pop 1 -1 scale}if
xflip yflip and{pop exch neg exch translate 180 rotate 1 -1 scale ppr 3 get ppr 1 get neg sub neg ppr 2 get ppr 0 get neg sub neg translate}if 
xflip yflip not and{pop exch neg exch translate pop 180 rotate ppr 3 get ppr 1 get neg sub neg 0 translate}if yflip xflip not and{ppr 1 get neg ppr 0 get neg translate}if}
{noflips{translate pop pop 270 rotate 1 -1 scale}if xflip yflip and{translate pop pop 90 rotate 1 -1 scale ppr 3 get ppr 1 get neg sub neg ppr 2 get ppr 0 get neg sub neg translate}if
xflip yflip not and{translate pop pop 90 rotate ppr 3 get ppr 1 get neg sub neg 0 translate}if yflip xflip not and{translate pop pop 270 rotate ppr 2 get ppr 0 get neg sub neg 0 exch translate}if}ifelse
statusdict begin/waittimeout where{pop waittimeout 300 lt{statusdict/waittimeout 300 put}if}if end 
scaleby96{ppr aload pop 4 -1 roll add 2 div 3 1 roll add 2 div 2 copy translate .96 dup scale neg exch neg exch translate}if}bdf
/fr{4 copy pgr aload pop 3 -1 roll add 3 1 roll exch add 6 2 roll 3 -1 roll
sub 3 1 roll exch sub 3 -1 roll exch div 3 1 roll div exch scale pop pop xl}bdf
/obl{{0.212557 mul}{pop 0}ifelse}bdf
/sfd{ps fg 5 -1 roll get mul 100 div 0 ps 5 -1 roll obl ps neg 0 0 6a astore makefont setfont}bdf
/fnt{findfont sfd}bdf
/bt{sa 3 1 roll 3 index and put}bdf
/sa(\\000\\000\\000\\000\\000\\000\\000\\000\\000\\000)def
/fs{0 1 bt 1 2 bt 2 4 bt 3 8 bt 4 16 bt 5 32 bt 6 64 bt 7 128 bt sa exch 8 exch put}bdf
/mx1 matrix def
/mx2 matrix def
/mx3 matrix def
/bu{currentpoint 4colors{currentcmykcolor}{currentrgbcolor}ifelse currentlinewidth currentlinecap currentlinejoin 
currentdash exch aload length fg 5 sfl{1}{0}ifelse put pnsv pnsh 
2t aload pop 3a aload pop mx2 aload pop mx1 aload pop mtx currentmatrix aload pop
mx3 aload pop ps pm restore/ps xdf mx3 astore pop}bdf
/bn{/pm save def mx3 setmatrix newpath 0 0 moveto ct dup 39 get 0 exch getinterval cvx exec mtx astore setmatrix mx1 astore pop mx2 astore pop 3a 
astore pop 2t astore pop/pnsh xdf/pnsv xdf gw
/sfl fg 5 get 0 ne def array astore exch setdash setlinejoin setlinecap 
setlinewidth 4colors{mysetcmykcolor}{setrgbcolor}ifelse moveto}bdf
/fc{save vmstatus exch sub 50000 lt
{(%%[|0|]%%)=print flush}if pop restore}bdf
/tc{32768 div add 3 1 roll 32768 div add 2t astore pop}bdf
/3a [0 0 0] def
/2t 2 array def
/tp{3a astore pop}bdf
/tt{mx2 currentmatrix pop currentpoint 2 copy 2t aload pop qa 2 copy translate 3a aload pop exch dup 0 eq
{pop}{1 eq{-1 1}{1 -1}ifelse scale}ifelse rotate pop neg exch neg exch translate moveto}bdf
/te{mx2 setmatrix}bdf
/th{3 -1 roll div 3 1 roll exch div 2 copy mx1 scale pop scale/sfl true def}bdf
/tu{1 1 mx1 itransform scale/sfl false def}bdf
/ts{1 1 mx1 transform scale/sfl true def}bdf
/fz{/ps xdf}bdf
/dv{dup 0 ne{div}{pop}ifelse}bdf
/pop4{pop pop pop pop}bdf
/it{sfl{mx1 itransform}if}bdf
/gm{exch it moveto}bdf/rm{it rmoveto}bdf
/lm{currentpoint sfl{mx1 transform}if exch pop sub 0 exch it rmoveto}bdf
/fm{statusdict/manualfeed known}bdf
/se{statusdict exch/manualfeed exch put}bdf
/mf{dup/ma exch def 0 gt{fm se/t1 5 st ok ma 1 gt and{/t2 0 st/t3 0 st
statusdict/manualfeedtimeout 3600 put
}if}if}bdf
/jn{/statusdict where exch pop{statusdict exch /jobname exch put}if}bdf
/pen{pnm mul/pnsh xdf pnm mul/pnsv xdf pnsh setlinewidth}bdf
/min{2 copy gt{exch}if pop}bdf
/max{2 copy lt{exch}if pop}bdf
/dh{fg 6 1 put array astore dup {1 pxs div mul exch}forall astore exch pop exch pop exch setdash}bdf
/ih[currentdash]def
/rh{fg 6 0 put ih aload pop setdash}bdf
/dl{gsave nlw pys div setlinewidth 0 setgray}bdf
/dlin{exch currentpoint currentlinewidth 2 div dup
translate newpath moveto lineto currentpoint stroke grestore moveto}bdf
/lin{fg 6 get 0 ne{exch lineto currentpoint 0 doop moveto}
{exch currentpoint/pnlv xdf/pnlh xdf gsave newpath/@1 xdf/@2 xdf fp{pnlh @2 lt{pnlv @1 ge
{pnlh pnlv moveto @2 @1 lineto pnsh 0 rlineto
0 pnsv rlineto pnlh pnsh add pnlv pnsv add lineto pnsh neg 0 rlineto}
{pnlh pnlv moveto pnsh 0 rlineto @2 pnsh add @1 lineto 0 pnsv rlineto
pnsh neg 0 rlineto pnlh pnlv pnsv add lineto}ifelse}{pnlv @1 gt
{@2 @1 moveto pnsh 0 rlineto pnlh pnsh add pnlv lineto 0 pnsv rlineto
pnsh neg 0 rlineto @2 @1 pnsv add lineto}{pnlh pnlv moveto pnsh 0 rlineto
0 pnsv rlineto @2 pnsh add @1 pnsv add lineto pnsh neg 0 rlineto
0 pnsv neg rlineto}ifelse}ifelse
closepath fill}if @2 @1 grestore moveto}ifelse}bdf
/gw{/pnm fg 3 get fg 4 get div def}bdf
/lw{fg exch 4 exch put fg exch 3 exch put gw pnsv pnsh pen}bdf
/barc{/@1 xdf/@2 xdf/@3 xdf/@4 xdf/@5 xdf
/@6 xdf/@7 xdf/@8 xdf gsave
@5 @7 add 2 div @6 @8 add 2 div translate newpath 0 0 moveto
@5 @7 sub @6 @8 sub mtx currentmatrix pop scale @1{newpath}if
0 0 0.5 @4 @3 arc @4 @3 sub abs 360 ge{closepath}if
mtx setmatrix @2 doop grestore}bdf
/ar{dup 0 eq barc}bdf
/ov{0 exch 360 exch true barc}bdf
/rc{dup/@t xdf 0 eq{4 copy 3 -1 roll eq 3 1 roll eq and{pnsv 2 div sub exch pnsh 2 div sub exch 4 2 roll pnsv 2 div add exch pnsh 2 div add exch
/@t 1 def}if}if currentpoint 6 2 roll newpath 4 copy 4 2 roll exch moveto 6 -1 roll lineto lineto lineto closepath @t doop moveto}bdf
/mup{dup pnsh 2 div le exch pnsv 2 div le or}bdf
/rr{/@1 xdf 2. div/@2 xdf 2. div/@3 xdf
/@4 xdf/@5 xdf/@6 xdf/@7 xdf
@7 @5 eq @6 @4 eq @2 mup or or{@7 @6 @5 @4 @1 rc}
{@4 @6 sub 2. div dup @2 lt{/@2 xdf}{pop}ifelse
@5 @7 sub 2. div dup @2 lt{/@2 xdf}{pop}ifelse
@1 0 eq{/@2 @2 pnsh 2 div 2 copy gt{sub def}{0 pop4}ifelse}if
currentpoint newpath
@4 @6 add 2. div @7 moveto
@4 @7 @4 @5 @2 arcto pop4
@4 @5 @6 @5 @2 arcto pop4
@6 @5 @6 @7 @2 arcto pop4
@6 @7 @4 @7 @2 arcto pop4
closepath @1 doop moveto}ifelse}bdf
/pr{gsave newpath/pl{exch moveto/pl{exch lineto}def}def}bdf
/pl{exch lineto}bdf
/ep{dup 0 eq{{moveto}{exch lin}{}{(%%[|1|]%%)= flush}pathforall
pop grestore}{doop grestore}ifelse currentpoint newpath moveto}bdf
/gr{64. div setgray}bdf
/savescreen{ns not{/ns true def systemdict/currentcolorscreen known{currentcolorscreen/pkspf xdf/pkrot xdf/pkfreq xdf/pyspf xdf/pyrot xdf/pyfreq xdf/pmspf xdf/pmrot xdf/pmfreq xdf
/pcspf xdf/pcrot xdf/pcfreq xdf}{currentscreen/sspf xdf/srot xdf/sfreq xdf}ifelse}if}bdf
/restorescreen{/ns false def systemdict/setcolorscreen known{pcfreq pcrot/pcspf load pmfreq pmrot/pmspf load pyfreq pyrot/pyspf load
pkfreq pkrot/pkspf load setcolorscreen}{sfreq srot/sspf load setscreen}ifelse}bdf
/pat{savescreen sa8 
copy pop 9.375 pa por not{90 add}if{1 add 4 mul cvi sa8 exch get exch 1 add 4 mul cvi 7 sub bitshift 1 and}setscreen exch not{gr}{pop}ifelse}bdf
/sg{restorescreen gr}bdf
/cpat{savescreen 10 2 roll 7 -1 roll sa8 copy pop 9.375 pa por not{90 add}if{1 add 4 mul cvi sa8 exch get exch 1 add 4 mul cvi 7 sub bitshift 1 and}8 -1 roll sb8 copy pop 9.375 pa por not{90 add}if{1 add 4 mul cvi sb8
exch get exch 1 add 4 mul cvi 7 sub bitshift 1 and}9 -1 roll sc8 copy pop 9.375 pa por not{90 add}if{1 add 4 mul cvi sc8 exch get exch 1 add 4 mul cvi 7 sub bitshift 1 and}10 -1 roll sd8 copy pop 9.375 pa por not{90 add}if{1 add 4 mul cvi sd8
exch get exch 1 add 4 mul cvi 7 sub bitshift 1 and}psuedo1 dsc 4{4 -1 roll 1 exch 64 div sub}repeat mysetcmykcolor pop pop}bdf
systemdict/setcolorscreen known statusdict/processcolors known and{/psuedo1 lnop bdf/dsc/setcolorscreen load def}{/psuedo1{16{pop}repeat sa8 copy pop 9.375 pa por not{90 add}if{1 add 4 mul cvi sa8 exch get exch 1 add 4 mul cvi 7 sub bitshift 1 and}}bdf
/bwsc{setscreen dup gr 0 exch 0 exch 64 exch 64 exch 64 exch}bdf/dsc/bwsc load def
}ifelse
systemdict/setcmykcolor known{/mysetcmykcolor /setcmykcolor load def}{/mysetcmykcolor{1 sub 4 1 roll 3{3 index add neg dup 0 lt{pop 0}if 3 1 roll}repeat setrgbcolor pop}bdf}ifelse
/dc{transform round .5 sub exch round .5 sub exch itransform}bdf
/sn{userdict/smooth4 known}bdf
/x8{3 bitshift}bdf
/x4{2 bitshift}bdf
/d4{-2 bitshift}bdf
/d8{-3 bitshift}bdf
/rb{15 add -4 bitshift 1 bitshift}bdf
/db{/@7 save def/@1 xdf/@2 xdf/@3 xdf/@4 xdf/@5 xdf/@6 @5 @3 4 add mul def
dc translate scale/xdbit 1 1 idtransform abs/ydbit exch def abs def{0 0 1 ydbit add 1 10 rc clip}if
@1 0 eq @1 4 eq or{currentrgbcolor 1 setgray ydbit 0 1 ydbit add 1 2 rc setrgbcolor}if
@1 3 eq @1 7 eq or{1 setgray}{currentrgbcolor 2 index eq exch 2 index eq and exch pop{0 setgray}if}ifelse/@9 @1 0 eq @1 1 eq @1 3 eq or or dbinvertflag xor def/@13 @6 def
@2 fBitStretch or{/@10 @4 x4 def/@11 @3 x4 def/@12 @10 rb def/@13 @12 @11 mul def/@15 1 1 dtransform abs/calcY 1 index def round cvi/@14 exch def
abs/calcX 1 index def round cvi scaleby96 not{1 add}if def/@16 @15 rb def/@17 @16 @14 mul def}if
sn @13 60000 lt and @2 fBitStretch or and{mtx currentmatrix dup 1 get exch 2 get 0. eq exch 0. eq and @17 60000 lt and fBitStretch and{@16 3 bitshift @14 @9 [calcX 0 0 calcY 0 0]{@17 string @13 string
currentfile @6 string readhexstring pop 1 index @4 @3 @5 @12 @2 smooth4
@10 @11 @12 dup string 5 index @15 @14 @16 dup string stretch}imagemask}{@12 x8 @11 @9 [@10 0 0 @11 0 0]{@13 string
currentfile @6 string readhexstring pop 1 index @4 @3 @5 @12 @2 smooth4}imagemask}ifelse}{@5 3 bitshift @3 4 add @9 [@4 0 0 @3 0 2]{currentfile @6 string readhexstring pop}imagemask}ifelse
@7 restore}bdf
systemdict/setcmykcolor known{/psuedo lnop bdf/di/colorimage load def}{/routines[{.3 mul add 1}bind{.59 mul add 2}bind{.11 mul add round cvi str exch i exch put/i i 1 add def 0 0}bind]def
/psuedo{/i 0 def 0 exch 0 exch{exch routines exch get exec}forall pop pop str}bdf/bwi{pop pop image}bdf/di/bwi load def}ifelse
/cdb{/@7 save def/@1 xdf/@2 xdf/@3 xdf/@4 xdf/@5 xdf
systemdict/setcmykcolor known not{dc}if translate scale /@6 xdf
/@18 @5 dup 60000 ge{pop 60000}if string def @6 not{/str @18 0 @18 length 3 idiv getinterval def}if @4 @3 8 [@4 0 0 @3 0 0]@6{{currentfile @18 readhexstring pop}image}{{currentfile @18 readhexstring pop psuedo}false 3 di}ifelse @7 restore}bdf
/wd 16 dict def
/mfont 14 dict def
/mdf{mfont wcheck not{/mfont 14 dict def}if mfont begin xdf end}bdf
/cf{{1 index/FID ne{def}{pop pop}ifelse}forall}bdf/rf{/@1 exch def/@2 exch def
FontDirectory @2 known{cleartomark pop}{findfont dup begin dup length @1 add dict begin
cf{/Encoding macvec def}{Encoding dup length array copy/Encoding exch def
counttomark 2 idiv{Encoding 3 1 roll put}repeat}ifelse
pop
exec currentdict end end @2 exch definefont pop}ifelse}bdf
/bmbc{exch begin wd begin
/cr xdf
save
CharTable cr 6 mul 6 getinterval{}forall
/bitheight xdf/bitwidth xdf
.96 div/width xdf
Gkernmax add/XOffset xdf Gdescent add/YOffset xdf/rowbytes xdf
rowbytes 255 eq{0 0 0 0 0 0 setcachedevice}
{Gnormsize dup scale
width 0 XOffset YOffset bitwidth XOffset add bitheight YOffset add
setcachedevice
rowbytes 0 ne{
XOffset YOffset translate newpath 0 0 moveto
bitwidth bitheight scale
sn{
/xSmt bitwidth x4 def
/ySmt bitheight x4 def
/rSmt xSmt rb def
rSmt x8 ySmt true
[xSmt 0 0 ySmt neg 0 ySmt]
{rSmt ySmt mul string CharData cr get
1 index bitwidth bitheight rowbytes rSmt tso smooth4}
}{rowbytes 3 bitshift bitheight 4 add true
[bitwidth 0 0 bitheight neg 0 bitheight 2 add]
{CharData cr get}
}ifelse
imagemask
}if
}ifelse
restore
end end
}bdf
/bb{.96 exch div/Gnormsize mdf 2 index
/Gkernmax mdf 1 index/Gdescent mdf
3 index div 4 1 roll
2 index div 1. 5 2 roll
exch div 4 1 roll
4 array astore/FontBBox mdf
}bdf
/cdf{mfont/CharData get 3 1 roll put}bdf
/bf{
mfont begin
/FontType 3 def
/FontMatrix [1 0 0 1 0 0] def
/Encoding macvec def
/MFontType 0 def
/BuildChar/bmbc load def
end
mfont definefont pop
}bdf
/wi LW 1 eq{{gsave 0 0 0 0 0 0 0 0 moveto lineto lineto lineto closepath clip stringwidth grestore}bind}{/stringwidth load}ifelse def
/aps{0 get 124 eq}bdf
/xc{s75 cvs dup}bdf
/xp{put cvn}bdf
/scs{xc 3 67 put dup 0 95 xp}bdf
/sos{xc 3 79 xp}bdf
/sbs{xc 1 66 xp}bdf
/sis{xc 2 73 xp}bdf
/sob{xc 2 79 xp}bdf
/sss{xc 4 83 xp}bdf
/dd{exch 1 index add 3 1 roll add exch}bdf
/smc{moveto dup show}bdf
/ndf2{udf{dup /FontType get 0 eq{/FDepVector get{dup /FontType get 0 eq{ndf2}{dup /df2 known{begin df2 0 null put end
}{pop}ifelse}ifelse}forall}{/df2 known{dup begin df2 0 null put end}if}ifelse}{pop}ifelse}bdf
/kwn{FontDirectory 1 index known{findfont dup ndf2 exch pop}}bdf
/gl{1 currentgray sub setgray}bdf
/newmm{dup /FontType get 0 eq{dup maxlength dict begin{1 index/FID ne 2 index/UniqueID ne and{def}{pop pop}ifelse}forall currentdict end
dup /FDepVector 2 copy get[exch 6 index exch 6 index exch{newmm 3 1 roll}forall pop pop] put dup
}{/mfont 10 dict def mfont begin/FontMatrix [1 0 0 1 0 0] def
/FontType 3 def/Encoding macvec def/df 1 index def/df2 1 array def/FontBBox [0 0 1 1] def/StyleCode 2 index def
/mbc{bcarray StyleCode get}def/BuildChar{exch begin	wd begin/cr exch def/cs s1 dup 0 cr put def df /MFontType known not{
df2 0 get null eq{df dup length 2 add dict begin{1 index/FID ne 2 index/UniqueID ne and{def}{pop pop}ifelse}forall
/StrokeWidth 1 0 FontMatrix idtransform pop dup nlw mul pys div ps div exch 0.012 mul 2 copy le{exch}if pop def/PaintType 2 def currentdict end
/q exch definefont df2 exch 0 exch put}if}if mbc exec end end}def end mfont}ifelse
3 index exch definefont exch pop}bdf
/mb{dup sbs kwn{0 2 index findfont newmm exch pop exch pop exch pop}ifelse sfd}bdf
/mo{dup sos kwn{2 2 index findfont newmm exch pop exch pop exch pop}ifelse sfd}bdf
/ms{dup sss kwn{4 2 index findfont newmm exch pop exch pop exch pop}ifelse sfd}bdf
/ou{dup sos kwn{mfont/df2 known{mfont begin df2 0 null put end}if 3 2 index findfont newmm exch pop exch pop exch pop}ifelse sfd}bdf
/su{dup sss kwn{mfont/df2 known{mfont begin df2 0 null put end}if 5 2 index findfont newmm exch pop exch pop exch pop}ifelse sfd}bdf
/ao{/fmv true def ou}bdf/as{/fmv true def su}bdf
/vo{/fmv false def ou}bdf/vs{/fmv false def su}bdf
/c{currentrgbcolor dup 4 1 roll eq 3 1 roll eq and/gray xdf}bdf
/bcarray[{/da .03 def df setfont gsave cs wi 1 index 0 ne{exch da add exch}if grestore setcharwidth
cs 0 0 smc da 0 smc da da smc 0 da moveto show}bind dup{/da 1 ps div def df setfont gsave cs wi 1 index 0 ne{exch da add exch}if grestore setcharwidth
cs 0 0 smc da 0 smc da da smc 0 da smc c gray{gl}{1 setgray}ifelse da 2. div dup moveto show}bind
{df setfont gsave cs wi grestore setcharwidth c gray{gl}{currentrgbcolor 1 setgray}ifelse cs 0 0 smc df2 0 get setfont
gray{gl}{4 1 roll setrgbcolor}ifelse 0 0 moveto show}bind
{/da 1 ps div def/ds .05 def/da2 da 2. div def df setfont gsave cs wi 1 index 0 ne{exch ds add da2 add exch}if grestore setcharwidth
cs ds da2 add .01 add 0 smc 0 ds da2 sub translate 0 0 smc da 0 smc da da smc 0 da smc c gray{gl}{1 setgray}ifelse da 2. div dup moveto show}bind
{/da .05 def df setfont gsave cs wi 1 index 0 ne{exch da add exch}if grestore setcharwidth c cs da .01 add 0 smc 0 da translate
gray{gl}{currentrgbcolor 1 setgray 4 -1 roll}ifelse 0 0 smc gray{gl}{4 1 roll setrgbcolor}ifelse df2 0 get setfont 0 0 moveto show}bind]def
/st{1000 mul usertime add dup 2147483647 gt{2147483647 sub}if def}bdf
/the{usertime sub dup 0 lt exch -2147483648 gt and}bdf
/6a 6 array def
/2a 2 array def
/3q 3 array def
/qs{3 -1 roll sub exch 3 -1 roll sub exch}bdf
/qa{3 -1 roll add exch 3 -1 roll add exch}bdf
/qm{3 -1 roll 1 index mul 3 1 roll mul}bdf
/qn{6a exch get mul}bdf
/qA .166667 def/qB .833333 def/qC .5 def
/qx{6a astore pop
qA 0 qn qB 2 qn add   qA 1 qn qB 3 qn add
qB 2 qn qA 4 qn add   qB 3 qn qA 5 qn add
qC 2 qn qC 4 qn add   qC 3 qn qC 5 qn add}bdf
/qp{6 copy 12 -2 roll pop pop}bdf
/qc{exch qp qx curveto}bdf
/qi{{exch 4 copy 2a astore aload pop qa .5 qm newpath moveto}{exch 2 copy 6 -2 roll 2 qm qs 4 2 roll}ifelse}bdf
/qq{{qc 2a aload pop qx curveto}{exch 4 copy qs qa qx curveto}ifelse}bdf
/pt{currentpoint newpath moveto}bdf
/qf{/fillflag true def}bdf
/ec{dup 4 and 0 ne{closepath}if 1 and 0 ne{0 doop}if grestore currentpoint newpath moveto/fillflag false def}bdf
/eu{currentpoint fp{0 ep}{grestore newpath}ifelse moveto/fillflag false def}bdf
/bp{currentpoint newpath 2 copy moveto}bdf
/ef{gsave fillflag{gsave eofill grestore}if}bdf
/sm{0 exch{@1 eq{1 add}if}forall}bdf
/lshow{4 1 roll exch/@1 exch def{1 index wi pop sub 1 index sm dv 0 @1 4 -1 roll widthshow}{1 index wi pop sub
1 index dup sm 10 mul exch length 1 sub add dv dup 10. mul 0 @1 4 -1 roll 0 6 -1 roll awidthshow}ifelse}bdf
/setTxMode{sa 9 2 index put exch not{3 eq{1}{0}ifelse setgray}{pop}ifelse}bdf
/SwToSym{{}mark false/Symbol/|______Symbol 0 rf 0 sa 6 get 0 ne{pop 1}{sa 7 get 0 eq{pop 2}if}ifelse
sa 1 get 0 ne/|______Symbol
sa 4 get 0 ne{vs}{sa 3 get 0 ne{vo}{fnt}ifelse}ifelse}bdf
/mc{0 3 1 roll transform neg exch pop}bdf
/ul{dup 0 ne sa 2 get 0 ne and{gsave 0 0
/UnderlinePosition kif{mc}{ps -10 div}ifelse/UnderlineThickness kif{mc}{ps 15 div}ifelse
abs setlinewidth neg rmoveto
sa 4 get 0 ne{gsave currentlinewidth 2. div dup rmoveto currentpoint newpath moveto
2 copy rlineto stroke grestore}if
sa 3 get sa 4 get or 0 ne{gsave currentrgbcolor dup 4 1 roll eq 3 1 roll eq and{gl}{1 setgray}ifelse 2 copy rlineto stroke grestore rlineto strokepath nlw pys div setlinewidth}{rlineto}ifelse
stroke grestore}{pop}ifelse}bdf
/sgt{2 copy known{get true}{pop pop false}ifelse}bdf
/kif{currentfont dup/FontMatrix get exch/FontInfo sgt{true}{currentfont/df sgt
{dup/FontInfo sgt{3 1 roll/FontMatrix get mtx concatmatrix exch true}{pop pop pop false}
ifelse}{pop pop false}ifelse}ifelse{3 -1 roll sgt{exch true}{pop false}ifelse}{false}ifelse}bdf
/blank/Times-Roman findfont/CharStrings get/space get def
/macvec 256 array def
/NUL/SOH/STX/ETX/EOT/ENQ/ACK/BEL/BS/HT/LF/VT/FF/CR/SO/SI
/DLE/DC1/DC2/DC3/DC4/NAK/SYN/ETB/CAN/EM/SUB/ESC/FS/GS/RS/US
macvec 0 32 getinterval astore pop
macvec 32/Times-Roman findfont/Encoding get
32 96 getinterval putinterval macvec dup 39/quotesingle put 96/grave put
/Adieresis/Aring/Ccedilla/Eacute/Ntilde/Odieresis/Udieresis/aacute
/agrave/acircumflex/adieresis/atilde/aring/ccedilla/eacute/egrave
/ecircumflex/edieresis/iacute/igrave/icircumflex/idieresis/ntilde/oacute
/ograve/ocircumflex/odieresis/otilde/uacute/ugrave/ucircumflex/udieresis
/dagger/degree/cent/sterling/section/bullet/paragraph/germandbls
/registered/copyright/trademark/acute/dieresis/notequal/AE/Oslash
/infinity/plusminus/lessequal/greaterequal/yen/mu/partialdiff/summation
/product/pi/integral/ordfeminine/ordmasculine/Omega/ae/oslash
/questiondown/exclamdown/logicalnot/radical/florin/approxequal/Delta/guillemotleft
/guillemotright/ellipsis/blank/Agrave/Atilde/Otilde/OE/oe
/endash/emdash/quotedblleft/quotedblright/quoteleft/quoteright/divide/lozenge
/ydieresis/Ydieresis/fraction/currency/guilsinglleft/guilsinglright/fi/fl
/daggerdbl/periodcentered/quotesinglbase/quotedblbase/perthousand/Acircumflex/Ecircumflex/Aacute
/Edieresis/Egrave/Iacute/Icircumflex/Idieresis/Igrave/Oacute/Ocircumflex
/apple/Ograve/Uacute/Ucircumflex/Ugrave/dotlessi/circumflex/tilde
/macron/breve/dotaccent/ring/cedilla/hungarumlaut/ogonek/caron
macvec 128 128 getinterval astore pop
{}mark true/Courier/|______Courier 0 rf
{/Metrics 21 dict begin/zero 600 def/one 600 def/two 600 def/three 600 def/four 600 def/five 600 def/six 600 def/seven 600 def/eight 600 def
/nine 600 def/comma 600 def/period 600 def/dollar 600 def/numbersign 600 def/percent 600 def/plus 600 def/hyphen 600 def/E 600 def/parenleft 600 def/parenright 600 def/space 600 def
currentdict end def currentdict/UniqueID known{/UniqueID 16#800000 def}if/FontBBox FontBBox 4 array astore def}mark true/Helvetica/|______Seattle 1 rf
/oldsettransfer/settransfer load def
/concatprocs{/proc2 exch cvlit def/proc1 exch cvlit def/newproc proc1 length proc2 length add array def
newproc 0 proc1 putinterval newproc proc1 length proc2 putinterval newproc cvx}def
/settransfer{currenttransfer concatprocs oldsettransfer}def
/PaintBlack{{1 exch sub}settransfer gsave newpath clippath 1 setgray fill grestore}def
/od{(Rvd\\001\\001\\000\\000\\177) fg copy pop txpose
1 0 mtx defaultmatrix dtransform exch atan/pa exch def
newpath clippath mark
{transform{itransform moveto}}{transform{itransform lineto}}
{6 -2 roll transform 6 -2 roll transform 6 -2 roll transform
{itransform 6 2 roll itransform 6 2 roll itransform 6 2 roll curveto}}
{{closepath}}pathforall newpath counttomark array astore/gc xdf pop ct 39 0 put
10 fz 0 fs 2 F/|______Courier fnt invertflag{PaintBlack}if
statusdict/processcolors known{statusdict begin processcolors end 4 eq{/4colors true def}if}if}bdf
/cd{}bdf
/op{/sfl false def systemdict/currentcolorscreen known{dcfreq dcrot/dcspf load dmfreq dmrot/dmspf load dyfreq dyrot/dyspf load
dkfreq dkrot/dkspf load setcolorscreen}{freq rot/spf load setscreen}ifelse savescreen
/ns false def/pm save def}bdf
/cp{not{userdict/#copies 0 put}if ma 0 gt{{t1 the{exit}if}loop}if{/copypage load exec}{/showpage load exec}ifelse pm restore}bdf
/px{0 3 1 roll tp tt}bdf
/psb{/us save def}bdf
/pse{us restore}bdf
/ct 40 string def
/nc{currentpoint initclip newpath gc{dup type dup/arraytype eq exch/packedarraytype eq or{exec}if}
forall clip newpath moveto}def
/kp{ct 0 2 index length 2 index 39 2 index put getinterval copy cvx exec mx3 currentmatrix pop}bdf
end
LW 1 eq userdict/a4small known not and{/a4small
[[300 72 div 0 0 -300 72 div -120 3381]
280 3255
{statusdict/jobstate (printing) put 0 setblink
margins
exch 196 add exch 304 add 8 div round cvi frametoroket
statusdict/jobstate (busy) put
1 setblink}
/framedevice load
60 45{dup mul exch dup mul add 1.0 exch sub}/setscreen load
{}/settransfer load/initgraphics load/erasepage load]cvx
statusdict begin bind end readonly def}if
md begin/bigs[lnop userdict/letter known{/letter load}{lnop}ifelse userdict/legal known{/legal load}{lnop}ifelse userdict/a4 known{/a4 load}{lnop}ifelse userdict/b5 known{/b5 load}{lnop}ifelse 
lnop lnop lnop /tab load/a3Size load]def
/smalls[lnop userdict/lettersmall known{/lettersmall load}{userdict/note known{/note load}{lnop}ifelse}ifelse
userdict/legal known{/legal load}{lnop}ifelse userdict/a4small known{/a4small load}{lnop}ifelse 
userdict/b5 known{/b5 load}{userdict/note known{/note load}{lnop}ifelse}ifelse lnop lnop lnop /tab load/a3Size load]def end
systemdict/currentpacking known{setpacking}if
{currentfile eexec} ( %endeexec) ok userdict/stretch known not and checkload
373A767D4B7FD94FE5903B7014B1B8D3BED02632C855D56F458B118ACF3AF73FC4EF5E81F5749042B5F9CF1016D093B75F250B7D8280B2EACE05A37037F7BDF6E12226D7D4E2DF2C52FAFD5FD40FE72A0D3AC4BD485D8369D4C87636E920D1DAF222D92155A9CB1667E715F0B82799B37CC8F5B32B74B39CF494536DC39C7EF04A7BCB29E2CEC79073CADCCFB23B4AA1363F876F5121B618071B7B4EB1E5DE75FAA2368A3E5DB2B198623AFE92AE9484270FE7F57A850E88C0D3EEA156611C91D8E480D4370B025CCA6929A2BF40AD3D01B2CB7EE6DFB46E12A830542337F7819B67F9765210F76DB06F34DA5B13A11759305C582E16D2B854939F6D9121F2A4F285282F5DCD3D15896D121E3D6F5BE79E087451BB0ED233CDBEF090D3B4AC2DC34B97E70C61D95FB072B8C12D2ABD843520949A39DCF99E2C1AA8FBCD025E47E0A82A8D96E75BAF40F52AD402495BBD4DE0F356C8B14E764874E639C9F045A0D1908EC6456EB6C5B8A6F826192F767EF2C55A21C58F5F9CC1F59247B55F2387828C7FE89D5E7D8484D1BC86CB6673BDBE4FE17DD9BDE95224FE645136F41330BF155A4DDE1B0A32233BF471CE58FBC660DC7E641B0A0D30018454E2191C414A3011FF3FED1C0D88FE1FF9F75DCC456D097947226FBEC92509146D3A4CFFC0471B31C53222ED9DD88566F60F6C0D705AD79DACF53B070026F083ED28B5CF757AAA0A169F6F320A75E9D2ED50ABD939AF85B6346C2ADB25D168F10508E1516D194C635E6B187FADEA0829DBF0390C0F003F0265E215BC96CA3CC13D4A8E01570BE193CA75A620728CD275ACF1986EFFB3A13419FE55EA7C4467B7E7EEDC1FC29C9F8C46A557D2CCDB914EF7B93E7530D555DFC2398AFC68CAD991F062EF85BAA1884EC166C7C5DF8543666D8C41BE267D706BD1588F1F662F705CAE4D29DC38EF66BFAA89470D8A099B6F1B4587F7B024412276106FCD3EB5AE17A5D1DF1781992DC40EA0A992F706F701304CEA9D9073E7A74F1E687D81C3E5841D31CF86855BAAAD9B5D30317C75150A857C6B114735315CDD1AEF36C26BBB0645499406DEE2F24B3B1C72FEC97C7BA31AA2CDAB25418BB1DC4C7E4757F1D625087B0FD0300C03A65F2A72CE734925735277E034CDCF599129679F70CC8B66E03878851DB75041F275E1E5761F3EC753BE1359CA364A22047AE4886217F9259FE19FF5B116E8019B98B143114B313E8BEF87EC949D85C82E0812E6F50525E73890AF362CC8EE8A85F4197E6AC18638EF12E56A808D439AF1BFD363F140314BF4E534485C42F1856688CC35288E8D770120A420FB9F1FCF8AE8BD6D6156CC23E6C51119FE4DE1B68C9DF3487E9974BF9ED31F8D3CE93FF101867319F2FF492D5D398B4F09A66F2F55BCAB34B99173B7EE89039D00DD21A7B3A52E9F028F8301B5FC12D409412E064513BC579AAC498F577EA8ECD1FE3E42DC3CC320786C7B00194FEDF344402C33FC492D4BA86992B01683F440220FFE756BC88A94223D316078D69D33560E8EAB76B24CB7AA4320CF435593D76F624324ABE00B5587A4F283C725EA24567133F25F472B5E2E4474DDB5A16AC5F2DF32350395D3E3892FE361F4D5C9A610C654C9227614FBBAFF3356A90A2266E00F66234061075491571A65616211257F160000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
cleartomark
 %endeexec

{currentfile eexec} ( %endeexec) ok userdict/smooth4 known not and checkload
F94E00EE41A71C59E5CAEED1EDBCF23D1DBA1EE99B9BB356492923BD8B1BA83A87CEB0E07377A31FD6241E814681118E17DC7CACE570399506E6E441B871B6043831BD03EFC11DBBD8001EE2FF8CFBD485065D455A2E15AC36F1A84AD8789FA6461199C7CD14CB9FD64D4B06452B7FC0A8FC263F70F1CCB893295D4DE70ADAB771C0F84396FA98C60B11DA02ABA157298DF0A23621853BEF167443A985ADC09BEFFD51CB4D29179E2B34609EF38A49DA61F4BFC256A3DE0732D7D29754A194857B9C9E9971227AA1DD0611FBB10E44E5FF66C062D9C24ED3290529330BC317825E876929582DB0E39B9FC5EFD20CC1D4F94920EB9C534D0DA90DE70D25BC7287319CF28602B3F46633C242CAFC8905E960317E3C2FA20AB8DB06ADBAF292FC7BA2CA14EE65DF28B99CC11666B70AD33E8E1D57D63D4B89ECC615AE5747C1CA752C833D8D6DE54CD4A0350B44310555CE3BD2C615ADD27B634CDB350AF3A432CE78AACD2909A5B586F666CD87919A36DB1CBE86B3CE281DFD01CD7E1B8A18A4B415CECBFF79A5C4390A15EA77D14D6BE12BAB5A8268C3F286D0590060647CABED674443CD258F11415E866AB330A251691B61F2422A61AFE59B6B4FBDCF85ED9BA0F8E483C034089E6877FF5923698D3A0DC0EED6B9CFD32DF0839BC4EA5F6D1FCB6DD0920391E57E84745131D02D100179F4E0A68EC0A5FF6680A6F463D038B04AF63FFA13D743B995A26A743C26D387209023C91DE43DF047A16F328AC9DDC08573B38BE9EA341EA16C78EC32F3A1B36B90D95A50610F4D050EC1C33497F3F3A81A1B4C8BEF0BA84EE2FAA32DC112DAC490AF53E1749C4A0D866CAF7B893E52383B0D38065C333FB122B700D7246F7EE87D942AE3DB5C1DD77E9E76C80CC5AD63D28DFED0E229CE604673F78CD47F258FDF5BF3A3EAEC5C9BC8E482D8DBA9D268A35DA8C095A690679ED2123E8B8F5E4826FA3B199EAA5D482D4B6AA86572E387CECEB7149C8947F41D6339328A748A17F8C4AD3B0555F1E409450BA0C564F1F488BB5096EB003568D4D5EF6489897E27409547D0EE4487D30184793B0F27BD265A64BDB3EA6761569DA955620C612E718677B77D6D81B999C6298877AFE0D1D6F6F358377A8BD2402F669C64B972B3A065EF7DD4BDEFFFE17E63DB8898FA6E69166B710AAD6BA2EA9AF61E4B8C8701638D4D6E4DFFFC192AEF6BC027095C4C72D748979675BA29FAF61E75343E14E61034602E5A79CD2519796ED6A9CC4EDEA46A9B59D4A807E786B5EE46F25B0360BC8E7C12D723122CDEEF247C9776F4C99C8EBED6828AA19744B5ADF0D07D95D98B3072372388D41B0FAB1CCE2775170679575ECDCA13B22A17FE9C6605C3445F58F1A829512DAB6C528F83580C8AA53C35D605F626F5AD0B7FC1EA87D69A835E3F53A1F450FB0AF42A5772F89D92A50D10F15BDBDA409F50C0B8AB93FE8A16D029DD8BB5C480D1466735ED4D9CAF637E5ECD6C2ECB6BF3B3EFBEE7AB936D2C568E3009D156B87CACB1FB3A48A70BC91B2EC35CC9147FFB1A524E2B2F2E4E2C1B12F1C1C63768BB95CD62FEC01CBA79B9FA282DD4DF49990F27FF8EE4E2DDE2F0ACD83BC9D4BE0090192C7A799967EC4DC2D63C0835E22D4C4B366D7FDCF3A05A4B53DF780F986EF25C79B665D5C00EFF7F17C0BB6D544F9D83A7FDAC47D9C5683A656011374253C918FF6EA64749DD971B2300DD5320033E01EC591F6318CCE94CE2B81C04322EC52B624E50643B52391CCD2AB56396A2AD8E2D3CA61B80D9D4CC363B2DF7863526958CDF3497E36648406C317E58EC563E7C26149A2A3C643ADFB39A8DD92974C6D2A2A9D7B71CDF3FEBBF32BB02E7B45CF53AAEAD5E963A4AA4AF9A149A08A4EC303D5F2369977E93F54897EEAD31B06C5845D63F49D65F8E5573962241A57CCD717CE6CA8C784A11192943616EA059B51BC38429E18D0121FCBB6FBD5D909B0D89E616C66DEF6A0F165A7030BD911A1B120468329CBB006C8D37720E531CF31E878CB4AAAC137633675C3D546F5162487AB35F470C042BDEB945E0F2532BF92AA6FD53434440221ECD3533A7AA89900CB19EFE2CD872DF8B7969AF0D3B72BF31DC5DD69CA6460966F61AB17CB507964098DBA3AF122EEC3128A9BAFE1034493F372B36BD1351205E9043A67C544402D8BCE24358C8A5CE33867A00794CF7097D59C88279A11EE9C854E7E7AAE881F9828C569D208F5F33375F59E9A3818CFA38AAD0CBFBA32F9F44A8BB79DE4C40E3886457C16DA4A27953AA1E99472E35F2323F0BAA5E37DC28CBA46FEFB73B190016055ADD4D27615D748499A0E1C4B8C7EC339C1C4D95A813A85918A8D01EEB485DDCDCEA6EA3F2C2A9D85C139CD90CCB352634F9AFE836BCAC0C274E352BA2071B5269D5DE4CCDE3FF990CBA974980C7332AE1545A9C60D5D1459D3AE95C1AC065733AF14FADB440A110DD539563B8D850CD0704C52F3F7CCCB53630D776560CBD22D8FF08F5B354487A171AEC15F5F54DE9CAB668BCAC573E788D92762EF63E76087005F4AC2D02E0CAC173C11BE62ACE5DC4D3374F2F9746C9981E125FF9AB8CAE76D13039E2C54DFD708E028A619EA1ED78E6B46F06DF0D0B74BBEDD8C190C7C0CEBDE8F7A4888CC36575313478DD2CFE392E9BB7B2416955D44B7024A3BA43FBF37293B386D64746D7748895411D243FAEC50638F2AA33337D7FA018ADDAC5835A0DDFAE99AD6299DFB4CA6872C59853E3AC12FC9E3D26629C5B49CF844C87B3C4BFBE3074E3A1CE6984758C20C661084381CD6B4582D84F19C0000B5FC0DCB42B567E396031601C095D7016283EBE5F13CD8A3A374A74DDBBABD36081149F8BC242085F2F7297CC97FD3B8BAD206D8AC9707A39ECCC7963B522E08DA391A1EF12DD4D746DBDDDCC0834F88160CF189A9645567CEC2F023A571AF0DFD15DB85B744C28C000DF53B05F8F210841F6E87A04F20C777B7C0BE6182BE2E90226E5301A12532A745F2FAAA81637CF11B78CD2B99A4D18B862D6C5DBD31793FB16A2D9AAD376D4484D75AA833D0068B1D34DB74E3302480854E3B5484D8A47E39A89A2FA927BC3641EA7F8E004FDE4C2F08D40D99F1ACB47CAF6887629BF6DFE12968D297596D28CE0CF148B12E7DCB49FB94F5ADBD214C3A6CE1E249831BA9EB8A189F2CE1ABE39A7B537253E369A508A2AF2ADB9463F9B56BBBFF31D535FF997F537C6675C196E7ECBD493F652FA7CC6D9C1CA3379BFDB5AF7513C6E834054494296B91A6EE800114363D5D5D0759F41B4DECB653B9DE3E94583579EF549ED5F3FAFB12661ABC0C57A332406517ED3454EDED34B386C60F78DC976266E0EAF54FC245FB0E3EFC8016236436B599C1C97A8C5E0AC8F7836161873C71F01ED9CC25C236420F41FD8277993D3959205912FA0927B59E3DAE7377D82079447D6E41EE5AEC0DFFF79AF8F4ED47F17EE708FEA45877860D56F8CBCE65A061E8E1CA4A5FBAF0E13429A7F0ADB6F178FA449F46CC539BBC0107E3A53B1C362A04B20E6D721E7E6E1E4976A11DDC98C7614D22B53DFBB6DAE533AC9BE882021A735C30DAA4A44AED09F49A390E8CFF59BD9C30667AF21B03EC5CEBD5C2C3AA2769E8D714191A48E7DDF50B13D1560E82EFB65FCE601AE9E8C351FBA1DED80B7351314E7F9F9A784BFE3759B7E322A84E7B51F9DC5F5D9C8050CD79B27C0A4B0DD68A3C27A948AD6858E35B960D2DEA838C479CAEA83B1A912174ACB2100E55E7A14892D7A9B3711FF0B20065C1995B49E1F23464A92DD140642E3A7B1973849E64D1A3CF60000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000
cleartomark
 %endeexec

%%EndProcSet\n"))


(defun insert-document-setup ()
  (insert "%%BeginDocumentSetup
md begin
F sgd
svsc

T T 0 0 781 538 -30 -28 811 566 100 72 72 3 F F F F T T T F psu\n")
  (insert "\(" user-mail-address  " ; document: " pshand-bname "\)jn\n")
  (insert "0 mf
od
%%EndDocumentSetup\n"))


(defun insert-font ()
  (insert "%%BeginFont: Joepie
bu fc
30 -9 -10 52 36 bb
256 array /CharData mdf
<002809000000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
002809080000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
002809000000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
002809080000
020A0E08031E
021C0B0E0A0A
040C09161418
04050A121323
040D0F1C141B
040C141C0F19
021B0B08060D
02080B0C091E
0208080C0B20
041A0B100F0C
040E09141314
02060B08060B
0219090C0C03
020C0B080304
04090C120F1F
04080A121017
020C10120514
040A09121216
040C09121216
02090B120D15
040C0B121015
040A0A12121B
020209120D1D
040A09121018
04040A12101C
020C0D080412
020408080717
040B0B141116
040C0B16140B
040B0B141018
020A0D120C1C
04090B1C191F
040A091A1C1C
0408071A1E20
040A0B1A1A1E
040E0D201E1A
040C0916181C
040E0916171A
04010B1C1C27
060A0C20201E
040E060E141A
0401070E1327
040E0A1C1D1A
040C0916181C
060C07262B1C
060C0520261C
04080D1E1C20
040A05161C1C
040A0B1E1E1E
040A091A1D1E
040C0914171C
040E07161A1A
060E091E1F1A
040E071A1D1A
060A0B2C2C1E
040808181B20
040409161722
040C08181A1C
020A080A0A1D
040A0610121E
0208090A0A1F
041C0714160A
040505161903
021B0C0C060D
040C08141713
040C0916181C
040C08121412
040D0916181B
040C09141615
0202090C0D26
04040814171B
040A0916181E
020C090A0D1B
0202090A0C23
040A0814171E
020C080A0D1C
060C09222412
040C08161910
040C08141611
04000816191C
04020914171B
040A090E1014
040A09101213
040C090C0F1B
040C09161812
040A09121412
040C09181A10
040C08121511
04020812151C
040C08101312
040A050A0F1E
020A1114031C
0208070A0E20
04140A141409
FF28FFFF0000
040A071A1B1C
040A091A181C
040A0B1A171E
020F0F160E16
040A0F201A1E
04080D1E1A20
040E0B1E1D1A
040C0814171B
040D0714171B
040D0714171A
040D0814171A
040D0814171B
040D0814171B
04000912141C
040C0914151C
040C0914161C
040C0914151C
040C0914151B
020B070A0D1C
020B070A0D1C
020B080A0D1C
020B070A0D1A
040C0616191A
040B0714171D
040B08141719
040B0814171A
040B08141718
040B08141718
040B0716191B
040B0716191B
040B0716191C
040B08161919
040A0912111C
02180C0E090C
04050712141F
040A0816151C
04080A121220
020F0A120E10
040408181522
04040816191D
040A0D201A1E
040A09201C1C
06190B24210E
021C0B0C0708
0221090E0D03
040809141415
040C121E1619
04080F1E1920
04130F1A1309
040C0B141216
04090914141E
040A0914131B
040209161823
04030A161519
020B0B120E19
0407091A1921
040A0B1E141E
040C0914140F
0401090C0F25
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
040A0B14121A
04010910101B
FF28FFFF0000
040A0614181C
040A09141618
0407090E1221
040D0A141510
002800160000
04080A141119
04080614151A
040A0B1E1804
FF28FFFF0000
040A071A1B1D
040A071A191D
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
041408141303
061409262404
041A0B120F0B
021D0B120D0A
021E0E0A050A
021E0B0A060A
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
040600060F1C
FF28FFFF0000
020C0A0C0B16
020D080C0C17
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
02140B0A0506
02040908060E
FF28FFFF0000
0608092A291E
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
040A0B1C181B
060C071E221C
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
021D0C0E0C08
021E090E0C06
0421060E1105
FF28FFFF0000
021F0A0A0506
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
FF28FFFF0000
041E09101107
>
/CharTable mdf
255<
00000000
00000000
80018000
60030000
38060000
0E1C0000
03F00000
01C00000
01800000
00000000
00000000
>cdf
250<
0000
0000
6000
7800
7800
F800
F800
7000
0000
0000
>cdf
248<
00000000
00000000
F8000000
3F800000
03F80000
007E0000
00038000
00000000
00000000
>cdf
247<
0000
0000
0030
0060
38E0
7FC0
C300
8000
0000
0000
>cdf
246<
0000
0000
0C00
1E00
3F00
3380
60C0
4040
C020
0010
0000
0000
>cdf
241<
000000000000
000000000000
008000000000
00C000000000
03C000000000
038000000000
000000000000
000000000000
000000000000
000000000000
010000000000
010000000000
010007C00000
03001FE00000
070038700000
070030300000
0F0060300000
1B00C0700000
3300C0600000
6300C0C0C000
C300C3818000
030046030000
03007C020000
0300F0060000
0180F00C0000
018198180000
01C30C300000
00CE06300000
00FC03E00000
007801C00000
000000000000
000000000000
>cdf
240<
00000000
00000000
00004000
0003C000
00038000
000E0000
000C0000
00080000
00180000
0E187C00
1F98FC00
3FDCFF00
3FFFFF00
7FFFFE00
7FFFFC00
FFFFF800
FFFFF000
FFFFF000
FFFFF000
FFFFF000
FFFFF000
FFFFFC00
7FFFFC00
7FFFFF00
3FFFFF00
1FFFFE00
0FFFFE00
07FFF800
03F3F000
00000000
00000000
>cdf
228<
000000000000
000000000000
03C010000000
07C0F0000000
1C7330000000
383F20000000
303040000000
2030C0000000
6030C0000000
C01080000000
C01180000000
C03100000000
406300000000
604200000000
60C40E000080
71843F01E080
3F0CE183B180
1E0880CE1F00
001180FA0600
003100460600
0023004C0200
0047004C0300
004B004C0300
00B300CC0300
00E300CC0200
01C1818C0200
03C1830C0600
0380830C0C00
0600FE061800
1C007C03F000
180000000000
300000000000
000000000000
000000000000
>cdf
226<
0000
0000
2000
1800
1800
0C00
0C00
0C00
0C00
0C00
0C00
1800
1000
3000
6000
8000
0000
0000
>cdf
225<
0000
0000
7000
E800
F800
F800
F000
F000
0000
0000
>cdf
221<
0000
0000
8000
E000
3000
1C00
0E00
0700
0380
01C0
01C0
00E0
0030
0030
0030
0060
00C0
0180
0300
0200
0600
0C00
1800
3000
4000
0000
0000
>cdf
220<
0000
0000
0060
0040
00C0
0180
0300
0600
0C00
1C00
3000
7000
6000
C000
4000
6000
3000
3000
1800
1C00
0E00
0600
0300
0100
0000
0000
>cdf
218<
00000000
00000000
00020000
00060000
000E0000
000C0000
000C0000
00180000
00180000
00100000
00300000
00200000
00200000
00400000
00C00000
00C00000
01800000
01800000
03000000
03000000
06000000
06000000
0C000000
1C000000
18000000
38000000
30000000
70000000
60000000
C0000000
00000000
00000000
>cdf
213<
0000
0000
2000
1800
0C00
0400
0400
0C00
1800
3000
6000
8000
0000
0000
>cdf
212<
0000
0000
0800
3000
6000
C000
C000
C000
6000
3000
1000
0800
0000
0000
>cdf
211<
0000
0000
0040
1020
0810
0C18
0C18
1818
3010
4030
8060
0040
0000
0000
>cdf
210<
00000000
00000000
00060000
04180000
18300000
30600000
60C00000
C0C00000
C0C00000
C0C00000
C0400000
40200000
20000000
00000000
00000000
>cdf
209<
000000000000
000000000000
FFF000000000
0FFFC0000000
000FFFFE0000
000007FFF000
000000000000
000000000000
>cdf
208<
00000000
00000000
FFC00000
7FFF8000
007FE000
00000000
00000000
>cdf
204<
00000000
00000000
00000200
00038C00
0007F800
001CF000
00380000
00200000
00000000
00000000
00080000
00060000
00030000
00078000
0007C000
000CE000
001CF000
00303000
00303E00
003FF800
00FF1800
00400C00
00C00C00
01800600
01800600
03000300
06000300
1C000380
30000380
60000380
E0000700
00000000
00000000
>cdf
203<
00000000
00000000
00300000
00180000
000E0000
00038000
0003C000
0000E000
0000F000
00001800
00060800
00030000
00038000
00078000
000EC000
000C6000
00182000
00302000
00703000
01FC3000
00FFF800
01807F80
03800E00
07000200
06000200
0E000300
1C000100
38000180
600000C0
C0000060
C0000060
00000000
00000000
>cdf
201<
00000000
00000000
80180600
E03C0700
E07C0700
703C0E00
00000000
00000000
>cdf
200<
00000000
00000000
00100000
00180000
E00C0000
30060000
18060000
0C070000
0E018000
0E01C000
0780C000
03806000
00E07000
00E03000
00703800
00303800
00607000
00606000
01C0C000
0180C000
03018000
06070000
0C060000
380C0000
30080000
00100000
00200000
00600000
00000000
00000000
>cdf
199<
00000000
00000000
00010000
00620000
00C60000
018C0000
03180000
06180000
0C300000
18600000
10600000
70C00000
60C00000
E1C00000
61C00000
30E00000
38600000
18300000
1C100000
0E080000
070C0000
03060000
01C30000
00C30000
00438000
00418000
00008000
00000000
00000000
>cdf
197<
00000000
00000000
00003800
0000F000
0C038000
3F870000
70FE0000
603C0000
C0000800
80001000
00003000
03802000
0FE06000
38F06000
701CC000
600F8000
60078000
00030000
00000000
00000000
>cdf
196<
00000000
00000000
0001C000
001F8000
00380000
00600000
00600000
00400000
00400000
00C00000
00C00000
00C00000
00C00000
00CC0000
00980000
01F00000
0F800000
0D800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
03000000
02000000
06000000
8C000000
78000000
00000000
00000000
>cdf
195<
00000000
00000000
00000400
00001800
0003F000
00078000
00060000
00040000
000C0000
000C0000
600C0000
F00C0000
300C0000
18180000
18180000
18180000
08100000
08300000
0C300000
0C600000
0C600000
0C400000
0DC00000
07800000
07000000
02000000
00000000
00000000
>cdf
194<
00000000
00000000
03018000
0F07C000
188C6000
10C83000
20701800
60300C00
40200600
C0200300
C0000300
C0000300
40000300
40000300
60000300
60000600
20000C00
30000800
10001800
18003000
0C006000
0C00C000
06018000
03070000
010C0000
00980000
00F00000
00600000
00400000
00400000
00000000
00000000
>cdf
192<
00000000
00000000
00C00000
00E00000
00C00000
00000000
00000000
00000000
00C00000
00C00000
00600000
00600000
00C00000
01800000
03000000
06000000
1C000000
38000000
60000000
60040000
C0060000
C0020000
C0010000
E0030000
70060000
380C0000
1C3C0000
0FF80000
07E00000
00000000
00000000
>cdf
191<
00000000
00000000
06000000
1F000000
318C0000
609F0000
60B38000
60E18000
40418000
C040C000
C000C000
C000C000
C000C000
C0018000
40030000
60060000
600C0000
30180000
30100000
18300000
18600000
08400000
0CC00000
0C800000
05800000
03000000
03000000
02000000
00000000
00000000
>cdf
186<
00000000
00000000
00060000
001C0000
00F80000
01F00000
01800000
03800000
03800000
07000000
07000000
07000000
07000000
07000000
07000000
07000000
07000000
07000000
07000000
07000000
07000000
0F000000
0F000000
0F000000
0F000000
0F000000
0F000000
0F000000
0E000000
0E000000
0E000000
0E000000
0E000000
0E000000
0E000000
8C000000
FC000000
F8000000
30000000
00000000
00000000
>cdf
185<
00000000
00000000
00004000
7C003000
FF003000
CF806000
CDC0C000
0CFF0000
0C1F0000
0C030000
0C030000
0C030000
0C030000
0C010000
0C018000
3800D000
30006000
00000000
00000000
>cdf
184<
00000000
00000000
1FC00000
7FFC0000
EE0F0000
83038000
0301C000
0180C000
01806000
01802000
01803000
01803000
01803000
01803000
01803000
01803000
01806000
0180E000
0181C000
018F8000
03FC0000
0FE00000
03000000
03000000
03000000
03000000
06000000
06000000
06000000
06000000
06000000
06000000
00000000
00000000
>cdf
183<
00000000
00000000
000FF800
003FFE00
00F00700
03C00180
0F000080
07000000
03C00000
01E00000
00780000
001E0000
00070000
0001C000
0000E000
00007000
0000F800
0001C000
00038000
00060000
000C0000
00380000
00700000
00E00000
01C00000
03800080
0F000180
1C000180
38000180
70000300
E0000300
FC000600
7FFC0C00
03FFF800
0003F000
00000000
00000000
>cdf
182<
0000
0000
0F00
3F80
40E0
8070
0038
0018
000C
000C
000C
000C
038C
0FEC
1C3C
301C
201C
600C
600C
600C
600C
6008
6018
6030
3060
1FC0
0F80
0000
0000
>cdf
181<
00000000
00000000
0600C000
0600C000
0600C000
0600C000
06008000
06018000
06018000
0E018000
0E018000
0E018000
0E038000
0E038000
0E038800
1E079800
1B0DF000
19F8E000
10F04000
30000000
30000000
30000000
30000000
60000000
60000000
40000000
80000000
00000000
00000000
>cdf
180<
00000000
00000000
08000000
0C030000
0C038000
00000000
00000000
00000000
00000000
04030000
0C030000
0C030000
18030000
38030000
30030000
70060300
F0060600
301C0C00
301C0C00
303C1800
186C3000
1C6C6000
0CCC6000
078CC000
030D8000
000F0000
000E0000
003C0000
007C0000
00CC0000
018C0000
030C0000
030C0000
03180000
03300000
01E00000
00C00000
00000000
00000000
>cdf
179<
00000000
00000000
38000000
0E000000
03800000
00C00000
00600000
00300000
001C0000
00070000
0003C000
00018000
00030000
00060000
001C0000
00780000
00C00000
01800000
07000000
0E000000
18000000
00000000
000FE000
1FFF0000
7FE00000
00000000
00000000
3FFFC000
FFFF8000
00000000
00000000
>cdf
178<
00000000
00000000
00018000
00070000
000C0000
00180000
00700000
01C00000
07000000
0C000000
38000000
60000000
C0000000
E0000000
30000000
18000000
0E000000
07000000
01C00000
00E00000
00380000
001C0000
00070000
00000000
00000000
3FFFF000
0FFFE000
00000000
00000000
0FE00000
07FF8000
001FE000
00000000
00000000
>cdf
177<
00000000
00000000
01000000
03000000
03000000
03000000
03000000
03000000
03000000
03000000
FFF80000
FFFF0000
03000000
03000000
03000000
01800000
01800000
01800000
01800000
01800000
00000000
00000000
FFFFC000
0FFFC000
00000000
00000000
>cdf
176<
00000000
00000000
000F0000
003FC000
3C60E000
7EC06000
C1806000
C180C000
61FF8000
7F1F0000
1E000000
00000000
00000000
>cdf
175<
00000000
00000000
00000300
00000600
00000400
001FEC00
00783800
00E03C00
03806600
06006200
0E004300
0C00C300
0C018180
0C018180
18030180
18020180
18060180
180C0180
18080180
18180180
18300300
18700300
18E00200
09800600
0D000C00
0F000800
07003800
0780F000
0DFFC000
0C7F0000
18000000
30000000
60000000
C0000000
00000000
00000000
>cdf
174<
00000000
00000000
00FF0000
03C1C000
0700E000
1C003000
30001000
70001800
60001800
60000C00
60000C00
C0000C00
C0000C00
C0000C00
C0000C00
C0000C00
C0000C00
C0001800
C0001800
C0001000
40003000
60006000
70004000
3801C000
1C078000
0FFE0000
03F80000
00000000
00000000
>cdf
173<
00000000
00000000
00040000
000C0000
000C0000
000C0000
00180000
001FF000
07FFC000
3FF80000
78300000
00600000
00600000
00C00000
00FFE000
3FFFC000
FFC00000
01800000
01800000
03000000
03000000
06000000
04000000
00000000
00000000
>cdf
172<
0000
0000
E038
6038
C030
0000
0000
>cdf
171<
0000
0000
0600
1C00
3000
6000
4000
C000
C000
8000
0000
0000
>cdf
170<
000000000000
000000000000
000F20000000
01F82C700000
FF803EF80000
FF00638C0000
6300630C0000
330063040000
130063060000
1B0063060000
0B00C3060000
0B00C3060000
0B00C3068000
0E0183028000
060101038000
000000030000
000000000000
000000000000
>cdf
169<
00000000
00000000
0000F800
0003FC00
003F1E00
00F80700
03800180
06000180
1C0000C0
300000C0
3007C0C0
601FF0E0
60301820
60200C30
C0600430
C0C00030
C0C00030
C0C00030
C0C00030
C0C02030
C0402030
C0606030
6030C020
603F8060
380E01C0
3C000300
03001E00
03C03C00
00FFE000
00FF0000
00000000
00000000
>cdf
168<
00000000
00000000
00078000
000FC000
000CE000
000C7000
007E1800
00FC0C00
07C00E00
0F800700
0C060300
1C9F8300
30B0C180
30C0C180
70C1C180
E0C38080
C0FF00C0
C0FC00C0
C1C800C0
C1CC00C0
C30E01C0
C3030180
E2038300
6201C300
30000700
30000E00
38003C00
1C007800
0E00E000
0F03C000
07FF8000
01FC0000
00000000
00000000
>cdf
167<
00000000
00000000
00070000
003F8000
0C70C000
1EC04000
1E804000
1700C000
37018000
36030000
360C0000
661F0000
66018180
C6008300
0600C600
0600CC00
0600D800
0600F000
0600E000
0600C000
0E038000
0E0E0000
0C180000
0C200000
0C000000
0C000000
08000000
08000000
10000000
10000000
10000000
00000000
00000000
>cdf
166<
00000000
00000000
00060000
00038000
01F0C000
07AFE000
3FBDF000
5F5C3000
4FFC3000
5FFC3000
BFFC1000
FFFC1000
7FFC1000
7FFC1800
7FF81800
7FF81800
FFF81800
FFF80800
7FF80800
7FF80800
3FF80800
00580800
00180800
00180800
00181800
00181800
00181800
00181000
00183000
00182000
00186000
00186000
0018C000
001CC000
000F8000
00078000
00000000
00000000
>cdf
165<
0000
0000
3000
4F00
9FC0
FFE0
FFF0
7FF8
FFF8
FFF8
FFF8
FFFC
9FE4
AFE4
4FD8
7FF0
2700
1E00
0000
0000
>cdf
164<
00000000
00000000
00F00000
01FC0000
03060000
06020000
06000000
07000000
03200000
01C00000
07F00000
1C1C0000
30060000
60030000
C0018000
C0018000
C0018000
E0070000
700C0000
1E780000
07E00000
03F00000
06380000
081C0000
000C0000
00060000
00030000
00018000
0400C000
0400C000
06018000
03038000
01FE0000
00F00000
00000000
00000000
>cdf
163<
00000000
00000000
00018000
0007C000
001C6000
00303000
00601800
00600800
00400000
00C00000
00C00000
00C00000
00C00000
00C00000
00C00000
00C00000
1FC00000
03FE0000
01FFC000
01C00000
01800000
0180E000
03013000
07001800
06001800
0C001000
18002000
3FC0E000
F07FC000
601F0000
00000000
00000000
>cdf
162<
00000000
00000000
00180000
00300000
00200000
00600000
00600000
00600000
00600000
01F00000
07F80000
0CCC0000
08C40000
18C00000
30C00000
70800000
F1801000
31803000
31806000
31804000
3180C000
31818000
19818000
19830000
0D8E0000
07980000
01F00000
01800000
01000000
01000000
03000000
03000000
02000000
00000000
00000000
>cdf
161<
0000
0000
3C00
6600
6300
4180
C180
C080
C080
E180
FF00
4C00
6000
3000
0000
0000
>cdf
160<
00000000
00000000
00C00000
00C00000
00C00000
00C00000
00C00000
00C00000
00C00000
00C00000
01C00000
E1C00000
FFE00000
07FC0000
01CF8000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01800000
01000000
00000000
00000000
>cdf
159<
00000000
00000000
01000000
0300C000
0300C000
00006000
00000000
00000000
00000000
00000000
00008000
04008000
0E018000
3B018000
63010000
43010180
C3030300
02030600
06030400
06030C00
06030C00
06070800
06059800
060D9800
06189000
03F0F000
01E06000
00000000
00000000
>cdf
158<
00000000
00000000
00100000
00780000
01CE0000
03870000
06018000
0400C000
08006000
00002000
00000000
00000000
00000000
00004000
00004000
0C018000
1E018000
33030000
63030180
C3030300
03030600
06030600
06030400
06070C00
06070C00
06051800
06091800
07191000
03F1F000
01E0E000
00000000
00000000
>cdf
157<
00000000
00000000
01800000
00C00000
00F00000
00300000
00180000
000C0000
00060000
00000000
00000000
00000000
00008000
00018000
00018000
3C018000
7E018000
43018180
C3010300
03030600
06030600
06030C00
06070C00
06070C00
06059800
060D9800
07089000
03F8F000
01F06000
00000000
00000000
>cdf
156<
00000000
00000000
00007000
0001C000
00030000
000E0000
00380000
00200000
00E00000
00000000
00000000
00004000
0E00C000
1E00C000
13018000
33018000
63030000
43030180
C3030300
03030600
06030600
06070C00
06070C00
060F0C00
06099800
06199800
07309000
03E0F000
01C06000
00000000
00000000
>cdf
155<
00000000
00000000
00C0C000
01E38000
11370000
0E1E0000
04000000
00000000
00000000
00F00000
03FC0000
071E0000
0E070000
38018000
7801C600
D801EC00
1801B800
18018000
18018000
0C018000
0C018000
06010000
06030000
03060000
01FC0000
00F00000
00000000
00000000
>cdf
154<
00000000
00000000
0C040000
0C0E0000
100E0000
00000000
00000000
00000000
00000000
00F00000
03FC0000
079E0000
0F070000
3F038000
7203C600
C6017C00
06013000
06018000
0E018000
0C018000
0C010000
06030000
06060000
030C0000
01F80000
00F00000
00000000
00000000
>cdf
153<
00000000
00000000
00C00000
00600000
00F00000
01BC0000
030E0000
06070000
18018000
1000E000
00000000
00F00000
03FC0000
079E0000
1F070000
3C038000
7803C600
D8016C00
18013800
18018000
18018000
0C018000
0C010000
06030000
06060000
030C0000
01F80000
00F00000
00000000
00000000
>cdf
152<
00000000
00000000
06000000
07000000
01C00000
00780000
003C0000
00070000
00000000
00000000
00F00000
03FC0000
071E0000
1C030000
38038000
7801C600
D8016C00
18013800
18018000
18018000
08018000
0C010000
0C030000
06060000
030C0000
01F80000
00F00000
00000000
00000000
>cdf
151<
00000000
00000000
00010000
00020000
000C0000
00180000
00300000
00E00000
03800000
00000000
00000000
00000000
00000000
00000000
00F00000
03FC0000
079C0000
1C060000
38030000
7801C600
D8016C00
18013800
18018000
18018000
1C018000
0C030000
0C020000
0C060000
060C0000
07F80000
01F00000
00000000
00000000
>cdf
150<
00000000
00000000
00000600
00E00C00
01B83800
010E6000
0603C000
0C000000
00000000
00000000
00000000
00000000
00000000
0F1C0000
3FBF0000
31B38000
61E18000
C1C18180
81818180
01818300
01818300
01818600
03018600
07018400
06018C00
0600CC00
06007800
06003000
00000000
00000000
>cdf
149<
0000
0000
1800
3830
3878
0070
0000
0000
0000
0000
0C00
1800
1800
3000
3000
7008
F018
B030
3060
3060
30C0
30C0
3180
3180
1B00
1E00
1C00
0800
0000
0000
>cdf
148<
0000
0000
0300
0780
0D80
18C0
3040
2060
2030
4030
0018
0000
0C00
1800
1800
3000
3000
7008
F018
B030
3060
3060
30C0
30C0
3180
3180
1B00
1E00
1C00
0800
0000
0000
>cdf
147<
0000
0000
8000
E000
7000
1800
0C00
0700
0780
0000
0000
0000
0C00
1800
1800
3000
3000
7008
F018
B030
3060
3060
30C0
30C0
3180
3180
1B00
1E00
1C00
0800
0000
0000
>cdf
146<
0000
0000
0018
0030
0060
01C0
0300
0E00
1C00
0000
0000
0000
0C00
1800
1800
3000
3000
7008
F018
B030
3060
3060
30C0
30C0
3180
3180
1B00
1E00
1C00
0800
0000
0000
>cdf
145<
00000000
00000000
301C0000
380C0000
30000000
00000000
00000000
00000000
00000000
03E00000
0FF00000
1C180000
30180000
60180000
40300000
C0600000
C0C00000
C7800000
FE001800
F0001000
60003000
30006000
3000C000
18018000
0C030000
0C060000
061C0000
03F80000
00E00000
00000000
00000000
>cdf
144<
00000000
00000000
03E00000
0FF80000
300E0000
20030000
40010000
C0000000
00000000
00000000
03E00000
07300000
0C180000
180C0000
300C0000
200C0000
60380000
60E00000
67C00000
FE001800
60003000
60006000
3000C000
30018000
10030000
18060000
0C0C0000
06380000
03F00000
01C00000
00000000
00000000
>cdf
143<
00000000
00000000
38000000
0C000000
07000000
01800000
00C00000
00400000
00000000
00000000
01C00000
07E00000
0C300000
18180000
30080000
30380000
60E00000
C3800000
CE000000
F8000C00
F0001800
60003000
30006000
3000C000
18018000
08030000
0C060000
060C0000
03F80000
01E00000
00000000
00000000
>cdf
142<
00000000
00000000
001C0000
00780000
00C00000
00800000
03000000
02000000
00000000
00000000
07800000
1FC00000
30600000
60300000
60300000
40600000
C1C00000
C7800000
9E000000
F0000800
C0001800
C0003000
60006000
2000C000
30018000
18030000
080E0000
0C180000
07F00000
03C00000
00000000
00000000
>cdf
141<
00000000
00000000
03E00000
0FF00000
18180000
30080000
60000000
60003000
40006000
C000C000
C0008000
C0018000
C0018000
C0030000
40030000
60020000
30060000
1F0C0000
07F80000
00E00000
00C00000
00400000
00780000
001C0000
00040000
04060000
06060000
03060000
030C0000
01F80000
00000000
00000000
>cdf
140<
00000000
00000000
00F00000
01F80000
02880000
00880000
00D80000
00700000
00000000
00000000
01E08000
03F98000
060D8000
0C070000
18060000
10060000
30060000
70060000
E0060600
E00C0400
600C0C00
600C1800
201C1000
30163000
30366000
18366000
18624000
0FC3C000
07818000
00000000
00000000
>cdf
139<
00000000
00000000
00004000
00008000
0E030000
339E0000
40F00000
00000000
00000000
00000000
01E08000
03F98000
060D8000
0C070000
18060000
10060000
30060000
70060000
E0060600
E00C0400
600C0C00
600C1800
201C1000
30163000
30366000
18366000
18624000
0FC3C000
07818000
00000000
00000000
>cdf
138<
00000000
00000000
00080000
040C0000
1C0C0000
18000000
00000000
00000000
00000000
01E08000
03F98000
060D8000
0C070000
18060000
10060000
30060000
70060000
E0060600
E00C0400
600C0C00
600C1800
201C1000
30163000
30366000
18366000
18624000
0FC3C000
07818000
00000000
00000000
>cdf
137<
00000000
00000000
00F00000
031C0000
0E060000
18038000
00008000
00000000
00000000
01E08000
03F98000
060D8000
0C070000
18060000
10060000
30060000
70060000
E0060600
E00C0400
600C0C00
600C1800
201C1000
30163000
30366000
18366000
18624000
0FC3C000
07818000
00000000
00000000
>cdf
136<
00000000
00000000
10000000
0C000000
04000000
06000000
03000000
01800000
00000000
00000000
01E08000
03F98000
060D8000
0C070000
18060000
10060000
30060000
70060000
E0060600
E00C0400
600C0C00
600C1800
201C1000
30163000
30366000
18366000
18624000
0FC3C000
07818000
00000000
00000000
>cdf
135<
00000000
00000000
00030000
000E0000
00180000
00300000
00400000
00000000
00000000
00000000
01E08000
03F98000
060D8000
0C070000
18060000
10060000
30060000
70060000
E0060600
E00C0400
600C0C00
600C1800
201C1000
30163000
30366000
18366000
18624000
0FC3C000
07818000
00000000
00000000
>cdf
134<
00000000
00000000
38003800
38003800
30001800
00000000
00000000
20000000
60001800
60001800
60001800
C0001800
C0001800
C0001800
C0003000
C0003000
C0006000
C0006000
C000C000
C001C018
C001C030
C003C060
6006C060
600C60C0
30186180
3C303300
0FE01E00
07C00C00
00000000
00000000
>cdf
133<
00000000
00000000
0F878000
1FCFC000
307CF800
303C3C00
30300C00
30300600
30300300
30300300
30300300
30300300
30300700
60300E00
C031FC00
C073F800
C0FF0000
C0FF0000
C0C30000
C0C30000
C0C18000
C0C0C000
C0C0E000
C0C07000
E0C03000
E0C03800
70C00E00
30C00E00
30C00300
30C00300
38C00380
1CC00180
0F8000C0
0F000040
00000000
00000000
>cdf
132<
00000000
00000000
0F000100
1F800100
30E00300
30700300
30700300
30700300
20580300
204C0300
20CC0380
60CC01C0
E0CE00C0
E0C700C0
C0C300C0
C0C300C0
C0C381C0
C0C1C380
C0C0C300
C0C0C300
C0C0C300
C0C0C300
C0C06300
C0C06300
C0C03300
C0C03300
C0C03F00
C0C01F00
C0C00F00
C1C00F00
FF800F00
3F000600
00000000
00000000
>cdf
131<
0000
0000
000C
0018
0030
00E0
0380
0000
1FC0
09FC
181C
1000
3000
2000
7F00
6FE0
E000
C000
C000
8000
C000
F000
3E00
07F0
0000
0000
>cdf
130<
00000000
00000000
003FC000
00FFE000
07F03000
0FC03800
1F000C00
3B000E00
33000000
73000000
63000000
63000000
C3000000
C3000000
C3000000
C3000000
C3000000
C3000000
C3000000
C3000000
C3000000
C3000000
C3000000
C3000000
C6000000
CC000200
7C000C00
3C001C00
07003000
03807000
01FFE000
00FF8000
00000000
00000000
>cdf
129<
00000000
00000000
000F0000
001F8000
0030C000
0030C000
0010C000
001F0000
00000000
00000000
000C0000
000E0000
000E0000
000F0000
00338000
00338000
0040C000
00C0C000
00FFC000
00FFE000
0101E000
03003000
04003800
0C003800
10000C00
30000C00
60000E00
60000E00
C0000600
C0000700
00000000
00000000
>cdf
128<
00000000
00000000
00C00000
00E0E000
00E0E000
0070F000
00000000
00000000
00000000
00000000
00030000
00030000
00038000
0003C000
000CE000
000CF000
00383000
00303800
0037FC00
003FBC00
00C00C00
00C00E00
03000700
03000300
0C000380
0C000180
3C0000C0
380000C0
E0000060
C0000060
00000000
00000000
>cdf
126<
00000000
00000000
00001000
00003000
00004000
1C018000
3E030000
63060000
819C0000
00F00000
00600000
00000000
00000000
>cdf
125<
0000
0000
0780
1FE0
3870
0030
0030
0030
0030
0030
0030
0070
0060
0040
0060
0030
0018
001C
0070
0060
0040
00C0
00C0
00C0
00C0
00C0
00C0
0060
0020
0030
C030
7060
1DE0
0F80
0000
0000
>cdf
124<
0000
0000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
C000
C000
C000
C000
C000
C000
C000
C000
C000
C000
C000
C000
4000
0000
0000
>cdf
123<
00000000
00000000
00F80000
03FC0000
07020000
06000000
0C000000
0C000000
0C000000
0C000000
0C000000
0C000000
18000000
18000000
70000000
E0000000
38000000
0C000000
0C000000
0C000000
0C000000
0C000000
0C000000
0C000000
18000000
18000000
30000000
30000000
30040000
183C0000
1CF00000
07800000
00000000
00000000
>cdf
122<
00000000
00000000
03020000
078E0000
0CFC0000
186C0000
30080000
30180000
60300000
C0306000
0060C000
0040C000
00C18000
00818000
01830000
01030000
03060000
078C0000
0CF80000
18300000
00000000
00000000
>cdf
121<
00000000
00000000
0600C000
0600C000
0C00C000
1C00C000
3C018000
38038000
78038000
D8031800
18031000
18073000
180F2000
181B6000
1C334000
0E62C000
07C78000
00070000
00060000
000E0000
001E0000
00360000
00660000
00C60000
01860000
01860000
01840000
018C0000
00F80000
00700000
00000000
00000000
>cdf
120<
00000000
00000000
0C000000
1E030000
1B078000
330C8000
31180000
61B00000
C1B01800
01A03000
01E02000
00C06000
00C04000
00C0C000
01C08000
63418000
36670000
3C3E0000
18180000
00000000
00000000
>cdf
119<
00000000
00000000
18004000
38084000
6C186000
4C186000
CC186040
08183980
18183F00
18183600
18183000
18383000
18683000
18682000
0C4C2000
0ECC6000
0386C000
01838000
00000000
00000000
>cdf
118<
00000000
00000000
18180000
3C1C0000
6C0C0000
4E0E0000
C6073000
0607E000
0606C000
0C020000
0C030000
0C030000
0C030000
0C030000
0C060000
040C0000
06180000
06300000
03E00000
01800000
00000000
00000000
>cdf
117<
00000000
00000000
00004000
0E00C000
1E00C000
13018000
33018000
63030000
43030000
C3030300
03030600
06030600
06070C00
06070C00
060F0C00
06099800
06199800
07309000
03E0F000
01C06000
00000000
00000000
>cdf
116<
00000000
00000000
00100000
00300000
00300000
00700000
00F00000
01B00000
01300000
03300000
06200000
06200000
0C600000
18600000
18400000
30400000
60400000
C6460000
0F4C0000
11580000
10F00000
1FC00000
0CC00000
00800000
00800000
00800000
00800000
00800000
00800000
00000000
00000000
>cdf
115<
00000000
00000000
00C00000
01C00000
07600000
1C600000
30300000
60304000
C010C000
00198000
00198000
001B0000
000F0000
000E0000
000C0000
00180000
00300000
00600000
18C00000
0F800000
07000000
00000000
00000000
>cdf
114<
00000000
00000000
0C100000
1E300000
33F00000
31900000
60300000
60600000
C0410000
80C20000
01860000
030C0000
03180000
03180000
03300000
06300000
06200000
06600000
06400000
06C00000
07800000
03000000
00000000
00000000
>cdf
113<
00000000
00000000
00004000
00FCC000
03FEC000
0F06C000
3C03C200
78018600
D8018400
10038C00
30078C00
30058800
300D9800
30099800
30119800
38333000
18633000
0FC23000
07062000
00066000
00064000
000CC000
000CC000
000D8000
000D0000
000D0000
000F0000
000E0000
00060000
00000000
00000000
>cdf
112<
00000000
00000000
00380000
0C7C0000
1EC60000
36C20000
67830000
67030180
C6010300
06018600
06018400
0E018C00
0C008800
0C00D800
0C007000
0C002000
0C000000
0C000000
0C000000
18000000
18000000
18000000
18000000
18000000
18000000
18000000
18000000
18000000
10000000
10000000
00000000
00000000
>cdf
111<
00000000
00000000
00F00000
03FC0000
079E0000
1F070000
3C038000
7803C000
D8016C00
18013800
18018000
18018000
0C018000
0C010000
06030000
06060000
030C0000
01F80000
00F00000
00000000
00000000
>cdf
110<
00000000
00000000
0E0E0000
1F1F0000
33B30000
21A18000
61E18000
C1C18180
81818180
01818300
03018300
03018600
07018600
06018400
0E018C00
0C00CC00
0C007800
0C003000
00000000
00000000
>cdf
109<
000000000000
000000000000
000000E00000
060001F00000
0F1E01300000
19BF03180000
31E382180000
31C0C6180000
6180C4180000
C180CC183000
8180C8186000
0180D8186000
0180F018C000
0180E0198000
0380C0190000
0300C01B0000
0601800E0000
0601800E0000
060180040000
060180000000
000000000000
000000000000
>cdf
108<
0000
0000
0700
0F80
08C0
18C0
18C0
18C0
30C0
30C0
30C0
3080
3180
3300
3600
3600
3C00
3800
3008
7018
F030
B020
3060
3040
30C0
30C0
3080
1980
1F00
0E00
0000
0000
>cdf
107<
00000000
00000000
00300000
00F80000
018C0000
018C0000
030C0000
030C0000
03180000
03300000
03600000
03C00000
03800000
03000000
07000000
0F000000
1B1E0000
323F0000
66618000
C6C18600
86C30C00
078E1800
07383000
07E03000
07806000
06C06000
0C60C000
0C20C000
0C318000
18330000
181E0000
180C0000
00000000
00000000
>cdf
106<
0000
0000
0080
0300
0300
0000
0000
0600
0600
0600
0E00
1E00
1600
3600
6610
C630
8660
06C0
0780
0700
0600
0E00
1E00
3600
6600
6600
C600
C600
C600
C600
C600
C600
CC00
CC00
CC00
7800
3000
0000
0000
>cdf
105<
0000
0000
0600
0600
0200
0000
0000
0000
0000
0000
0000
0C00
1800
1800
3000
3000
7008
F018
B030
3060
3060
30C0
30C0
3180
3180
1B00
1E00
1C00
0800
0000
0000
>cdf
104<
00000000
00000000
01E00000
03300000
03180000
02180000
06180000
06180000
06180000
06300000
06200000
06600000
06C00000
07800000
07000000
0E000000
1E000000
36000000
66000000
C6000300
06380600
067C0600
06C60C00
07830C00
07030C00
06031800
06031800
06031800
0C033000
0C033000
1801E000
1800C000
00000000
00000000
>cdf
103<
00000000
00000000
00300000
00F88000
038D8000
06070000
0C030000
18070000
38070000
780F0000
D81B0600
98330C00
18331800
18633000
0CC36000
0783C000
03078000
000F0000
003B0000
00630000
00C30000
01830000
03060000
06060000
06060000
060C0000
06180000
03F00000
01E00000
00000000
00000000
>cdf
102<
0000
0000
0380
07C0
0CC0
08C0
18C0
18C0
1980
1980
1900
1B00
1A00
1C00
1C00
1800
3800
3800
5800
D818
1C70
1FC0
1E00
1B00
1980
18C0
18C0
1860
1860
1860
1860
1860
1860
1860
1860
18C0
18C0
0C80
0F80
0700
0000
0000
>cdf
101<
00000000
00000000
03E00000
0FF80000
1C1C0000
300C0000
200C0000
600C0000
60380000
40E00000
C7800000
DE000400
F0000C00
E0001800
60003000
30006000
3000C000
18018000
08030000
0C060000
079C0000
03F80000
00E00000
00000000
00000000
>cdf
100<
00000000
00000000
00004000
00004000
00004000
0000C000
0000C000
0000C000
0000C000
00018000
00018000
00018000
01E18000
07F18000
0C1B8000
181F0000
180F0000
30060000
70060000
F0020300
B0060600
30060400
300E0C00
300B0C00
101B1800
18319800
0C61B000
07C1F000
0380E000
00000000
00000000
>cdf
99<
00000000
00000000
01F00000
07380000
0E0C0000
0C040000
18000000
30000000
70000000
F0001000
30003000
30006000
30004000
3000C000
30018000
18018000
18030000
0E0E0000
07380000
01F00000
00000000
00000000
>cdf
98<
00000000
00000000
00600000
00F00000
01980000
010C0000
030C0000
030C0000
03180000
02100000
06300000
06600000
07C00000
07000000
06000000
0E000000
1E000000
36000000
66008000
C600C300
0601FE00
0601B800
02018000
03018000
03018000
01010000
01830000
00C60000
007C0000
00380000
00000000
00000000
>cdf
97<
00000000
00000000
01E08000
03F98000
060D8000
0C070000
18060000
10060000
30060000
70060000
E0060600
E00C0400
600C0C00
600C1800
201C1000
30163000
30366000
18366000
18624000
0FC3C000
07818000
00000000
00000000
>cdf
96<
0000
0000
1800
3000
3000
6000
6000
C000
C000
C000
C000
6000
3000
1800
0400
0000
0000
>cdf
95<
00000000
00000000
FF800000
3FFFFF00
00FFFF80
00000000
00000000
>cdf
94<
00000000
00000000
00700000
01F80000
038C0000
06060000
1C030000
3001C000
6000E000
C0003000
00001800
00000C00
00000000
00000000
>cdf
93<
0000
0000
3FC0
FFC0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
00C0
0180
0180
3F80
7F00
0000
0000
>cdf
92<
00000000
00000000
E0000000
70000000
30000000
18000000
18000000
0C000000
0C000000
0E000000
06000000
06000000
03000000
03000000
01800000
01800000
00C00000
00C00000
00400000
00600000
00600000
00200000
00300000
00180000
00180000
00180000
000C0000
000E0000
00060000
00070000
00038000
0000C000
00000000
00000000
>cdf
91<
0000
0000
7800
7F00
63C0
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
C000
C000
C000
C000
C000
C000
C000
C000
E000
7C00
7F00
0000
0000
>cdf
90<
00000000
00000000
0C01F000
3E07B000
630C2000
41F86000
8070C000
00018000
00030000
00060000
00040000
000C0000
00180000
00180000
00300000
07E00000
00FC0000
00C00000
00800000
01800040
030000C0
03000180
06000180
06000300
0C780600
09F80600
1B0F0C00
1E039800
3C00F000
70007000
00000000
00000000
>cdf
89<
00000000
00000000
00002000
00002000
20002000
70006000
C8006000
CC006000
CC00C000
8C00C000
0C00C000
0C00C000
0C00C000
0C00C000
0C01C000
0C01C000
0403C000
0602C000
0602C600
0604CC00
021CD800
03F0F000
01C0E000
0001C000
0003C000
0006C000
000CC000
0018C000
0030C000
00608000
00418000
00C30000
00860000
008C0000
00D80000
00700000
00000000
00000000
>cdf
88<
00000000
00000000
1F000000
31C00C00
30601E00
30303300
30387100
18186000
0C18C000
000CC000
000C8000
000D8000
000F8000
000F0000
000F0000
00070000
00060000
00060000
00060000
00060000
00060060
000600C0
00060080
000F0180
400F0180
C01F0180
C0198100
C0318300
6030C300
20604200
30C06600
1C803C00
0F801800
03000000
00000000
00000000
>cdf
87<
000000000000
000000000000
100000000000
780000000000
4C0000000000
8C0000000000
0C0030000000
0C00300000C0
0C00600001A0
0C0060000100
0C0060000300
0C0060000300
0C00C0000300
0C00C0000300
1800C0000300
1800C0000300
1001C0000600
3001C0000600
3003C0000C00
3003C0000C30
3006C0001860
300CC00030C0
3018C00020C0
3018C0006180
3030C000C380
1830C0018700
1860C0019C00
0CC0C003F800
0F80600FE000
0600703E0000
00003FF00000
00000FC00000
000000000000
000000000000
>cdf
86<
00000000
00000000
30000100
78000380
CC0003F0
8C000360
0C000300
0C000300
0C000300
0C000300
0C000600
0C000600
0C000600
0C000600
0C000C00
06000C00
06000C00
06000C00
06001800
06001818
06001070
060011E0
06003F80
06007C00
0300F000
0187C000
01FF0000
00780000
00000000
00000000
>cdf
85<
000000000000
000000000000
300003000000
780003000000
CC0003000000
8C0003000000
0C0003000000
0C0003000000
180006000000
180006000000
180006000000
300006000000
300006000000
300006000000
30000C000000
30000C000000
300018000000
300018000000
300030000000
300070060000
3000700C0000
3000F0180000
1801B0180000
180318300000
0C0618600000
0F0C0CC00000
03F807800000
01F003000000
000000000000
000000000000
>cdf
84<
00000000
00000000
0F800100
3FC00200
607E0C00
803FF800
000CE000
000C0000
000C0000
000C0000
00060000
00060000
00030000
00030000
00030000
00030000
00030000
00030000
000300C0
00030180
02030300
02030300
02030600
03030C00
01833800
01C7F000
00FF8000
003C0000
00000000
00000000
>cdf
83<
00000000
00000000
00F00000
01FC0000
031E0000
02030000
06018000
0600C000
06004000
06004000
06008000
03000000
03800000
00F00000
003C0000
000F0000
00038000
00018000
0000C600
00006400
00006C00
00003800
00003000
20007000
70006000
9C00C000
06038000
030F0000
03FC0000
01F80000
00000000
00000000
>cdf
82<
00000000
00000000
020FE000
033FF800
03F80C00
03C00600
07000300
0F000300
1B000300
3B000300
33000600
33000C00
63003800
6300E000
43038000
C30F0000
C3FC0000
C3FE0000
C3070018
C6070030
CC038060
4C01C0C0
7C00C080
7C006180
38006180
38006100
38003300
38003300
38001E00
38001E00
30000C00
10000000
00000000
00000000
>cdf
81<
00000000
00000000
001FE000
007FF000
01C03800
03001C00
06000C00
0C000C00
0C000C00
18000C00
18000600
10000600
30000300
30000300
30000300
30000300
60000300
60000200
60000600
C0000600
C000060C
C000060C
C0000C18
C0000C18
C0001830
603C3020
304E7060
3083C040
1C01E0C0
0E037180
03FE1F00
00FC0E00
00000000
00000000
>cdf
80<
00000000
00000000
03FC0000
0FFF0000
3C01C000
7C007000
4C003800
8C001800
0C000C00
0C000C00
0C000C00
0C000C00
06000C00
06000800
06001800
06003000
261FE000
1FFF8030
0F800060
060000C0
06000180
06000300
06000E00
06003800
0600F000
0603C000
0C0F0000
0C3C0000
0FF00000
1FC00000
00000000
00000000
>cdf
79<
00000000
00000000
00FE0000
01FF8000
0700C000
0E00C000
0C006000
18003000
30003000
30003000
70003000
60001800
40001C00
C0000C00
C0000C00
C0000C00
C0000600
C0000600
C0000700
C0000730
C00006E0
C0000600
60000600
60000600
30000400
30000C00
30001800
18003000
0C006000
0C00C000
06018000
060F0000
03FC0000
00F00000
00000000
00000000
>cdf
78<
000000000000
000000000000
1E0380000000
3F07E0000000
631C38000000
81B81C000000
00F00C000000
00E006000000
00C002000000
00C003000000
00C003000000
01C003000000
018003000000
018003000000
030003000000
030003000000
030003800000
030001800000
030001800400
030001800800
030001801800
060001803000
060000806000
0C0000C0C000
0C0000C0C000
0C0000C18000
0C0000C30000
0C0000C60000
0000007C0000
000000380000
000000000000
000000000000
>cdf
77<
000000000000
000000000000
070180000000
1F83C0000000
7186600F0000
E0C6703F8000
00C43030C000
00CC1860C000
00CC18606000
00C808C06000
00F80CC02000
00F006803000
00F007803000
00E007803000
00E003803000
00E003003000
00E003003000
00E003006000
00E003006000
00E003006060
00E0030060C0
00E0030060C0
00E003006180
00E003006180
00E003006100
00E003006300
00C001006200
00C001006600
00C001003C00
00C001001800
000000000000
000000000000
>cdf
76<
00000000
00000000
00300000
00780000
004C0000
00CC0000
00CC0000
00CC0000
00CC0000
00D80000
00F00000
00E00000
01C00000
03C00000
0EC00000
38C00000
60C00000
C0C00000
00C00000
00C00300
00C00600
00C00C00
00C00C00
00C01800
0FE01800
1FF03000
31B83000
331C6000
1E07C000
0C038000
00000000
00000000
>cdf
75<
00000000
00000000
18300060
3C7800C0
664C0300
43CC0600
838C1C00
010C3800
000C6000
000CC000
000F8000
000F0000
00098000
0018C000
00186000
00186000
00183000
00183000
00183018
00301830
00301830
00201860
00600C60
20C00CC0
218004C0
330006C0
1E000380
0C000300
00000000
00000000
>cdf
74<
00000000
00000000
00040000
180E0000
3E1B0000
23198000
63318000
C1B18000
C1E18000
80C18000
80018000
00030000
00030000
00060000
00060000
00060000
00060000
00062000
00066000
0006C000
00078000
00070000
00060000
000C0000
001C0000
003C0000
006C0000
00CC0000
018C0000
030C0000
060C0000
0C0C0000
0C0C0000
0C0C0000
0C0C0000
0C180000
0C180000
06180000
03980000
03F80000
00F00000
00000000
00000000
>cdf
73<
00000000
00000000
000C0000
1C3C0000
36660000
63C60000
41860000
80060000
80060000
00060000
00060000
000C0000
000C0000
000C0000
000C0000
000C0000
000C0000
00180000
00183000
00186000
00186000
0018C000
80198000
801F0000
C03C0000
60700000
3FC00000
1F800000
00000000
00000000
>cdf
72<
000000000000
000000000000
000001800000
000007C00000
1C100C600000
3E3808600000
67D818600000
439818600000
801818400000
801818C00000
001818800000
001819800000
00181F000000
00181C000000
001878000000
001BF0000000
007E30000000
00F830000000
019830000000
031860030000
061860060000
0618600C0000
0C18600C0000
181860180000
181860180000
181060300000
183030300000
186030600000
0FC018600000
070008C00000
00000F800000
000007000000
000000000000
000000000000
>cdf
71<
00000000
00000000
00180000
00FF0000
03C78000
0600E000
0C003000
18001000
30000000
30000000
60000000
E0000000
C0000000
C0000000
C0000000
C0000000
C0000000
60008000
60018000
30038030
30078060
180F80C0
081D8380
0C310700
07E31C00
03C37000
0003C000
001F8000
007F0000
01E30000
03830000
07030000
06060000
0C060000
0C040000
0C0C0000
0C180000
0C100000
06300000
03E00000
01C00000
00000000
00000000
>cdf
70<
00000000
00000000
0801FC00
0C0FF800
3FFE0000
FFF00000
8C000000
0C000000
0C000000
04000000
06000000
06000000
03000000
0303F000
03FF8000
03FC0000
03000000
02000000
06000000
06000600
04000C00
0C001800
0C003000
0C00E000
18078000
180F0000
1FF80000
3FE00000
00000000
00000000
>cdf
69<
00000000
00000000
03FC0000
07FF0000
1C038000
3C01C000
3000C000
30004000
30000000
38000000
1E000000
0E000000
03F00000
01F80000
03800000
0E000000
18000000
30000000
60000000
60000300
C0000600
C0000600
C0000C00
C0000C00
C0001800
70003000
3800E000
1E01C000
07FF0000
01FC0000
00000000
00000000
>cdf
68<
00000000
00000000
07FE0000
1FFF8000
3060C000
30C06000
30C03000
19801800
19800800
0D000C00
07000400
07000600
03000300
03800300
06C00300
06600300
06300300
061C0300
0C06030C
0C038318
1C01C670
18007FC0
30003F00
3E001800
3B803000
71F0E000
C03FC000
C01F8000
00000000
00000000
>cdf
67<
00000000
00000000
007F8000
01FFE000
07007000
0C003000
18001800
38001800
30001800
60003000
60006000
6011C000
C01F0000
C0060000
C0000000
C0000000
C0000000
C0000000
C0000000
C0000000
C00000C0
C0000180
C0000180
60000300
60000300
60000600
30000C00
38003800
1E00E000
0F03C000
01FF0000
00FE0000
00000000
00000000
>cdf
66<
00000000
00000000
03FC0000
0FFF0000
1B818000
2300C000
0300C000
0300C000
0300C000
06018000
06038000
060F0000
041C0000
04700000
0CF00000
0C1C0000
0C0F0000
0C078000
0C00C000
0C00600C
0C003018
0C003818
0C001C30
0C000C30
0C000CE0
0C000DC0
18001F00
18001E00
30003800
30007000
F800C000
7F018000
47FF0000
00FE0000
00000000
00000000
>cdf
65<
00000000
00000000
001E0400
007F0C00
01C1C800
0300E800
06003800
0C003000
18003000
10003000
30003000
60006000
60006000
60006000
C0006000
C0006000
C000C000
C000C030
C000C060
C000C0C0
4000C180
6000C300
2001C300
3001C600
3803C600
1802CC00
0C06CC00
06046C00
039C7800
00F83000
00000000
00000000
>cdf
64<
00000000
00000000
001F0000
00FFE000
07C0F000
0F001800
1C000C00
18000C00
30000700
70000300
60000180
60000180
400E0180
401F8180
C030C180
C070C180
C0C04180
C1C0C180
C180C180
8300C180
C301C180
C3018100
43018300
61018300
2186EE00
30CE7C00
107C0000
18000000
0C000000
06000000
0300E000
01FFC000
007E0000
00000000
00000000
>cdf
63<
0000
0000
1E00
3F00
6180
C0C0
8060
8020
C030
6030
1030
0020
0060
00C0
0180
0300
0200
0400
0C00
0800
1800
1000
3000
3000
1800
0000
0000
0000
1800
1C00
0000
0000
>cdf
62<
00000000
00000000
C0000000
60000000
30000000
18000000
0C000000
06000000
03000000
01800000
00C00000
00600000
00380000
001C0000
00060000
00030000
00070000
001C0000
00700000
01C00000
07000000
0E000000
38000000
60000000
C0000000
80000000
00000000
00000000
>cdf
61<
00000000
00000000
FFE00000
FFFFF000
001FF000
00000000
00000000
00000000
00000000
00000000
00000000
FFFFC000
FFFFC000
00000000
00000000
>cdf
60<
00000000
00000000
00018000
000F0000
00380000
00E00000
03800000
06000000
0C000000
10000000
30000000
E0000000
60000000
20000000
30000000
18000000
0C000000
06000000
03000000
01800000
00E00000
00380000
000E0000
00038000
00000000
00000000
>cdf
59<
0000
0000
1800
3800
3000
2000
0000
0000
0000
0000
0000
0000
0000
0000
0000
0000
0C00
0600
0600
0600
0600
1C00
3000
E000
8000
0000
0000
>cdf
58<
0000
0000
6000
F000
C000
C000
0000
0000
0000
0000
0000
0000
0000
0000
0000
0000
0000
8000
F000
E000
0000
0000
>cdf
57<
00000000
00000000
03F00000
0FFC0000
180E0000
38060000
60020000
C0030000
C0030000
C0030000
C0030000
C0030000
C0020000
40060000
60060000
601E0000
30340000
18640000
0FEC0000
078C0000
00080000
00080000
00180000
00180000
00100000
80300000
C0300000
70600000
1FC00000
0F000000
00000000
00000000
>cdf
56<
00000000
00000000
007C0000
00C60000
01820000
03030000
03010000
03010000
03010000
03030000
01030000
01030000
03860000
0F8E0000
18F80000
30700000
60100000
60180000
60080000
C0080000
C0080000
C0180000
40180000
60300000
3FE00000
0FC00000
00000000
00000000
>cdf
55<
0000
0000
0008
3078
78F8
CC98
8798
0710
0030
0030
0020
0060
0040
00C0
0080
3980
0FE0
03F8
0300
0300
0600
0600
0600
0C00
0800
1800
3000
6000
6000
4000
4000
0000
0000
>cdf
54<
00000000
00000000
00040000
001C0000
00300000
00E00000
01800000
03000000
06000000
0C000000
08000000
18F00000
33F80000
360E0000
6C070000
78010000
60018000
60018000
C000C000
C000C000
C000C000
C0008000
C0018000
60010000
30030000
18060000
1C0C0000
0F180000
03F00000
00000000
00000000
>cdf
53<
00000000
00000000
01FF0000
03FF0000
03000000
03000000
06000000
06000000
06000000
06000000
06000000
03C00000
00F80000
001C0000
000E0000
00030000
00030000
00030000
000E0000
E01C0000
70300000
3FE00000
0F800000
00000000
00000000
>cdf
52<
0000
0000
8018
8018
C018
C018
C018
C018
C018
FF18
3FF8
01F8
0030
0030
0070
0060
0060
0060
00E0
00C0
00C0
0080
0080
0000
0000
>cdf
51<
00000000
00000000
01E00000
07F00000
0E180000
0C0C0000
000C0000
000C0000
00180000
00300000
00600000
00C00000
00780000
001E0000
00070000
00018000
0001C000
0000C000
8000C000
C0018000
C0070000
7C0E0000
3FF80000
03F00000
00000000
00000000
>cdf
50<
00000000
00000000
07800000
1FC00000
38600000
70300000
C0300000
80100000
00100000
00100000
00300000
00300000
00200000
00600000
00C00000
01800000
01000000
03000000
06000000
06000000
0F004000
1DC08000
307F8000
303E0000
00000000
00000000
>cdf
49<
0000
0000
3000
7800
9800
1800
1800
1800
1800
1800
1800
1800
1800
3000
3000
3000
3000
3000
3000
3000
3000
3000
0000
0000
>cdf
48<
00000000
00000000
00E00000
03F80000
0E180000
180C0000
30060000
30060000
60020000
E0030000
C0030000
C0030000
C0030000
C0030000
C0030000
C0030000
C0020000
60060000
60060000
20040000
300C0000
18180000
0C180000
07F00000
01E00000
00000000
00000000
>cdf
47<
00000000
00000000
00040000
00060000
000C0000
000C0000
00180000
00180000
00180000
00300000
00300000
00200000
00600000
00600000
00C00000
00C00000
01800000
01800000
03000000
03000000
06000000
06000000
0C000000
0C000000
18000000
18000000
38000000
30000000
60000000
60000000
60000000
C0000000
80000000
00000000
00000000
>cdf
46<
0000
0000
8000
C000
E000
C000
0000
0000
>cdf
45<
0000
0000
F000
7FF0
07F0
0000
0000
>cdf
44<
0000
0000
1000
1800
0C00
0C00
0C00
0C00
0800
1800
3000
6000
8000
0000
0000
>cdf
43<
00000000
00000000
00100000
00100000
00300000
00300000
00300000
00300000
00300000
0033E000
3FFFC000
FFFC0000
00300000
00300000
00300000
00300000
00300000
00300000
00600000
00600000
00400000
00400000
00000000
00000000
>cdf
42<
00000000
00000000
30180000
18300000
0C600000
04C00000
0FFE0000
3FF80000
EC600000
86200000
06300000
03180000
03000000
01000000
00000000
00000000
>cdf
41<
0000
0000
6000
F000
1C00
0E00
0300
0180
0080
00C0
00C0
00C0
00C0
00C0
0060
0060
0060
0060
0060
0060
0060
00C0
00C0
00C0
00C0
00C0
0080
0180
0380
0300
0600
0C00
1800
7000
0000
0000
>cdf
40<
0000
0000
0700
0E00
1800
3000
2000
6000
6000
6000
6000
6000
C000
C000
C000
C000
C000
C000
C000
C000
6000
6000
6000
6000
3000
3000
1800
1800
0C00
0600
0700
0380
0000
0000
>cdf
39<
0000
0000
2000
3000
1800
1800
0C00
0C00
0C00
1800
3000
3000
6000
4000
8000
0000
0000
>cdf
38<
00000000
00000000
0F000000
1F800000
30C00000
20600000
20300000
30300000
18200000
18400000
0CC00000
05800000
0F000000
1F000000
19820000
30C20000
60660000
602C0000
C0380000
C0300000
C0300000
C0780000
60680000
61CC0000
3F0C0000
1E040000
00060000
00000000
00000000
>cdf
37<
00000000
00000000
0E03C000
3FDF8000
61F30000
60C70000
C0C60000
C0CC0000
618C0000
630C0000
3E180000
0C300000
00300000
00600000
00600000
00C00000
01C00000
03800000
0307C000
060FE000
0C3C2000
18F03000
31E03000
37603000
6CC03000
78C06000
E0F0C000
C07FC000
000F0000
00000000
00000000
>cdf
36<
00000000
00000000
01040000
03040000
030C0000
030C0000
030C0000
037C0000
03DE0000
0F0F0000
0E0D8000
1A0C8000
360C0000
260C0000
260C0000
1E0C0000
1FCC0000
07FC0000
043C0000
040E0000
8C0F8000
CC0FC000
7C0C4000
1C0C6000
0E0CE000
0FFFC000
0CFE0000
0C180000
0C180000
0C180000
0C180000
0C180000
0C100000
08100000
08100000
08100000
00100000
00000000
00000000
>cdf
35<
00000000
00000000
00430000
00430000
00C30000
00C30000
00C30000
00C30000
00C3F000
00FFF000
1FFF0000
3F870000
018C0000
018C0000
018C0000
018C0000
030C0000
030FC000
7FFFC000
FFF80000
06180000
06180000
06180000
0C180000
0C180000
0C100000
00000000
00000000
>cdf
34<
0000
0000
8300
6180
30C0
10C0
10C0
10C0
3180
6300
C000
8000
0000
0000
>cdf
33<
0000
0000
4000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
6000
E000
C000
C000
C000
C000
C000
C000
8000
0000
0000
0000
C000
C000
4000
0000
0000
>cdf
/|______Joepie bf
bn
%%EndFont\n"))



(defun insert-ending ()
  (insert "64 gr
F T cp
%%Trailer
cd
end\n"))
  


(define-key-after
  (lookup-key global-map [menu-bar edit])
  [cursive]
  '("Write by hand" . menu-bar-cursive-map)
  'spell)



(defvar menu-bar-cursive-map (make-sparse-keymap "Cursive functions."))
(fset 'menu-bar-cursive-map (symbol-value 'menu-bar-cursive-map))







(define-key menu-bar-cursive-map [mo]
  '("Outlined" . cursive-outlined))

(define-key menu-bar-cursive-map [mb]
  '("Bold" . cursive-bold))

(define-key menu-bar-cursive-map [fnt]
  '("Plain *" . cursive-plain))



(defun cursive-plain ()
  (interactive)
  (setq pshand-style "fnt")
  (define-key menu-bar-cursive-map [mo]
    '("Outlined" . cursive-outlined))

  (define-key menu-bar-cursive-map [mb]
    '("Bold" . cursive-bold))
  
  (define-key menu-bar-cursive-map [fnt]
    '("Plain *" . cursive-plain))
  (message "Joepie in plain text")
  )

(defun cursive-bold ()
  (interactive)
  (setq pshand-style "mb")
  (define-key menu-bar-cursive-map [mo]
    '("Outlined" . cursive-outlined))
  
  (define-key menu-bar-cursive-map [mb]
    '("Bold *" . cursive-bold))

  (define-key menu-bar-cursive-map [fnt]
    '("Plain" . cursive-plain))
  (message "Joepie bold faced")
  )

(defun cursive-outlined ()
  (interactive)
  (setq pshand-style "mo")
  (define-key menu-bar-cursive-map [mo]
    '("Outlined *" . cursive-outlined))

  (define-key menu-bar-cursive-map [mb]
    '("Bold" . cursive-bold))

  (define-key menu-bar-cursive-map [fnt]
    '("Plain" . cursive-plain))
  (message "Joepie outlined")
  )

(define-key menu-bar-cursive-map [10pt]
  '("10 pt" . cursive-10pt))

(define-key menu-bar-cursive-map [11pt]
  '("11 pt *" . cursive-11pt))

(define-key menu-bar-cursive-map [12pt]
  '("12 pt" . cursive-12pt))

(define-key menu-bar-cursive-map [13pt]
  '("13 pt" . cursive-13pt))

(define-key menu-bar-cursive-map [pshand]
  '("Write by hand" . pshand))

(defun cursive-10pt ()
  (interactive)
  (setq pshand-fontsize 10)
  (setq pshand-linespace 11)
  (setq pshand-numlines 65)
(define-key menu-bar-cursive-map [10pt]
  '("10 pt *" . cursive-10pt))

(define-key menu-bar-cursive-map [11pt]
  '("11 pt" . cursive-11pt))

(define-key menu-bar-cursive-map [12pt]
  '("12 pt" . cursive-12pt))

(define-key menu-bar-cursive-map [13pt]
  '("13 pt" . cursive-13pt))
  (message "Joepie set to 10 points"))


(defun cursive-11pt ()
  (interactive)
  (setq pshand-fontsize 11)
  (setq pshand-linespace 12)
  (setq pshand-numlines 60)
(define-key menu-bar-cursive-map [10pt]
  '("10 pt" . cursive-10pt))

(define-key menu-bar-cursive-map [11pt]
  '("11 pt *" . cursive-11pt))

(define-key menu-bar-cursive-map [12pt]
  '("12 pt" . cursive-12pt))

(define-key menu-bar-cursive-map [13pt]
  '("13 pt" . cursive-13pt))
  (message "Joepie set to 11 points"))

(defun cursive-12pt ()
  (interactive)
  (setq pshand-fontsize 12)
  (setq pshand-linespace 13)
  (setq pshand-numlines 55)
(define-key menu-bar-cursive-map [10pt]
  '("10 pt" . cursive-10pt))

(define-key menu-bar-cursive-map [11pt]
  '("11 pt" . cursive-11pt))

(define-key menu-bar-cursive-map [12pt]
  '("12 pt *" . cursive-12pt))

(define-key menu-bar-cursive-map [13pt]
  '("13 pt" . cursive-13pt))
  (message "Joepie set to 12 points"))

(defun cursive-13pt ()
  (interactive)
  (setq pshand-fontsize 13)
  (setq pshand-linespace 14)
  (setq pshand-numlines 50)
(define-key menu-bar-cursive-map [10pt]
  '("10 pt" . cursive-10pt))

(define-key menu-bar-cursive-map [11pt]
  '("11 pt" . cursive-11pt))

(define-key menu-bar-cursive-map [12pt]
  '("12 pt" . cursive-12pt))

(define-key menu-bar-cursive-map [13pt]
  '("13 pt *" . cursive-13pt))
  (message "Joepie set to 13 points"))

(define-key-after
  (lookup-key menu-bar-cursive-map [ ])
  [pshand-separator1]
  '("----" . nil)
  'pshand)

(define-key-after
  (lookup-key menu-bar-cursive-map [ ])
  [pshand-separator2]
  '("----" . nil)
  '10pt)



(provide 'handwrite)
