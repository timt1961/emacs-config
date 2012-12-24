;;; latin-9.el --- Latin-9 support for Emacs 20

;; Copyright (C) 1999, 2001  Free Software Foundation, Inc.

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

;; Latin-9 support back-ported from what I did for Emacs 21.1
;; (latin-9.el and elsewhere) for the benefit of euro-peans tied to
;; Emacs 20 (e.g. Debian 3.0).  Use Emacs 21 if you can.

;; This is meant to be `load'ed explictly (not `require'd), and not
;; preloaded (dumped).  There's no point in compiling it.  See also
;; latin-9-pre.el for an input method.  Obviously assumes a Latin-9
;; font, e.g. from XFree 4.  Only lightly tested.

;; Beware that Emacs 21 has a latin-9.el, so don't leave this around
;; in a directory that might cause confusion when you install 21.
;; Somewhere like /usr/local/share/emacs/20.7/site-lisp is a good bet.

;;; Code:

(require 'case-table)

;; Test if we're already set up for multibyte so as not to lose if
;; we're `require'd for unibyte syntax, for instance.  The stuff in
;; this clause comes from several files in lisp/international and in
;; european.el.
(unless (charsetp 'latin-iso8859-15)
  ;; charset
  (define-charset 142 'latin-iso8859-15
    [1 96 1 0 ?b 1 "RHP of Latin-9" "RHP of Latin-9 (ISO 8859-15): ISO-IR-203"
       "Right-Hand Part of Latin Alphabet 9 (ISO/IEC 8859-15): ISO-IR-203"])
  ;; coding systems
  (make-coding-system
   'iso-latin-9 2 ?0			; `0' for `Latin-0'
   "ISO 2022 based 8-bit encoding for Latin-9 (MIME:ISO-8859-15)."
   '(ascii latin-iso8859-15 nil nil
	   nil nil nil nil nil nil nil nil nil nil nil nil t)
   '((safe-charsets ascii latin-iso8859-15)
     (mime-charset . iso-8859-15)))
  (define-coding-system-alias 'iso-8859-15 'iso-latin-9)
  (define-coding-system-alias 'latin-9 'iso-latin-9)
  (define-coding-system-alias 'latin-0 'iso-latin-9)
  ;; categories
  (modify-category-entry (make-char 'latin-iso8859-15) ?l)
  (modify-category-entry (make-char 'latin-iso8859-15 160) ?\ )
  ;; fonts
  (setq x-font-name-charset-alist
	(cons '("iso8859-15" ascii latin-iso8859-15)
	      x-font-name-charset-alist))
  ;; This may not be completely TRT -- I only really know/remember
  ;; Mule 5 properly.
  (put-charset-property 'latin-iso8859-15 'x-charset-registry  "ISO8859-15")
  (set-fontset-font "fontset-standard" 'latin-iso8859-15  "*-iso8859-15")
  (set-fontset-font "fontset-startup" 'latin-iso8859-15
		    "-*-*-medium-r-normal-*-*-*-*-*-*-*-iso8859-15")
  ;; language environment
  (set-language-info-alist
   "Latin-9" '((charset ascii latin-iso8859-15)
	       (coding-system iso-latin-9)
	       (coding-priority iso-latin-9)
	       (nonascii-translation . latin-iso8859-15)
	       (unibyte-syntax . "latin-9")
	       (unibyte-display . iso-latin-9)
	       (input-method . "latin-9-prefix")
	       (documentation . "\
This language environment is a generic one for the Latin-9 (ISO-8859-15)
character set which supports the same languages as Latin-1 with the
addition of the Euro sign and some additional French and Finnish letters.
Latin-9 is sometimes nicknamed `Latin-0'."))
   '("European"))
  ;; not useful?
  (put-charset-property 'latin-iso8859-15
			'preferred-coding-system 'iso-latin-9)
  ;; Locale processing, perhaps a bit late.  See startup.el.
  (let ((ctype
	 (or (let ((string (getenv "LC_ALL")))
	       (and (not (equal string "")) string))
	     (let ((string (getenv "LC_CTYPE")))
	       (and (not (equal string "")) string))
	     (let ((string (getenv "LANG")))
	       (and (not (equal string "")) string))))
	(case-fold-search nil))
    ;; Match traditional Emacs-style ...8859-15 &c, and things like
    ;; de_DE@euro.  Emacs 21 does this better.
    (when (and ctype (string-match "\\(8859[-_]?15\\|@euro\\)\\>" ctype))
      (set-language-environment "Latin-9")
      (unless (or noninteractive (eq window-system 'x))
	(when default-enable-multibyte-characters
	  (set-terminal-coding-system 'latin-9)))
      (standard-display-european-internal)))

  ;; Input method.  We can't include it directly since we need to
  ;; define the coding system for the file first.  You could instead
  ;; install it in the leim directory and re-build.
  (require 'latin-9-pre))

;; The following is basically Emacs 21.1 latin-9.el.

(let ((tbl (standard-case-table))
      (set-case-syntax-offset
       (if set-case-syntax-set-multibyte
	   (- (make-char 'latin-iso8859-15) 128)
	 0)))
  ;; The differences from Latin-1 are starred on the comments below.
  (set-case-syntax 160 " " tbl)		;no-break space
  (set-case-syntax 161 "." tbl)		;inverted exclamation mark
  (set-case-syntax 162 "w" tbl)		;cent sign
  (set-case-syntax 163 "w" tbl)		;pound sign
  (set-case-syntax 164 "w" tbl)		;euro sign *
  (set-case-syntax 165 "w" tbl)		;yen sign
  (set-case-syntax-pair 166 168 tbl)	;latin letter s with caron *
  (set-case-syntax 167 "." tbl)		;section sign
  (set-case-syntax 169 "_" tbl)		;copyright sign
  (set-case-syntax 170 "w" tbl)		;feminine ordinal indicator
  ;;(set-case-syntax-delims 171 187 tbl)	;left-pointing double angle quotation mark
  (set-case-syntax 171 "." tbl)
  (set-case-syntax 187 "." tbl)
  (set-case-syntax 172 "_" tbl)		;not sign
  (set-case-syntax 173 "_" tbl)		;soft hyphen
  (set-case-syntax 174 "_" tbl)		;registered sign
  (set-case-syntax 175 "w" tbl)		;macron
  (set-case-syntax 176 "_" tbl)		;degree sign
  (set-case-syntax 177 "_" tbl)		;plus-minus sign
  (set-case-syntax 178 "w" tbl)		;superscript two
  (set-case-syntax 179 "w" tbl)		;superscript three
  (set-case-syntax-pair 180 184 tbl)	;latin letter z with caron *
  (set-case-syntax 181 "_" tbl)		;micro sign
  (set-case-syntax 182 "." tbl)		;pilcrow sign
  (set-case-syntax 183 "_" tbl)		;middle dot
  (set-case-syntax 185 "w" tbl)		;superscript one
  (set-case-syntax 186 "w" tbl)		;masculine ordinal indicator
  (set-case-syntax-pair 188 189 tbl)	;latin ligature oe *
  (set-case-syntax-pair 190 255 tbl)	;latin letter y with diaeresis *
  (set-case-syntax 191 "." tbl)		;inverted question mark
  (set-case-syntax-pair 192 224 tbl)	;latin letter a with grave
  (set-case-syntax-pair 193 225 tbl)	;latin letter a with acute
  (set-case-syntax-pair 194 226 tbl)	;latin letter a with circumflex
  (set-case-syntax-pair 195 227 tbl)	;latin letter a with tilde
  (set-case-syntax-pair 196 228 tbl)	;latin letter a with diaeresis
  (set-case-syntax-pair 197 229 tbl)	;latin letter a with ring above
  (set-case-syntax-pair 198 230 tbl)	;latin letter ae
  (set-case-syntax-pair 199 231 tbl)	;latin letter c with cedilla
  (set-case-syntax-pair 200 232 tbl)	;latin letter e with grave
  (set-case-syntax-pair 201 233 tbl)	;latin letter e with acute
  (set-case-syntax-pair 202 234 tbl)	;latin letter e with circumflex
  (set-case-syntax-pair 203 235 tbl)	;latin letter e with diaeresis
  (set-case-syntax-pair 204 236 tbl)	;latin letter i with grave
  (set-case-syntax-pair 205 237 tbl)	;latin letter i with acute
  (set-case-syntax-pair 206 238 tbl)	;latin letter i with circumflex
  (set-case-syntax-pair 207 239 tbl)	;latin letter i with diaeresis
  (set-case-syntax-pair 208 240 tbl)	;latin letter eth
  (set-case-syntax-pair 209 241 tbl)	;latin letter n with tilde
  (set-case-syntax-pair 210 242 tbl)	;latin letter o with grave
  (set-case-syntax-pair 211 243 tbl)	;latin letter o with acute
  (set-case-syntax-pair 212 244 tbl)	;latin letter o with circumflex
  (set-case-syntax-pair 213 245 tbl)	;latin letter o with tilde
  (set-case-syntax-pair 214 246 tbl)	;latin letter o with diaeresis
  (set-case-syntax 215 "_" tbl)		;multiplication sign
  (set-case-syntax-pair 216 248 tbl)	;latin letter o with stroke
  (set-case-syntax-pair 217 249 tbl)	;latin letter u with grave
  (set-case-syntax-pair 218 250 tbl)	;latin letter u with acute
  (set-case-syntax-pair 219 251 tbl)	;latin letter u with circumflex
  (set-case-syntax-pair 220 252 tbl)	;latin letter u with diaeresis
  (set-case-syntax-pair 221 253 tbl)	;latin letter y with acute
  (set-case-syntax-pair 222 254 tbl)	;latin letter thorn
  (set-case-syntax 223 "w" tbl)		;latin small letter sharp s
  (set-case-syntax 247 "_" tbl))	;division sign

;; When preloading this file, don't provide the feature.
;; Explicit `require' is used to load this for 8-bit characters.
(or set-case-syntax-set-multibyte
    (provide 'latin-9))

;;; latin-9.el ends here
