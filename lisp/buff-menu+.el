From gnu-emacs-sources-admin@gnu.org  Wed Jan 10 23:45:24 2001
X-VM-v5-Data: ([nil nil nil t nil nil nil nil nil]
	["31193" "Wednesday" "10" "January" "2001" "14:42:28" "-0800" "Drew Adams" "dadams@objectstream.com" nil "810" "buff-menu+.el     - extends buff-menu.el" "^From:" nil nil "1" nil nil nil nil nil]
	nil)
Received: from titan.asml.nl (titan [146.106.1.9])
	by nlsfts01.asml.nl (8.8.8+Sun/8.8.8) with ESMTP id XAA04511
	for <utt@asml.nl>; Wed, 10 Jan 2001 23:45:24 +0100 (MET)
Received: from pollux.asml.nl (pollux.asml.nl [195.109.200.66])
	by titan.asml.nl (8.9.3+Sun/8.9.3) with ESMTP id XAA04429
	for <tim.timmerman@asml.nl>; Wed, 10 Jan 2001 23:45:24 +0100 (MET)
Received: from fencepost.gnu.org (fencepost.gnu.org [199.232.76.164])
	by pollux.asml.nl (8.8.8+Sun/8.8.8) with ESMTP id XAA14306
	for <tim.timmerman@asml.nl>; Wed, 10 Jan 2001 23:45:23 +0100 (MET)
Received: from localhost ([127.0.0.1] helo=fencepost.gnu.org)
	by fencepost.gnu.org with esmtp (Exim 3.16 #1 (Debian))
	id 14GTzN-0004Yt-00; Wed, 10 Jan 2001 17:45:05 -0500
Received: from pophq.objectstream.com ([207.21.151.146])
	by fencepost.gnu.org with esmtp (Exim 3.16 #1 (Debian))
	id 14GTxg-0004W8-00
	for <gnu-emacs-sources@gnu.org>; Wed, 10 Jan 2001 17:43:20 -0500
Received: from exchange.objectstream.com (exchange.objectstream.com [172.16.1.250])
	by pophq.objectstream.com (8.11.0/8.11.0) with ESMTP id f0AMcO819267
	for <gnu-emacs-sources@gnu.org>; Wed, 10 Jan 2001 14:38:24 -0800
Received: by exchange.objectstream.com with Internet Mail Service (5.5.1960.3)
	id <Z7CAJZ6S>; Wed, 10 Jan 2001 14:42:32 -0800
Message-ID: <B1727F288F09D411A4AE00D0B757BC2461D737@exchange.objectstream.com>
MIME-Version: 1.0
X-Mailer: Internet Mail Service (5.5.1960.3)
Content-Type: multipart/mixed;
	boundary="---- =_NextPart_000_01C07B56.9C958820"
Errors-To: gnu-emacs-sources-admin@gnu.org
X-BeenThere: gnu-emacs-sources@gnu.org
X-Mailman-Version: 2.0
Precedence: bulk
List-Help: <mailto:gnu-emacs-sources-request@gnu.org?subject=help>
List-Post: <mailto:gnu-emacs-sources@gnu.org>
List-Subscribe: <http://mail.gnu.org/mailman/listinfo/gnu-emacs-sources>,
	<mailto:gnu-emacs-sources-request@gnu.org?subject=subscribe>
List-Id: GNU Emacs source code postings and patches <gnu-emacs-sources.gnu.org>
List-Unsubscribe: <http://mail.gnu.org/mailman/listinfo/gnu-emacs-sources>,
	<mailto:gnu-emacs-sources-request@gnu.org?subject=unsubscribe>
List-Archive: <http://mail.gnu.org/pipermail/gnu-emacs-sources/>
Content-Length: 31192
From: Drew Adams <dadams@objectstream.com>
Sender: gnu-emacs-sources-admin@gnu.org
To: "Gnu Emacs (E-mail)" <gnu-emacs-sources@gnu.org>
Subject: buff-menu+.el     - extends buff-menu.el
Date: Wed, 10 Jan 2001 14:42:28 -0800

This message is in MIME format. Since your mail reader does not understand
this format, some or all of this message may not be legible.

------ =_NextPart_000_01C07B56.9C958820
Content-Type: text/plain

Cf. files README & DEPENDENCIES, posted previously.


======================================================
E-MAIL DISCLAIMER

This e-mail message (including all documents and/or attachments) is for
the sole use of the intended recipient(s) and may contain confidential
and/or privileged information. Any review, use, disclosure or
distribution by persons or entities other than the intended recipient(s)
is prohibited. If you are not the intended recipient, please contact the
sender by reply and destroy all copies of the original message. Thank
you.

To reply to our E-mail Administrator directly, send an email to
postmaster@objectstream.com or call (925) 224-8080 and delete this
email.

OBJECTSTREAM, INC.

http://www.objectstream.com
------
Attachments are virus free!

This message has been scanned for viruses at the originating end by
Nemx Anti-Virus for MS Exchange Server/IMC
	http://www.nemx.com/products/antivirus

  


------ =_NextPart_000_01C07B56.9C958820
Content-Type: application/octet-stream;
	name="buff-menu+.el"
Content-Transfer-Encoding: quoted-printable
Content-Disposition: attachment;
	filename="buff-menu+.el"

;;; buff-menu+.el --- Extensions to `buff-menu.el'.  New bindings.=0A=
;; =0A=
;; Emacs Lisp Archive Entry=0A=
;; Filename: buff-menu+.el=0A=
;; Description: Extensions to `buff-menu.el'=0A=
;; Author: Drew Adams=0A=
;; Maintainer: Drew Adams=0A=
;; Copyright (C) 1996-2001, Drew Adams, all rights reserved.=0A=
;; Created: Mon Sep 11 10:29:56 1995=0A=
;; Version: $Id: buff-menu+.el,v 1.6 2001/01/08 22:23:20 dadams Exp =
$=0A=
;; Last-Updated: Mon Jan  8 14:23:01 2001=0A=
;;           By: dadams=0A=
;;     Update #: 724=0A=
;; Keywords: mouse, local=0A=
;; Compatibility: GNU Emacs 20.x=0A=
;; =0A=
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;=0A=
;; =0A=
;;; Commentary: =0A=
;;=0A=
;;    Extensions to `buff-menu.el'.  New bindings & fonts & menu.=0A=
;;=0A=
;;  `Buffer-menu-mouse-3-menu' popup menu added.=0A=
;;  New prefix arg options for `buffer-menu'.=0A=
;;=0A=
;;  Main new functions defined here:=0A=
;;=0A=
;;    `Buffer-menu-mouse-3-menu', `Buffer-menu-mouse-delete',=0A=
;;    `Buffer-menu-mouse-execute', `Buffer-menu-mouse-modified',=0A=
;;    `Buffer-menu-mouse-other-window', `Buffer-menu-mouse-save',=0A=
;;    `Buffer-menu-mouse-unmark'.=0A=
;;=0A=
;;=0A=
;;  ***** NOTE: The following functions defined in `buff-menu.el'=0A=
;;              have been REDEFINED HERE:=0A=
;;=0A=
;;  `buffer-menu' -=0A=
;;     1. Different help message.=0A=
;;     2. Prefix ARG =3D< 0 now means list (all) buffers =
alphabetically.=0A=
;;        (It used to mean the same as ARG > 0.)=0A=
;;        Prefix ARG >=3D 0 means list just file buffers.=0A=
;;  `Buffer-menu-execute' - Deletes windows (frame) when kills =
buffer.=0A=
;;  `Buffer-menu-mode' -=0A=
;;     1. Doc string reflects new bindings.=0A=
;;     2. mouse-face on whole line, not just buffer name.=0A=
;;  `Buffer-menu-select' - When Buffer Menu is `window-dedicated-p',=0A=
;;                         uses `pop-to-buffer' to display.=0A=
;;=0A=
;;=0A=
;;  This file should be loaded after loading the standard GNU file=0A=
;;  `buff-menu.el'.  So, in your `~/.emacs' file, do this:=0A=
;;  (eval-after-load "buff-menu" '(require 'buff-menu+))=0A=
;;=0A=
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;=0A=
;; =0A=
;;; Change log:=0A=
;; =0A=
;; RCS $Log: buff-menu+.el,v $=0A=
;; RCS Revision 1.6  2001/01/08 22:23:20  dadams=0A=
;; RCS Adapted file header for Emacs Lisp Archive.=0A=
;; RCS=0A=
;; RCS Revision 1.5  2001/01/03 17:30:07  dadams=0A=
;; RCS *** empty log message ***=0A=
;; RCS=0A=
;; RCS Revision 1.4  2001/01/03 00:32:55  dadams=0A=
;; RCS *** empty log message ***=0A=
;; RCS=0A=
;; RCS Revision 1.3  2001/01/02 23:16:47  dadams=0A=
;; RCS Protect undefine-killer-commands via fboundp.=0A=
;; RCS=0A=
;; RCS Revision 1.2  2000/11/28 19:15:53  dadams=0A=
;; RCS Optional require's via 3rd arg=3Dt now.=0A=
;; RCS=0A=
;; RCS Revision 1.1  2000/09/13 20:06:09  dadams=0A=
;; RCS Initial revision=0A=
;; RCS=0A=
; Revision 1.4  1999/08/26  09:10:34  dadams=0A=
; 1. Require when compile: def-face-const.el.=0A=
; 2. Added: buffer-menu-*-face's, buffer-menu-font-lock-keywords.=0A=
; 3. Add buffer-menu-font-lock-keywords to buffer-menu-mode-hook.=0A=
;=0A=
; Revision 1.3  1999/08/25  13:44:22  dadams=0A=
; *** empty log message ***=0A=
;=0A=
; Revision 1.2  1997/03/21  16:26:45  dadams=0A=
; Buffer-menu-execute, Buffer-menu-mouse-execute:=0A=
;   Only use kill-buffer-and-its-windows if fboundp.=0A=
;=0A=
; Revision 1.1  1997/03/21  16:09:46  dadams=0A=
; Initial revision=0A=
;=0A=
; Revision 1.21  1996/07/01  15:13:57  dadams=0A=
; buffer-menu: Prefix arg =3D< 0 sorts alphabetically now.=0A=
;=0A=
; Revision 1.20  1996/07/01  12:43:07  dadams=0A=
; 1. Added redefinition of Buffer-menu-select.=0A=
; 2. Require cl.el.=0A=
;=0A=
; Revision 1.19  1996/06/14  13:04:14  dadams=0A=
; Updated file header Commentary to mention new fns defined here.=0A=
;=0A=
; Revision 1.18  1996/06/06  12:38:38  dadams=0A=
; Update of file dependency comments (e.g. "Autoloaded from...").=0A=
;=0A=
; Revision 1.17  1996/04/26  08:51:35  dadams=0A=
; Put escaped newlines on long-line strings.=0A=
;=0A=
; Revision 1.16  1996/04/23  08:32:35  dadams=0A=
; Call undefine-kill-commands, so require misc-fns.el when compile and =
load.=0A=
;=0A=
; Revision 1.15  1996/04/05  14:03:54  dadams=0A=
; Improved Commentary:  List redefinitions.=0A=
;=0A=
; Revision 1.14  1996/03/06  08:17:06  dadams=0A=
; 1. Copyright.  2. drew-misc-19.el -> misc-cmds.el.=0A=
;=0A=
; Revision 1.13  1996/02/12  09:02:33  dadams=0A=
; Updated header keywords (for finder).=0A=
;=0A=
; Revision 1.12  1996/01/30  14:15:41  dadams=0A=
; Removed require of drew-misc-19.el.  Autoloaded.=0A=
;=0A=
; Revision 1.11  1996/01/25  12:33:31  dadams=0A=
; 1. kill-buffer -> kill-buffer-and-its-windows.  Require =
drew-misc-19.el.=0A=
; 2. Buffer-menu-mode: Put mouse-face on whole buffer line.=0A=
;=0A=
; Revision 1.10  1996/01/12  16:50:29  dadams=0A=
; 1. Added redefn of buffer-menu.  2. Added ;;;###autoload's (not =
used).=0A=
;=0A=
; Revision 1.9  1996/01/09  09:08:18  dadams=0A=
; kill-buffer -> kill-buffer-delete-frames=0A=
;=0A=
; Revision 1.8  1995/12/28  14:40:21  dadams=0A=
; 1. Added ;;;###autoload's.=0A=
; 2. Removed require of drew-misc-19.el, because autoloaded.=0A=
; 3. Buffer-menu-mouse-3-menu: Corrected by adding temp local var.=0A=
;=0A=
; Revision 1.7  1995/12/14  14:58:41  dadams=0A=
; 1. Highlight buffer line when mouse-3 menu displayed.=0A=
;    Added Buffer-menu-overlay.=0A=
; 2. mouse-3 menu is reduced to non-buffer-specifics when not on a =
buffer line.=0A=
;=0A=
; Revision 1.6  1995/12/13  17:59:08  dadams=0A=
; Added Buffer-menu-mouse-3-menu.  Use it instead of =
Buffer-menu-mouse-3-map.=0A=
;=0A=
; Revision 1.5  1995/12/13  13:58:10  dadams=0A=
; 1) Put back Buffer-menu-select, in place of =
Buffer-menu-mouse-other-window.=0A=
; 2) Added menu on mouse-3: Added: Buffer-menu-mouse-3-map,=0A=
;    Buffer-menu-mouse-execute, Buffer-menu-mouse-modified,=0A=
;    Buffer-menu-mouse-delete, Buffer-menu-mouse-save,=0A=
;    Buffer-menu-mouse-unmark.=0A=
;=0A=
; Revision 1.4  1995/10/31  13:07:59  dadams=0A=
; (trivial - Keywords)=0A=
;=0A=
; Revision 1.3  1995/09/11  14:16:09  dadams=0A=
; Buffer-menu-mode: Added bindings list to doc string.=0A=
;=0A=
; Revision 1.2  1995/09/11  12:33:16  dadams=0A=
; Redefined Buffer-menu-execute: Deletes frame w/ kill. Require =
drew-misc-19.el=0A=
;=0A=
; Revision 1.1  1995/09/11  12:16:39  dadams=0A=
; Initial revision=0A=
;; =0A=
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;=0A=
;; =0A=
;; This program is free software; you can redistribute it and/or =
modify=0A=
;; it under the terms of the GNU General Public License as published =
by=0A=
;; the Free Software Foundation; either version 2, or (at your =
option)=0A=
;; any later version.=0A=
=0A=
;; This program is distributed in the hope that it will be useful,=0A=
;; but WITHOUT ANY WARRANTY; without even the implied warranty of=0A=
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the=0A=
;; GNU General Public License for more details.=0A=
=0A=
;; You should have received a copy of the GNU General Public License=0A=
;; along with this program; see the file COPYING.  If not, write to =
the=0A=
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,=0A=
;; Boston, MA 02111-1307, USA.=0A=
;;=0A=
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;=0A=
;; =0A=
;;; Code: =0A=
=0A=
 ;; Cannot do (require 'buff-menu), because `buff-menu.el' does no =
`provide'.=0A=
 ;; Don't want to do a (load-library "buff-menu") either, because it =
wouldn't=0A=
 ;; allow doing (eval-after-load "buff-menu" '(progn (require =
'buff-menu+)))=0A=
=0A=
(require 'cl) ;; push, pop, unless=0A=
=0A=
;; Get macro `define-face-const' when this is compiled,=0A=
;; or run interpreted, but not when the compiled code is loaded.=0A=
(eval-when-compile (require 'def-face-const))=0A=
=0A=
(require 'misc-fns nil t) ;; (no error if not found): =
undefine-killer-commands=0A=
(require 'misc-cmds nil t) ;; (no error if not found): =
kill-buffer-and-its-windows=0A=
=0A=
=0A=
(provide 'buff-menu+)=0A=
=0A=
;;;;;;;;;;;;;;;;;;;;;;;;;;;=0A=
=0A=
=0A=
;;; Undefine some bindings that would try to modify a buffer-menu =
buffer.=0A=
;;; Their key sequences will then appear to the user as available =
for=0A=
;;; local (Buffer Menu) definition.=0A=
(when (fboundp 'undefine-killer-commands)=0A=
  (undefine-killer-commands Buffer-menu-mode-map =
(current-global-map)))=0A=
=0A=
;;; Faces used to fontify buffer.=0A=
(unless (boundp 'orange-on-darkgreen-face)=0A=
  (define-face-const "Orange" "DarkGreen"))=0A=
(defvar buffer-menu-headings-face orange-on-darkgreen-face=0A=
  "Face used for headings in *Buffer List* buffer.")=0A=
(unless (boundp 'red-on-aquamarine-face)=0A=
  (define-face-const "Red" "Aquamarine"))=0A=
(defvar buffer-menu-current-buffer-face red-on-aquamarine-face=0A=
  "Face used for current buffer mark in *Buffer List* buffer.")=0A=
(unless (boundp 'red-on-aquamarine-face)=0A=
  (define-face-const "Red" "Aquamarine"))=0A=
(defvar buffer-menu-view-mark-face red-on-aquamarine-face=0A=
  "Face used for buffers to view mark (>) in *Buffer List* buffer.")=0A=
(unless (boundp 'aquamarine-on-red-face)=0A=
  (define-face-const "Aquamarine" "Red"))=0A=
(defvar buffer-menu-delete-mark-face aquamarine-on-red-face=0A=
  "Face used for buffers to delete mark (D) in *Buffer List* =
buffer.")=0A=
(unless (boundp 'orange-on-blue-face)=0A=
  (define-face-const "Orange" "Blue"))=0A=
(defvar buffer-menu-save-mark-face orange-on-blue-face=0A=
  "Face used for buffers to save mark (S) in *Buffer List* buffer.")=0A=
(unless (boundp 'darkorange-foreground-face)=0A=
  (define-face-const "DarkOrange" nil))=0A=
(defvar buffer-menu-modified-mark-face darkorange-foreground-face=0A=
  "Face used for modified buffers mark (*) in *Buffer List* =
buffer.")=0A=
(unless (boundp 'yellow-foreground-face)=0A=
  (define-face-const "Yellow" nil))=0A=
(defvar buffer-menu-read-only-mark-face yellow-foreground-face=0A=
  "Face used for read-only buffers mark (%) in *Buffer List* =
buffer.")=0A=
(unless (boundp 'blue-foreground-face)=0A=
  (define-face-const "Blue" nil))=0A=
(defvar buffer-menu-buffer-name-face blue-foreground-face=0A=
  "Face used for buffer names in *Buffer List* buffer.")=0A=
(unless (boundp 'darkgreen-foreground-face)=0A=
  (define-face-const "DarkGreen" nil))=0A=
(defvar buffer-menu-mode-face darkgreen-foreground-face=0A=
  "Face used for buffer modes in *Buffer List* buffer.")=0A=
(unless (boundp 'darkred-foreground-face)=0A=
  (define-face-const "DarkRed" nil))=0A=
(defvar buffer-menu-size-face darkred-foreground-face=0A=
  "Face used for buffer sizes in *Buffer List* buffer.")=0A=
(unless (boundp 'darkmagenta-foreground-face)=0A=
  (define-face-const "DarkMagenta" nil))=0A=
(defvar buffer-menu-file-name-face darkmagenta-foreground-face=0A=
  "Face used for file names in *Buffer List* buffer.")=0A=
=0A=
=0A=
;;;###autoload=0A=
(defvar buffer-menu-font-lock-keywords=0A=
  '(=0A=
    ("^\\( M.*\\)" 1 buffer-menu-headings-face) ; Headings=0A=
    ("^\\([.]\\)" 1 buffer-menu-current-buffer-face) ; Current buffer =
mark (.)=0A=
    ("^\\(>\\)" 1 buffer-menu-view-mark-face) ; To view mark (>)=0A=
    ("^\\(D\\)" 1 buffer-menu-delete-mark-face) ; Deletion flag (D)=0A=
    ("^.\\(S\\)" 1 buffer-menu-save-mark-face) ; Save flag (S)=0A=
    ("^.\\([*]\\)" 1 buffer-menu-modified-mark-face) ; =
Buffer-modified-p (*)=0A=
    ("^..\\(%\\)" 1 buffer-menu-read-only-mark-face) ; Read-only-p =
(%)=0A=
    ("^....\\(.+\\)[ \t\n][0-9]" 1 buffer-menu-buffer-name-face) ; =
Buffer name=0A=
    ("^.*[ \t][0-9]+[ \t]+\\([^/\n]+\\)" 1 buffer-menu-mode-face) ; =
Mode=0A=
    ("^.*[ \t]\\([0-9]+\\)[ \t]+[^/\n]+" 1 buffer-menu-size-face) ; =
Size=0A=
    ("\\(/.*\\)$" 1 buffer-menu-file-name-face) ; File name=0A=
    ) "Expressions to highlight in Buffer Menu mode.")=0A=
=0A=
;; Fontify by default.=0A=
(add-hook 'buffer-menu-mode-hook=0A=
          '(lambda ()=0A=
             (make-local-variable 'font-lock-defaults)=0A=
             (setq font-lock-defaults '(buffer-menu-font-lock-keywords =
t))))=0A=
=0A=
=0A=
;; REPLACES ORIGINAL in `buff-menu.el':=0A=
;;   1. Different help message.=0A=
;;   2. Prefix ARG =3D< 0 now means list all buffers alphabetically.=0A=
;;      (It used to mean the same as ARG > 0.)=0A=
;;      Prefix ARG >=3D 0 means list just file buffers.=0A=
;;;###autoload=0A=
(defun buffer-menu (&optional arg)=0A=
  "Make a menu of buffers so you can save, delete or select them.=0A=
By default (no or null prefix arg), the buffers are listed in order =
of=0A=
last access.  With a non-nil prefix ARG:=0A=
  ARG >=3D 0   =3D> Only buffers visiting files are listed.=0A=
  ARG =3D< 0   =3D> The buffers are listed alphabetically.=0A=
 (ARG =3D  0   =3D> Only buffers visiting files, listed =
alphabetically.)=0A=
=0A=
Type `?' in buffer \"*Buffer List*\" to get help on available =
commands.=0A=
Type `q' there to quit the buffer menu."=0A=
  (interactive "P")=0A=
  (let ((num-arg (prefix-numeric-value arg)))=0A=
    (if (and arg (< num-arg 0))=0A=
        (list-buffers)=0A=
      (list-buffers arg))=0A=
    (let ((newpoint (save-excursion (set-buffer "*Buffer List*") =
(point))))=0A=
      (pop-to-buffer "*Buffer List*")=0A=
      (when (and arg (not (> num-arg 0))) ; Sort lines after header.=0A=
        (let ((buffer-read-only nil))=0A=
          (goto-char (point-min)) (forward-line 2) (forward-char 4) ; =
Header.=0A=
          (sort-columns nil (point)=0A=
                        (save-excursion (goto-char (point-max))=0A=
                                        (when (bolp) (backward-char =
1))=0A=
                                        (point)))))=0A=
      (goto-char newpoint)))=0A=
  (message "Help: ?;   Menu: mouse-3;   Show: v;   Mark: u,m,s,d;   =
\=0A=
Save/Delete: x;   Misc: g,~,%%,t"))=0A=
=0A=
=0A=
;; REPLACES ORIGINAL in `buff-menu.el':=0A=
;; 1. Doc string reflects new bindings.=0A=
;; 2. mouse-face on whole line, not just buffer name.=0A=
;;;###autoload=0A=
(defun Buffer-menu-mode ()=0A=
  "Major mode for editing a list of buffers.=0A=
Each line describes one of the buffers in Emacs.=0A=
In Buffer menu mode, chars do not insert themselves, but are =
commands.=0A=
\\<Buffer-menu-mode-map>=0A=
\(\"Current line\" here is the line of the text cursor or the =
mouse.)=0A=
=0A=
Also, pressing `mouse-3' on a buffer name in this mode provides a=0A=
popup menu that duplicates most of the functions below.=0A=
=0A=
=0A=
Display buffers:=0A=
---------------=0A=
\\[Buffer-menu-mouse-select], \\[Buffer-menu-select] -- Select current =
line's \=0A=
buffer.=0A=
\\[Buffer-menu-mark]\t-- Mark current line's buffer `>' to be displayed =
(via \=0A=
`\\[Buffer-menu-select]').=0A=
\\[Buffer-menu-select]\t-- Show buffers marked `>'.  Select current =
line's \=0A=
buffer.=0A=
\\[Buffer-menu-1-window]\t-- Select current line's buffer (only) in a =
\=0A=
full-frame window.=0A=
\\[Buffer-menu-2-window]\t-- Select current line's buffer in one =
window.=0A=
\t   Display previous buffer in a second window.=0A=
\\[Buffer-menu-switch-other-window]\t-- Display current line's buffer =
in \=0A=
another window. No select.=0A=
=0A=
Mark/Unmark buffers to be Saved/Deleted:=0A=
---------------------------------------=0A=
\\[Buffer-menu-save]\t-- Mark current line's buffer `S' to be saved.    =
\=0A=
Cursor down.=0A=
\\[Buffer-menu-delete]\t-- Mark current line's buffer `D' to be =
deleted.  \=0A=
Cursor down.=0A=
\\[Buffer-menu-delete-backwards]\t-- Mark current line's buffer `D' to =
be \=0A=
deleted.  Cursor up.=0A=
\\[Buffer-menu-unmark]\t-- Unmark current line. Cursor down. (Prefix =
arg: \=0A=
Cursor up.)=0A=
\\[Buffer-menu-backup-unmark]\t-- Cursor up, then unmark line.=0A=
=0A=
Save/Delete buffers:=0A=
-------------------=0A=
\\[Buffer-menu-execute]\t-- Save / Delete marked buffers (marks `S', =
`D').=0A=
=0A=
Miscellaneous:=0A=
-------------=0A=
\\[Buffer-menu-not-modified]\t-- Clear modified-flag on current line's =
buffer.=0A=
\\[Buffer-menu-toggle-read-only]\t-- Toggle read-only status of current =
\=0A=
line's buffer.=0A=
\\[Buffer-menu-visit-tags-table]\t-- `visit-tags-table' using current =
line's \=0A=
buffer.=0A=
=0A=
=0A=
Bindings in Buffer Menu mode:=0A=
----------------------------=0A=
=0A=
\\{Buffer-menu-mode-map}"=0A=
  (kill-all-local-variables)=0A=
  (use-local-map Buffer-menu-mode-map)=0A=
  (setq major-mode 'Buffer-menu-mode)=0A=
  (setq mode-name "Buffer Menu")=0A=
  (save-excursion=0A=
    (goto-char (point-min))=0A=
    (search-forward "Buffer")=0A=
    (backward-word 1)=0A=
    (setq Buffer-menu-buffer-column (current-column))=0A=
    (forward-line 2)=0A=
    (while (not (eobp))=0A=
      (put-text-property (point)=0A=
                         (save-excursion (end-of-line) (point))=0A=
                         'mouse-face 'highlight)=0A=
      (forward-line 1)))=0A=
  (make-local-variable 'revert-buffer-function)=0A=
  (setq revert-buffer-function 'Buffer-menu-revert-function)=0A=
  (setq truncate-lines t)=0A=
  (setq buffer-read-only t)=0A=
  (run-hooks 'buffer-menu-mode-hook))=0A=
=0A=
=0A=
;; REPLACES ORIGINAL in `buff-menu.el': Deletes frame when kills =
buffer.=0A=
;;;###autoload=0A=
(defun Buffer-menu-execute ()=0A=
  "Save or delete buffers marked `S' or `D', respectively.=0A=
Buffers are so marked using command `\\<Buffer-menu-mode-map>\=0A=
\\[Buffer-menu-save]' or =
`\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]', respectively."=0A=
  (interactive)=0A=
  (save-excursion=0A=
    (goto-char (point-min))=0A=
    (forward-line 1)=0A=
    (while (re-search-forward "^.S" nil t)=0A=
      (let ((modp nil))=0A=
        (save-excursion=0A=
          (set-buffer (Buffer-menu-buffer t))=0A=
          (save-buffer)=0A=
          (setq modp (buffer-modified-p)))=0A=
        (let ((buffer-read-only nil))=0A=
          (delete-char -1)=0A=
          (insert (if modp ?* ? ))))))=0A=
  (save-excursion=0A=
    (goto-char (point-min))=0A=
    (forward-line 1)=0A=
    (let ((buff-menu-buffer (current-buffer))=0A=
          (buffer-read-only nil))=0A=
      (while (search-forward "\nD" nil t)=0A=
        (forward-char -1)=0A=
        (let ((buf (Buffer-menu-buffer nil)))=0A=
          (or (eq buf nil) (eq buf buff-menu-buffer)=0A=
              (save-excursion (if (fboundp =
'kill-buffer-and-its-windows)=0A=
                                  (kill-buffer-and-its-windows buf)=0A=
                                (kill-buffer buf)))))=0A=
        (if (Buffer-menu-buffer nil)=0A=
            (progn (delete-char 1) (insert ? ))=0A=
          (delete-region (point) (progn (forward-line 1) (point)))=0A=
          (forward-char -1))))))=0A=
=0A=
=0A=
;; REPLACES ORIGINAL in `buff-menu.el':=0A=
;; When Buffer Menu is `window-dedicated-p', uses `pop-to-buffer' to =
display.=0A=
;;;###autoload=0A=
(defun Buffer-menu-select ()=0A=
  "Select this line's buffer; also display buffers marked with `>'.=0A=
You can mark buffers with the =
\\<Buffer-menu-mode-map>\\[Buffer-menu-mark] \=0A=
command."=0A=
  (interactive)=0A=
  (let ((buff (Buffer-menu-buffer t))=0A=
        (menu (current-buffer))       =0A=
        (others ())=0A=
        tem)=0A=
    (goto-char (point-min))=0A=
    (while (search-forward "\n>" nil t)=0A=
      (setq tem (Buffer-menu-buffer t))=0A=
      (let ((buffer-read-only nil)) (delete-char -1) (insert ?\ ))=0A=
      (or (eq tem buff) (memq tem others) (push tem others)))=0A=
    (setq others (nreverse others))=0A=
    (cond ((window-dedicated-p (selected-window)) ; Can't split =
dedicated win.=0A=
           (pop-to-buffer buff)=0A=
           (unless (eq menu buff) (bury-buffer menu))=0A=
           (while others=0A=
             (pop-to-buffer (car others))=0A=
             (pop others)))=0A=
          (t=0A=
           (setq tem (/ (1- (frame-height)) (1+ (length others))))=0A=
           (delete-other-windows)=0A=
           (switch-to-buffer buff)=0A=
           (unless (eq menu buff) (bury-buffer menu))=0A=
           (if (equal (length others) 0)=0A=
               (progn=0A=
;;;              ;; Restore previous window configuration before =
displaying=0A=
;;;              ;; selected buffers.=0A=
;;;              (if Buffer-menu-window-config=0A=
;;;                  (progn (set-window-configuration=0A=
;;;                            Buffer-menu-window-config)=0A=
;;;                         (setq Buffer-menu-window-config nil)))=0A=
                 (switch-to-buffer buff))=0A=
             (while others=0A=
               (split-window nil tem)=0A=
               (other-window 1)=0A=
               (switch-to-buffer (car others))=0A=
               (pop others))=0A=
             (other-window 1))))))      ; Back to the beginning.=0A=
=0A=
=0A=
(define-key Buffer-menu-mode-map [down-mouse-3] =
'Buffer-menu-mouse-3-menu)=0A=
(define-key Buffer-menu-mode-map [mouse-3] 'ignore)=0A=
=0A=
;; Another way, but it shows the menu even if not on a buffer line,=0A=
;; and it doesn't show it if on the line but not on the buffer name =
itself.=0A=
;;(defvar Buffer-menu-mouse-3-map (make-sparse-keymap "Buffers"))=0A=
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-execute]=0A=
;;  '("Execute: Save/Delete Marked Buffers" . =
Buffer-menu-mouse-execute))=0A=
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-modified]=0A=
;;  '("Mark as Modified/Unmodified (*)" . =
Buffer-menu-mouse-modified))=0A=
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-delete]=0A=
;;  '("Mark to Delete Buffer (D)" . Buffer-menu-mouse-delete))=0A=
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-save]=0A=
;;  '("Mark to Save Buffer (S)" . Buffer-menu-mouse-save))=0A=
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-unmark]=0A=
;;  '("Unmark Buffer" . Buffer-menu-mouse-unmark))=0A=
;;(define-key Buffer-menu-mouse-3-map [Buffer-menu-mouse-select]=0A=
;;  '("Select Buffer" . Buffer-menu-mouse-select))=0A=
=0A=
;; Used to highlight buffer name's line during popup of Mouse-3 =
menu.=0A=
(defvar Buffer-menu-overlay nil)=0A=
=0A=
;;;###autoload=0A=
(defun Buffer-menu-mouse-3-menu (event)=0A=
  "Pop up menu for Mouse-3 for buffer listed in buffer menu."=0A=
  (interactive "e")=0A=
  (let* ((mouse-pos (event-start event))=0A=
         bol eol temp=0A=
         (buffer-name=0A=
          (save-excursion=0A=
            (set-buffer (window-buffer (posn-window mouse-pos)))=0A=
            (save-excursion=0A=
              (goto-char (posn-point mouse-pos))=0A=
              (save-excursion=0A=
                (setq bol (progn (beginning-of-line) (point)))=0A=
                (setq eol (progn (end-of-line) (point))))=0A=
              (if Buffer-menu-overlay   ; Don't recreate if exists.=0A=
                  (move-overlay Buffer-menu-overlay bol eol =
(current-buffer))=0A=
                (setq Buffer-menu-overlay (make-overlay bol eol))=0A=
                (overlay-put Buffer-menu-overlay 'face 'region))=0A=
              (setq temp (and (not (eobp)) (Buffer-menu-buffer =
nil)))=0A=
              ;; Nil if mouse is not on a buffer name.=0A=
              (and temp (buffer-name temp)))))) ; temp no longer =
used.=0A=
    (sit-for 0)=0A=
    (let ((selection=0A=
           (x-popup-menu=0A=
            event=0A=
            (list=0A=
             "Menu"=0A=
             (if buffer-name=0A=
                 (list=0A=
                  buffer-name=0A=
                  '("Select Buffer" . Buffer-menu-mouse-select)=0A=
                  '("Unmark Buffer" . Buffer-menu-mouse-unmark)=0A=
                  '("Mark to Save Buffer (S)" . =
Buffer-menu-mouse-save)=0A=
                  '("Mark to Delete Buffer (D)" . =
Buffer-menu-mouse-delete)=0A=
                  '("Mark as Modified/Unmodified (*)" .=0A=
                    Buffer-menu-mouse-modified)=0A=
                  '("--")               ; Separator: next not =
buffer-specific.=0A=
                  '("Execute: Save/Delete Marked Buffers" .=0A=
                    Buffer-menu-mouse-execute))=0A=
               (list "" '("Execute: Save/Delete Marked Buffers" .=0A=
                          Buffer-menu-mouse-execute)))))))=0A=
      (when Buffer-menu-overlay (delete-overlay =
Buffer-menu-overlay))=0A=
      (and selection (call-interactively selection)))))=0A=
=0A=
;; Don't need this if use dedicated frame for buffer menu.=0A=
;;;###autoload=0A=
(defun Buffer-menu-mouse-other-window (event)=0A=
  "Select, in another window, the buffer on whose line you click."=0A=
  (interactive "e")=0A=
  (let (buffer)=0A=
    (save-excursion=0A=
      (set-buffer (window-buffer (posn-window (event-end event))))=0A=
      (save-excursion=0A=
        (goto-char (posn-point (event-end event)))=0A=
        (setq buffer (Buffer-menu-buffer t))))=0A=
    (select-window (posn-window (event-end event)))=0A=
    (switch-to-buffer-other-window buffer)))=0A=
=0A=
;;;###autoload=0A=
(defun Buffer-menu-mouse-unmark (event)=0A=
  "Cancel all requested operations on buffer."=0A=
  (interactive "e")=0A=
  (let (buffer)=0A=
    (save-excursion=0A=
      (set-buffer (window-buffer (posn-window (event-end event))))=0A=
      (save-excursion=0A=
        (goto-char (posn-point (event-end event)))=0A=
        (setq buffer (Buffer-menu-buffer t))))=0A=
    (select-window (posn-window (event-end event)))=0A=
    (goto-char (posn-point (event-end event)))=0A=
    (beginning-of-line)=0A=
    (if (looking-at " [-M]")            ;header lines=0A=
        (ding)=0A=
      (let* ((mod (buffer-modified-p buffer))=0A=
             (readonly (save-excursion (set-buffer buffer) =
buffer-read-only))=0A=
             (buffer-read-only nil))=0A=
        (delete-char 3)=0A=
        (insert (if readonly (if mod " *%" "  %") (if mod " * " "   =
")))))=0A=
    (beginning-of-line)))=0A=
=0A=
;;;###autoload=0A=
(defun Buffer-menu-mouse-save (event)=0A=
  "Mark buffer to be saved.=0A=
Actual deletion is done via =
`\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' \=0A=
or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'."=0A=
  (interactive "e")=0A=
  (select-window (posn-window (event-end event)))=0A=
  (goto-char (posn-point (event-end event)))=0A=
  (beginning-of-line)=0A=
  (forward-char 1)=0A=
  (if (looking-at " [-M]")              ;header lines=0A=
      (ding)=0A=
    (let ((buffer-read-only nil))=0A=
      (delete-char 1)=0A=
        (insert ?S)))=0A=
  (beginning-of-line))=0A=
=0A=
;;;###autoload=0A=
(defun Buffer-menu-mouse-delete (event)=0A=
  "Mark buffer to be deleted.=0A=
Actual deletion is done via =
`\\<Buffer-menu-mode-map>\\[Buffer-menu-execute]' \=0A=
or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-execute]'."=0A=
  (interactive "e")=0A=
  (select-window (posn-window (event-end event)))=0A=
  (goto-char (posn-point (event-end event)))=0A=
  (beginning-of-line)=0A=
  (if (looking-at " [-M]")              ;header lines=0A=
      (ding)=0A=
    (let ((buffer-read-only nil))=0A=
      (delete-char 1)=0A=
      (insert ?D)))=0A=
  (beginning-of-line))=0A=
=0A=
;;;###autoload=0A=
(defun Buffer-menu-mouse-modified (event)=0A=
  "Mark buffer as unmodified (no changes to save) if modified, and vice =
versa."=0A=
  (interactive "e")=0A=
  (select-window (posn-window (event-end event)))=0A=
  (goto-char (posn-point (event-end event)))=0A=
  (beginning-of-line)=0A=
  (forward-char 1)=0A=
  (let ((buffer-read-only nil)=0A=
        modified-p)=0A=
    (save-excursion=0A=
      (set-buffer (Buffer-menu-buffer t))=0A=
      (set-buffer-modified-p (not (buffer-modified-p))))=0A=
    (cond ((=3D ?\* (char-after (point)))=0A=
           (delete-char 1)=0A=
           (insert ?\ ))=0A=
          (t=0A=
           (delete-char 1)=0A=
           (insert ?\*))))=0A=
  (beginning-of-line))=0A=
=0A=
;;;###autoload=0A=
(defun Buffer-menu-mouse-execute (event)=0A=
  "Save and/or delete buffers marked `S' or `D', respectively.=0A=
Buffers can be marked via commands `\\<Buffer-menu-mode-map>\=0A=
\\[Buffer-menu-save]' and =
`\\<Buffer-menu-mode-map>\\[Buffer-menu-delete]'=0A=
\(or `\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-save]' and \=0A=
`\\<Buffer-menu-mode-map>\\[Buffer-menu-mouse-delete]')."=0A=
  (interactive "e")=0A=
  (select-window (posn-window (event-end event)))=0A=
  (save-excursion=0A=
    (goto-char (point-min))=0A=
    (forward-line 1)=0A=
    (while (re-search-forward "^.S" nil t)=0A=
      (let ((modp nil))=0A=
        (save-excursion=0A=
          (set-buffer (Buffer-menu-buffer t))=0A=
          (save-buffer)=0A=
          (setq modp (buffer-modified-p)))=0A=
        (let ((buffer-read-only nil))=0A=
          (delete-char -1)=0A=
          (insert (if modp ?* ? ))))))=0A=
  (save-excursion=0A=
    (goto-char (point-min))=0A=
    (forward-line 1)=0A=
    (let ((buff-menu-buffer (current-buffer))=0A=
          (buffer-read-only nil))=0A=
      (while (search-forward "\nD" nil t)=0A=
        (forward-char -1)=0A=
        (let ((buf (Buffer-menu-buffer nil)))=0A=
          (or (eq buf nil)=0A=
              (eq buf buff-menu-buffer)=0A=
              (save-excursion (if (fboundp =
'kill-buffer-and-its-windows)=0A=
                                  (kill-buffer-and-its-windows buf)=0A=
                                (kill-buffer buf)))))=0A=
        (if (Buffer-menu-buffer nil)=0A=
            (progn (delete-char 1)=0A=
                   (insert ? ))=0A=
          (delete-region (point) (progn (forward-line 1) (point)))=0A=
          (forward-char -1))))))=0A=
=0A=
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;=0A=
;;;  buff-menu+.el ends here=0A=

------ =_NextPart_000_01C07B56.9C958820--

_______________________________________________
Gnu-emacs-sources mailing list
Gnu-emacs-sources@gnu.org
http://mail.gnu.org/mailman/listinfo/gnu-emacs-sources

