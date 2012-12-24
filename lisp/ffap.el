;;; ffap.el: find-file-at-point, by Michelangelo Grigni
;; Time-stamp: <95/03/30 17:41:21 mic>

;;Following up on the highlighting and fetching of URL's in VM message
;;buffers, ffap.el has hooks for the latter.  I have not worked out
;;the hook setup for vm, but it is probably similar to this for gnus:

;;(autoload 'gnus-ffap-next-url "ffap")
;;(defun ffap-gnus-hook nil (local-set-key "l" 'gnus-ffap-next-url))
;;(add-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
;;(add-hook 'gnus-article-mode-hook 'ffap-gnus-hook)

;;ffap also supports external viewers, see `ffap-url-fetcher'. 
;;-- Mic

;;PS: thanks to Rodney Peck <rpeck@nas.nasa.gov> for recently suggesting
;;    such extensions.

;; A drop-in replacement for find-file {C-x C-f}: finds file or URL,
;; guessing the default from text around point.  Many features!
;; Send bugs or suggestions to mailto:mic@cs.ucsd.edu.
;;
;; The most recent version (volatile!):
;;  /cs.ucsd.edu:/pub/mic/ffap.el.gz
;;  /cs.ucsd.edu:/pub/mic/COPYING.gz [GNU General Public License, version 2]
;; (also mirrored in ftp://ftp.hmc.edu/pub/emacs/packages/ffap/ffap.el.gz)
;;
;; For the last "stable" version submitted to the elisp archive:
;;   /ftp.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/misc/ffap.el.Z
;; (also has mirrors, such as gatekeeper.dec.com:/pub/GNU/elisp-archive)
;; The following blurb describes the last archive-submitted version:
;;
;; LCD Archive Entry:
;; ffap|Michelangelo Grigni|mic@cs.ucsd.edu|
;; find-file-at-point guesses file or URL from text around point|
;; 28-Oct-1994|1.0|~/misc/ffap.el.Z|


;;; Copyright (C) 1994, 1995  Michelangelo Grigni
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.


;;; Description:
;;
;; Command find-file-at-point is a replacement for find-file.  With a
;; prefix, it behaves exactly like find-file.  Without a prefix, it
;; first tries to guess a default file or URL based on the text around
;; the point. (You may want to swap these two behaviors by setting
;; `ffap-require-prefix'.)  This is a quick way to fetch URL and file
;; references in many situations, such as in mail or news messages,
;; README's, and MANIFEST's.
;;
;; If you have hyperbole, you may not need this package, although ffap
;; is much smaller and smarter at this particular task.  Also note
;; that w3 offers the similar command w3-follow-url-at-point.


;;; Installation:
;;
;; This package is more useful with ange-ftp or efs (ange-ftp is
;; standard in emacs v19 and efs is ange-ftp's eventual successor) and
;; w3 (see ftp.cs.indiana.edu:/pub/elisp/w3/README), although it does
;; not require either package.  In your .emacs, first load or autoload
;; those packages if you plan to use them.
;;
;; If you use packages complete.el or ff-paths.el, load them before ffap.
;; Finally, add the following two lines to your ~/.emacs file:
;;
;; (require 'ffap)
;; (global-set-key "\C-x\C-f" 'find-file-at-point)
;;
;; For some advanced features (such as making ffap less intrusive, or
;; using an external viewer instead of w3) see the "Peanut Gallery"
;; section below.
;;
;; SPEED: ffap suffers from feature bloat.  If ffap is too slow, turn
;; it off most of the time using (setq ffap-require-prefix t).  For
;; slow or broken pinging, try modifying the `ffap-machine-p-*'
;; variables.  You could trim `ffap-alist' down to just the entries
;; you are likely to use.  Also if you do not want ffap to use URL's
;; at all, do (setq ffap-url-regexp nil).


;;; Examples:
;;
;; Try M-x find-file-at-point (maybe {C-x C-f}) on these examples:
;;
;;    ffap.el, /etc/motd, $MAIL     -- find local or absolute files
;;    .emacs book.sty info/cl pwd.h -- search paths depending on filename
;;    (require 'rmail)              -- search paths depending on major-mode
;;
;; These remote file examples need ange-ftp or efs:
;;
;;    cs.ucsd.edu                   -- ping, then ftp path if successful
;;    cs:, cs                       -- ping if (eq ffap-machine-p-local 'ping)
;;    cs.ucsd.edu:/pub              -- no ping
;;    ftp.x.org:README              -- no ping, also a nice recursive example
;;    anonymous@ftp.x.org:/README   -- synonym
;;    ftp.x.org://README            -- synonym
;;    ftp://ftp.x.org:/README       -- synonym, if `ffap-prefer-file-to-url'
;;    file://ftp.x.org:/README      -- synonym
;;
;; These URL examples need w3 or a modified `w3-url-fetcher':
;;
;;    http://infopath.ucsd.edu
;;    http://www.cs.indiana.edu/elisp/w3/docs.html
;;    http://info.cern.ch/default.html 
;;    news:news.newusers.questions
;;    mailto:mic@cs.ucsd.edu
;;
;; Multiline gopher blocks (as in .gopherrc and usenet):
;;
;;    Type=1
;;    Name=Electronic Texts (this field is ignored)
;;    Path=
;;    Host=etext.archive.umich.edu
;;    Port=70


;;; Todo:

;; * w3 (in emacs 19.29?) plans to make url's part of the emacs filesystem!
;;   this package would then benefit from a rewrite (make it shorter)
;; * instead of prompting with just the first match, gather a list of
;;   all matches and pass it in through the history list
;; * a mouse binding
;; * improve minibuffer-completion-help display of long completions (URL's)
;; * user-defined actions on certain, for example:
;;    comp.sources.unix/volume4/   -- goto an archive (easy)
;;    |~/pathto/file.Z|            -- LCD archive entry (notice bars)
;;    (dired)Virtual Dired         -- Info-goto-node (parentheses, space!)
;;    alt.foo                      -- news:alt.foo (toplevel news hierarchies)
;; * More aggresively search for "machine.dom path/file",

;; In "Uniform Resource Locators", Tim Berners-Lee recommends <angle
;; brackets> around URL's, but of course we cannot rely on these.  For
;; info on URL syntax, try:
;; <ftp://ftp.ncsa.uiuc.edu/Web/Mosaic/Papers/url-primer.ps.Z>
;; <gopher://gopher.well.sf.ca.us/00/matrix/internet/curling.up.02>


;;; History:

;; 3/29/93: first draft: find-file-at-point, dired-dir-at-point,
;;          file-name-at-point, file-name-remote-p, ffap-machine-p.
;; 4/13/93: modified to make guesses only with a prefix, not all the time.
;; 4/15/93: added "Pinging ..." message
;; 6/23/93: added primitive URL support, via w3 package
;; 12/3/93: translate bogus URL "site.dom://path/" to an ange-ftp path
;; 12/4/93: removed dependence on ange-ftp;
;;          suggestions from pot@fly.CNUCE.CNR.IT (Francesco Potorti`):
;;           eliminated non-standard save-match-data macro,
;;           c-mode #include file support
;; 12/5/93: introduced file-name-at-point-mode-alist variable, added
;;          expand-load-file-name (from ange-load.el)
;; 12/6/93: extended comments 
;; 1/21/94: added math-mode-ffap, for Mathematica files
;; 2/12/94: convert ftp urls to paths; var ffap-prefer-file-to-url
;; 4/26/94: added ffap-use-ange-ftp, for better emacs19 support
;; 5/11/94: junked dired-dir-at-point, added remote default-directory check
;; 5/18/94: ffap-machine-p-likely-suffixes replaced by v19 domain-name property
;; 6/27/94: advice to read URL's via v19.22 read-file-name ...
;; 7/8/94:  added ffap-dired-wildcards, implementing a suggestion of
;;          Peter Galbraith <rhogee@bathybius.meteo.mcgill.ca>
;; 8/10/94: search relative to ftp host root if default-directory is remote,
;;          or if buffer is in internal-ange-ftp-mode
;; 10/23/94: reversed prefix meaning, as suggested by Peter Galbraith.
;;          experienced users can get the old less-intrusive behavior
;;          by doing (setq ffap-require-prefix t)
;; 10/28/94: submitted to elisp archive, set version 1.0
;; 12/14/94: strip "URL:" prefix, request of Jared Rhine <jared@math.hmc.edu> 
;; 12/19/94: after further interactions with efs-testers, in particular
;;           Jared Rhine and John Interrante <interran@uluru.Stanford.EDU>:
;;           replace "ange-ftp" with "ftp" (intending to work with efs),
;;           suppressed behavior is *exactly* find-file (URL's don't work),
;;           `file-name-at-point-mode-alist' generalized to `ffap-alist',
;;           folded "ffap-use-*" variables into "ffap-*-regexp" variables,
;;           added "ffap-" prefix to all symbols except the main command.
;; 01/13/95: modifed ffap-file-name-at-point to avoid common false-positives:
;;           "/" (as in /*C*/, thanks Haruhisa Tamai <lincoln!haru@netcom.com>)
;;           "\\`[0-9]+:" (common in lists such as the gnus *Summary* buffer)
;; 01/19/95: introduced ffap-read-file-or-url-internal, to fix a bad
;;           interaction with complete.el and exit-minibuffer.el.
;;           Thanks to Jens-U.H. Petersen <petersen@kurims.kyoto-u.ac.jp>
;; 01/21/95: Added ffap-locate-jka-suffixes to search for compressed files,
;;           for example ffap on info/cl can locate /usr/gnu/info/cl.gz
;; 01/30/95: added ffap-file-finder, to better work with ff-paths.el.
;;           Bug reported by Sudish Joseph <joseph@cis.ohio-state.edu>
;; 03/24/95: `ffap-url-fetcher' allow external viewers as suggested by
;;           Rodney C. Peck <rpeck@nas.nasa.gov>
;; 03/30/95: gnus-fetch-next-url, suggested by rpeck, not enabled

;;; Setup Utilities:

;; These need to be near the top for some reason...

(defun ffap-soft-value (name &optional default)
  ;; Avoid intern'ing a variable if it is not yet defined.
  ;; One bug: (ffap-soft-value "nil" 'unbound) --> unbound
  (let ((sym (intern-soft name)))
    (if (and sym (boundp sym)) (symbol-value sym) default)))


;;; User Variables:

(defvar ffap-ftp-regexp
  (and 
   (or (featurep 'ange-ftp)
       (featurep 'efs)
       (and (boundp 'file-name-handler-alist) ; v19
	    (or (rassq 'ange-ftp-hook-function file-name-handler-alist)
		(rassq 'efs-file-handler-function file-name-handler-alist))))
   ;; Apparently this is good enough for both ange-ftp and efs:
   "\\`/[^/:]+:")
  "*Treat paths matching this as remote ftp paths.  Nil to disable.
Nil also disables the generation of such paths by ffap.")

(defvar ffap-prefer-file-to-url (and ffap-ftp-regexp t)
  "*If not nil, ffap converts \"ftp:\" URL's to remote ftp paths.")

(defvar ffap-ftp-default-user
  (if (or (equal (ffap-soft-value "ange-ftp-default-user") "anonymous")
	  (equal (ffap-soft-value "efs-default-user") "anonymous"))
      nil
    "anonymous")
  "*User name in ftp paths generated by ffap (see host-to-ftp-path).
Nil to fall back on `efs-default-user' or `ange-ftp-default-user'.")

(defvar ffap-afs-regexp (and (file-exists-p "/afs") "\\`/afs/.")
  "*Treat paths matching this as remote AFS paths.  Nil to disable.")

(defvar ffap-url-regexp 
  ;; Could use `url-nonrelative-link' of w3, except it may not be loaded.
  ;; Certain url types ("news") commonly appear without a host.
   "\\`\\(news:\\|mailto:\\|\\(ftp\\|file\\|http\\|telnet\\|gopher\\|www\\)://\\)"
   ;; Note: the "smarter" form (and (fboundp 'w3) ...) is gone, since
   ;; it is load-order dependent, and the resulting errors are more
   ;; mysterious than an old-fashioned `void-function' error when we
   ;; call w3-fetch.
   "Regexp matching URL's, or nil to disable.")


;;; Peanut Gallery Features:

;; Users of ffap occasionally suggest new features.  If I consider
;; those features interesting but not clear winners (a matter of
;; personal taste) I try to leave options to enable them.  Read
;; through this section, and for any features you like, put an
;; appropriate form in your ~/.emacs file.

;; From: Peter Galbraith <rhogee@bathybius.meteo.mcgill.ca>
;; Date: Thu, 07 Jul 1994 13:49:18 -0400
(defvar ffap-dired-wildcards nil	; "[*?][^/]*$"
  ;; Disabled: dired is still available by "C-x C-d <pattern>", and
  ;; valid filenames may contain wildcard characters.
  "*A regexp matching filename wildcard characters, or nil.
If find-file-at-point gets a filename matching this pattern,
it passes it on to dired instead of find-file.")

;; From: Peter Galbraith <rhogee@bathybius.meteo.mcgill.ca>
;; Date: Mon, 11 Jul 1994 13:20:13 -0400
(defvar ffap-newfile-prompt nil		; t
  ;; Disabled: this is better handled by `find-file-not-found-hooks'.
  "*Whether find-file-at-point prompts about a nonexistent file.")

;; From: Peter Galbraith <rhogee@bathybius.meteo.mcgill.ca>
;; Date: Thu, 20 Oct 1994 10:08:21 -0400
(defvar ffap-require-prefix nil
  ;; I leave this nil so that neophytes benefit from ffap.  Experts
  ;; instead may find ffap too aggressive to use all the time, and so
  ;; will probably want to set this to t.
  "*If set, reverses the prefix argument to find-file-at-point.")

;; From: Peter Galbraith <rhogee@bathybius.meteo.mcgill.ca>
;; Date: Thu, 20 Oct 1994 10:08:21 -0400
(defvar ffap-file-finder
  ;; This allows compatibility with ff-paths, if ffap is loaded last.
  ;; Really, ffap and ff-paths need a unification of path-searching code.
  (if (commandp 'find-file-using-paths)
      'find-file-using-paths
    'find-file)
  "The command called by find-file-at-point to find a file.
Probably find-file, or find-file-using-paths if you use ff-paths.")

;; "Rodney C. Peck" <rpeck@nas.nasa.gov>
;; Date: Thu, 23 Mar 1995 18:29:47 -0800
(defvar ffap-url-fetcher 'w3-fetch
  ;; This is great if you do not have w3, or prefer the external viewers.
  "*A function of one argument, called by ffap to fetch URL's.
The default is w3-fetch, to use the w3 package.  Some alternatives:

To launch a new XMosaic process:
\(setq ffap-url-fetcher
      \'(lambda (url) (shell-command (format \"xmosaic -ngh '%s' &\" url)))\)

To tell an existing Netscape process to fetch the URL:
\(setq ffap-url-fetcher
      '(lambda (url)
	 (set-process-sentinel
	  (start-process \"netscape-remote\" nil
			 \"netscape\" \"-remote\" (format \"openURL(%s)\" url))
	  '(lambda (proc str) (message \"%s: %s\" proc str))))\)"
  ;; We could also start and reuse an xmosaic subprocess, as done by
  ;; html-preview-document in html-mode.el.  See also
  ;; http://www.ncsa.uiuc.edu/SDG/Software/XMosaic/remote-control.html
  )

;; From: "Rodney C. Peck" <rpeck@nas.nasa.gov>
;; Date: Thu, 30 Mar 1995 13:00:46 -0800
;;
;; Code to find next url in news messages, slightly extended.
;; Enable this with the following forms:
;;
;; (autoload 'gnus-ffap-next-url "ffap") ; if ffap is not preloaded
;; (defun ffap-gnus-hook nil (local-set-key "l" 'gnus-ffap-next-url))
;; (add-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
;; (add-hook 'gnus-article-mode-hook 'ffap-gnus-hook)

(defvar ffap-next-url-regexp
  (and ffap-url-regexp (concat "\\<" (substring ffap-url-regexp 2))))

(defun ffap-next-url nil
  "Search for next url (with wraparound), and run find-file-at-point."
  (interactive)
  (let ((pt (point)))
    (if (or (re-search-forward ffap-next-url-regexp nil t)
	    (progn 
	      (goto-char (point-min))
	      (re-search-forward ffap-next-url-regexp (+ pt 100) t)))
	(find-file-at-point)
      (goto-char pt)
      (message "No urls found."))))

(defun gnus-ffap-next-url ()
  "Run ffap-next-url in the gnus article buffer."
  (interactive)
  (set-buffer gnus-article-buffer)
  (widen)
  ;; Avoid header "X-URL: news:<msgid@host>" as the first match:
  (if (eq (point) (point-min)) (search-forward "\n\n" nil t))
  (ffap-next-url))


;;; Remote machine and path utilities:

(fset 'ffap-replace-path-component
      (if (or (featurep 'efs)
	      (rassq 'efs-file-handler-function file-name-handler-alist))
	  'efs-replace-path-component
	'ange-ftp-replace-name-component))

(defun ffap-file-exists-string (file)
  ;; With certain packages (ange-ftp, jka-compr?) file-exists-p
  ;; sometimes returns a nicer string than it is given.  Otherwise, it
  ;; just returns nil or t.
  "Return FILE \(maybe modified\) if it exists, else nil."
  (let ((exists (file-exists-p file)))
    (and exists (if (stringp exists) exists file))))

;; I cannot decide a "best" strategy here, so these are variables.  In
;; particular, if `Pinging...' is broken or takes too long on your
;; machine, try setting these all to accept or reject.
(defvar ffap-machine-p-local 'reject	; this happens often
  "A symbol, one of: ping, accept, reject.
This is what ffap-machine-p does with hostnames that have no domain.")
(defvar ffap-machine-p-known 'ping
  "A symbol, one of: ping, accept, reject.
This is what ffap-machine-p does with hostnames that have a known domain
\(see lisp/mail-extr.el for the list of known domains\).")
(defvar ffap-machine-p-unknown 'reject
  "A symbol, one of: ping, accept, reject.
This is what ffap-machine-p does with hostnames that have an unknown domain
\(see mail-extr.el for the list of known domains\).")
  
(defun ffap-machine-p (host &optional service quiet)
  "Indicate whether HOST is the name of a real machine.
The variables ffap-machine-p-local, ffap-machine-p-known, and ffap-machine-p-unknown
control ffap-machine-p depending on HOST's domain \(none/known/unknown\).
Pinging is done using open-network-stream to decide HOST existence.
Optional SERVICE specifies the service used \(default \"discard\"\).
Optional QUIET flag suppresses the \"Pinging...\" message.
Returned values:
A t value means that HOST answered.
A symbol \(accept\) means the relevant variable told us to accept.
A string means the machine exists, but does not respond for some reason."
  ;; Try some:
  ;; (ffap-machine-p "cs")
  ;; (ffap-machine-p "nonesuch")
  ;; (ffap-machine-p "cs.ucsd.edu")
  ;; (ffap-machine-p "foo.bonk")
  ;; (ffap-machine-p "foo.bonk.com")
  ;; (ffap-machine-p "cs" 5678)
  ;; (ffap-machine-p "gopher.house.gov")
  (if (or (string-match "[^-a-zA-Z0-9.]" host) ; Illegal chars (?)
	  (not (string-match "[^0-9]" host))) ; all numeric! reject it
      nil
    (let* ((domain
	    (and (string-match "\\.[^.]*$" host)
		 (downcase (substring host (1+ (match-beginning 0))))))
	   (domain-name			; t, "Country", "Local", or nil
	    (cond
	     ((not domain) "Local")
	     ;; common non-country domains (some imply US though):
	     ((member domain '("arpa" "com" "edu" "net" "org" "mil" "gov")) t)
	     (t 
	      ;; Use domain-name properties from v19 lisp/mail-extr.el;
	      ;; bbdb/mail-extr also puts this in `all-top-level-domains'.
	      (if (get 'edu 'domain-name)
		  nil
		(load "mail-extr" nil t)
		;; avoid loading it again
		(put 'edu 'domain-name t))
	      (get (intern-soft domain) 'domain-name)
	      )))
	   (strategy
	    (cond ((not domain) ffap-machine-p-local)
		  ((not domain-name) ffap-machine-p-unknown)
		  (ffap-machine-p-known))))
      ;; (elog domain domain-name strategy)
      (cond
       ((eq strategy 'accept) 'accept)
       ((eq strategy 'reject) nil)
       ;; assume (eq strategy 'ping)
       (t
	(or quiet
	    (if (stringp domain-name)
		(message "Pinging %s (%s)..." host domain-name)
	      (message "Pinging %s ..." host)))
	(condition-case error
	    (progn
	      (delete-process
	       (open-network-stream
		"ffap-machine-p" nil host (or service "discard")))
	      t)
	  (error
	   (let ((mesg (car (cdr error))))
	     (cond
	      ;; (elog error)
	      ;; v18:
	      ((string-match "^Unknown host" mesg) nil)
	      ((string-match "not responding$" mesg) mesg)
	      ;; v19:
	      ;; (file-error "connection failed" "permission denied"
	      ;;             "nonesuch" "ffap-machine-p")
	      ;; (file-error "connection failed" "host is unreachable" 
	      ;;	     "gopher.house.gov" "ffap-machine-p")
	      ;; (file-error "connection failed" "address already in use" 
	      ;;	     "ftp.uu.net" "ffap-machine-p")
	      ((equal mesg "connection failed")
	       (if (equal (nth 2 error) "permission denied")
		   nil			; host does not exist
		 ;; Other errors mean host exists:
		 (nth 2 error)))
	      ;; Could be "Unknown service":
	      (t (signal (car error) (cdr error))))))))))))

(defun ffap-file-remote-p (filename)
  "If FILENAME looks remote, return it \(maybe slightly improved\)."
  ;; (ffap-file-remote-p "/user@foo.bar.com:/pub")
  ;; (ffap-file-remote-p "/foo.dom://path")
  (or (and ffap-ftp-regexp
	   (string-match ffap-ftp-regexp filename)
	   ;; Convert "/host://path" to "/host:/path", to handle a dieing
	   ;; practice of advertising ftp paths as "host.dom://path".
	   (if (string-match "//" filename)
	       (concat (substring filename 0 (match-beginning 0))
		       (substring filename (1- (match-end 0))))
	     filename))
      (and ffap-afs-regexp
	   (string-match ffap-afs-regexp filename)
	   filename)))

(defun ffap-host-to-path (host)
  "Convert \"HOST\" to \"/anonymous@HOST:\" (or \"\" if localhost).
Variable `ffap-ftp-default-user' can override the \"anonymous\"."
  (if (equal host "localhost")
      ""
    (if ffap-ftp-default-user
	(concat "/" ffap-ftp-default-user "@" host ":")
      (concat "/" host ":"))))

(defun ffap-url-p (string)
  ;; Does it look like an url?
  (and ffap-url-regexp (string-match ffap-url-regexp string)))

(defun ffap-fixup-url (url)
  ;; Given URL, maybe clean it up and/or modify it into something else.
  (cond
   ((not (stringp url)) nil)
   ;; Maybe convert "ftp://" url to a remote ftp filename:
   ((and  ffap-prefer-file-to-url
	  (string-match "\\`\\(ftp\\|file\\)://\\([^:/]+\\):?\\(/.*\\)" url))
    (concat
     (ffap-host-to-path (substring url (match-beginning 2) (match-end 2)))
     (substring url (match-beginning 3) (match-end 3))))
   (t url)))

(defun ffap-machine-at-point nil
  "Return machine name from around point if it exists, or nil."
  ;; ... "To avoid useless pinging, the name must have a domain."
  (let ((mach (ffap-string-at-point "-a-zA-Z0-9." nil "."))
	;; (ffap-machine-p-local 'reject)
	)
    (and (ffap-machine-p mach) mach)))

(defun ffap-fixup-machine (mach)
  ;; Convert a machine into an URL, an ftp path, or nil.
  (cond
   ((not (and ffap-url-regexp (stringp mach))) nil)
   ((string-match "\\`gopher[-.]" mach)	; or "info"?
    (concat "gopher://" mach "/"))
   ((and (string-match "\\`\\(www\\|web\\)[-.]" mach))
    (concat "http://" mach "/"))
   ;; insert more here ...? "telnet://" for archie?
   (ffap-ftp-regexp (ffap-host-to-path mach))
   ))


;;; ffap-alist:
;;
;; Search actions depending on the major-mode or extensions of the
;; current name.  Note all the little defun's could be broken out, at
;; some loss of locality.  I have had a vote for eliminating this
;; from ffap (featuritis)

;; Helper functions used in ffap-alist:

(defun ffap-list-env (env &optional empty)
  "Directory list parsed from \":\"-separated ENVinronment variable.
Optional EMPTY acts as a default \(if ENV is not defined\) and is also
substituted for the first empty component, if any."
  ;; Derived from psg-list-env in Peter S. Galbraith's ff-paths and
  ;; bib-cite packages.  The `empty' argument is intended to mimic
  ;; the semantics of TeX/BibTeX environment variables.
  (let ((start 0) match dir ret)
    (setq env (concat (getenv env) ":")) ; note: unbound --> ":"
    (while (setq match (string-match ":" env start))
      (setq dir (substring env start match) start (1+ match))
      ;;(and (file-directory-p dir) (not (member dir ret)) ...)
      (setq ret (cons dir ret)))
    (setq ret (nreverse ret))
    (and empty (setq match (member "" ret))
	 (progn 
	   (setcdr match (append (cdr-safe empty) (cdr match)))
	   (setcar match (or (car-safe empty) empty))))
    ret))

(defun ffap-reduce-path (path)
  "Remove duplicates or non-dirs from PATH."
  (let (ret tem)
    (while path
      (setq tem path path (cdr path))
      (or (member (car tem) ret)
	  (not (file-directory-p (car tem)))
	  (progn (setcdr tem ret) (setq ret tem))))
    (nreverse ret)))

(defun ffap-add-subdirs (path)
  "Return PATH augmented with its immediate subdirectories."
  ;; (ffap-add-subdirs '("/notexist" "~"))
  (let (ret subs)
    (while path
      (mapcar
       (lambda (f) (and (file-directory-p f) (setq ret (cons f ret))))
       (condition-case nil
	   (directory-files (car path) t "[^.]" t)
	 (error nil)))
      (setq ret (cons (car path) ret)
	    path (cdr path)))
    (nreverse ret)))
	      
(defvar ffap-locate-jka-suffixes t
  "List of compression suffixes that ffap-locate-file tries.  
If not a list, it will be initialized by the next run of ffap-locate-file,
and it will be made nil unless you are using jka-compr.

You may preset this to nil or a list like '(\".gz\" \".z\" \".Z\").")

(defun ffap-locate-file (file &optional nosuffix path) 
  ;; If this package is only working in v19 now, maybe should
  ;; replace this with (a quiet version of) locate-library.
  "A generic path-searching function, defaults mimic load behavior.
Returns path of an existing FILE that (load FILE) would load, or nil.
Optional second argument NOSUFFIX, if t, is like the fourth argument
for load, i.e. don't try adding suffixes \".elc\" and \".el\".
If a list, it is taken as a list of suffixes to try instead.
Optional third argument PATH specifies a different search path, it
defaults to `load-path'."
  (or path (setq path load-path))
  (if (file-name-absolute-p file)
      (setq path (list (file-name-directory file))
	    file (file-name-nondirectory file)))
  (let ((suffixes-to-try
	 (cond
	  ((consp nosuffix) nosuffix)
	  (nosuffix '(""))
	  (t '(".elc" ".el" "")))))
    ;; Compensate for modern (19.28) jka-compr, that no longer searches
    ;; for foo.gz when you asked for foo:
    (or (listp ffap-locate-jka-suffixes)
	(setq ffap-locate-jka-suffixes
	      (and (featurep 'jka-compr) ; an early version was jka-compr19
		   jka-compr-file-name-handler-entry
		   (not (string-match
			 (car jka-compr-file-name-handler-entry)
			 "foo"))
		   ;; Hard to do cleverly across various jka-compr versions:
		   '(".gz" ".Z"))))
    (if ffap-locate-jka-suffixes
	(setq suffixes-to-try
	      (apply
	       'nconc
	       (mapcar
		(function
		 (lambda (suf)
		   (cons suf
			 (mapcar 
			  (function (lambda (x) (concat suf x)))
			  ffap-locate-jka-suffixes))))
		suffixes-to-try))))
    (let (found suffixes)
      (while (and path (not found))
	(setq suffixes suffixes-to-try)
	(while (and suffixes (not found))
	  (let ((try (expand-file-name 
		      (concat file (car suffixes))
		      (car path))))
	    (if (and (file-exists-p try) (not (file-directory-p try)))
		(setq found try)))
	  (setq suffixes (cdr suffixes)))
	(setq path (cdr path)))
      found)))

(defvar ffap-alist
  ;; Some of this mess should be ripped out for the next release.
  (list
   (cons "\\.elc?\\'"
	 (defun ffap-el (name) (ffap-locate-file name t)))
   (cons 'emacs-lisp-mode
	 (defun ffap-el-mode (name)
	   ;; We do not bother with "" here, since it already failed.
	   ;; Prefer "el" to "elc", since presumably we are editing.
	   (ffap-locate-file name '(".el" ".elc"))))
   '(finder-mode . ffap-el-mode)	; v19: {C-h p}
   (cons 'c-mode
	 (progn
	   ;; Need better default here:
	   (defvar ffap-c-path '("/usr/include" "/usr/local/include"))
	   (defun ffap-c-mode (name)
	     (ffap-locate-file name t ffap-c-path))))
   '(c++-mode . ffap-c-mode)
   '(cc-mode . ffap-c-mode)
   '("\\.\\([chCH]\\|cc\\|hh\\)\\'" . ffap-c-mode)
   (cons 'tex-mode
	 ;; complicated example! auctex may not be loaded yet ...
	 (progn
	   (defvar ffap-tex-path
	     (ffap-reduce-path
	      (append
	       (list ".")
	       (ffap-list-env "TEXINPUTS")
 	       ;; (ffap-list-env "BIBINPUTS")
	       (ffap-add-subdirs 
		(ffap-list-env "TEXINPUTS_SUBDIR"
			       (ffap-soft-value
				"TeX-macro-global"
				'("/usr/local/lib/tex/macros"))))))
	     "*Where ffap-tex-mode looks for tex files.")
	   (defun ffap-tex-mode (name)
	     ;; Any real need for "" here?
	     (ffap-locate-file name '(".tex" ".sty" "") ffap-tex-path))))
   '(latex-mode . ffap-tex-mode)
   (cons "\\.bib\\'"
	 (defun ffap-bib (name)
	   (ffap-locate-file
	    name t
	    (ffap-list-env "BIBINPUTS" '("/usr/local/lib/tex/macros/bib")))))
   (cons "\\.\\(tex\\|sty\\|doc\\)\\'"
	 (defun ffap-tex (name)
	   (ffap-locate-file name t ffap-tex-path)))
   (cons 'math-mode
	 (defun ffap-math-mode (name)
	   (while (string-match "`" name)
	     (setq name (concat (substring name 0 (match-beginning 0))
				"/"
				(substring name (match-end 0)))))
	   (ffap-locate-file
	    name '(".m" "") (ffap-soft-value "Mathematica-search-path"))))
   (cons "\\`\\." (defun ffap-home (name) (ffap-locate-file name t '("~"))))
   (cons "\\.info\\'"
	 (defun ffap-info (name)
	     (ffap-locate-file 
	      name t
	      (ffap-soft-value
	       "Info-directory-list" Info-default-directory-list))))
   ;; Since so many info files do not have .info extension, also do this:
   (cons "\\`info/"
	 (defun ffap-info-2 (name) (ffap-info (substring name 5))))
   )
  "Alist of \(KEY . FUNCTION\), applied to text around point.

If ffap-file-at-point has a string NAME (maybe \"\") which is not an
existing filename, it looks for a matching KEY.  If KEY is a symbol,
it should eq `major-mode'.  If KEY is a string, it should match NAME
as a regular expression.  If a KEY matches, then \(FUNCTION NAME\)
returns a filename to try, or nil.")


;;; "At-Point" functions:

(defun ffap-string-at-point (chars &optional begpunct endpunct)
  "Return maximal string of CHARS (a string) around point.
Optional BEGPUNCT chars before point are stripped from the beginning;
Optional ENDPUNCT chars after point are stripped from the end."
  (let ((pt (point)))
    (buffer-substring 
     (save-excursion
       (skip-chars-backward chars)
       (and begpunct (skip-chars-forward begpunct pt))
       (point))
     (save-excursion 
       (skip-chars-forward chars)
       (and endpunct (skip-chars-backward endpunct pt))
       (point)))))
  
(defun ffap-url-at-point nil
  "Return URL from around point if it exists, or nil."
  ;; Could use url-get-url-at-point instead ... how do they compare?
  ;; Both handle "URL:", ignore non-relative links, trim punctuation.
  ;; The other will actually look back if point is in whitespace, but
  ;; I would rather ffap be non-rabid in such situations.
  (and 
   ffap-url-regexp
   (or
    ;; In a w3 buffer button zone?
    (let (tem)
      (and (eq major-mode 'w3-mode)
	   ;; assume: (boundp 'w3-zone-at) (boundp 'w3-zone-data)
	   (setq tem (w3-zone-at (point)))
	   (consp (setq tem (w3-zone-data tem)))
	   (nth 2 tem)))
    (let ((case-fold-search t) name)
      (setq name (ffap-string-at-point "--:<>?$+@-Z_a-z~#,%"
				       "^<A-Za-z" ";.,!?"))
      (if (string-match "\\`<[^<>]+@[^<>]+>\\'" name)
	  ;; maybe a news message-id (or a mail address)?
	  (setq name (concat "news:" (substring name 1 -1)))
	(setq name (ffap-string-at-point "--:?$+@-Z_a-z~#,%"
					 "^A-Za-z" ";.,!?")))
      (and (string-match "^url:" name) (setq name (substring name 4)))
      (and (ffap-url-p name)
	   ;; do not load w3 just for this:
	   (if (fboundp 'url-normalize-url)
	       (url-normalize-url name)
	     name))))))

(defvar ffap-gopher-regexp 
  "^.*\\<\\(Type\\|Name\\|Path\\|Host\\|Port\\) *= *\\(.*\\) *$"
  "Regexp Matching a line in a gopher bookmark (maybe indented).
Two subexpressions are the KEY and VALUE.")

(defun ffap-gopher-at-point nil
  "If point is inside a gopher bookmark block, return its url."
  ;; We could use gopher-parse-bookmark from gopher.el, but it is not
  ;; so robust, and w3 users are better off without gopher.el anyway.
  (save-excursion
    (beginning-of-line)
    (if (looking-at ffap-gopher-regexp)
	(progn
	  (while (and (looking-at ffap-gopher-regexp) (not (bobp)))
	    (forward-line -1))
	  (or (looking-at ffap-gopher-regexp) (forward-line 1))
	  (let ((type "1") name path host (port "70"))
	    (while (looking-at ffap-gopher-regexp)
	      (let ((var (intern
			  (downcase
			   (buffer-substring (match-beginning 1)
					     (match-end 1)))))
		    (val (buffer-substring (match-beginning 2)
					   (match-end 2))))
		;; (elog var val)
		(set var val)
		(forward-line 1)))
	    (if (and path (string-match "^ftp:.*@" path))
		(concat "ftp://"
			(substring path 4 (1- (match-end 0)))
			(substring path (match-end 0)))
	      (and (= (length type) 1)
		   host;; (ffap-machine-p host)
		   (concat "gopher://" host 
			   (if (equal port "70") "" (concat ":" port))
			   "/" type path))))))))

(defun ffap-file-at-point nil
  "Return filename from around point if it exists, or nil.
Existence test is skipped for names that look like remote ftp paths."
  ;; Note: this function does not need to look for URL's, just
  ;; filenames.  On the other hand, it is responsible for converting
  ;; a pseudo-URL "site.dom://path" to an ftp path "/site.dom:/path"
  (let* ((case-fold-search nil)		; why was this t?
	 (data (match-data))
	 (name
	  (substitute-in-file-name
	   (ffap-string-at-point ",-:$+<>@-Z_a-z~`" "<" ">;.,!?`:")))
	 (abs (file-name-absolute-p name))
	 ;; The above string arguments could become mode specific, e.g.
	 ;; "`" is there mostly for Mathematica.
	 ;; We might temporarily change this:
	 (default-directory default-directory))
    (unwind-protect
	(cond
	 ;; Immediate rejects (/ and // are too common in C comments):
	 ((member name '("" "/" "//")) nil)
	 ;; Immediately test local filenames.  If default-directory is
	 ;; remote, you probably already have a connection.
	 ((and (not abs) (ffap-file-exists-string name)))
	 ;; Accept remote names without actual checking (too slow):
	 ((if abs
	      (ffap-file-remote-p name)
	    ;; Try adding a leading "/" (a common omission with ftp
	    ;; paths), but not for numbers like "123:", which are too
	    ;; common in numbered lists (like the Gnus *Summary*)
	    (and
	     (not (string-match "\\`[0-9]+:" name))
	     (ffap-file-remote-p (concat "/" name)))))
	 ;; Ok, not remote, try the existence test even if it is absolute:
	 ((and abs (ffap-file-exists-string name)))
	 ;; File does not exist, try the alist:
	 ((let ((alist ffap-alist) tem try
		case-fold-search)
	    (while (and alist (not try))
	      (setq tem (car alist) alist (cdr alist))
	      (if (or (eq major-mode (car tem))
		      (and (stringp (car tem))
			   (string-match (car tem) name)))
		  (setq try (funcall (cdr tem) name))))
	    (and (stringp try) (ffap-file-exists-string try))))
	 ;; Alist failed?  Try to guess an active remote connection
	 ;; from buffer variables, and try once more, both as an
	 ;; absolute and relative path on that remote host.
	 ((let* (ffap-afs-regexp	; suppress
		 (remote-dir
		  (cond
		   ((ffap-file-remote-p default-directory))
		   ((and (eq major-mode 'internal-ange-ftp-mode)
			 (string-match "^\\*ftp \\(.*\\)@\\(.*\\)\\*$"
				       (buffer-name)))
		    (concat "/" (substring (buffer-name) 5 -1) ":"))
		   ;; This is too often a bad idea:
		   ;;((and (eq major-mode 'w3-mode)
		   ;;	   (stringp url-current-server))
		   ;; (host-to-ange-path url-current-server))
		   )))
	    (and remote-dir
		 (or
		  (and (string-match "\\`\\(/?~?ftp\\)/" name)
		       (ffap-file-exists-string
			(ffap-replace-path-component 
			 remote-dir (substring name (match-end 1)))))
		  (ffap-file-exists-string
		   (ffap-replace-path-component remote-dir name))))))
	 )
      (store-match-data data))))


;;; ffap-read-file-or-url:
;;
;; Want to read filenames with completion as in read-file-name, but
;; also allow URL's which read-file-name-internal would truncate at
;; the "//" string.  Solution here is to replace read-file-name-internal
;; with another function that does not attempt to complete url's.

;; We have to implement a pretty clean completion semantics to still
;; work with packages like complete.el and exit-minibuffer.el.  Even
;; for complete.el, we still need to make a small patch (it has a
;; hardwired list of minibuffer-completion-table values which it
;; expects to deal with filenames).

(defun ffap-read-file-or-url (prompt guess)
  "Read a file or url from minibuffer, with PROMPT and initial GUESS."
  (let ((filep (not (ffap-url-p guess))) dir)
    (if filep
	(or (setq dir (file-name-directory guess))
	    (setq dir (abbreviate-file-name default-directory)
		  guess (concat dir guess))))
    (completing-read
     prompt
     'ffap-read-file-or-url-internal
     (or dir default-directory)
     nil
     (if dir (cons guess (length dir)) guess)
     'file-name-history			; reasonable?
     )))

(defvar url-global-history-completion-list nil)	; w3 var, url.el

(defun ffap-read-url-internal (string dir action)
  ;; Complete URL's from history, treat given url as acceptable.
  (let ((hist url-global-history-completion-list))
    (cond
     ((not action)
      (or (try-completion string hist) string))
     ((eq action t)
      (or (all-completions string hist) (list string)))
     ;; lambda?
     (t string))))

(defun ffap-read-file-or-url-internal (string dir action)
  (if (ffap-url-p string)
      (ffap-read-url-internal string dir action)
    (read-file-name-internal string dir action)))

;; Unfortunately, for complete.el to work correctly, we need to vary
;; the value it sees of minibuffer-completion-table, depending on the
;; current minibuffer contents!  It would be nice if it were written a
;; little more easily.  I consider this a bug in complete.el, since
;; the builtin emacs functions do not have this problem.
(and
 (featurep 'complete)
 (require 'advice)
 (defadvice PC-do-completion (around ffap-fix act)
   "Work with ffap.el."
   (let ((minibuffer-completion-table minibuffer-completion-table)
	 ;; (minibuffer-completion-predicate minibuffer-completion-predicate)
	 )
     (and (eq minibuffer-completion-table 'ffap-read-file-or-url-internal)
	  (setq minibuffer-completion-table
		(if (ffap-url-p (buffer-string))
		    ;; List would work better with icomplete ...
		    'ffap-read-url-internal
		  'read-file-name-internal)))
     ad-do-it)))


;;; The big enchilada:

(defun ffap-prompter nil
  ;; Do guessing and prompting for find-file-at-point.
  (ffap-read-file-or-url
   "Find file or URL: "
   (or (and ffap-url-regexp
	    (ffap-fixup-url (or (ffap-url-at-point)
				(ffap-gopher-at-point))))
       (ffap-file-at-point)
       (ffap-fixup-machine (ffap-machine-at-point))
       "")))

(defun find-file-at-point (&optional filename)
  "Find FILENAME (or URL), guessing default from text around point.
If `ffap-dired-wildcards' is set, wildcard patterns are passed to dired.
See also the functions ffap-file-at-point, ffap-url-at-point.
With a prefix, this command behaves *exactly* like `ffap-file-finder'.
If `ffap-require-prefix' is set, the prefix meaning is reversed."
  (interactive)
  (if (and (interactive-p)
	   (if ffap-require-prefix (not current-prefix-arg)
	     current-prefix-arg))
      ;; Do exactly the ffap-file-finder command, even the prompting:
      (call-interactively ffap-file-finder)
    (or filename (setq filename (ffap-prompter)))
    (cond
     ((and (ffap-url-p filename))
      (funcall ffap-url-fetcher filename))
     ;; This junk more properly belongs in a modified ffap-file-finder:
     ((and ffap-dired-wildcards (string-match ffap-dired-wildcards filename))
      (dired filename))
     ((or (not ffap-newfile-prompt)
	  (file-exists-p filename)
	  (y-or-n-p "File does not exist, create buffer? "))
      (funcall ffap-file-finder filename))
     ;; User does not want to find a non-existent file:
     ((signal 'file-error (list "Opening file buffer"
				"no such file or directory"
				filename))))))


;;; X support:
(defun ffap-at-mouse (e)
  "Like find-file-at-point, guessing from text around mouse click."
  (interactive "e")
  ;; Maybe less surprising without the save-excursion ...?
  (find-file-at-point
   (save-excursion (mouse-set-point e) (ffap-prompter))))
;; Look at the X selections if active?
;; (x-get-selection PRIMARY/SECONDARY LENGTH/TEXT)


;;; End.
(provide 'ffap)
;; Local Variables?
;; eof





