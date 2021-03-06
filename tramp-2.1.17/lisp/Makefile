# -*- coding: utf-8; -*-
# Emacs Makefile for TRAMP
# lisp/Makefile.  Generated from Makefile.in by configure.

# Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#   2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.

# Author: Kai Großjohann <kai.grossjohann@gmx.net>
#         Michael Albinus <michael.albinus@gmx.de>
# Keywords: comm, processes

# This file is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

# This Makefile requires GNU make.

EMACS		= emacs
EMACS_GVFS	= no
EMACS_GW	= yes
EMACS_IMAP	= no
EMACS_UTILS	= yes
INSTALL		= install -m644
MKDIR		= @mkdir -p
RM		= -rm -f

prefix		= /home/utt/emacs/lisp
datarootdir	= ${prefix}/share
datadir		= ${datarootdir}
lispdir		= ${datadir}/emacs/site-lisp
DESTDIR		=

CONTRIB_FILES	= 
CLEAN_FILES	= .\\\#* \\\#* .*~ *~ *.aux *.cp *.cps *.diff *.dvi *.elc *.fn *.fns *.html *.info *.ky *.log *.pg *.tmp *.toc *.tp *.vr *.vrs

# trampver.el and tramp.el must be handled first due to dependencies.
LISP_FILES	= trampver.el tramp.el tramp-cache.el tramp-cmds.el	\
		  tramp-compat.el tramp-fish.el tramp-smb.el tramp-uu.el

# tramp-gvfs.el needs Emacs 23 at least.
ifeq ($(EMACS_GVFS), yes)
  LISP_FILES += tramp-gvfs.el
endif

# tramp-gw.el needs Emacs 22 at least.
ifeq ($(EMACS_GW), yes)
  LISP_FILES += tramp-gw.el
endif

# tramp-imap.el needs Emacs 23.2 at least.
ifeq ($(EMACS_IMAP), yes)
  LISP_FILES += tramp-imap.el
endif

# tramp-util.el and tramp-vc.el are not needed with Emacs 23.
ifeq ($(EMACS_UTILS), yes)
  LISP_FILES += tramp-util.el tramp-vc.el
endif

# tramp-ftp.el and tramp-efs.el are useful for Emacs resp XEmacs only.
ifeq ($(EMACS_INFO), emacs)
  LISP_FILES += tramp-ftp.el
else
  LISP_FILES += tramp-efs.el
endif

INSTALL_FILES	= $(LISP_FILES) $(CONTRIB_FILES) \
		  $(addsuffix .elc, $(basename $(LISP_FILES) $(CONTRIB_FILES)))

EVAL = --eval "(add-to-list 'load-path \".\")"

ifeq ($(EMACS_INFO), emacs)
  EM = $(EMACS) --no-site-file -batch $(EVAL) -f batch-byte-compile
else
  EM = $(EMACS) -vanilla -batch $(EVAL) -f batch-byte-compile
endif


.PHONY: all install clean distclean tramp xemacs

.SUFFIXES: .elc .el

all: $(addsuffix .elc, $(basename $(LISP_FILES) $(CONTRIB_FILES)))

%.elc: %.el
	$(EM) $?

install: all
	$(MKDIR) $(DESTDIR)$(lispdir)
	$(INSTALL) $(INSTALL_FILES) $(DESTDIR)$(lispdir)

clean:
	$(RM) $(CLEAN_FILES)

distclean: clean
	$(RM) $(CONTRIB_FILES)

# Maintainer targets.

tramp: all

xemacs:
	cp $(LISP_FILES) ChangeLog $(XEMACS_TARGET)/lisp

### TODO:

## * Update Tramp's autoladed symbols in loaddefs.el during install.
