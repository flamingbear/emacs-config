# Makefile for auto-insert
# Copyright (C) 1994 Kevin Davidson <tkld@cogsci.ed.ac.uk>
# $Header: /tmp_mnt/FILES/mhs/mhs-lisp/autoinsert/Makefile,v 1.1.1.1 2002-02-28 18:00:54 savoie Exp $
# $Log: not supported by cvs2svn $
# Revision 1.9  1994/05/06  09:24:58  tkld
# Generate LCD-entry and paramterised shar/tar
#
# Revision 1.8  1994/05/04  17:51:26  tkld
# tar again
#
# Revision 1.7  1994/05/04  17:50:05  tkld
# Fixing tar target and adding to RUBBISH
#
# Revision 1.6  1994/05/04  17:45:56  tkld
# Add tar target.
#
# Revision 1.5  1994/05/04  17:19:24  tkld
# Fixed install booboo.
#
# Revision 1.4  1994/04/29  17:13:09  tkld
# Working version
#
# Revision 1.3  1994/04/26  16:48:06  tkld
# Fixed realclean target to remove all tex files.
#
# Revision 1.2  1994/04/26  16:28:39  tkld
# Working/complete Makefile
#
# Revision 1.1  1994/04/22  16:16:28  tkld
# Initial revision
#

# Pick your version of Emacs
EMACS=emacs19
# Program to turn .texinfo into .dvi file, normally tex or texi2dvi
TEX=texi2dvi
TEXINDEX=texindex
#Program to turn dvi file into PostScript
DVIPS=dvips

# Set this to `cp' if you don't have `install'
INSTALL=install -c -m 644
#INSTALL=echo
#INSTALL=cp

# Change these to reflect where things live on your system
# Location of Emacs Lisp libraries
LISPDIR=/usr/local/share/lib/emacs/site-lisp
# Location of Info files
# You'll have to modify the top level `dir' file to include a
# reference to auto-insert. Something like this will do fine:
# * Auto Insert: (auto-insert).		tkld's Auto Insert package
INFODIR=/usr/local/share/lib/info
# Set this to where you want the central insert file repository to be.
INSERTDIR=/usr/local/share/lib/insert

# Don't edit anything below here
VERSION=`./version auto-insert-tkld.el`
RUBBISH=auto-insert.tp auto-insert.vr auto-insert.cp auto-insert.fn \
	auto-insert.ky auto-insert.pg auto-insert.toc auto-insert.aux \
	auto-insert.kys auto-insert.cps auto-insert.fns auto-insert.pgs \
	auto-insert.vrs auto-insert.log auto-insert.tps \
	auto-insert-tkld*.shar auto-insert-tkld*.tar.gz
TARGETS=auto-insert-tkld.elc auto-insert.info auto-insert.dvi
IFILES=c-insert.c elisp-insert.el h-insert.h makefile.inc perl-insert.pl \
	test-insert texinfo-insert.texinfo pascal-insert.p c++-insert.cc \
	prolog-insert.pl tcsh-insert.csh csh-insert.csh sh-insert.sh \
	latex-insert.tex
# Files distributed in package
DISTFILES=LCD-entry README auto-insert-tkld.el Makefile \
	auto-insert.texinfo insert/*


all: $(TARGETS)
	@echo All done.

install: $(TARGETS)
	$(INSTALL) auto-insert.info $(INFODIR)
	$(INSTALL) auto-insert-tkld.el auto-insert-tkld.elc  $(LISPDIR)
	-mkdir $(INSERTDIR)
	for file in $(IFILES) ; do \
		$(INSTALL) insert/$$file $(INSERTDIR) ; \
	done
	@echo
	@echo Auto Insert installed.
	@echo Remember to add menu entry to top level Info
	@echo directory: $(INFODIR)/dir
	@echo '* Auto Insert (auto-insert).	A File Template Package'
	@echo and check that $(LISPDIR) is in Emacs load-path.

auto-insert-tkld.elc: auto-insert-tkld.el
	$(EMACS) -batch -f batch-byte-compile auto-insert-tkld.el

auto-insert.info: auto-insert.texinfo
	makeinfo auto-insert.texinfo

auto-insert.dvi: auto-insert.texinfo
	$(TEX) auto-insert.texinfo
# Uncomment these if you don't have texi2dvi
#	$(TEXINDEX) auto-insert.tp auto-insert.vr auto-insert.cp \
#			auto-insert.fn auto-insert.ky auto-insert.pg
#	$(TEX) auto-insert.texinfo

printed: auto-insert.dvi
	$(DVIPS) auto-insert

postscript: auto-insert.dvi
	$(DVIPS) auto-insert -o auto-insert.ps

clean:
	-rm $(RUBBISH)

realclean:
	-rm $(RUBBISH) $(TARGETS)

LCD-entry: auto-insert-tkld.el
	sed -n -e '/^;; LCD Arch/,/^$$/ p' < auto-insert-tkld.el > LCD-entry

shar: LCD-entry
	-rm auto-insert-tkld*.shar
	shar $(DISTFILES) > auto-insert-tkld-$(VERSION).shar

tar: LCD-entry
	-rm auto-insert-tkld*.tar.gz
	tar cvf auto-insert-tkld-$(VERSION).tar $(DISTFILES)
	gzip auto-insert-tkld-$(VERSION).tar
