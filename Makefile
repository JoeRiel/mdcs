# -*- mode:makefile-gmake; mode:folding -*-
#
# Makefile - for the mdb-mode distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>
# version: VERSIONTAG
#

SHELL = /bin/sh

# {{{ Binaries

CP = cp
EMACS = emacs-snapshot
INSTALL_INFO = install-info
MAKEINFO = makeinfo
MAPLE = smaple
MKDIR = mkdir -p
TEXI2PDF = texi2pdf

# }}}
# {{{ Directories

# where local software is found
prefix = /usr/local

# where local lisp files go
lispdir = $(prefix)/share/emacs/site-lisp

# where info files go
# infodir = $(prefix)/share/info
 infodir = /usr/share/info

# where the maple archive goes
mapleinstalldir = $(HOME)/maple/lib

# }}}
# {{{ Emacs

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(progn \
                    (add-to-list (quote load-path) (expand-file-name \"./lisp\")) \
	            (add-to-list (quote load-path) \"$(lispdir)\"))"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = mdb \
      mdb-showstat \
      mdb-ir

LISPFILES = $(ELS:%=lisp/%.el)
ELCFILES = $(LISPFILES:.el=.elc)

%.elc : %.el
	$(ELC) $<

# }}}

mla = mdb.mla

TEXIFILES = doc/mdb.texi
INFOFILES = doc/mdb

default: byte-compile

doc/mdb.pdf: doc/mdb.texi
	(cd doc; $(TEXI2PDF) mdb.texi)

doc/mdb: doc/mdb.texi
	(cd doc; $(MAKEINFO) --no-split mdb.texi --output=mdb)


byte-compile: $(ELCFILES)
doc: doc/mdb.pdf
info: doc/mdb
pdf: doc/mdb.pdf

# preview pdf
p:
	make pdf && evince doc/mdb.pdf

# preview info
i:
	make info && info doc/mdb

mla: $(mla)
dist: mdb.zip

$(mla): mdb.mpl
	$(MAPLE) -q $^

install-maple: $(mla)
	$(CP) --archive $+ $(mapleinstalldir)

install-lisp: $(LISPFILES) $(ELCFILES)
	if [ ! -d $(lispdir) ]; then $(MKDIR) $(lispdir); else true; fi ;
	$(CP) $+ $(lispdir)

install-info: $(INFOFILES)
	if [ ! -d $(infodir) ]; then $(MKDIR) $(infodir); else true; fi ;
	$(CP) $(INFOFILES) $(infodir)
	for file in $(INFOFILES); do $(INSTALL_INFO) --info-dir=$(infodir) $${file}; done

install: install-lisp install-maple install-info

# Install el files but not elc files; useful for checking old versions of emacs.
install-el: $(el-files)
	$(CP) --archive $+ $(installdir)

dist = $(ELS) $(TEXIFILE) $(INFOFILES)  Makefile README

mdb.zip: $(dist)
	zip $@ $?

clean:
	-$(RM) lisp/*.elc

# P4 upload

p4dir = /home/joe/work/MapleSoft/sandbox/groups/share/emacs/mdb

p4put: $(el-files) Makefile README
	(cd $(p4dir); p4 edit $+)
	$(CP) $+ $(p4dir)

.PHONY: default compile install install-el dist clean p4put mla install-maple install-lisp