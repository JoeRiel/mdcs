# -*- mode:makefile-gmake; mode:folding -*-
#
# Makefile - for the mdb-mode distribution
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL = /bin/sh

.PHONY: byte-compile build default
default: byte-compile
build: byte-compile pmaple doc mla

# {{{ Executables

# Assign local variables for needed binaries

# Modify these to something appropriate, or
# specify values on the command line.

EMACS = emacs-snapshot
MAPLE = $(MAPLE_ROOT)/bin/smaple

CP = cp
INSTALL_INFO = install-info
MAKEINFO = makeinfo
MKDIR = mkdir -p
TEXI2PDF = texi2pdf

# }}}
# {{{ Directories

# where local software is found
PREFIX = /usr/local

# where local lisp files go
LISPDIR = $(PREFIX)/share/emacs/site-lisp

# where info files go
# INFODIR = $(PREFIX)/share/info
INFODIR = /usr/share/info

# where the maple archive goes
MAPLEINSTALLDIR = $(HOME)/maple/lib

# }}}

# {{{ doc

TEXIFILES = doc/mdb.texi
INFOFILES = doc/mdb

doc/mdb.pdf: doc/mdb.texi
	(cd doc; $(TEXI2PDF) mdb.texi)

doc/mdb: doc/mdb.texi
	(cd doc; $(MAKEINFO) --no-split mdb.texi --output=mdb)


doc: info pdf
info: doc/mdb
pdf: doc/mdb.pdf

# preview pdf
p:
	make pdf && evince doc/mdb.pdf

# preview info
i:
	make info && info doc/mdb

# }}}

# {{{ Emacs

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(add-to-list (quote load-path) (expand-file-name \"./lisp\"))"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = mdb \
      mdb-showstat \
      mdb-ir

LISPFILES = $(ELS:%=lisp/%.el)
ELCFILES = $(LISPFILES:.el=.elc)

byte-compile: $(ELCFILES)

%.elc : %.el
	$(ELC) $<

# }}}

# {{{ pmaple

pmaple := c

.PHONY: pmaple $(pmaple)

pmaple: $(pmaple)

$(pmaple):
	$(MAKE) --directory=$@

# }}}
# {{{ mla

.PHONY: mla
mla = maple/mdb.mla
mla: $(mla)

$(mla): maple/mdb.mpl
	cd maple; $(MAPLE) -q $(notdir $^)

# }}}

# {{{ install

.PHONY: install-pmaple install-el install-maple install-lisp install-info install

install-pmaple: $(pmaple)
	$(MAKE) --directory=c install

install-maple: $(mla)
	$(CP) --archive $+ $(MAPLEINSTALLDIR)

install-lisp: $(LISPFILES) $(ELCFILES)
	if [ ! -d $(LISPDIR) ]; then $(MKDIR) $(LISPDIR); else true; fi ;
	$(CP) $+ $(LISPDIR)

install-info: $(INFOFILES)
	if [ ! -d $(INFODIR) ]; then $(MKDIR) $(INFODIR); else true; fi ;
	$(CP) $(INFOFILES) $(INFODIR)
	for file in $(INFOFILES); do $(INSTALL_INFO) --info-dir=$(INFODIR) $${file}; done

install: install-lisp install-maple install-info install-pmaple

# Install el files but not elc files; useful for checking old versions of emacs.
install-el: $(el-files)
	$(CP) --archive $+ $(installdir)

# }}}
# {{{ distribution

PHONY: dist

dist = $(ELS) $(TEXIFILE) $(INFOFILES)  Makefile README

dist: mdb.zip

mdb.zip: $(dist)
	zip $@ $?

# }}}

# {{{ clean

.PHONY: clean
clean:
	-$(RM) lisp/*.elc
	-$(RM) -f $(filter-out doc/mdb.texi,$(wildcard doc/mdb*))
	-$(RM) maple/mdb.mla
	$(MAKE) --directory=c $@

# }}}

