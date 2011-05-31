# -*- mode:makefile-gmake; mode:folding -*-
#
# Makefile - for mdcs, a Maple Debugger Client/Server
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL := /bin/sh

include help-system.mak

.PHONY: byte-compile build default

comma := ,
OS := $(shell uname -o)

$(info $(OS))

default: $(call print-help,default,Byte-compile the elisp)
default: byte-compile
build: $(call print-help,build,Byte-compile$(comma) doc$(comma) and mla)
build: byte-compile compile doc mla

# {{{ Executables

# Assign local variables for needed binaries

# Modify these to something appropriate, or specify values on the
# command line.  

EMACS := emacs
ifeq ($(OS),Cygwin)
  MAPLE := cmaple 
else
  MAPLE := maple
endif

CP = cp --archive
INSTALL_INFO = install-info
MAKEINFO = makeinfo
MKDIR = mkdir -p
TEXI2PDF = texi2pdf

# }}}
# {{{ Directories

# where executables go
export BIN_INSTALL_DIR := $(HOME)/bin

# where lisp files go
LISP_DIR := $(HOME)/.emacs.d

# where info files go
INFO_DIR := $(HOME)/share/info

# where the maple archive goes
MAPLE_INSTALL_DIR := $(HOME)/maple/lib

# Cypathify, as needed
ifeq ($(OS),Cygwin)
 LISP_DIR := $(shell cygpath --mixed "$(LISP_DIR)")
 INFO_DIR := $(shell cygpath --mixed "$(INFO_DIR)")
 MAPLE_INSTALL_DIR := $(shell cygpath --mixed "$(MAPLE_INSTALL_DIR)")
# MAPLE := $(shell cygpath --mixed "$(MAPLE)")
endif

# }}}

# {{{ doc

PKG := mds

TEXIFILES = doc/$(PKG).texi
INFOFILES = doc/$(PKG)

doc/$(PKG).pdf: doc/$(PKG).texi
	(cd doc; $(TEXI2PDF) $(PKG).texi)

doc/$(PKG): doc/$(PKG).texi
	(cd doc; $(MAKEINFO) --no-split $(PKG).texi --output=$(PKG))


doc: $(call print-help,doc,Create info and pdf)
doc: info pdf
info: $(call print-help,info,Create info)
info: doc/$(PKG)
pdf: $(call print-help,pdf,Create pdf)
pdf: doc/$(PKG).pdf

# preview pdf
p: $(call print-help,p,Update and display pdf)
p:
	make pdf && evince doc/$(PKG).pdf

# preview info
i: $(call print-help,i,Update and display info)
i:
	make info && info doc/$(PKG)

# }}}

# {{{ emacs


ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(add-to-list (quote load-path) (expand-file-name \"./lisp\"))" \
	  --eval "(add-to-list (quote load-path) \"$(LISP_DIR)\")"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = mds mds-showstat mds-output

LISPFILES = $(ELS:%=lisp/%.el)
ELCFILES = $(LISPFILES:.el=.elc)

byte-compile: $(print-help,byte-compile,Byte compile $(LISPFILES))
byte-compile: $(ELCFILES)

%.elc : %.el
	$(ELC) $<

# }}}

# {{{ mla

.PHONY: mla 
mla := mdc.mla
mla: $(call print-help,mla,Create Maple archive: $(mla))
mla: $(mla)

%.mla: maple/src/%.mpl maple/src/*.mm
	$(RM) $@
	echo $(MAPLE)
	$(MAPLE) -q -I maple -D BUILD_MLA $^

# }}}

# {{{ install

.PHONY: install-el install-maple install-lisp install-info install

install: $(call print-help,install,Install everything)
install: install-lisp install-info install-maple

install-maple: $(call print-help,install-maple,Install mla in $(MAPLE_INSTALL_DIR))
install-maple: $(mla)
	$(MKDIR) $(MAPLE_INSTALL_DIR)
	$(CP) --archive $+ $(MAPLE_INSTALL_DIR)

install-lisp: $(call print-help,install-lisp,Install lisp in $(LISP_DIR))
install-lisp: $(LISPFILES) $(ELCFILES)
	$(MKDIR) $(LISP_DIR)
	$(CP) $+ $(LISP_DIR)

install-info: $(call print-help,install-info,Install info files in $(INFO_DIR) and update dir)
install-info: $(INFOFILES)
	$(MKDIR) $(INFO_DIR)
	$(CP) $(INFOFILES) $(INFO_DIR)
	@if [ -f $(INFO_DIR)/dir ]; then \
		for file in $(INFOFILES); do $(INSTALL_INFO) --info-dir=$(INFO_DIR) $${file}; done \
	fi

# Install el files but not elc files; useful for checking old versions of emacs.
install-el: $(call print-help,install-el,Install el files but not elc files)
install-el: $(LISPFILES)
	$(MKDIR) $(LISP_DIR)
	$(CP) --archive $+ $(LISP_DIR)

# }}}
# {{{ distribution

PHONY: dist

dist = $(ELS) $(TEXIFILE) $(INFOFILES) Makefile README

dist: $(PKG).zip

$(PKG).zip: $(dist)
	zip $@ $?

# }}}

# {{{ clean

.PHONY: clean

clean: $(call print-help,clean,Remove files)
clean:
	-$(RM) lisp/*.elc
	-$(RM) -f $(filter-out doc/mds.texi,$(wildcard doc/*))
	-$(RM) $(mla)

# }}}

# end Make
