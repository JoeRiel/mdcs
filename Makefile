# Makefile - for mdcs, a Maple Debugger Client/Server
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL := /bin/bash

VERSION := 0.1.1.15

include help-system.mak

.PHONY: byte-compile build default

comma := ,
OS := $(shell uname -o)

$(info $(OS))

build: $(call print-help,build,Byte-compile$(comma) doc$(comma) and mla)
build: byte-compile compile doc mla

# {{{ Executables

# Assign local variables for needed binaries

# Modify these to something appropriate, or specify values on the
# command line.  

EMACS := emacs
ifeq ($(OS),Cygwin)
  MAPLE := cmaple
  INSTALL_INFO = install-info
else
  MAPLE := maple
ifeq ($(OS),GNU/Linux)
  INSTALL_INFO = ginstall-info
else
  INSTALL_INFO = install-info
endif
endif

CP := cp --archive
BROWSER := firefox
MAKEINFO := makeinfo
MKDIR := mkdir -p
TEXI2PDF := texi2pdf
TEXI2HTML := makeinfo --html --number-sections

# }}}
# {{{ Directories

# where executables go
export BIN_INSTALL_DIR := $(HOME)/bin

# where lisp files go
LISP_BASE := $(HOME)/.emacs.d
LISP_DIR := $(LISP_BASE)/mds

# where html files go.
# there is no standard place for this.
HTML_DIR := $(HOME)/maple/lib

# where info files go
INFO_DIR := $(HOME)/share/info

# where the maple archive goes
MAPLE_INSTALL_DIR := $(HOME)/maple/lib

# Cypathify, as needed
ifeq ($(OS),Cygwin)
 LISP_BASE := $(shell cygpath --mixed "$(LISP_BASE)")
 LISP_DIR  := $(shell cygpath --mixed "$(LISP_DIR)")
 INFO_DIR  := $(shell cygpath --mixed "$(INFO_DIR)")
 MAPLE_INSTALL_DIR := $(shell cygpath --mixed "$(MAPLE_INSTALL_DIR)")
# MAPLE := $(shell cygpath --mixed "$(MAPLE)")
endif

# }}}

# {{{ doc

.PHONY: doc info html h i p

PKG := mds

TEXI_FILES = doc/$(PKG).texi
INFO_FILES = doc/$(PKG)
HTML_FILES = doc/$(PKG).html
PDF_FILES  = doc/$(PKG).pdf

doc/$(PKG).pdf: doc/$(PKG).texi
	(cd doc; $(TEXI2PDF) $(PKG).texi)

doc/$(PKG): doc/$(PKG).texi
	@echo "Creating info file $@"
	@(cd doc; $(MAKEINFO) --no-split $(PKG).texi --output=$(PKG))


doc: $(call print-help,doc,Create info$(comma) html$(comma) and pdf)
doc: info pdf html
info: $(call print-help,info,Create info)
info: doc/$(PKG)
pdf: $(call print-help,pdf,Create pdf)
pdf: doc/$(PKG).pdf
html: $(call print-help,html,Create html documentation)
html: doc/$(PKG).html

doc/$(PKG).html: doc/$(PKG).texi
	(cd doc; $(TEXI2HTML) --no-split -o $(PKG).html $(PKG).texi)

# preview html
h: $(call print-help,h,Preview the html)
h: doc/$(PKG).html
	$(BROWSER) $^

# preview info
i: $(call print-help,i,Update and display info)
i: doc/$(PKG)
	info $^
# preview pdf
p: $(call print-help,p,Update and display pdf)
p: doc/$(PKG).pdf
	evince $^

# }}}

# {{{ emacs

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(push (expand-file-name \"./lisp\") load-path)" \
	  --eval "(push \"$(LISP_BASE)\" load-path)"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = mds-re mds-ss mds-out mds-wm mds-login mds-cp mds-client mds

LISP_FILES = $(ELS:%=lisp/%.el)
ELC_FILES = $(LISP_FILES:.el=.elc)

byte-compile: $(call print-help,byte-compile,Byte compile $(LISP_FILES))
byte-compile: $(ELC_FILES)

%.elc : %.el
	@$(RM) $@
	@echo Byte-compiling $+
	@err=$$($(ELC) $< 2>&1 > /dev/null | sed '/^Wrote/d') ; \
		if [ ! -z "$$err" ]; then \
			echo $(call warn,$$err); \
		fi

# }}}

# {{{ hdb

.PHONY: hdb

hdb := mdc.hdb
hdb: $(call print-help,hdb,Create Maple help database)
hdb: install-mla mdc.hdb

mdc-new.hdb : maple/src/mdc.mpl maple/src/*.mm maple/include/*.mpi
	@echo "Creating Maple help database"
	@mpldoc -c nightly $+
	@shelp mwhelpload --config=doc/MapleHelp_en.xml --input=. --output=.

mdc.hdb : maple/src/mdc.mpl maple/src/*.mm maple/include/*.mpi
	@echo "Creating Maple help database"
	@err=$$(mpldoc --config etc/mpldoc/doti.xml $+ 2>&1 | sed -n '/Warning/{p;n};/Error/p' ; ) ; \
		if [ ! -z "$$err" ]; then \
			echo $(call warn,$$err); \
		fi
	@cp maple/etc/empty.hdb $@
	ls maple/doti/*.i | xargs shelp -h $@ load

# }}}

# {{{ mla

.PHONY: mla 
mla := mdc.mla
mla: $(call print-help,mla,Create Maple archive: $(mla))
mla: $(mla)

txtbold   := $(shell tput bold)
txtred    := $(shell tput setaf 1)
txtnormal := $(shell tput sgr0)
warn = "$(txtred)$(textbold)$1$(txtnormal)"

%.mla: maple/src/%.mpl maple/src/*.mm
	@$(RM) $@
	@echo "Building Maple archive $@"
	@err=$$($(MAPLE) -q -I maple -D BUILD_MLA $^ ) ; \
		if [ ! -z "$$err" ]; then \
			echo $(call warn,$$err); \
		fi

# }}}

# {{{ tags

.PHONY: tags
tags: $(call print-help,tags,Create TAGS file)
tags:
	bin/mtags maple/src/*
	etags --append --language=lisp lisp/*.el

# }}}

# {{{ install

.PHONY: install $(addprefix install-,dev el elc hdb html info lisp maple)

INSTALLED_EL_FILES  := $(addprefix $(LISP_DIR)/,$(notdir $(LISP_FILES)))
INSTALLED_ELC_FILES := $(addprefix $(LISP_DIR)/,$(notdir $(ELC_FILES)))

install: $(call print-help,install,Install everything)
install: $(addprefix install-,dev hdb html info lisp maple)

install-dev: $(call print-help,install-dev,Install everything but hdb)
install-dev: install-elc install-info install-maple

# Install el files but not elc files; useful for checking old versions of emacs.
install-el: $(call print-help,install-el,Install el files but not elc files)
install-el: $(LISP_FILES)
	$(MKDIR) $(LISP_DIR)
	$(CP) $+ $(LISP_DIR)


install-elc: $(call print-help,install-elc,Install elc files and link *.el files)
install-elc: $(ELC_FILES)
	@echo "Installing elc files into $(LISP_DIR)"
	@$(MKDIR) $(LISP_DIR)
	@$(CP) $+ $(LISP_DIR)
	@$(RM) $(INSTALLED_EL_FILES)

install-hdb: $(call print-help,install-hdb,Install hdb in $(MAPLE_INSTALL_DIR))
install-hdb: hdb
	@$(MKDIR) $(MAPLE_INSTALL_DIR)
	@echo "Installing Maple help data base $(hdb) into $(MAPLE_INSTALL_DIR)/"
	@$(CP) $(hdb) $(MAPLE_INSTALL_DIR)

install-html: $(call print-help,install-html,Install html files in $(HTML_DIR) and update dir)
install-html: $(HTML_FILES)
	@echo "Installing html file(s) into $(HTML_DIR)/"
	@$(MKDIR) $(HTML_DIR)
	$(CP) $(HTML_FILES) $(HTML_DIR)

install-info: $(call print-help,install-info,Install info files in $(INFO_DIR) and update dir)
install-info: $(INFO_FILES)
	@echo "Installing info file(s) into $(INFO_DIR)/ and updating $(INFO_DIR)/dir"
	@$(MKDIR) $(INFO_DIR)
	@$(CP) $(INFO_FILES) $(INFO_DIR)
	@for file in $(INFO_FILES); \
		do $(INSTALL_INFO) --dir-file=$(INFO_DIR)/dir $${file}; done

install-lisp: $(call print-help,install-lisp,Install lisp in $(LISP_DIR))
install-lisp: $(LISP_FILES) $(ELC_FILES)
	@echo "Installing lisp files into $(LISP_DIR)/"
	@$(MKDIR) $(LISP_DIR)
	@$(RM) $(INSTALLED_EL_FILES)
	@$(CP) $+ $(LISP_DIR)

install-mla: $(call print-help,install-mla,Install mla in $(MAPLE_INSTALL_DIR))
install-mla: $(mla)
	@$(MKDIR) $(MAPLE_INSTALL_DIR)
	@echo "Installing Maple archive $(mla) into $(MAPLE_INSTALL_DIR)/"
	@$(CP) $+ $(MAPLE_INSTALL_DIR)

# }}}
# {{{ zip

PHONY: zip

dist := $(LISP_FILES) $(mla) $(hdb) $(INFO_FILES) $(HTML_FILES) RELEASE-NOTES README install

zip: $(call print-help,zip,Create zipfile)
zip: $(dist)
	zip mdcs-$(VERSION).zip $+

# }}}

# {{{ clean

.PHONY: clean cleanall

clean: $(call print-help,clean,Remove built files)
clean:
	-$(RM) lisp/*.elc maple/src/_preview_.mm maple/mhelp/* maple/mhelp/* maple/mtest/*
	-$(RM) $(filter-out doc/mds.texi doc/MapleHelp_en.xml,$(wildcard doc/*))
	-$(RM) $(mla) $(hdb) 

cleanall: $(call print-help,cleanall,Remove installed files and built files)
cleanall: clean
	-$(RM) $(INSTALLED_EL_FILES) $(INSTALLED_ELC_FILES) 
	-$(RM) $(MAPLE_INSTALL_DIR)/$(mla) $(MAPLE_INSTALL_DIR)/$(hdb)
	-$(RM) $(INFO_DIR)/$(INFO_FILES)

# }}}

# end Make
