# Makefile - for mdcs, a Maple Debugger Client/Server
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

SHELL := /bin/sh

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

CP = cp --archive
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
	@echo "Creating info file $@"
	@(cd doc; $(MAKEINFO) --no-split $(PKG).texi --output=$(PKG))


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

ELS = mds-login mds-showstat mds-output mds-windows mds

LISPFILES = $(ELS:%=lisp/%.el)
ELCFILES = $(LISPFILES:.el=.elc)

byte-compile: $(call print-help,byte-compile,Byte compile $(LISPFILES))
byte-compile: $(ELCFILES)

%.elc : %.el
	@echo Byte-compiling $+
	@$(ELC) $<

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
	etags --language=lisp lisp/*.el
	bin/mtags maple/src/*

# }}}

# {{{ install

.PHONY: install-el install-maple install-lisp install-info install install-dev

INSTALLED_EL_FILES  := $(addprefix $(LISP_DIR)/,$(notdir $(LISPFILES)))
INSTALLED_ELC_FILES := $(addprefix $(LISP_DIR)/,$(notdir $(ELCFILES)))

install: $(call print-help,install,Install everything)
install: install-lisp install-info install-maple

install-dev: $(call print-help,install-dev,Install everything but link el files to source)
install-dev: install-elc install-info install-maple

install-maple: $(call print-help,install-maple,Install mla in $(MAPLE_INSTALL_DIR))
install-maple: $(mla)
	@$(MKDIR) $(MAPLE_INSTALL_DIR)
	@echo "Installing Maple archive into $(MAPLE_INSTALL_DIR)/"
	@$(CP) $+ $(MAPLE_INSTALL_DIR)

install-lisp: $(call print-help,install-lisp,Install lisp in $(LISP_DIR))
install-lisp: $(LISPFILES) $(ELCFILES)
	@echo "Installing lisp files into $(LISP_DIR)/"
	@$(MKDIR) $(LISP_DIR)
	@$(RM) $(INSTALLED_EL_FILES)
	@$(CP) $+ $(LISP_DIR)

install-info: $(call print-help,install-info,Install info files in $(INFO_DIR) and update dir)
install-info: $(INFOFILES)
	@echo "Installing info file(s) into $(INFO_DIR)/ and updating $(INFO_DIR)/dir"
	@$(MKDIR) $(INFO_DIR)
	@$(CP) $(INFOFILES) $(INFO_DIR)
	@for file in $(INFOFILES); \
		do $(INSTALL_INFO) --dir-file=$(INFO_DIR)/dir $${file}; done

# Install el files but not elc files; useful for checking old versions of emacs.
install-el: $(call print-help,install-el,Install el files but not elc files)
install-el: $(LISPFILES)
	$(MKDIR) $(LISP_DIR)
	$(CP) $+ $(LISP_DIR)


# Install elc files but not elc files; instead create symm links to the source
install-elc: $(call print-help,install-elc,Install elc files and link *.el files)
install-elc: $(ELCFILES)
	@echo "Installing elc files, and symbolic links to *.el files, into $(LISP_DIR)"
	@$(MKDIR) $(LISP_DIR)
	@$(CP) $+ $(LISP_DIR)
	@$(RM) $(INSTALLED_EL_FILES)
	@ln --symbolic --target-directory=$(LISP_DIR) $(LISPFILES)
	@for file in $(LISPFILES); do touch --reference=$$file --no-dereference $(LISP_DIR)/$${file##*/}; done

# }}}
# {{{ distribution

PHONY: dist

dist = $(ELS) $(TEXIFILE) $(INFOFILES) Makefile README

dist: $(PKG).zip

$(PKG).zip: $(dist)
	zip $@ $?

# }}}

# {{{ clean

.PHONY: clean cleanall

clean: $(call print-help,clean,Remove built files)
clean:
	-$(RM) lisp/*.elc
	-$(RM) $(filter-out doc/mds.texi,$(wildcard doc/*))
	-$(RM) $(mla)

cleanall: $(call print-help,cleanall,Remove installed files and built files)
cleanall: clean
	-$(RM) $(INSTALLED_EL_FILES) $(INSTALLED_ELC_FILES)
	-$(RM) $(MAPLE_INSTALL_DIR)/$(mla)
	-$(RM) $(INFO_DIR)/$(INFOFILES)

# }}}

# end Make
