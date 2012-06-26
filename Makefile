# Makefile - for mdcs, a Maple Debugger Client/Server
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

pkg := mdcs
maple-pkg := mdc
emacs-pkg := mds
SHELL := /bin/bash

VERSION := 1.14.1

include help-system.mak

.PHONY: default

comma := ,
OS := $(shell uname -o)

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

MINT := mint

CP := cp --archive
BROWSER := firefox
MAKEINFO := makeinfo
MKDIR := mkdir -p
MTAGS := mtags
TEXI2PDF := texi2pdf
TEXI2HTML := makeinfo --html --number-sections

# }}}
# {{{ Directories

# where lisp files go
LISP_BASE := $(HOME)/.emacs.d
LISP_DIR := $(LISP_BASE)/maple

# where info files go
INFO_DIR := $(HOME)/share/info

# Maple toolbox directory
TOOLBOX_DIR := $(HOME)/maple/toolbox/emacs

# where html files go.
# there is no standard place for this.
HTML_DIR := $(TOOLBOX_DIR)/doc

# where the maple archive and hdb go
MAPLE_INSTALL_DIR := $(TOOLBOX_DIR)/lib

MAPLE_DATA_DIR := $(TOOLBOX_DIR)/data

# Cypathify, as needed
ifeq ($(OS),Cygwin)
 LISP_BASE := $(shell cygpath --mixed "$(LISP_BASE)")
 LISP_DIR  := $(shell cygpath --mixed "$(LISP_DIR)")
 INFO_DIR  := $(shell cygpath --mixed "$(INFO_DIR)")
 MAPLE_INSTALL_DIR := $(shell cygpath --mixed "$(MAPLE_INSTALL_DIR)")
# MAPLE := $(shell cygpath --mixed "$(MAPLE)")
endif

# }}}

# {{{ Auxiliary functions (warn, shellerr)

txtbold   := $(shell tput bold)
# 0=black 1=red 2=green 3=yellow 4=blue 5=magenta 6=cyan 7=white
txthilite := $(shell tput setaf 3)
txtnormal := $(shell tput sgr0)
warn = "$(txthilite)$1$(txtnormal)"
shellerr = $(call showerr,$1 2>&1 > /dev/null)
showerr = err="$$($1)" ; if [ "$$err" ]; then echo $(call warn,$$err); fi

# }}}

# {{{ emacs

ELFLAGS	= --no-site-file \
	  --no-init-file \
	  --eval "(push (expand-file-name \"./lisp\") load-path)" \
	  --eval "(push \"$(LISP_DIR)\" load-path)"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

ELS = mds-re mds-ss mds-out mds-patch mds-queue mds-wm mds-login mds-client mds-custom mds-thing mds-li mds

LISP_FILES = $(ELS:%=lisp/%.el)
ELC_FILES = $(LISP_FILES:.el=.elc)

byte-compile: $(call print-help,byte-compile,Byte compile $$(LISP_FILES))
byte-compile: $(ELC_FILES)

%.elc : %.el
	@$(RM) $@
	@echo Byte-compiling $+
	@$(call showerr,$(ELC) $< 2>&1 > /dev/null | sed '/^Wrote/d')

clean-elisp: $(call print-help,clean-elisp,Remove byte-compiled files)
clean-elisp:
	$(RM) $(ELC_FILES)

.PHONY: byte-compile clean-elisp

# }}}
# {{{ mla

help: $(call print-separator)

mms = $(wildcard maple/src/*.mm)

.PHONY: mla
mla := $(maple-pkg).mla
mla: $(call print-help,mla,Create Maple archive: $(mla))
mla: remove-preview $(mla)

# build mla.  This assumes the %.mpl file
# contains a Library:-Save command
%.mla: maple/src/%.mpl $(mms)
	@$(RM) $@
	@echo "Building Maple archive $@"
	@$(call showerr,echo $$($(MAPLE) -I maple -D BUILD_MLA -q $< ))

# }}}

# doc 
# {{{ info

help: $(call print-separator)

.PHONY: doc info html h i p clean-info

TEXI_FILES = doc/$(emacs-pkg).texi
INFO_FILES = doc/$(emacs-pkg)
HTML_FILES = doc/$(emacs-pkg).html
PDF_FILES  = doc/$(emacs-pkg).pdf

doc/$(emacs-pkg).pdf: doc/$(emacs-pkg).texi
	(cd doc; $(TEXI2PDF) $(emacs-pkg).texi)

doc/$(emacs-pkg): doc/$(emacs-pkg).texi
	@echo "Creating info file $@"
	@$(call shellerr,cd doc; $(MAKEINFO) --no-split $(emacs-pkg).texi --output=$(emacs-pkg))

doc: $(call print-help,doc,Create info$(comma) html$(comma) and pdf)
doc: info pdf html
info: $(call print-help,info,Create info)
info: doc/$(emacs-pkg)
pdf: $(call print-help,pdf,Create pdf)
pdf: doc/$(emacs-pkg).pdf
html: $(call print-help,html,Create html documentation)
html: doc/$(emacs-pkg).html

doc/$(emacs-pkg).html: doc/$(emacs-pkg).texi
	@echo "Creating html file $@"
	@(cd doc; $(TEXI2HTML) --no-split -o $(emacs-pkg).html $(emacs-pkg).texi)

# preview html
h: $(call print-help,h,Preview the html)
h: doc/$(emacs-pkg).html
	$(BROWSER) $^

# preview info
i: $(call print-help,i,Update and display info)
i: doc/$(emacs-pkg)
	@info $^


# preview pdf
p: $(call print-help,p,Update and display pdf)
p: doc/$(emacs-pkg).pdf
	evince $^

clean-info:
	$(RM) $(INFO_FILES) $(HTML_FILES) $(PDF_FILES)

# }}}
# {{{ hdb

help: $(call print-separator)

.PHONY: hdb remove-preview

remove-preview :
	$(RM) maple/src/_preview_.mm

hdb := $(maple-pkg).hdb
hdb: $(call print-help,hdb,Create Maple help database)
hdb: install-mla $(maple-pkg).hdb

$(maple-pkg).hdb : maple/src/$(maple-pkg).mpl $(mms)
	@echo "Creating Maple help database"
	@$(RM) maple/src/_preview_.mm
	@$(call showerr,mpldoc --config nightly $+ 2>&1 | sed -n '/Warning/{p;n};/Error/p')
	@shelp mwhelpload --config=doc/MapleHelp_en.xml --input=. --output=.


# }}}

# misc
help: $(call print-separator)

# {{{ tags

.PHONY: tags
tags: $(call print-help,tags	,Create TAGS file)
tags:
	$(MTAGS) maple/src/*
	@$(call shellerr,etags --append --language=lisp lisp/*.el)

# }}}
# {{{ mint

mint: $(call print-help,mint	,Check Maple syntax)
mint:
	@$(call showerr,$(MINT) -q -i2 -I $(HOME)/maple < maple/src/$(maple-pkg).mpl)

# }}}
# {{{ test

.PHONY: test test-extract test-run

TESTER := tester -maple "smaple -B -I $(dir $(PWD))"

test: $(call print-help,test	,Extract and run test suite)
test-extract: $(call print-help,test-extract,Extract test suite)
test-run: $(call print-help,test-run,Run test suite)

test: test-extract test-run


test-run:
	@echo Running tests
	@$(RM) *.fail
	@$(call shellerr,ls maple/mtest/*.tst 2> /dev/null | xargs -P4 -n1 $(TESTER))

test-extract:
	@echo Extracting tests
	@$(RM) maple/src/_preview_.mm
	@$(RM) maple/mtest/*
	@$(call showerr,mpldoc --config nightly maple/src 2>&1 | egrep --after-context=2 'Warning|Error')

# }}}

# {{{ install

help: $(call print-separator)

install-all := $(addprefix install-,hdb maple data)

.PHONY: install $(install-all) uninstall

INSTALLED_EL_FILES  := $(addprefix $(LISP_DIR)/,$(notdir $(LISP_FILES)))
INSTALLED_ELC_FILES := $(addprefix $(LISP_DIR)/,$(notdir $(ELC_FILES)))

install-all: $(install-all)

install: $(call print-help,install	,Install everything)
install: $(install-all)

install: $(addprefix install-,dev data hdb html info lisp maple)

install-data: $(call print-help,install-data,Install data)
install-data: data/*.mpl
	@echo "Installing data files into $(MAPLE_DATA_DIR)"
	@$(MKDIR) $(MAPLE_DATA_DIR)
	@$(CP) --target-directory=$(MAPLE_DATA_DIR) $+

install-dev: $(call print-help,install-dev,Install everything but hdb)
install-dev: install-elc install-info install-maple

# Install el files but not elc files; useful for checking old versions of emacs.
install-el: $(call print-help,install-el,Install el files but not elc files)
install-el: $(LISP_FILES)
	$(MKDIR) $(LISP_DIR)
	$(CP) $+ $(LISP_DIR)

install-links: $(call print-help,install-links,Install links to the lisp files)
install-links: $(LISP_FILES) $(ELC_FILES)
	@$(MKDIR) $(LISP_DIR)
	ln -nfst $(LISP_DIR) $(realpath $(LISP_FILES))
	ln -nfst $(LISP_DIR) $(realpath $(ELC_FILES))


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
	@$(call shellerr,$(CP) $(hdb) $(MAPLE_INSTALL_DIR))

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
	@$(call shellerr,$(CP) $+ $(MAPLE_INSTALL_DIR))

uninstall: $(call print-help,uninstall,Remove directory $(TOOLBOX_DIR))
uninstall:
	@rm -rf $(TOOLBOX_DIR)

# }}}
# {{{ installer

help: $(call print-separator)

.PHONY: installer installer-zip

installer := $(pkg)-installer-$(VERSION).mla

installer: $(call print-help,installer,Create Maple installer: $(installer))
installer: $(installer)

$(installer): hdb mla
	@$(call shellerr, $(MAPLE) -q maple/installer/CreateInstaller.mpl)

installer-zip: $(call print-help,installer-zip,Create Maple installer zip file)
installer-zip: installer
	zip mdcs-ins-$(subst .,-,$(VERSION)).zip $(installer) README-installer run-installer run-installer.bat

# }}}
# {{{ zip

PHONY: zip-installer

zip := $(pkg)-$(VERSION).zip
dist := $(installer) $(hdb) RELEASE-NOTES README

zip-installer: $(call print-help,zip-installer,Create zipfile of installer)
zip-installer: $(dist)
	zip $(zip) $+

# }}}
# {{{ clean

help: $(call print-separator)

.PHONY: clean cleanall sweep

clean: $(call print-help,clean	,Remove built files)
clean: clean-info
	-$(RM) lisp/*.elc maple/src/_preview_.mm maple/mdoc/* maple/mhelp/* maple/mtest/* maple/doti/* maple/src/*.{mtest,tst,mw} *.fail
	-$(RM) $(mla) $(hdb) 

clean: $(call print-help,sweep	,Remove editor temp files)
sweep:
	@find . -wholename './.git' -prune -o -name '*~' -print -exec rm {} \+

cleanall: $(call print-help,cleanall,Remove installed files and built files)
cleanall: clean
	-$(RM) $(MAPLE_INSTALL_DIR)/$(mla) $(MAPLE_INSTALL_DIR)/$(hdb) $(installer)

# }}}

# end Make
