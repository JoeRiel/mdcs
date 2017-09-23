# Makefile - for mdcs, a Maple Debugger Client/Server
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

pkg := mdcs
SHELL := /bin/bash

# {{{ Version

ifneq ($(wildcard .git),)
  GITHASH    ?= $(shell git rev-parse HEAD)
  GIT-VERSION ?= $(shell git describe --match release\* --abbrev=6 HEAD)
  MDS-VERSION ?= $(subst release-,,$(shell git describe --match release\* --abbrev=0 HEAD))
  GITSTATUS ?= $(shell git status -uno --porcelain)
else
  GITHASH    ?= N/A
  GIT-VERSION ?= N/A
  MDS-VERSION ?= N/A
endif
DATE := $(shell date +%Y-%m-%d)
ifneq ($(GITSTATUS),)
  GIT-VERSION := $(GIT-VERSION:.dirty=).dirty
endif

# }}}

include help-system.mak

.PHONY: default

comma := ,
OS := $(shell uname -o)

# {{{ Executables

# Assign local variables for needed binaries

# Modify these to something appropriate, or specify values on the
# command line.  

EMACS := emacs
MAPLE := smaple

ifeq ($(OS),GNU/Linux)
  INSTALL-INFO = ginstall-info
else
  INSTALL-INFO = install-info
endif

MINT  := mint
MLOAD := mload
MHELP := mhelp

CP := cp --archive
BROWSER := x-www-browser
MAKEINFO := makeinfo
MKDIR := mkdir -p
MTAGS := mtags
TEXI2PDF := texi2pdf
TEXI2HTML := makeinfo --html --number-sections

# }}}
# {{{ Directories

# where lisp files go
LISP-BASE := $(HOME)/.emacs.d
LISP-DIR := $(LISP-BASE)/maple

# where info files go
INFO-DIR := $(HOME)/share/info

# Maple toolbox directory
TOOLBOX-DIR := $(HOME)/maple/toolbox/emacs

# where html files go.
# there is no standard place for this.
HTML-DIR := $(TOOLBOX-DIR)/doc

# where the maple archive and help database go
MAPLE-INSTALL-DIR := $(TOOLBOX-DIR)/lib

MAPLE-DATA-DIR := $(TOOLBOX-DIR)/data

# Cypathify, as needed
ifeq ($(OS),Cygwin)
 LISP-BASE := $(shell cygpath --mixed "$(LISP-BASE)")
 LISP-DIR  := $(shell cygpath --mixed "$(LISP-DIR)")
 INFO-DIR  := $(shell cygpath --mixed "$(INFO-DIR)")
 MAPLE-INSTALL-DIR := $(shell cygpath --mixed "$(MAPLE-INSTALL-DIR)")
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

# {{{ build

.PHONY: build

build: $(call print-help,build,	byte-compile and install mla)
build: byte-compile mla-install

# }}}

# {{{ emacs

help: $(call print-separator)

ELFLAGS	= --no-site-file \
	  --no-init-file \
          --no-splash \
	  --eval "(push (expand-file-name \"./lisp\") load-path)" \
	  --eval "(push \"$(LISP-DIR)\" load-path)" \
	  --eval "(push (concat user-emacs-directory \"elisp\") load-path)" \
	  --eval "(delete \"/usr/share/emacs/23.3/site-lisp/emacs-goodies-el\" load-path)"

ELC = $(EMACS) --batch $(ELFLAGS) --funcall=batch-byte-compile

EL-FILES = $(wildcard lisp/*.el)
LISP-VERSION := lisp/mds-version.el
EL-FILES-NO-VERSION = $(filter-out $(LISP-VERSION),$(EL-FILES))
ELC-FILES = $(EL-FILES-NO-VERSION:.el=.elc)

$(LISP-VERSION): $(EL-FILES-NO-VERSION)
	@echo "Creating $@"
	@$(RM) $@
	@echo ";;; mds-version.el --- auto-generated file, do not edit." > $@
	@echo "(defun mds-release () \"Return mds-release.\" \"$(MDS-VERSION)\")" >> $@
	@echo "(defun mds-git-version () \"Return mds-git-version.\" \"$(GIT-VERSION)\")" >> $@
	@echo "(provide 'mds-version)" >> $@
	@echo ";; Local Variables:" >> $@
	@echo ";; version-control: never" >> $@
	@echo ";; no-byte-compile: t" >> $@
	@echo ";; coding: utf-8" >> $@
	@echo ";; End:" >> $@
	@echo ";;; mds-version.el ends here" >> $@

byte-compile: $(call print-help,byte-compile,Byte compile $$(EL-FILES))
byte-compile: $(LISP-VERSION) $(EL-FILES-NO-VERSION) $(ELC-FILES)

%.elc: %.el
	@$(RM) $@
	@echo Byte-compiling $+
	@$(call showerr,$(ELC) $< 2>&1 > /dev/null | sed '/^Wrote/d')

clean-elisp: $(call print-help,clean-elisp,Remove byte-compiled files)
clean-elisp:
	$(RM) $(ELC-FILES)

lisp-install: $(call print-help,lisp-install,Install lisp in $(LISP-DIR))
lisp-install: $(EL-FILES) $(ELC-FILES)
	@$(MKDIR) $(LISP-DIR)
	$(CP) $+ $(LISP-DIR)

# Install el files but not elc files; useful for checking old versions of emacs.
el-install: $(call print-help,el-install,Install el files but not elc files)
el-install: $(LISP-VERSION) $(EL-FILES)
	$(MKDIR) $(LISP-DIR)
	$(CP) $^ $(LISP-DIR)

links-install: $(call print-help,links-install,Install links to the lisp files)
links-install: $(LISP-VERSION) $(EL-FILES) $(ELC-FILES)
	@$(MKDIR) $(LISP-DIR)
	@ln --no-dereference --force --symbol --target-directory=$(LISP-DIR) $(realpath $^)

links-uninstall: $(call print-help,links-uninstall,Remove links to the lisp files)
links-uninstall:
	$(RM) $(addprefix $(LISP-DIR)/,$(notdir $(LISP-VERSION) $(EL-FILES) $(ELC-FILES)))

.PHONY: byte-compile clean-elisp links-install links-uninstall lisp-install

# }}}
# {{{ mla

help: $(call print-separator)

mms = $(wildcard maple/src/*.mm)

.PHONY: mla mla-install
mla := mdc.mla
mla: $(call print-help,mla,	Create Maple archive: $(mla))
mla: remove-preview $(mla)

# Build mla. 
# Note that we need to provide the full path to the include directive,
# otherwise the lineinfo data is relative.
%.mla: maple/src/%.mpl $(mms) maple/include/*.mi
	@$(RM) $@
	@echo "Building Maple archive $@"
	$(MLOAD) --quiet --skipini --include=$(CURDIR)/maple \
	         --define='__GIT_HASH__="$(GITHASH)"' \
	         --define='__GIT_BUILD__="$(GIT-VERSION)"' \
	         --define='__VERSION__="$(MDS-VERSION)"' \
	         --reindex --readonly \
	         --maple=$(MAPLE) \
	         --lib=${MAPLE_ROOT}/lib/maple.mla \
	         --mla=$@ $<

mla-install: $(call print-help,mla-install,Install mla into $(MAPLE-INSTALL-DIR))
mla-install: $(mla)
	@$(MKDIR) $(MAPLE-INSTALL-DIR)
	@echo "Installing Maple archive $(mla) into $(MAPLE-INSTALL-DIR)/"
	@$(call shellerr,$(CP) $+ $(MAPLE-INSTALL-DIR))

# }}}

# doc 
# {{{ info pdf html

help: $(call print-separator)

.PHONY: doc info html h i p info-clean info-install

INFO-FILES = doc/mds.info
HTML-FILES = doc/mds.html
PDF-FILES  = doc/mds.pdf
VERSION-TEXI = doc/mds-version.texi

MAKEINFO-VARS = -D 'VERSION $(GIT-VERSION)' -D 'DATE $(DATE)'

# $(VERSION-TEXI): doc/mds.texi
# 	@echo "Update $@: $(MDS-VERSION) ($(GIT-VERSION))"
# 	@echo "@c mds-version.texi --- auto-generated file, do not edit." > $@
# 	@echo "@set VERSION $(MDS-VERSION) ($(GIT-VERSION))" >> $@
# 	@echo "@set DATE $(DATE)" >> $@
# 	@echo "@c mds-version.texi ends here" >> $@

doc/mds.info: doc/mds.texi $(VERSION-TEXI)
	@echo "Creating info file $@"
	@$(call shellerr,cd doc; $(MAKEINFO) \
	   --no-split mds.texi \
	   --output=mds.info \
	   $(MAKEINFO-VARS) \
	  )

doc/mds.pdf: doc/mds.texi $(VERSION-TEXI)
	(cd doc; $(TEXI2PDF) $(MAKEINFO-VARS) mds.texi)


doc: $(call print-help,doc,	Create info$(comma) html$(comma) and pdf)
doc: info pdf html
info: $(call print-help,info,	Create info)
info: doc/mds.info
pdf: $(call print-help,pdf,	Create pdf)
pdf: doc/mds.pdf
html: $(call print-help,html,	Create html documentation)
html: doc/mds.html $(VERSION-TEXI)

doc/mds.html: doc/mds.texi
	@echo "Creating html file $@"
	@(cd doc; $(TEXI2HTML) --no-split -o mds.html mds.texi)

# preview html
h: $(call print-help,h,	Preview the html)
h: doc/mds.html
	$(BROWSER) $^

# preview info
i: $(call print-help,i,	Update and display info)
i: doc/mds.info
	@info $^


# preview pdf
p: $(call print-help,p,	Update and display pdf)
p: doc/mds.pdf
	evince $^

html-install: $(call print-help,html-install,Install html files in $(HTML-DIR) and update dir)
html-install: $(HTML-FILES)
	@echo "Installing html file(s) into $(HTML-DIR)/"
	@$(MKDIR) $(HTML-DIR)
	$(CP) $(HTML-FILES) $(HTML-DIR)

info-install: $(call print-help,info-install,Install info files in $(INFO-DIR) and update dir)
info-install: $(INFO-FILES)
	@echo "Installing info file(s) into $(INFO-DIR)/ and updating $(INFO-DIR)/dir"
	@$(MKDIR) $(INFO-DIR)
	@$(CP) $(INFO-FILES) $(INFO-DIR)
	@for file in $(INFO-FILES); \
		do $(INSTALL-INFO) --dir-file=$(INFO-DIR)/dir $${file}; done

info-clean:
	$(RM) $(INFO-FILES) $(HTML-FILES) $(PDF-FILES)
	$(RM) $(filter-out $(wildcard doc/*.texi),$(wildcard doc/*))o

# }}}
# {{{ hlp

help: $(call print-separator)

.PHONY: hlp remove-preview

remove-preview :
	$(RM) maple/src/_preview_.mm

hlp := mdc.help
hlp: $(call print-help,hlp,	Create Maple help database: $(hlp))
hlp: mla-install data-install remove-preview mdc.help

mdc.help : maple/src/mdc.mpl $(mms)
	@echo "Creating Maple help database"
	@$(RM) $@ maple/src/_preview_.mm
	@$(call showerr,mpldoc --config nightly $+ 2>&1 | sed -n '/Warning/{p;n};/Error/p')
	@$(MHELP) --replace --readonly mdc

hlp-install: $(call print-help,hlp-install,Install $(hlp) in $(MAPLE-INSTALL-DIR))
hlp-install: hlp
	@$(MKDIR) $(MAPLE-INSTALL-DIR)
	@echo "Installing Maple help data base $(hlp) into $(MAPLE-INSTALL-DIR)/"
	@$(call shellerr,$(CP) $(hlp) $(MAPLE-INSTALL-DIR))

# }}}
# {{{ data

help: $(call print-separator)
data-install: $(call print-help,data-install,Install data)
data-install: data/*.mpl
	@echo "Installing data files, $^, into $(MAPLE-DATA-DIR)"
	@$(MKDIR) $(MAPLE-DATA-DIR)
	@$(CP) --target-directory=$(MAPLE-DATA-DIR) $+

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
	@$(call showerr,$(MINT) -l -q -i2 -I $(HOME)/emacs/mdcs/maple < maple/src/mdc.mpl)

# }}}
# {{{ test

.PHONY: test test-extract test-run

TESTER := tester -maple "smaple -B -I $(dir $(PWD)/maple/include/)"

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

install-all := $(addsuffix -install,hlp maple data)

.PHONY: install $(install-all) uninstall

INSTALLED-EL-FILES  := $(addprefix $(LISP-DIR)/,$(notdir $(EL-FILES)))
INSTALLED-ELC-FILES := $(addprefix $(LISP-DIR)/,$(notdir $(ELC-FILES)))

install-all: $(install-all)

install: $(call print-help,install	,Install everything)
install: $(install-all)

install: $(addsuffix -install,hlp html info lisp maple)

uninstall: $(call print-help,uninstall,Remove directory $(TOOLBOX-DIR))
uninstall:
	$(RM) --recursive $(TOOLBOX-DIR)
	$(RM) $(LISP-DIR)/mds*.*

# }}}
# {{{ installer

help: $(call print-separator)

.PHONY: installer installer-zip

CreateInstaller := maple/installer/CreateInstaller.mpl

installer := $(pkg)-installer-$(MDS-VERSION).mla

installer: $(call print-help,installer,Create Maple installer: $(installer))
installer: $(installer)

$(installer): $(CreateInstaller) hlp mla info .emacs maple/installer/config.mpl data/Sample.mpl
	@[ "$$(git rev-parse --abbrev-ref HEAD)" = release ] || echo $(call warn,"Not on release branch")
	@$(call shellerr, $(MAPLE) -q $<)

installer-zip := $(pkg)-ins-$(subst .,-,$(MDS-VERSION)).zip
installer-zip: $(call print-help,installer-zip,Create Maple installer zip file: $(installer-zip))
installer-zip: $(installer-zip)

$(installer-zip): $(installer) README-installer run-installer run-installer.bat
	zip $@ $+

# }}}
# {{{ zip

PHONY: zip-src

zip-src := $(pkg)-$(MDS-VERSION)-src.zip
zip-src: $(call print-help,zip-src,	Create zipfile of source)
zip-src:
	git archive HEAD --output=$(zip-src)

# }}}
# {{{ clean

help: $(call print-separator)

.PHONY: clean cleanall sweep

clean: $(call print-help,clean	,Remove built files)
clean: info-clean
	-$(RM) lisp/*.elc maple/src/_preview_.mm maple/mdoc/* maple/mhelp/* maple/mtest/* maple/doti/* maple/src/*.{mtest,tst,mw} *.fail
	-$(RM) $(mla) $(hlp) 

clean: $(call print-help,sweep	,Remove editor temp files)
sweep:
	@find . -wholename './.git' -prune -o -name '*~' -print -exec rm {} \+

cleanall: $(call print-help,cleanall,Remove installed files and built files)
cleanall: clean
	$(RM) -r $(TOOLBOX-DIR)/ $(installer)

# }}}

# end Make
