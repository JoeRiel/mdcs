base = mdb
maple = smaple
installdir = ~/emacs
mapleinstalldir = $(HOME)/maple/lib
localpath = $(installdir)
CP = cp

#emacs	= emacs --debug-init --no-site-file --no-init-file --eval '(setq debug-on-error t)'
emacs = emacs --no-site-file --no-init-file

#ELFLAGS = --eval '(setq load-path (append (list "." "$(elibdir)" "$(lispdir)") load-path))'
ELFLAGS	= --eval '(add-to-list (quote load-path) ".")' --eval '(add-to-list (quote load-path) "$(localpath)")'
#ELC	= $(emacs) --batch $(ELFLAGS) --funcall=batch-byte-compile-if-not-done
ELC	= $(emacs) --batch $(ELFLAGS) --funcall=batch-byte-compile

el-files  = $(addsuffix .el,mdb ir)
elc-files = $(el-files:.el=.elc)
mla = mdb.mla

default: compile
compile: $(elc-files)
mla: $(mla)
dist: $(base).zip

mdb.elc:

$(elc-files): $(el-files)
	$(ELC) $+

$(mla): mdb.mpl
	$(maple) -q $^

installmaple: $(mla)
	$(CP) --archive $+ $(mapleinstalldir)

installemacs: $(el-files) $(elc-files)
	$(CP) --archive $+ $(installdir)

install: installmaple installemacs

# Does not install elc files; useful for checking old versions of emacs.
installel: $(el-files)
	$(CP) --archive $+ $(installdir)

dist = $(el-files) Makefile README

$(base).zip: $(dist)
	zip $@ $?

clean:
	-$(RM) *.elc

# P4 upload

p4dir = /home/joe/work/MapleSoft/sandbox/groups/share/emacs/$(base)

p4put: $(el-files) Makefile README
	(cd $(p4dir); p4 edit $+)
	$(CP) $+ $(p4dir)

.PHONY: default compile install installel dist clean p4put mla installmaple installemacs