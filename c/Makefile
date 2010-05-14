# Makefile for pmaple
#
# Maintainer: Joe Riel <jriel@maplesoft.com>

# Variables

# This Makefile uses the environmental variable MAPLE_ROOT, which
# points to the directory where your maple is installed.

# MAPLE points to command-line Maple

MAPLE   := $(MAPLE_ROOT)/bin/smaple
INCLUDE := -I $(MAPLE_ROOT)/extern/include

targets := pmaple
CC 	:= gcc 
RM 	:= rm -rf
SYSTYPE := $(shell ./maple.system.type)
MAPLE_BIN := $(MAPLE_ROOT)/internal/$(SYSTYPE)
# This should be equivalent, but requires firing up maple.
# MAPLE_BIN := $(shell $(MAPLE) -q -c "printf(\"%s\n\",kernelopts('bindir'))" -c "done" )
LIBS 	:= -L $(MAPLE_BIN)
ENV     := LD_LIBRARY_PATH=$(shell ./getlibpath) MAPLE=$(MAPLE_ROOT)

#CFLAGS  := -m32
CFLAGS  :=

.PHONY: default clean

default: $(targets)

%: %.c
	@echo "compiling and linking $< ..."
	@echo $(ENV) $(CC) $^ -o $@ $(INCLUDE) $(OPTS) $(LIBS) -l rt -l maplec
	$(ENV) $(CC) $^ -o $@ $(INCLUDE) $(CFLAGS) $(LIBS) -l rt -l maplec

clean:
	$(RM) $(targets)

# Installation

.PHONY: install uninstall

BIN_INSTALL_DIR := $(HOME)/bin

installs = pmaple emaple getlibpath maple.system.type

install: $(installs)
	install $+ $(BIN_INSTALL_DIR)

uninstall:
	$(RM) -f $(addprefix $(BIN_INSTALL_DIR)/,$(installs))
