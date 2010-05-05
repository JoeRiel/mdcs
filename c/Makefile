targets := pmaple
CC 	:= gcc 
RM 	:= rm -rf
MAPLE 	:= $(MAPLE_ROOT)
SYSTYPE := $(shell ./maple.system.type)
LIB     := $(MAPLE)/internal/$(SYSTYPE)
INCLUDE := -I $(MAPLE)/extern/include
LIBS 	:= -L $(LIB)
#ENV     := LD_LIBRARY_PATH=$(LIB) MAPLE=$(MAPLE)
ENV     := LD_LIBRARY_PATH=$(shell ./getlibpath) MAPLE=$(MAPLE)

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

.PHONY: install

BINDIR := ${HOME}/bin

install: pmaple emaple getlibpath maple.system.type
	install $+ $(BINDIR)

