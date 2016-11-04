ELS   = emir.el

DEPS  = borg
DEPS += closql
DEPS += dash
DEPS += elx
DEPS += epkg
DEPS += emacsql
DEPS += finalize
DEPS += ghub
DEPS += magit/lisp
DEPS += packed
DEPS += with-editor

ELCS    = $(ELS:.el=.elc)
DFLAGS  = $(addprefix -L ../,$(DEPS))
EFLAGS ?= $(DFLAGS)
EMACS  ?= emacs
BATCH   = $(EMACS) -batch -Q -L . $(EFLAGS)

.PHONY: help clean

help:
	$(info make lisp   - create *.elc)
	$(info make clean  - remove *.elc)
	@printf "\n"

all: lisp

lisp: $(ELCS)
%.elc: %.el
	@printf "Compiling %s\n" $<
	@$(BATCH) -f batch-byte-compile $<

clean:
	@printf "Cleaning...\n"
	@rm -f $(ELCS)
