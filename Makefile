ELS  = emir.el
ELS += emir-report.el

DEPS  = ape
DEPS += dash
DEPS += elx
DEPS += epkg
DEPS += emacsql
DEPS += finalize
DEPS += ghub
DEPS += magit
DEPS += melpa-db
DEPS += packed
DEPS += request
DEPS += with-editor

ELCS    = $(ELS:.el=.elc)
DFLAGS  = $(addprefix -L ../,$(DEPS)) -L ../epkg/lisp -L ../magit/lisp
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
