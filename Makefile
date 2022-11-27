# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

.PHONY: clean test compile deps

EMACS_BATCH_OPTS=--batch -Q \
-L . \
-l deps/popup.el \
-l vc-msg.el

RM = @rm -rf

clean:
	$(RM) *~
	$(RM) \#*\#
	$(RM) *.elc

deps:
	@mkdir -p deps;
	@if [ ! -f deps/popup.el ]; then curl -L https://raw.githubusercontent.com/auto-complete/popup-el/20ce6cbd2f06423be35b3b700c698f0e109e880c/popup.el > deps/popup.el; fi;

compile: deps
	$(RM) *.elc
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/my-byte-compile.el 2>&1 | grep -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0

test: compile deps
	@$(EMACS) $(EMACS_BATCH_OPTS) -l tests/vc-msg-tests.el
