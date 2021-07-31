# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

clean:
	@rm -f *~
	@rm -f \#*\#
	@rm -f *.elc

.PHONY: test
test: clean
	@$(EMACS) -batch -Q -L . -l cliphist.el -l tests/cliphist-tests.el
