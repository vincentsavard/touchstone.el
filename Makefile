# Default Emacs command
EMACS ?= emacs

.PHONY: test
test:
	$(EMACS) -batch -L . -l tests/test-helper.el \
		$(patsubst %,-l %,$(wildcard tests/*-test.el)) \
		-f ert-run-tests-batch-and-exit
