# Default Emacs command
EMACS ?= emacs

# Test files
TEST_HELPER = tests/test-helper.el
TEST_FILE = tests/touchstone-test.el

.PHONY: test
test:
	$(EMACS) -batch -l $(TEST_HELPER) -l $(TEST_FILE) -f ert-run-tests-batch-and-exit
