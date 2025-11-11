# Default Emacs command
EMACS ?= emacs

# Test files
TEST_HELPER = tests/test-helper.el
TEST_FILE = tests/touchstone-test.el
PYTEST_TEST_FILE = tests/touchstone-pytest-test.el
CORE_TEST_FILE = tests/touchstone-core-test.el

.PHONY: test
test:
	$(EMACS) -batch -l $(TEST_HELPER) -l $(TEST_FILE) -l $(PYTEST_TEST_FILE) -l $(CORE_TEST_FILE) -f ert-run-tests-batch-and-exit
