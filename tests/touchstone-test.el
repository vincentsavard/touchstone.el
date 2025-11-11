;;; touchstone-test.el --- Tests for touchstone.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for touchstone.el using ERT framework

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest touchstone-test-simple-passing ()
  "Test parsing simple pytest output with only passing tests."
  ;; Get the touchstone buffer
  (let ((test-buffer (touchstone--get-results-buffer)))
    (with-current-buffer test-buffer
      ;; Initialize for new test run
      (touchstone--init-results-buffer)

      ;; Simulate pytest output line by line
      (let ((output (touchstone-test-load-fixture "pytest-simple.txt")))
        (touchstone-test-simulate-output output))

      ;; Validate hash table has correct number of tests
      (should (= (hash-table-count touchstone--test-results) 4))

      ;; Validate buffer contains test results
      (let ((buffer-contents (buffer-string)))
        (should (string-match-p "PASS  tests/test_example\\.py::test_one" buffer-contents))
        (should (string-match-p "PASS  tests/test_example\\.py::test_two" buffer-contents))
        (should (string-match-p "PASS  tests/test_example\\.py::test_three" buffer-contents))
        (should (string-match-p "PASS  tests/test_example\\.py::test_four" buffer-contents))

        ;; Validate no + indicators (no failures with details)
        (should-not (string-match-p "\\+" buffer-contents)))

      ;; Validate each test has correct metadata
      (maphash
       (lambda (key value)
         (should (string= "tests/test_example.py" (plist-get value :file)))
         (should (member (plist-get value :test) '("test_one" "test_two" "test_three" "test_four")))
         (should (eq 'passed (plist-get value :status)))
         (should-not (plist-get value :details)))
       touchstone--test-results))))

(ert-deftest touchstone-test-with-failures ()
  "Test parsing pytest output with failures."
  (let ((test-buffer (touchstone--get-results-buffer)))
    (with-current-buffer test-buffer
      (touchstone--init-results-buffer)

      ;; Simulate pytest output
      (let ((output (touchstone-test-load-fixture "pytest-with-failures.txt")))
        (touchstone-test-simulate-output output))

      ;; Validate correct number of tests
      (should (= (hash-table-count touchstone--test-results) 4))

      ;; Validate buffer contains all test results with correct statuses
      (let ((buffer-contents (buffer-string)))
        (should (string-match-p "PASS  tests/test_example\\.py::test_one" buffer-contents))
        (should (string-match-p "FAIL\\+ tests/test_example\\.py::test_two" buffer-contents))
        (should (string-match-p "PASS  tests/test_example\\.py::test_three" buffer-contents))
        (should (string-match-p "FAIL\\+ tests/test_example\\.py::test_four" buffer-contents)))

      ;; Validate individual test metadata and details
      (let ((test-one (gethash "tests/test_example.py::test_one" touchstone--test-results))
            (test-two (gethash "tests/test_example.py::test_two" touchstone--test-results))
            (test-three (gethash "tests/test_example.py::test_three" touchstone--test-results))
            (test-four (gethash "tests/test_example.py::test_four" touchstone--test-results)))

        ;; Check statuses
        (should (eq 'passed (plist-get test-one :status)))
        (should (eq 'failed (plist-get test-two :status)))
        (should (eq 'passed (plist-get test-three :status)))
        (should (eq 'failed (plist-get test-four :status)))

        ;; Passed tests should have no details
        (should-not (plist-get test-one :details))
        (should-not (plist-get test-three :details))

        ;; Failed tests should have details
        (should (plist-get test-two :details))
        (should (plist-get test-four :details))

        ;; Validate test_two details
        (let ((details (plist-get test-two :details)))
          (should (plist-get details :error-lines))
          (should (plist-get details :location))
          (should (string-match-p "AssertionError" (plist-get details :location))))

        ;; Validate test_four details
        (let ((details (plist-get test-four :details)))
          (should (plist-get details :error-lines))
          (should (plist-get details :location))
          (should (string-match-p "ValueError" (plist-get details :location))))))))

(ert-deftest touchstone-test-with-output ()
  "Test parsing pytest output with captured stdout/stderr."
  (let ((test-buffer (touchstone--get-results-buffer)))
    (with-current-buffer test-buffer
      (touchstone--init-results-buffer)

      ;; Simulate pytest output
      (let ((output (touchstone-test-load-fixture "pytest-with-output.txt")))
        (touchstone-test-simulate-output output))

      ;; Validate correct number of tests
      (should (= (hash-table-count touchstone--test-results) 2))

      ;; Validate both tests are FAILED
      (let ((test-stdout (gethash "test_output.py::test_with_stdout" touchstone--test-results))
            (test-stderr (gethash "test_output.py::test_with_stderr" touchstone--test-results)))

        (should (eq 'failed (plist-get test-stdout :status)))
        (should (eq 'failed (plist-get test-stderr :status)))

        ;; Both should have details
        (should (plist-get test-stdout :details))
        (should (plist-get test-stderr :details))

        ;; Validate captured stdout
        (let ((details (plist-get test-stdout :details)))
          (should (plist-get details :stdout))
          (should (string-match-p "Debug output line 1" (plist-get details :stdout)))
          (should (string-match-p "Debug output line 2" (plist-get details :stdout))))

        ;; Validate captured stderr
        (let ((details (plist-get test-stderr :details)))
          (should (plist-get details :stderr))
          (should (string-match-p "Error message 1" (plist-get details :stderr)))
          (should (string-match-p "Error message 2" (plist-get details :stderr))))))))

(provide 'touchstone-test)

;;; touchstone-test.el ends here
