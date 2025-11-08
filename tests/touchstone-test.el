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
        (should (string-match-p "tests/test_example\\.py::test_one.*PASSED" buffer-contents))
        (should (string-match-p "tests/test_example\\.py::test_two.*PASSED" buffer-contents))
        (should (string-match-p "tests/test_example\\.py::test_three.*PASSED" buffer-contents))
        (should (string-match-p "tests/test_example\\.py::test_four.*PASSED" buffer-contents))

        ;; Validate no [+] indicators (no failures)
        (should-not (string-match-p "\\[+\\]" buffer-contents)))

      ;; Validate each test has correct metadata
      (maphash
       (lambda (key value)
         (should (string= "tests/test_example.py" (plist-get value :file)))
         (should (member (plist-get value :test) '("test_one" "test_two" "test_three" "test_four")))
         (should (string= "PASSED" (plist-get value :status)))
         (should-not (plist-get value :details)))
       touchstone--test-results))))

(provide 'touchstone-test)

;;; touchstone-test.el ends here
