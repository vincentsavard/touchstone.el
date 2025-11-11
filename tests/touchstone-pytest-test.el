;;; touchstone-pytest-test.el --- Tests for touchstone-pytest.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Isolated unit tests for the pytest backend using only the public interface

;;; Code:

(require 'ert)
(require 'test-helper)

;;; Helper Functions

(defun touchstone-pytest-test-get-backend ()
  "Get the pytest backend plist from the registry."
  (cdr (assq 'pytest touchstone--backends)))

(defun touchstone-pytest-test-process-lines (lines backend)
  "Process LINES through BACKEND's :parse-line, return (results . final-state)."
  (let* ((create-state-fn (plist-get backend :create-parser-state))
         (parse-fn (plist-get backend :parse-line))
         (state (funcall create-state-fn))
         (results nil))
    (dolist (line lines)
      (let* ((result-and-state (funcall parse-fn line state))
             (result (car result-and-state))
             (new-state (cdr result-and-state)))
        (setq state new-state)
        (when result
          (push result results))))
    (cons (nreverse results) state)))

(defun touchstone-pytest-test-load-and-process-fixture (filename)
  "Load fixture FILENAME and process through pytest backend.
Returns (results . final-state) where results includes both
:parse-line results and :finish result."
  (let* ((backend (touchstone-pytest-test-get-backend))
         (output (touchstone-test-load-fixture filename))
         (lines (split-string output "\n" t))
         (results-and-state (touchstone-pytest-test-process-lines lines backend))
         (results (car results-and-state))
         (state (cdr results-and-state))
         (finish-fn (plist-get backend :finish))
         (finish-result-and-state (funcall finish-fn state))
         (finish-result (car finish-result-and-state))
         (final-state (cdr finish-result-and-state)))
    (when finish-result
      (setq results (append results (list finish-result))))
    (cons results final-state)))

;;; Tests

(ert-deftest touchstone-pytest-test-parse-simple-passing-tests ()
  "Test parsing simple pytest output with only passing tests."
  (let* ((results-and-state (touchstone-pytest-test-load-and-process-fixture "pytest-simple.txt"))
         (results (car results-and-state)))

    ;; Should have exactly 4 results
    (should (= (length results) 4))

    ;; Extract the results
    (let ((test-one (nth 0 results))
          (test-two (nth 1 results))
          (test-three (nth 2 results))
          (test-four (nth 3 results)))

      ;; Verify test_one
      (should (string= (plist-get test-one :id) "tests/test_example.py::test_one"))
      (should (string= (plist-get test-one :file) "tests/test_example.py"))
      (should (string= (plist-get test-one :test) "test_one"))
      (should (eq (plist-get test-one :status) 'passed))

      ;; Verify test_two
      (should (string= (plist-get test-two :id) "tests/test_example.py::test_two"))
      (should (string= (plist-get test-two :file) "tests/test_example.py"))
      (should (string= (plist-get test-two :test) "test_two"))
      (should (eq (plist-get test-two :status) 'passed))

      ;; Verify test_three
      (should (string= (plist-get test-three :id) "tests/test_example.py::test_three"))
      (should (string= (plist-get test-three :file) "tests/test_example.py"))
      (should (string= (plist-get test-three :test) "test_three"))
      (should (eq (plist-get test-three :status) 'passed))

      ;; Verify test_four
      (should (string= (plist-get test-four :id) "tests/test_example.py::test_four"))
      (should (string= (plist-get test-four :file) "tests/test_example.py"))
      (should (string= (plist-get test-four :test) "test_four"))
      (should (eq (plist-get test-four :status) 'passed)))))

(ert-deftest touchstone-pytest-test-parse-test-line-passed ()
  "Test parsing a single PASSED test line."
  (let* ((backend (touchstone-pytest-test-get-backend))
         (create-state-fn (plist-get backend :create-parser-state))
         (parse-fn (plist-get backend :parse-line))
         (state (funcall create-state-fn))
         (line "tests/test_example.py::test_one PASSED")
         (result-and-state (funcall parse-fn line state))
         (result (car result-and-state)))

    ;; Should have a result
    (should result)

    ;; Verify all fields
    (should (string= (plist-get result :id) "tests/test_example.py::test_one"))
    (should (string= (plist-get result :file) "tests/test_example.py"))
    (should (string= (plist-get result :test) "test_one"))
    (should (eq (plist-get result :status) 'passed))))

(ert-deftest touchstone-pytest-test-parse-test-line-failed ()
  "Test parsing a single FAILED test line."
  (let* ((backend (touchstone-pytest-test-get-backend))
         (create-state-fn (plist-get backend :create-parser-state))
         (parse-fn (plist-get backend :parse-line))
         (state (funcall create-state-fn))
         (line "tests/test_example.py::test_two FAILED")
         (result-and-state (funcall parse-fn line state))
         (result (car result-and-state)))

    ;; Should have a result
    (should result)

    ;; Verify all fields
    (should (string= (plist-get result :id) "tests/test_example.py::test_two"))
    (should (string= (plist-get result :file) "tests/test_example.py"))
    (should (string= (plist-get result :test) "test_two"))
    (should (eq (plist-get result :status) 'failed))))

(provide 'touchstone-pytest-test)

;;; touchstone-pytest-test.el ends here
