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

(ert-deftest touchstone-pytest-test-parse-test-line-skipped ()
  "Test parsing a single SKIPPED test line."
  (let* ((backend (touchstone-pytest-test-get-backend))
         (create-state-fn (plist-get backend :create-parser-state))
         (parse-fn (plist-get backend :parse-line))
         (state (funcall create-state-fn))
         (line "tests/test_example.py::test_skip SKIPPED")
         (result-and-state (funcall parse-fn line state))
         (result (car result-and-state)))

    ;; Should have a result
    (should result)

    ;; Verify all fields
    (should (string= (plist-get result :id) "tests/test_example.py::test_skip"))
    (should (string= (plist-get result :file) "tests/test_example.py"))
    (should (string= (plist-get result :test) "test_skip"))
    (should (eq (plist-get result :status) 'skipped))))

(ert-deftest touchstone-pytest-test-parse-test-line-error ()
  "Test parsing a single ERROR test line."
  (let* ((backend (touchstone-pytest-test-get-backend))
         (create-state-fn (plist-get backend :create-parser-state))
         (parse-fn (plist-get backend :parse-line))
         (state (funcall create-state-fn))
         (line "tests/test_example.py::test_error ERROR")
         (result-and-state (funcall parse-fn line state))
         (result (car result-and-state)))

    ;; Should have a result
    (should result)

    ;; Verify all fields
    (should (string= (plist-get result :id) "tests/test_example.py::test_error"))
    (should (string= (plist-get result :file) "tests/test_example.py"))
    (should (string= (plist-get result :test) "test_error"))
    (should (eq (plist-get result :status) 'error))))

(ert-deftest touchstone-pytest-test-parse-with-failures-test-statuses ()
  "Test parsing pytest output with failures - verify test statuses."
  (let* ((results-and-state (touchstone-pytest-test-load-and-process-fixture "pytest-with-failures.txt"))
         (results (car results-and-state))
         (test-results (seq-filter (lambda (r) (plist-get r :id)) results)))

    ;; Should have exactly 4 test results
    (should (= (length test-results) 4))

    ;; Extract test results by name
    (let ((test-one (seq-find (lambda (r) (string= (plist-get r :test) "test_one")) test-results))
          (test-two (seq-find (lambda (r) (string= (plist-get r :test) "test_two")) test-results))
          (test-three (seq-find (lambda (r) (string= (plist-get r :test) "test_three")) test-results))
          (test-four (seq-find (lambda (r) (string= (plist-get r :test) "test_four")) test-results)))

      ;; Verify all tests were found
      (should test-one)
      (should test-two)
      (should test-three)
      (should test-four)

      ;; Verify test statuses
      (should (eq (plist-get test-one :status) 'passed))
      (should (eq (plist-get test-two :status) 'failed))
      (should (eq (plist-get test-three :status) 'passed))
      (should (eq (plist-get test-four :status) 'failed)))))

(ert-deftest touchstone-pytest-test-parse-with-failures-details ()
  "Test parsing pytest output with failures - verify failure details."
  (let* ((results-and-state (touchstone-pytest-test-load-and-process-fixture "pytest-with-failures.txt"))
         (results (car results-and-state))
         (detail-results (seq-filter (lambda (r) (and (not (plist-get r :id))
                                                       (plist-get r :test))) results)))

    ;; Should have exactly 2 detail results
    (should (= (length detail-results) 2))

    ;; Extract detail results by test name
    (let ((test-two-details (seq-find (lambda (r) (string= (plist-get r :test) "test_two")) detail-results))
          (test-four-details (seq-find (lambda (r) (string= (plist-get r :test) "test_four")) detail-results)))

      ;; Verify detail results exist
      (should test-two-details)
      (should test-four-details)

      ;; Verify test_two details
      (let ((details (plist-get test-two-details :details)))
        (should details)
        (should (plist-get details :error-lines))
        (should (plist-get details :location))
        (should (string-match-p "AssertionError" (plist-get details :location))))

      ;; Verify test_four details
      (let ((details (plist-get test-four-details :details)))
        (should details)
        (should (plist-get details :error-lines))
        (should (plist-get details :location))
        (should (string-match-p "ValueError" (plist-get details :location)))))))

(ert-deftest touchstone-pytest-test-parse-with-output-test-statuses ()
  "Test parsing pytest output with captured output - verify test statuses."
  (let* ((results-and-state (touchstone-pytest-test-load-and-process-fixture "pytest-with-output.txt"))
         (results (car results-and-state))
         (test-results (seq-filter (lambda (r) (plist-get r :id)) results)))

    ;; Should have exactly 2 test results
    (should (= (length test-results) 2))

    ;; Extract test results by name
    (let ((test-stdout (seq-find (lambda (r) (string= (plist-get r :test) "test_with_stdout")) test-results))
          (test-stderr (seq-find (lambda (r) (string= (plist-get r :test) "test_with_stderr")) test-results)))

      ;; Verify both tests were found
      (should test-stdout)
      (should test-stderr)

      ;; Verify both tests have failed status
      (should (eq (plist-get test-stdout :status) 'failed))
      (should (eq (plist-get test-stderr :status) 'failed)))))

(ert-deftest touchstone-pytest-test-parse-with-output-captured-stdout ()
  "Test parsing pytest output with captured output - verify captured stdout."
  (let* ((results-and-state (touchstone-pytest-test-load-and-process-fixture "pytest-with-output.txt"))
         (results (car results-and-state))
         (detail-results (seq-filter (lambda (r) (and (not (plist-get r :id))
                                                       (plist-get r :test))) results)))

    ;; Find the stdout test details
    (let ((test-stdout-details (seq-find (lambda (r) (string= (plist-get r :test) "test_with_stdout")) detail-results)))

      ;; Verify detail result exists
      (should test-stdout-details)

      ;; Verify captured stdout
      (let ((details (plist-get test-stdout-details :details)))
        (should details)
        (should (plist-get details :stdout))
        (should (string-match-p "Debug output line 1" (plist-get details :stdout)))
        (should (string-match-p "Debug output line 2" (plist-get details :stdout)))))))

(ert-deftest touchstone-pytest-test-parse-with-output-captured-stderr ()
  "Test parsing pytest output with captured output - verify captured stderr."
  (let* ((results-and-state (touchstone-pytest-test-load-and-process-fixture "pytest-with-output.txt"))
         (results (car results-and-state))
         (detail-results (seq-filter (lambda (r) (and (not (plist-get r :id))
                                                       (plist-get r :test))) results)))

    ;; Find the stderr test details
    (let ((test-stderr-details (seq-find (lambda (r) (string= (plist-get r :test) "test_with_stderr")) detail-results)))

      ;; Verify detail result exists
      (should test-stderr-details)

      ;; Verify captured stderr
      (let ((details (plist-get test-stderr-details :details)))
        (should details)
        (should (plist-get details :stderr))
        (should (string-match-p "Error message 1" (plist-get details :stderr)))
        (should (string-match-p "Error message 2" (plist-get details :stderr)))))))

(provide 'touchstone-pytest-test)

;;; touchstone-pytest-test.el ends here
