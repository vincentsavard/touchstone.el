;;; touchstone-core-test.el --- Tests for touchstone.el core -*- lexical-binding: t; -*-

;;; Commentary:
;; Isolated unit tests for the core using a fake backend

;;; Code:

(require 'ert)
(require 'test-helper)

;;; Fake Backend Implementation

(defun touchstone-core-test-create-fake-backend (results)
  "Create a simple fake backend for testing core.
RESULTS is a list of test result plists to return."
  (list
   :name "fake"
   :detect (lambda (root) t)
   :build-command (lambda (root)
                    (let ((n (length results)))
                      (list "printf" (concat (make-string n ?\n)))))
   :create-parser-state (lambda () (list :results results))
   :parse-line (lambda (line state)
                 (let ((results (plist-get state :results)))
                   (if results
                       (let ((result (car results))
                             (remaining (cdr results)))
                         (cons result (plist-put state :results remaining)))
                     (cons nil state))))
   :finish (lambda (state) (cons nil state))))

;;; Helper Functions

(defun touchstone-core-test-register-fake-backend (results)
  "Register a fake backend that returns RESULTS."
  (let ((backend (touchstone-core-test-create-fake-backend results)))
    (touchstone-register-backend 'fake backend)
    backend))

(defun touchstone-core-test-wait-for-process ()
  "Wait for the touchstone process to complete."
  (when-let ((proc (get-buffer-process (get-buffer " *touchstone-process*"))))
    (while (eq (process-status proc) 'run)
      (accept-process-output proc 0.1))))

(defun touchstone-core-test-get-buffer-text ()
  "Get buffer text without properties."
  (with-current-buffer (get-buffer touchstone-buffer-name)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun touchstone-core-test-buffer-contains (text)
  "Return t if buffer contains TEXT."
  (string-match-p (regexp-quote text) (touchstone-core-test-get-buffer-text)))

;;; Tests

(ert-deftest touchstone-core-test-display-single-passed-test ()
  "Test that a single passing test is displayed correctly."
  (touchstone-core-test-register-fake-backend
   (list (list :id "file.txt::test_one"
               :file "file.txt"
               :test "test_one"
               :status 'passed)))

  (touchstone-run-tests)
  (touchstone-core-test-wait-for-process)

  (should (touchstone-core-test-buffer-contains "PASS  file.txt::test_one")))

(ert-deftest touchstone-core-test-display-single-failed-test ()
  "Test that a single failing test is displayed correctly."
  (touchstone-core-test-register-fake-backend
   (list (list :id "file.txt::test_two"
               :file "file.txt"
               :test "test_two"
               :status 'failed)))

  (touchstone-run-tests)
  (touchstone-core-test-wait-for-process)

  (should (touchstone-core-test-buffer-contains "FAIL  file.txt::test_two")))

(ert-deftest touchstone-core-test-display-multiple-tests ()
  "Test that multiple tests are displayed."
  (touchstone-core-test-register-fake-backend
   (list (list :id "file.txt::test_one"
               :file "file.txt"
               :test "test_one"
               :status 'passed)
         (list :id "file.txt::test_two"
               :file "file.txt"
               :test "test_two"
               :status 'failed)
         (list :id "file.txt::test_three"
               :file "file.txt"
               :test "test_three"
               :status 'passed)))

  (touchstone-run-tests)
  (touchstone-core-test-wait-for-process)

  (should (touchstone-core-test-buffer-contains "PASS  file.txt::test_one"))
  (should (touchstone-core-test-buffer-contains "FAIL  file.txt::test_two"))
  (should (touchstone-core-test-buffer-contains "PASS  file.txt::test_three")))

(provide 'touchstone-core-test)

;;; touchstone-core-test.el ends here
