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

(defun touchstone-core-test-get-header-line ()
  "Get the header line text from the touchstone buffer."
  (with-current-buffer (get-buffer touchstone-buffer-name)
    ;; format-mode-line doesn't properly evaluate :eval forms in batch mode,
    ;; so we manually evaluate the form from header-line-format (:eval FORM)
    (eval (cadr header-line-format))))

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

(ert-deftest touchstone-core-test-display-test-with-details ()
  "Test that a test with details shows the + indicator."
  (touchstone-core-test-register-fake-backend
   (list (list :id "file.txt::test_two"
               :file "file.txt"
               :test "test_two"
               :status 'failed)
         (list :test "test_two"
               :details (list :message "assertion failed"))))

  (touchstone-run-tests)
  (touchstone-core-test-wait-for-process)

  (should (touchstone-core-test-buffer-contains "FAIL+ file.txt::test_two")))

(ert-deftest touchstone-core-test-detail-updates-existing-test ()
  "Test that detail result updates previously displayed test."
  (touchstone-core-test-register-fake-backend
   (list (list :id "file.txt::test_one"
               :file "file.txt"
               :test "test_one"
               :status 'passed)
         (list :id "file.txt::test_two"
               :file "file.txt"
               :test "test_two"
               :status 'failed)
         (list :test "test_two"
               :details (list :message "error message"))))

  (touchstone-run-tests)
  (touchstone-core-test-wait-for-process)

  (should (touchstone-core-test-buffer-contains "PASS  file.txt::test_one"))
  (should (touchstone-core-test-buffer-contains "FAIL+ file.txt::test_two")))

(ert-deftest touchstone-core-test-header-shows-backend-name ()
  "Test that header line shows backend name."
  (touchstone-core-test-register-fake-backend
   (list (list :id "file.txt::test_one"
               :file "file.txt"
               :test "test_one"
               :status 'passed)))

  (touchstone-run-tests)

  (let ((header (touchstone-core-test-get-header-line)))
    (should (string-match-p (regexp-quote "[fake]") header))))

(ert-deftest touchstone-core-test-header-shows-done-status ()
  "Test that header updates to done status."
  (touchstone-core-test-register-fake-backend
   (list (list :id "file.txt::test_one"
               :file "file.txt"
               :test "test_one"
               :status 'passed)))

  (touchstone-run-tests)
  (touchstone-core-test-wait-for-process)

  (let ((header (touchstone-core-test-get-header-line)))
    (should (string-match-p (regexp-quote "Done") header))))

(ert-deftest touchstone-core-test-multiple-details-for-same-test ()
  "Test that details with multiple error lines are displayed in buffer."
  (touchstone-core-test-register-fake-backend
   (list (list :id "file.txt::test_one"
               :file "file.txt"
               :test "test_one"
               :status 'failed)
         (list :test "test_one"
               :details (list :error-lines '("E   first error"
                                             "E   second error")))))

  (touchstone-run-tests)
  (touchstone-core-test-wait-for-process)

  (should (touchstone-core-test-buffer-contains "FAIL+ file.txt::test_one"))

  (with-current-buffer (get-buffer touchstone-buffer-name)
    (goto-char (point-min))
    (search-forward "FAIL+")
    (beginning-of-line)
    (touchstone--toggle-details-at-point)

    (let ((buffer-text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p (regexp-quote "first error") buffer-text))
      (should (string-match-p (regexp-quote "second error") buffer-text)))))

(ert-deftest touchstone-core-test-empty-input ()
  "Test that empty input produces empty buffer."
  (touchstone-core-test-register-fake-backend '())

  (touchstone-run-tests)
  (touchstone-core-test-wait-for-process)

  (should (string-empty-p (touchstone-core-test-get-buffer-text))))

(provide 'touchstone-core-test)

;;; touchstone-core-test.el ends here
