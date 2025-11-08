;;; test-helper.el --- Test helpers for touchstone.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper functions for testing touchstone.el

;;; Code:

;; Add parent directory to load path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Load touchstone
(require 'touchstone)

(defvar touchstone-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing touchstone tests.")

(defvar touchstone-test-fixtures-dir
  (expand-file-name "fixtures" touchstone-test-dir)
  "Directory containing test fixtures.")

(defun touchstone-test-load-fixture (filename)
  "Load the contents of fixture FILENAME as a string."
  (let ((filepath (expand-file-name filename touchstone-test-fixtures-dir)))
    (with-temp-buffer
      (insert-file-contents filepath)
      (buffer-string))))

(defun touchstone-test-simulate-output (output)
  "Simulate pytest OUTPUT by processing it line-by-line through the parser.
This mimics what the process filter would do."
  (let ((lines (split-string output "\n" t)))
    (dolist (line lines)
      (touchstone--process-line line))
    ;; Simulate process completion
    (touchstone--finish-current-failure)))

(provide 'test-helper)

;;; test-helper.el ends here
