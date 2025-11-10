;;; test-helper.el --- Test helpers for touchstone.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Helper functions for testing touchstone.el

;;; Code:

;; Add parent directory to load path
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Load touchstone and pytest backend
(require 'touchstone)
(require 'touchstone-pytest)

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
  ;; Select pytest backend for testing
  (let ((backend (cdr (assq 'pytest touchstone--backends))))
    (setq touchstone--current-backend backend)
    ;; Initialize parser state
    (let ((create-state-fn (plist-get backend :create-parser-state)))
      (setq touchstone--parser-state (funcall create-state-fn)))
    ;; Process lines
    (let ((lines (split-string output "\n" t)))
      (dolist (line lines)
        (touchstone--process-line line)))
    ;; Finalize parsing (simulates process exit)
    (let ((finish-fn (plist-get backend :finish)))
      (funcall finish-fn touchstone--parser-state))))

(provide 'test-helper)

;;; test-helper.el ends here
