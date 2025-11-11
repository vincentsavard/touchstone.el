;;; touchstone-pytest.el --- Pytest backend for touchstone -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Vincent Savard
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools, processes, tests
;; URL: https://github.com/vincentsavard/touchstone.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Pytest backend for touchstone.el
;;
;; This backend provides support for running Python tests using pytest.
;; It handles project detection, command construction, and output parsing
;; for pytest test runs.

;;; Code:

(require 'touchstone)

;;; Customization

(defgroup touchstone-pytest nil
  "Pytest backend for touchstone."
  :group 'touchstone
  :prefix "touchstone-pytest-")

(defcustom touchstone-pytest-executable '("uv" "run" "pytest")
  "Command to run pytest.
This should be a list of strings representing the command and any prefix.

Examples:
  - For uv (default): '(\"uv\" \"run\" \"pytest\")
  - For poetry: '(\"poetry\" \"run\" \"pytest\")
  - For pipenv: '(\"pipenv\" \"run\" \"pytest\")
  - Direct pytest: '(\"pytest\")

The command will be combined with `touchstone-pytest-args` when executing tests."
  :type '(repeat string)
  :group 'touchstone-pytest)

(defcustom touchstone-pytest-args '("-v" "--color=no" "-p" "no:progress")
  "Default arguments to pass to pytest."
  :type '(repeat string)
  :group 'touchstone-pytest)

;;; Detection

(defun touchstone-pytest--find-dominating-file-with-content (file regexp)
  "Search up directory tree for FILE containing REGEXP.
Returns the directory containing the file, or nil if not found."
  (let ((dir (locate-dominating-file default-directory file)))
    (when dir
      (let ((file-path (expand-file-name file dir)))
        (when (file-exists-p file-path)
          (with-temp-buffer
            (insert-file-contents file-path)
            (when (re-search-forward regexp nil t)
              dir)))))))

(defun touchstone-pytest--detect-project (root)
  "Detect if ROOT is a pytest project.
Returns non-nil if pytest configuration or test files are found."
  (let ((default-directory root))
    (or
     ;; Check for pytest.ini
     (file-exists-p (expand-file-name "pytest.ini" root))
     ;; Check for pyproject.toml with pytest config
     (touchstone-pytest--find-dominating-file-with-content
      "pyproject.toml" "\\[tool\\.pytest")
     ;; Check for setup.cfg with pytest config
     (touchstone-pytest--find-dominating-file-with-content
      "setup.cfg" "\\[tool:pytest\\]")
     ;; Check for tests directory
     (file-directory-p (expand-file-name "tests" root)))))

;;; Command Construction

(defun touchstone-pytest--build-command (root)
  "Build pytest command for project at ROOT.
Returns a list of strings representing the full command."
  (append touchstone-pytest-executable touchstone-pytest-args))

;;; Output Parsing

(defun touchstone-pytest--create-parser-state ()
  "Create initial parser state for pytest output parsing.
Returns a plist containing parser state variables."
  (list :parsing-failures nil
        :current-failure nil
        :current-failure-details nil))

(defun touchstone-pytest--test-identifier (file test)
  "Create a test identifier from FILE and TEST name."
  (concat file "::" test))

(defun touchstone-pytest--parse-test-line (line)
  "Parse a pytest test result LINE.
Returns a plist with :id, :file, :test, and :status, or nil if not a test result line."
  (when (string-match
         "^\\(.*?\\)::\\(.*?\\) \\(PASSED\\|FAILED\\|SKIPPED\\|ERROR\\)"
         line)
    (let* ((file (match-string 1 line))
           (test (match-string 2 line))
           (status-string (match-string 3 line))
           (status (cond
                    ((string= status-string "PASSED") 'passed)
                    ((string= status-string "FAILED") 'failed)
                    ((string= status-string "ERROR") 'error)
                    ((string= status-string "SKIPPED") 'skipped))))
      (list :id (touchstone-pytest--test-identifier file test)
            :file file
            :test test
            :status status))))

(defun touchstone-pytest--parse-failure-line (line state)
  "Parse a LINE from the FAILURES section using parser STATE.
Returns updated state."
  (let ((parsing-failures (plist-get state :parsing-failures))
        (current-failure (plist-get state :current-failure))
        (current-failure-details (plist-get state :current-failure-details)))
    (cond
     ;; Start of a new failure block: _____ test_name _____
     ((string-match "^_+ \\(.*\\) _+$" line)
      (let ((test-name (match-string 1 line)))
        ;; Finish previous failure if any
        (when current-failure
          (setq state (touchstone-pytest--finish-current-failure state)))
        ;; Start new failure
        (plist-put state :current-failure test-name)
        (plist-put state :current-failure-details
                   (list :traceback nil :error-lines nil :location nil :stdout nil :stderr nil))))

     ;; Captured stdout section
     ((string-match "^-+ Captured stdout call -+$" line)
      (setq current-failure-details (plist-get state :current-failure-details))
      (plist-put current-failure-details :capturing-stdout t)
      (plist-put state :current-failure-details current-failure-details))

     ;; Captured stderr section
     ((string-match "^-+ Captured stderr call -+$" line)
      (setq current-failure-details (plist-get state :current-failure-details))
      (plist-put current-failure-details :capturing-stdout nil)
      (plist-put current-failure-details :capturing-stderr t)
      (plist-put state :current-failure-details current-failure-details))

     ;; File location line: tests/file.py:9: AssertionError
     ((string-match "^\\([^:]+\\):\\([0-9]+\\): \\(.*\\)$" line)
      (setq current-failure-details (plist-get state :current-failure-details))
      (plist-put current-failure-details :location line)
      (plist-put state :current-failure-details current-failure-details))

     ;; Error detail line (starts with "E   ")
     ((string-match "^E   " line)
      (setq current-failure-details (plist-get state :current-failure-details))
      (let ((error-lines (plist-get current-failure-details :error-lines)))
        (plist-put current-failure-details :error-lines
                   (append error-lines (list line)))
        (plist-put state :current-failure-details current-failure-details)))

     ;; Captured output or traceback
     ((plist-get state :current-failure)
      (setq current-failure-details (plist-get state :current-failure-details))
      (cond
       ((plist-get current-failure-details :capturing-stdout)
        (let ((stdout (plist-get current-failure-details :stdout)))
          (plist-put current-failure-details :stdout
                     (if stdout (concat stdout "\n" line) line))
          (plist-put state :current-failure-details current-failure-details)))

       ((plist-get current-failure-details :capturing-stderr)
        (let ((stderr (plist-get current-failure-details :stderr)))
          (plist-put current-failure-details :stderr
                     (if stderr (concat stderr "\n" line) line))
          (plist-put state :current-failure-details current-failure-details)))

       ;; Regular traceback line (indented)
       ((string-match "^    " line)
        (let ((traceback (plist-get current-failure-details :traceback)))
          (plist-put current-failure-details :traceback
                     (append traceback (list line)))
          (plist-put state :current-failure-details current-failure-details))))))
    state))

(defun touchstone-pytest--finish-current-failure (state)
  "Finish parsing the current failure and store its details.
Returns updated STATE with current failure cleared."
  (when-let* ((current-failure (plist-get state :current-failure))
              (current-failure-details (plist-get state :current-failure-details)))
    ;; Find the test result by matching the test name
    (maphash
     (lambda (key value)
       (when (string-suffix-p current-failure key)
         (puthash key (plist-put value :details current-failure-details)
                  touchstone--test-results)
         (touchstone--update-test-display key)))
     touchstone--test-results))
  ;; Clear current failure state
  (plist-put state :current-failure nil)
  (plist-put state :current-failure-details nil)
  state)

(defun touchstone-pytest--finish (state)
  "Finalize pytest parsing on process exit.
Ensures any pending failure details are stored.
STATE is the current parser state."
  (touchstone-pytest--finish-current-failure state))

(defun touchstone-pytest--parse-line (line state)
  "Parse a single LINE of pytest output using parser STATE.
Returns (RESULT . NEW-STATE) where RESULT is a test result plist or nil."
  (let ((result nil))
    (cond
     ;; Check for FAILURES section start
     ((string-match "^=+ FAILURES =+$" line)
      (plist-put state :parsing-failures t))

     ;; Check for end of FAILURES section
     ((string-match "^=+ \\(short test summary\\|[0-9].* in [0-9]\\)" line)
      (setq state (touchstone-pytest--finish-current-failure state))
      (plist-put state :parsing-failures nil))

     ;; Inside FAILURES section
     ((plist-get state :parsing-failures)
      (setq state (touchstone-pytest--parse-failure-line line state)))

     ;; Test result line (outside FAILURES section)
     (t
      (setq result (touchstone-pytest--parse-test-line line))))

    (cons result state)))

;;; Backend Registration

(touchstone-register-backend
 'pytest
 (list :name "pytest"
       :detect #'touchstone-pytest--detect-project
       :build-command #'touchstone-pytest--build-command
       :create-parser-state #'touchstone-pytest--create-parser-state
       :parse-line #'touchstone-pytest--parse-line
       :finish #'touchstone-pytest--finish))

(provide 'touchstone-pytest)

;;; touchstone-pytest.el ends here
