;;; touchstone.el --- Async test runner with unified interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Vincent Savard
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools, processes, tests
;; URL: https://github.com/vincentsavard/touchstone.el

;; This file is not part of GNU Emacs.

;;; Commentary:

;; touchstone.el provides an asynchronous test runner that automatically
;; detects and executes the appropriate test backend for your project.
;; It displays results in a clean, unified interface with real-time updates.
;;
;; Features:
;; - Async test execution (non-blocking)
;; - Automatic test runner detection
;; - Real-time incremental result updates
;; - Collapsible test output for detailed inspection
;; - Clickable file paths for easy navigation
;;
;; Currently supported test runners:
;; - pytest (Python)
;;
;; Usage:
;;   M-x touchstone-run-tests
;;
;; Configuration:
;;   By default, pytest is run via uv: '("uv" "run" "pytest")
;;   Customize `touchstone-pytest-executable` for other setups:
;;
;;   ;; For poetry:
;;   (setq touchstone-pytest-executable '("poetry" "run" "pytest"))
;;
;;   ;; For pipenv:
;;   (setq touchstone-pytest-executable '("pipenv" "run" "pytest"))
;;
;;   ;; For direct pytest (no virtual env manager):
;;   (setq touchstone-pytest-executable '("pytest"))

;;; Code:

(require 'ansi-color)

;;; Customization

(defgroup touchstone nil
  "Async test runner with unified interface."
  :group 'tools
  :prefix "touchstone-")

(defcustom touchstone-buffer-name "*touchstone*"
  "Name of the touchstone results buffer."
  :type 'string
  :group 'touchstone)

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
  :group 'touchstone)

(defcustom touchstone-pytest-args '("-v" "--color=no")
  "Default arguments to pass to pytest."
  :type '(repeat string)
  :group 'touchstone)

;;; Variables

(defvar touchstone--process nil
  "The current touchstone test process.")

(defvar touchstone--buffer nil
  "The touchstone results buffer.")

(defvar touchstone--output-buffer ""
  "Accumulator for partial output from the test process.")

(defvar touchstone--line-buffer ""
  "Accumulator for partial lines from the test process.")

(defvar touchstone--project-root nil
  "Cached project root directory.")

;;; Pytest Output Parsing

(defun touchstone--parse-pytest-line (line)
  "Parse a pytest output LINE and return test result data.
Returns a plist with :file, :test, and :status, or nil if not a test result line."
  (when (string-match
         "^\\(.*?\\)::\\(.*?\\) \\(PASSED\\|FAILED\\|SKIPPED\\|ERROR\\)"
         line)
    (list :file (match-string 1 line)
          :test (match-string 2 line)
          :status (match-string 3 line))))

(defun touchstone--format-test-result (result)
  "Format a parsed test RESULT as a display line."
  (let* ((file (plist-get result :file))
         (test (plist-get result :test))
         (status (plist-get result :status))
         (status-face (cond
                       ((string= status "PASSED") 'success)
                       ((string= status "FAILED") 'error)
                       ((string= status "SKIPPED") 'warning)
                       ((string= status "ERROR") 'error))))
    (concat
     (propertize file 'face 'default)
     "::"
     (propertize test 'face 'default)
     " "
     (propertize (format "[%s]" status) 'face status-face)
     "\n")))

;;; Project Detection

(defun touchstone--find-dominating-file-with-content (file regexp)
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

(defun touchstone--find-project-root ()
  "Find the project root directory.
Uses the following methods in order:
1. project.el (if available)
2. projectile (if available)
3. Git root
4. Directory containing pytest.ini, pyproject.toml, or setup.cfg
5. Current directory"
  (or touchstone--project-root
      (setq touchstone--project-root
            (or
             ;; Try project.el (Emacs 27+)
             (when (fboundp 'project-current)
               (when-let* ((project (project-current)))
                 (if (fboundp 'project-root)
                     (project-root project)
                   (car (project-roots project)))))
             ;; Try projectile
             (when (fboundp 'projectile-project-root)
               (ignore-errors (projectile-project-root)))
             ;; Try git root
             (locate-dominating-file default-directory ".git")
             ;; Try pytest config files
             (locate-dominating-file default-directory "pytest.ini")
             (touchstone--find-dominating-file-with-content
              "pyproject.toml" "\\[tool\\.pytest")
             (touchstone--find-dominating-file-with-content
              "setup.cfg" "\\[tool:pytest\\]")
             ;; Try tests directory
             (locate-dominating-file default-directory "tests")
             ;; Fall back to current directory
             default-directory))))

(defun touchstone--reset-project-root ()
  "Clear the cached project root."
  (setq touchstone--project-root nil))

;;; Async Process Management

(defun touchstone--make-process (command-list)
  "Create and return an async process running COMMAND-LIST.
COMMAND-LIST should be a list of strings (command and arguments).
The process streams output to the touchstone results buffer."
  (let* ((process-name "touchstone")
         (process-buffer (get-buffer-create " *touchstone-process*"))
         (default-directory (touchstone--find-project-root)))
    (with-current-buffer process-buffer
      (erase-buffer))
    (make-process
     :name process-name
     :buffer process-buffer
     :command command-list
     :filter #'touchstone--process-filter
     :sentinel #'touchstone--process-sentinel
     :noquery t)))

(defun touchstone--process-filter (proc string)
  "Process filter for touchstone test process PROC.
Accumulates STRING output and updates the results buffer incrementally."
  (when (buffer-live-p (process-buffer proc))
    ;; Accumulate all output for debugging
    (setq touchstone--output-buffer
          (concat touchstone--output-buffer string))
    ;; Accumulate into line buffer
    (setq touchstone--line-buffer
          (concat touchstone--line-buffer string))
    ;; Process complete lines
    (let ((lines (split-string touchstone--line-buffer "\n")))
      ;; If the buffer ends with a newline, we have all complete lines
      ;; Otherwise, keep the last partial line in the buffer
      (if (string-suffix-p "\n" touchstone--line-buffer)
          (progn
            (dolist (line lines)
              (touchstone--process-line line))
            (setq touchstone--line-buffer ""))
        ;; Keep the last partial line
        (let ((complete-lines (butlast lines))
              (partial-line (car (last lines))))
          (dolist (line complete-lines)
            (touchstone--process-line line))
          (setq touchstone--line-buffer partial-line))))))

(defun touchstone--process-line (line)
  "Process a complete LINE from pytest output.
If the line is a test result, display it in the results buffer."
  (message "DEBUG: Processing line: %S" line)
  (let ((result (touchstone--parse-pytest-line line)))
    (message "DEBUG: Parse result: %S" result)
    (when result
      (touchstone--display-test-result result))))

(defun touchstone--process-sentinel (proc event)
  "Process sentinel for touchstone test process PROC.
Handles process completion and errors based on EVENT."
  (let ((status (process-status proc))
        (exit-code (process-exit-status proc)))
    ;; Process any remaining partial line
    (when (and (not (string-empty-p touchstone--line-buffer))
               (eq status 'exit))
      (touchstone--process-line touchstone--line-buffer)
      (setq touchstone--line-buffer ""))
    (cond
     ((eq status 'exit)
      (touchstone--handle-process-complete exit-code))
     ((memq status '(signal failed))
      (touchstone--handle-process-error event)))))

(defun touchstone--handle-process-complete (exit-code)
  "Handle test process completion with EXIT-CODE."
  (with-current-buffer (touchstone--get-results-buffer)
    (let ((inhibit-read-only t)
          (message (cond
                    ((zerop exit-code) "All tests passed")
                    ((= exit-code 1) "Some tests failed")
                    (t (format "Test run error (exit code: %d)" exit-code))))
          (face (if (zerop exit-code) 'success 'error)))
      (goto-char (point-max))
      (insert "\n")
      (insert (propertize (concat message "\n") 'face face)))))

(defun touchstone--handle-process-error (event)
  "Handle test process error with EVENT description."
  (with-current-buffer (touchstone--get-results-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n")
      (insert (propertize
               (format "Test process error: %s\n" (string-trim event))
               'face 'error)))))

(defun touchstone--kill-process ()
  "Kill the current touchstone test process if running."
  (when (and touchstone--process
             (process-live-p touchstone--process))
    (kill-process touchstone--process)
    (setq touchstone--process nil)))

;;; Results Buffer Management

(defun touchstone--get-results-buffer ()
  "Get or create the touchstone results buffer."
  (or (and (buffer-live-p touchstone--buffer)
           touchstone--buffer)
      (setq touchstone--buffer
            (with-current-buffer (get-buffer-create touchstone-buffer-name)
              (touchstone-mode)
              (current-buffer)))))

(defun touchstone--init-results-buffer ()
  "Initialize the results buffer for a new test run."
  (with-current-buffer (touchstone--get-results-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Running tests...\n\n"
                          'face 'bold))
      (setq touchstone--output-buffer "")
      (setq touchstone--line-buffer ""))))

(defun touchstone--display-test-result (result)
  "Display a formatted test RESULT in the results buffer."
  (with-current-buffer (touchstone--get-results-buffer)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert (touchstone--format-test-result result))))))

(defun touchstone--display-results-buffer ()
  "Display the touchstone results buffer."
  (let ((buffer (touchstone--get-results-buffer)))
    (display-buffer buffer
                    '((display-buffer-reuse-window
                       display-buffer-pop-up-window)
                      (reusable-frames . t)))
    buffer))

;;; Major Mode

(defvar touchstone-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'touchstone-run-tests)
    (define-key map (kbd "k") #'touchstone-kill-process)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap for `touchstone-mode'.")

(define-derived-mode touchstone-mode special-mode "Touchstone"
  "Major mode for touchstone test results.

\\{touchstone-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil))

;;; Public Commands

;;;###autoload
(defun touchstone-run-tests ()
  "Run tests for the current project."
  (interactive)
  (touchstone--kill-process)
  (touchstone--reset-project-root)
  (touchstone--init-results-buffer)
  (touchstone--display-results-buffer)
  (setq touchstone--process
        (touchstone--make-process
         (append touchstone-pytest-executable touchstone-pytest-args)))
  (message "Running tests..."))

;;;###autoload
(defun touchstone-kill-process ()
  "Kill the currently running test process."
  (interactive)
  (if (and touchstone--process
           (process-live-p touchstone--process))
      (progn
        (touchstone--kill-process)
        (message "Test process killed"))
    (message "No test process running")))

(provide 'touchstone)

;;; touchstone.el ends here
