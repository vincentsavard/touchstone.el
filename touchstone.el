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

(defvar touchstone--test-results nil
  "Hash table mapping test identifiers to result data.
Keys are strings like \"file::test_name\".
Values are plists with :file, :test, :status, :marker, :details.")

(defvar touchstone--parsing-failures nil
  "Non-nil when we're inside the FAILURES section.")

(defvar touchstone--current-failure nil
  "Current failure being parsed (test identifier).")

(defvar touchstone--current-failure-details nil
  "Accumulator for current failure details.")

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

(defun touchstone--test-identifier (file test)
  "Create a test identifier from FILE and TEST name."
  (concat file "::" test))

(defun touchstone--parse-failure-line (line)
  "Parse a LINE from the FAILURES section."
  (cond
   ;; Start of a new failure block: _____ test_name _____
   ((string-match "^_+ \\(.*\\) _+$" line)
    (touchstone--finish-current-failure)
    (setq touchstone--current-failure (match-string 1 line))
    (setq touchstone--current-failure-details
          (list :traceback nil :error-lines nil :location nil :stdout nil :stderr nil)))

   ;; Captured stdout section
   ((string-match "^-+ Captured stdout call -+$" line)
    (plist-put touchstone--current-failure-details :capturing-stdout t))

   ;; Captured stderr section
   ((string-match "^-+ Captured stderr call -+$" line)
    (plist-put touchstone--current-failure-details :capturing-stdout nil)
    (plist-put touchstone--current-failure-details :capturing-stderr t))

   ;; File location line: tests/file.py:9: AssertionError
   ((string-match "^\\([^:]+\\):\\([0-9]+\\): \\(.*\\)$" line)
    (plist-put touchstone--current-failure-details :location line))

   ;; Error detail line (starts with "E   ")
   ((string-match "^E   " line)
    (let ((error-lines (plist-get touchstone--current-failure-details :error-lines)))
      (plist-put touchstone--current-failure-details :error-lines
                 (append error-lines (list line)))))

   ;; Captured output or traceback
   (touchstone--current-failure
    (cond
     ((plist-get touchstone--current-failure-details :capturing-stdout)
      (let ((stdout (plist-get touchstone--current-failure-details :stdout)))
        (plist-put touchstone--current-failure-details :stdout
                   (if stdout (concat stdout "\n" line) line))))

     ((plist-get touchstone--current-failure-details :capturing-stderr)
      (let ((stderr (plist-get touchstone--current-failure-details :stderr)))
        (plist-put touchstone--current-failure-details :stderr
                   (if stderr (concat stderr "\n" line) line))))

     ;; Regular traceback line (indented)
     ((string-match "^    " line)
      (let ((traceback (plist-get touchstone--current-failure-details :traceback)))
        (plist-put touchstone--current-failure-details :traceback
                   (append traceback (list line)))))))))

(defun touchstone--finish-current-failure ()
  "Finish parsing the current failure and store its details."
  (when touchstone--current-failure
    ;; Find the test result by matching the test name
    ;; The failure header might be just the test name, we need to find the full identifier
    (maphash
     (lambda (key value)
       (when (string-suffix-p touchstone--current-failure key)
         (plist-put value :details touchstone--current-failure-details)
         (touchstone--update-test-display key)))
     touchstone--test-results)
    (setq touchstone--current-failure nil)
    (setq touchstone--current-failure-details nil)))

(defun touchstone--format-test-result (result)
  "Format a parsed test RESULT as a display line."
  (let* ((file (plist-get result :file))
         (test (plist-get result :test))
         (status (plist-get result :status))
         (identifier (touchstone--test-identifier file test))
         (has-details (and (member status '("FAILED" "ERROR"))
                          (plist-get result :details)))
         (status-face (cond
                       ((string= status "PASSED") 'success)
                       ((string= status "FAILED") 'error)
                       ((string= status "SKIPPED") 'warning)
                       ((string= status "ERROR") 'error))))
    (propertize
     (concat
      (if has-details
          (propertize "[+] " 'face 'shadow)
        "    ")
      (propertize file 'face 'default)
      "::"
      (propertize test 'face 'default)
      " "
      (propertize (format "[%s]" status) 'face status-face)
      "\n")
     'touchstone-test-id identifier)))

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
Handles test results, failure details, and section markers."
  (cond
   ;; Check for FAILURES section start
   ((string-match "^=+ FAILURES =+$" line)
    (setq touchstone--parsing-failures t))

   ;; Check for end of FAILURES section (short test summary or final summary)
   ((string-match "^=+ \\(short test summary\\|[0-9]+ failed\\)" line)
    (touchstone--finish-current-failure)
    (setq touchstone--parsing-failures nil))

   ;; Inside FAILURES section
   (touchstone--parsing-failures
    (touchstone--parse-failure-line line))

   ;; Test result line (outside FAILURES section)
   (t
    (when-let* ((result (touchstone--parse-pytest-line line)))
      (touchstone--display-test-result result)))))

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
    ;; Finish any pending failure parsing
    (when (eq status 'exit)
      (touchstone--finish-current-failure))
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
      (setq touchstone--line-buffer "")
      (setq touchstone--test-results (make-hash-table :test 'equal))
      (setq touchstone--parsing-failures nil)
      (setq touchstone--current-failure nil)
      (setq touchstone--current-failure-details nil))))

(defun touchstone--display-test-result (result)
  "Display a formatted test RESULT in the results buffer."
  (let* ((file (plist-get result :file))
         (test (plist-get result :test))
         (identifier (touchstone--test-identifier file test)))
    (with-current-buffer (touchstone--get-results-buffer)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (let ((start (point)))
            (insert (touchstone--format-test-result result))
            ;; Store the result with a marker that advances when text is inserted before it
            (puthash identifier
                     (plist-put result :marker (copy-marker start t))
                     touchstone--test-results)))))))

(defun touchstone--update-test-display (identifier)
  "Update the display for test IDENTIFIER with its details."
  (when-let* ((result (gethash identifier touchstone--test-results))
              (marker (plist-get result :marker)))
    (with-current-buffer (touchstone--get-results-buffer)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char marker)
          (delete-region (point) (line-end-position))
          (insert (substring (touchstone--format-test-result result) 0 -1))
          ;; Add collapsible details if present
          (when (plist-get result :details)
            (forward-line 1)
            (touchstone--create-details-overlay result (point))))))))

;;; Collapsible Details

(defun touchstone--format-details (details)
  "Format test DETAILS for display."
  (let ((parts nil))
    ;; Add traceback
    (when-let* ((traceback (plist-get details :traceback)))
      (push (propertize "        Traceback:\n" 'face 'bold) parts)
      (dolist (line traceback)
        (push (propertize (concat "        " line "\n") 'face 'default) parts)))

    ;; Add error lines
    (when-let* ((error-lines (plist-get details :error-lines)))
      (dolist (line error-lines)
        (push (propertize (concat "        " line "\n") 'face 'error) parts)))

    ;; Add location
    (when-let* ((location (plist-get details :location)))
      (push (propertize (concat "        " location "\n") 'face 'warning) parts))

    ;; Add captured stdout
    (when-let* ((stdout (plist-get details :stdout)))
      (push "\n" parts)
      (push (propertize "        Captured stdout:\n" 'face 'bold) parts)
      (dolist (line (split-string stdout "\n"))
        (push (propertize (concat "        " line "\n") 'face 'default) parts)))

    ;; Add captured stderr
    (when-let* ((stderr (plist-get details :stderr)))
      (push "\n" parts)
      (push (propertize "        Captured stderr:\n" 'face 'bold) parts)
      (dolist (line (split-string stderr "\n"))
        (push (propertize (concat "        " line "\n") 'face 'error) parts)))

    ;; Add newline at start and end
    (concat "\n" (apply #'concat (nreverse parts)) "\n")))

(defun touchstone--create-details-overlay (result position)
  "Create a collapsible overlay for test RESULT at POSITION."
  (let* ((details (plist-get result :details))
         (details-text (touchstone--format-details details))
         (start position)
         (end position))
    ;; Insert the details text (invisible by default)
    (save-excursion
      (goto-char position)
      (insert details-text)
      (setq end (point)))
    ;; Create overlay to make it collapsible
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'invisible t)
      (overlay-put ov 'touchstone-details t)
      (overlay-put ov 'evaporate t)
      ;; Store overlay reference in the result
      (plist-put result :overlay ov))))

(defun touchstone--toggle-details-at-point ()
  "Toggle visibility of test details at point."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         ;; Get the test identifier from the text property
         (test-id (get-text-property line-start 'touchstone-test-id)))
    (if (and test-id
             (gethash test-id touchstone--test-results))
        (let* ((result (gethash test-id touchstone--test-results))
               (overlay (plist-get result :overlay)))
          (if overlay
              (let ((currently-invisible (overlay-get overlay 'invisible)))
                (overlay-put overlay 'invisible (not currently-invisible))
                ;; Update the indicator on the test line
                (save-excursion
                  (goto-char line-start)
                  (when (looking-at "\\[.\\] ")
                    (let ((inhibit-read-only t)
                          (new-indicator (if currently-invisible "[-] " "[+] ")))
                      (delete-char 4)  ; Delete all 4 chars: [+] or [-] plus space
                      (insert (propertize new-indicator 'touchstone-test-id test-id))))))
            (message "No details available for this test")))
      (message "Not on a test result line"))))

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
    (define-key map (kbd "TAB") #'touchstone--toggle-details-at-point)
    (define-key map (kbd "RET") #'touchstone--toggle-details-at-point)
    map)
  "Keymap for `touchstone-mode'.")

(define-derived-mode touchstone-mode special-mode "Touchstone"
  "Major mode for touchstone test results.

Key bindings:
\\<touchstone-mode-map>
\\[quit-window] - Quit window
\\[touchstone-run-tests] - Rerun tests
\\[touchstone-kill-process] - Kill running test process
\\[touchstone--toggle-details-at-point] - Toggle test details (TAB or RET)
\\[next-line] - Next line
\\[previous-line] - Previous line

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
