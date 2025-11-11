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
;; - Pluggable backend architecture
;; - Real-time incremental result updates
;; - Collapsible test output for detailed inspection
;; - Clickable file paths for easy navigation
;;
;; Supported test runners:
;; - pytest (Python) - via touchstone-pytest.el
;;
;; Usage:
;;   M-x touchstone-run-tests
;;
;; The appropriate backend will be automatically detected and used.
;;
;; Configuration:
;;   Backend-specific configuration is available in the respective backend files.
;;   For pytest configuration, see touchstone-pytest.el

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

;;; Backend Protocol & Registry

(defvar touchstone--backends nil
  "List of registered test runner backends.
Each backend is a plist with the following keys:
  :name          - Symbol identifying the backend (e.g., 'pytest)
  :detect        - Function (root) -> t/nil to detect if backend applies
  :build-command - Function (root) -> command-list to build test command
  :create-parser-state - Function () -> initial parser state plist
  :parse-line    - Function (line state) -> (result . new-state)
                   Result must be a plist with:
                     :id     - Backend-specific test identifier string
                     :file   - Test file path
                     :test   - Test name
                     :status - Symbol: 'passed, 'failed, 'error, 'skipped, or nil
  :finish        - Function (state) -> () to finalize parsing on process exit
  :priority      - Integer priority (higher = checked first)")

(defvar touchstone--current-backend nil
  "The currently selected backend for the active test run.")

(defvar touchstone--parser-state nil
  "Current parser state for the active backend.")

(defun touchstone-register-backend (name backend-plist)
  "Register a test runner backend.
NAME is a symbol identifying the backend.
BACKEND-PLIST must contain:
  :name          - Display name string
  :detect        - Detection function
  :build-command - Command builder function
  :create-parser-state - Parser state initializer function
  :parse-line    - Line parser function
  :finish        - Finalization function
  :priority      - Integer priority (optional, default 0)"
  (let ((priority (or (plist-get backend-plist :priority) 0))
        (existing (assq name touchstone--backends)))
    ;; Remove existing registration if present
    (when existing
      (setq touchstone--backends (delq existing touchstone--backends)))
    ;; Add new registration
    (push (cons name backend-plist) touchstone--backends)
    ;; Sort by priority (higher first)
    (setq touchstone--backends
          (sort touchstone--backends
                (lambda (a b)
                  (> (or (plist-get (cdr a) :priority) 0)
                     (or (plist-get (cdr b) :priority) 0)))))))

(defun touchstone--select-backend (root)
  "Select appropriate backend for project at ROOT.
Returns backend plist or nil if no backend matches."
  (catch 'found
    (dolist (backend touchstone--backends)
      (let* ((backend-plist (cdr backend))
             (detect-fn (plist-get backend-plist :detect)))
        (when (and detect-fn (funcall detect-fn root))
          (throw 'found backend-plist))))
    nil))

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
Keys are backend-specific test identifier strings.
Values are plists with :id, :file, :test, :status, :marker, :details.")

(defvar-local touchstone--process-status nil
  "Current status of the test process: 'running, 'done, or 'aborted.")

(defvar-local touchstone--user-aborted nil
  "Flag indicating whether the user aborted the current test process.")

;;; Header Line

(defun touchstone--format-header-line ()
  "Format the header line for the touchstone buffer."
  (let ((backend-name (when touchstone--current-backend
                        (plist-get touchstone--current-backend :name)))
        (status-text (pcase touchstone--process-status
                       ('running (propertize "Running..." 'face 'success))
                       ('done (propertize "Done" 'face 'shadow))
                       ('aborted (propertize "Aborted" 'face 'warning))
                       (_ ""))))
    (if backend-name
        (concat "[" backend-name "] " status-text)
      status-text)))

(defun touchstone--update-header (status)
  "Update the header line with STATUS ('running or 'done)."
  (setq touchstone--process-status status)
  (force-mode-line-update))

;;; Test Result Formatting

(defun touchstone--normalize-status (status)
  "Normalize STATUS symbol to propertized display string.
'passed -> \"PASS\" with success face, 'failed -> \"FAIL\" with error face, etc."
  (pcase status
    ('passed (propertize "PASS" 'face 'success))
    ('failed (propertize "FAIL" 'face 'error))
    ('error (propertize "ERR!" 'face 'error))
    ('skipped (propertize "SKIP" 'face 'warning))
    (_ (propertize "????" 'face 'shadow))))

(defun touchstone--format-test-result (result)
  "Format a parsed test RESULT as a display line."
  (let* ((identifier (plist-get result :id))
         (status (plist-get result :status))
         (normalized-status (touchstone--normalize-status status))
         (has-details (plist-get result :details))
         (indicator (if has-details "+" " ")))
    (propertize
     (concat
      normalized-status
      indicator
      " "
      identifier
      "\n")
     'touchstone-test-id identifier)))

;;; Project Detection

(defun touchstone--find-project-root ()
  "Find the project root directory.
Uses the following methods in order:
1. project.el (if available)
2. projectile (if available)
3. Git root
4. Current directory"
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
     :connection-type 'pipe
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
  "Process a complete LINE of test output using the current backend.
Updates parser state and displays test results."
  (when touchstone--current-backend
    (let* ((parse-fn (plist-get touchstone--current-backend :parse-line))
           (result-and-state (funcall parse-fn line touchstone--parser-state))
           (result (car result-and-state))
           (new-state (cdr result-and-state)))
      ;; Update parser state
      (setq touchstone--parser-state new-state)
      ;; Display result if one was returned
      (when result
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
    ;; Finalize backend parsing on process exit
    (when (and (eq status 'exit) touchstone--current-backend)
      (let ((finish-fn (plist-get touchstone--current-backend :finish)))
        (funcall finish-fn touchstone--parser-state)))
    (cond
     ((eq status 'exit)
      (touchstone--handle-process-complete exit-code))
     ((memq status '(signal failed))
      (touchstone--handle-process-error event)))))

(defun touchstone--handle-process-complete (exit-code)
  "Handle test process completion with EXIT-CODE."
  (with-current-buffer (touchstone--get-results-buffer)
    (setq touchstone--user-aborted nil)
    (touchstone--update-header 'done)))

(defun touchstone--handle-process-error (event)
  "Handle test process error with EVENT description."
  (with-current-buffer (touchstone--get-results-buffer)
    (if touchstone--user-aborted
        (progn
          (setq touchstone--user-aborted nil)
          (touchstone--update-header 'aborted))
      (touchstone--update-header 'done))))

(defun touchstone--kill-process ()
  "Kill the current touchstone test process if running."
  (when (and touchstone--process
             (process-live-p touchstone--process))
    (with-current-buffer (touchstone--get-results-buffer)
      (setq touchstone--user-aborted t))
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
      (setq touchstone--output-buffer "")
      (setq touchstone--line-buffer "")
      (setq touchstone--test-results (make-hash-table :test 'equal))
      (setq touchstone--user-aborted nil)
      ;; Initialize backend parser state if backend is selected
      (when touchstone--current-backend
        (let ((create-state-fn (plist-get touchstone--current-backend :create-parser-state)))
          (setq touchstone--parser-state (funcall create-state-fn))))
      ;; Update header to show running status
      (touchstone--update-header 'running))))

(defun touchstone--display-test-result (result)
  "Display a formatted test RESULT in the results buffer."
  (let ((identifier (plist-get result :id)))
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
         (identifier (plist-get result :id))
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
      (overlay-put ov 'touchstone-test-id identifier)
      (overlay-put ov 'evaporate t)
      ;; Store overlay reference in the result
      (plist-put result :overlay ov))))

(defun touchstone--toggle-details-at-point ()
  "Toggle visibility of test details at point.
If point is on a test line, toggle its details.
If point is within details, collapse them and move to the test line."
  (interactive)
  (let* ((line-start (line-beginning-position))
         ;; Check if we're on a test line
         (test-id-from-line (get-text-property line-start 'touchstone-test-id))
         ;; Check if we're within a details overlay
         (overlays-here (overlays-at (point)))
         (details-overlay (seq-find (lambda (ov)
                                      (overlay-get ov 'touchstone-details))
                                    overlays-here))
         (test-id-from-overlay (when details-overlay
                                 (overlay-get details-overlay 'touchstone-test-id))))
    (cond
     ;; Case 1: We're on a test line
     ((and test-id-from-line
           (gethash test-id-from-line touchstone--test-results))
      (let* ((result (gethash test-id-from-line touchstone--test-results))
             (overlay (plist-get result :overlay)))
        (if overlay
            (let ((currently-invisible (overlay-get overlay 'invisible)))
              (overlay-put overlay 'invisible (not currently-invisible))
              ;; Update the indicator on the test line (character at position 4)
              (save-excursion
                (goto-char line-start)
                (when (looking-at "^....[+-]")
                  (let ((inhibit-read-only t)
                        (new-indicator (if currently-invisible "-" "+")))
                    (forward-char 4)
                    (delete-char 1)
                    (insert new-indicator)))))
          (message "No details available for this test"))))

     ;; Case 2: We're within a details overlay
     ((and details-overlay test-id-from-overlay
           (gethash test-id-from-overlay touchstone--test-results))
      (let* ((result (gethash test-id-from-overlay touchstone--test-results))
             (marker (plist-get result :marker))
             (line-start (save-excursion
                          (goto-char marker)
                          (beginning-of-line)
                          (point))))
        ;; Collapse the details
        (overlay-put details-overlay 'invisible t)
        ;; Update the indicator on the test line (character at position 4)
        (save-excursion
          (goto-char line-start)
          (when (looking-at "^....-")
            (let ((inhibit-read-only t))
              (forward-char 4)
              (delete-char 1)
              (insert "+"))))
        ;; Move point to the start of the test line
        (goto-char line-start)))

     ;; Case 3: Not on a test line or in details
     (t
      (message "Not on a test result line or within details")))))

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

Displays test results in a clean, status-first format.
Press TAB or RET on a test to toggle its details.

\\{touchstone-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil)
  (setq header-line-format '(:eval (touchstone--format-header-line))))

;;; Public Commands

;;;###autoload
(defun touchstone-run-tests ()
  "Run tests for the current project."
  (interactive)
  (touchstone--kill-process)
  (touchstone--reset-project-root)

  ;; Find project root and select backend
  (let* ((root (touchstone--find-project-root))
         (backend (touchstone--select-backend root)))
    (if (not backend)
        (error "No test backend found for this project")
      ;; Set current backend
      (setq touchstone--current-backend backend)

      ;; Initialize results buffer (this also initializes parser state)
      (touchstone--init-results-buffer)
      (touchstone--display-results-buffer)

      ;; Build command using backend
      (let* ((build-command-fn (plist-get backend :build-command))
             (command-list (funcall build-command-fn root)))
        ;; Create and start the process
        (setq touchstone--process
              (touchstone--make-process command-list))
        (message "Running tests with %s..." (plist-get backend :name))))))

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
