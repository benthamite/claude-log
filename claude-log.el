;;; claude-log.el --- Browse Claude Code conversation logs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/claude-log
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (markdown-mode "2.6"))
;; Keywords: tools

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Browse and render Claude Code conversation logs stored as JSONL files.
;; Provides a session browser, Markdown rendering, live updates via
;; file-notify, and outline-based navigation.
;;
;; Entry points:
;;   `claude-log-browse-sessions' - pick a session from history
;;   `claude-log-search-sessions' - search across all sessions
;;   `claude-log-open-file'       - open a specific JSONL file

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'filenotify)
(require 'outline)
(require 'markdown-mode)

;;;;; Customization

(defgroup claude-log nil
  "Browse Claude Code conversation logs."
  :group 'tools
  :prefix "claude-log-")

(defcustom claude-log-directory "~/.claude"
  "Root directory of Claude Code configuration."
  :type 'directory)

(defcustom claude-log-show-thinking 'collapsed
  "How to display assistant thinking blocks.
`hidden' omits them entirely, `collapsed' shows them folded under
a heading, `visible' shows them expanded."
  :type '(choice (const :tag "Hidden" hidden)
                 (const :tag "Collapsed" collapsed)
                 (const :tag "Visible" visible)))

(defcustom claude-log-show-tools 'collapsed
  "How to display tool-use and tool-result sections.
`hidden' omits them entirely, `collapsed' shows them folded under
a heading, `visible' shows them expanded."
  :type '(choice (const :tag "Hidden" hidden)
                 (const :tag "Collapsed" collapsed)
                 (const :tag "Visible" visible)))

(defcustom claude-log-timestamp-format "%Y-%m-%d %H:%M:%S"
  "Format string for timestamps in rendered output."
  :type 'string)

(defcustom claude-log-max-tool-input-length 200
  "Maximum characters to show for tool input summaries."
  :type 'integer)

(defcustom claude-log-max-tool-result-length 500
  "Maximum characters to show for tool result content."
  :type 'integer)

(defcustom claude-log-live-update t
  "Whether to watch the JSONL file for live updates."
  :type 'boolean)

(defcustom claude-log-group-by-project t
  "Whether to group sessions by project in the browser.
When non-nil, `claude-log-browse-sessions' first prompts for a
project, then for a session within that project."
  :type 'boolean)

(defcustom claude-log-display-width 60
  "Maximum width of the first-message column in the session browser."
  :type 'integer)

;;;;; Internal variables

(defvar-local claude-log--source-file nil
  "Path to the JSONL file being displayed.")

(defvar-local claude-log--file-offset 0
  "Byte offset into the JSONL file for incremental reads.")

(defvar-local claude-log--partial-line ""
  "Leftover partial line from the last incremental read.")

(defvar-local claude-log--watcher nil
  "File-notify descriptor for live updates.")

(defvar-local claude-log--session-project nil
  "Project name for the current session.")

(defvar-local claude-log--session-date nil
  "Date string for the current session.")

;;;;; Entry points

;;;###autoload
(defun claude-log-browse-sessions ()
  "Browse Claude Code sessions and open the selected one.
When `claude-log-group-by-project' is non-nil, first prompts for
a project, then for a session within that project."
  (interactive)
  (let ((sessions (claude-log--read-sessions)))
    (if claude-log-group-by-project
        (claude-log--browse-grouped sessions)
      (claude-log--browse-flat sessions))))

;;;###autoload
(defun claude-log-open-file (file)
  "Open and render the Claude Code JSONL log at FILE."
  (interactive "fJSONL file: ")
  (let* ((file (expand-file-name file))
         (buf (generate-new-buffer (claude-log--buffer-name file))))
    (with-current-buffer buf
      (claude-log--activate-mode)
      (setq claude-log--source-file file)
      (claude-log--render-full)
      (when (and claude-log-live-update (not claude-log--watcher))
        (claude-log--start-watcher)))
    (pop-to-buffer buf)))

(defun claude-log--activate-mode ()
  "Activate `claude-log-mode' with parent mode hooks suppressed.
Suppresses `markdown-mode-hook', `markdown-view-mode-hook', and
flycheck's global-mode hook to prevent unwanted side effects."
  (let ((markdown-mode-hook nil)
        (markdown-view-mode-hook nil)
        (after-change-major-mode-hook
         (remq 'flycheck-global-mode-enable-in-buffers
               after-change-major-mode-hook)))
    (claude-log-mode)))

;;;###autoload
(defun claude-log-search-sessions (&optional initial)
  "Search across all sessions and open a matching one.
When `consult' is available, search incrementally with ripgrep
showing matching lines in context.  Otherwise, prompt for a
search string.  INITIAL is the optional initial input."
  (interactive)
  (if (require 'consult nil t)
      (claude-log--search-with-consult initial)
    (claude-log--search-with-rg (read-string "Search sessions: "))))

;;;;; Session search (consult)

(declare-function consult--grep "consult")
(declare-function consult--grep-state "consult")
(declare-function consult--grep-format "consult")
(declare-function consult--ripgrep-make-builder "consult")
(declare-function consult--async-transform-by-input "consult")
(defvar consult-ripgrep-args)
(defvar consult--grep-match-regexp)

(defun claude-log--search-with-consult (&optional initial)
  "Search sessions incrementally using consult and ripgrep.
INITIAL is the optional initial input."
  (let* ((dir (expand-file-name "projects" claude-log-directory))
         (match
          (cl-letf (((symbol-function 'consult--grep-state)
                     (lambda () (lambda (_action _cand))))
                    ((symbol-function 'consult--grep-format)
                     #'claude-log--grep-format))
            (consult--grep "Search sessions"
                           #'claude-log--consult-ripgrep-builder
                           dir initial))))
    (when match
      (claude-log--open-grep-match match dir))))

(defun claude-log--consult-ripgrep-builder (paths)
  "Build a ripgrep command restricted to JSONL files in PATHS.
Limits to one match per file and shows truncated previews."
  (let ((consult-ripgrep-args
         (concat consult-ripgrep-args
                 " --glob=*.jsonl --max-count=1"
                 " --max-columns=500 --max-columns-preview")))
    (consult--ripgrep-make-builder paths)))

(defun claude-log--grep-format (builder)
  "Return a transform function that formats grep results.
Replaces encoded file paths with readable project names.
BUILDER is the command line builder function."
  (consult--async-transform-by-input
   (lambda (input)
     (let ((highlight (cdr (funcall builder input))))
       (lambda (cands)
         (claude-log--format-grep-candidates cands highlight))))))

(defun claude-log--format-grep-candidates (cands highlight)
  "Format CANDS from raw ripgrep output into readable candidates.
HIGHLIGHT is the match highlight function."
  (let (result)
    (save-match-data
      (dolist (str cands (nreverse result))
        (when (string-match consult--grep-match-regexp str)
          (let* ((file (match-string 1 str))
                 (line (match-string 2 str))
                 (content (substring str (match-end 0)))
                 (project (claude-log--project-from-path file))
                 (formatted (concat project ":" line ":" content))
                 (plen (length project))
                 (llen (length line)))
            (when highlight
              (funcall highlight content))
            (add-text-properties
             0 plen
             `(face consult-file consult--prefix-group ,project)
             formatted)
            (put-text-property
             (1+ plen) (+ 1 plen llen)
             'face 'consult-line-number formatted)
            (put-text-property
             0 (length formatted) 'claude-log--file file formatted)
            (push formatted result)))))))

(defun claude-log--project-from-path (path)
  "Extract a readable project name from encoded grep PATH.
PATH looks like `-Users-foo-repos-project/uuid.jsonl'."
  (let ((dir (directory-file-name (or (file-name-directory path) path))))
    (cond
     ((string-match ".*-repos-\\(.+\\)" dir)
      (match-string 1 dir))
     ((string-match "-\\([^/]+\\)\\'" dir)
      (match-string 1 dir))
     (t dir))))

(defun claude-log--open-grep-match (match dir)
  "Extract the file path from consult grep MATCH and open it.
DIR is the base directory that paths are relative to."
  (let ((file (or (get-text-property 0 'claude-log--file match)
                  (substring-no-properties
                   match 0
                   (next-single-property-change 0 'face match)))))
    (claude-log-open-file (expand-file-name file dir))))

;;;;; Session search (fallback)

(defun claude-log--search-with-rg (query)
  "Search sessions matching QUERY and present for selection."
  (let* ((matching-files (claude-log--grep-session-files query))
         (sessions (claude-log--read-sessions))
         (matching-sessions (claude-log--filter-sessions-by-files
                             sessions matching-files)))
    (unless matching-sessions
      (user-error "No sessions match \"%s\"" query))
    (message "Found %d matching session(s)" (length matching-sessions))
    (if claude-log-group-by-project
        (claude-log--browse-grouped matching-sessions)
      (claude-log--browse-flat matching-sessions))))

(defun claude-log--grep-session-files (query)
  "Return a list of JSONL file paths containing QUERY."
  (let* ((projects-dir (expand-file-name "projects" claude-log-directory))
         (output (claude-log--run-ripgrep query projects-dir)))
    (split-string output "\n" t)))

(defun claude-log--run-ripgrep (query dir)
  "Search for QUERY across JSONL files in DIR using ripgrep.
Returns stdout as a string of newline-separated file paths."
  (unless (executable-find "rg")
    (user-error "ripgrep (rg) is required for session search"))
  (with-temp-buffer
    (call-process "rg" nil t nil
                  "--files-with-matches" "--fixed-strings"
                  "--ignore-case" "--glob" "*.jsonl"
                  query dir)
    (buffer-string)))

(defun claude-log--filter-sessions-by-files (sessions files)
  "Filter SESSIONS to those whose file appears in FILES."
  (let ((file-set (make-hash-table :test #'equal)))
    (dolist (f files)
      (puthash (expand-file-name f) t file-set))
    (seq-filter (lambda (session)
                  (gethash (expand-file-name (plist-get (cdr session) :file))
                           file-set))
                sessions)))

;;;;; Session browser

(defun claude-log--read-sessions ()
  "Parse `history.jsonl' and return alist of session-id to metadata.
Each value is a plist (:display :timestamp :project :file)."
  (let ((history-file (expand-file-name "history.jsonl" claude-log-directory))
        (sessions (make-hash-table :test #'equal)))
    (unless (file-exists-p history-file)
      (user-error "History file not found: %s" history-file))
    (dolist (entry (claude-log--parse-jsonl-file history-file))
      (let ((sid (plist-get entry :sessionId)))
        (when (and sid (not (gethash sid sessions)))
          (puthash sid entry sessions))))
    (let (result)
      (maphash
       (lambda (sid entry)
         (when-let* ((file (claude-log--find-session-file sid)))
           (push (list sid
                       :display (or (plist-get entry :display) "")
                       :timestamp (plist-get entry :timestamp)
                       :project (or (plist-get entry :project) "")
                       :file file)
                 result)))
       sessions)
      (sort result (lambda (a b)
                     (> (or (plist-get (cdr a) :timestamp) 0)
                        (or (plist-get (cdr b) :timestamp) 0)))))))

(defun claude-log--find-session-file (session-id)
  "Find the JSONL file for SESSION-ID under the projects directory."
  (let ((projects-dir (expand-file-name "projects" claude-log-directory)))
    (when (file-directory-p projects-dir)
      (cl-block nil
        (dolist (dir (directory-files projects-dir t "^[^.]"))
          (when (file-directory-p dir)
            (let ((file (expand-file-name (concat session-id ".jsonl") dir)))
              (when (file-exists-p file)
                (cl-return file)))))))))

(defun claude-log--browse-flat (sessions)
  "Present all SESSIONS in a single `completing-read'."
  (let* ((candidates (claude-log--build-candidates sessions))
         (selected (claude-log--completing-read "Session: " candidates))
         (file (alist-get selected candidates nil nil #'equal)))
    (claude-log-open-file file)))

(defun claude-log--browse-grouped (sessions)
  "Present SESSIONS grouped by project: first pick project, then session."
  (let* ((grouped (claude-log--group-by-project sessions))
         (project-names (mapcar #'car grouped))
         (project (claude-log--completing-read "Project: " project-names))
         (project-sessions (alist-get project grouped nil nil #'equal))
         (candidates (claude-log--build-candidates project-sessions))
         (selected (claude-log--completing-read "Session: " candidates))
         (file (alist-get selected candidates nil nil #'equal)))
    (claude-log-open-file file)))

(defun claude-log--completing-read (prompt collection)
  "Read from COLLECTION with PROMPT, preserving display order."
  (completing-read prompt
                   (claude-log--preserve-order-table collection)
                   nil t))

(defun claude-log--group-by-project (sessions)
  "Group SESSIONS into an alist of (project-name . sessions).
Projects are sorted by most recent session timestamp."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (session sessions)
      (let* ((project (claude-log--short-project
                       (plist-get (cdr session) :project)))
             (existing (gethash project groups)))
        (puthash project (append existing (list session)) groups)))
    (let (result)
      (maphash (lambda (project sessions)
                 (push (cons project sessions) result))
               groups)
      (sort result (lambda (a b)
                     (let ((ts-a (plist-get (cdadr a) :timestamp))
                           (ts-b (plist-get (cdadr b) :timestamp)))
                       (> (or ts-a 0) (or ts-b 0))))))))

(defun claude-log--build-candidates (sessions)
  "Build an alist of (display-string . file-path) from SESSIONS."
  (mapcar
   (lambda (session)
     (let* ((meta (cdr session))
            (ts (plist-get meta :timestamp))
            (date (claude-log--format-epoch-ms ts))
            (project (claude-log--short-project (plist-get meta :project)))
            (display (claude-log--normalize-whitespace
                      (plist-get meta :display)))
            (display (claude-log--truncate-string display claude-log-display-width))
            (label (format "%s  %-20s  \"%s\"" date project display)))
       (cons label (plist-get meta :file))))
   sessions))

(defun claude-log--short-project (path)
  "Extract a short project name from PATH."
  (if (or (null path) (string-empty-p path))
      "unknown"
    (file-name-nondirectory (directory-file-name path))))

(defun claude-log--format-epoch-ms (ms)
  "Format millisecond epoch timestamp MS as a date-time string."
  (if (numberp ms)
      (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time (/ ms 1000.0)))
    "unknown"))

;;;;; JSONL parsing

(defun claude-log--parse-jsonl-file (file)
  "Parse FILE as JSONL, returning a list of plists."
  (let ((lines (claude-log--read-file-lines file)))
    (mapcar #'claude-log--parse-json-line
            (seq-filter (lambda (s) (not (string-empty-p s))) lines))))

(defun claude-log--read-file-lines (file)
  "Read FILE and return a list of non-empty lines."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

(defun claude-log--parse-json-line (line)
  "Parse a single JSON LINE into a plist."
  (json-parse-string line :object-type 'plist :array-type 'list))

(defun claude-log--filter-conversation (entries)
  "Filter ENTRIES to user and assistant messages, excluding system entries."
  (seq-filter #'claude-log--conversation-entry-p entries))

(defun claude-log--conversation-entry-p (entry)
  "Return non-nil if ENTRY is a genuine conversation message."
  (let ((type (plist-get entry :type)))
    (and (member type '("user" "assistant"))
         (not (claude-log--system-entry-p entry)))))

(defconst claude-log--system-tag-regexp
  (rx bos (0+ space) "<"
      (or "local-command-caveat"
          "local-command-stdout"
          "local-command-stderr"
          "command-name"
          "command-message"
          "task-notification"
          "teammate-message")
      (or ">" " "))
  "Regexp matching system-generated XML tags in user entries.")

(defun claude-log--system-entry-p (entry)
  "Return non-nil if ENTRY is a system-generated message.
These are user-role entries whose string content starts with a
known system XML tag."
  (let* ((content (plist-get (plist-get entry :message) :content)))
    (and (stringp content)
         (string-match-p claude-log--system-tag-regexp content))))

;;;;; Rendering

(defun claude-log--render-full ()
  "Render the full JSONL file into the current buffer."
  (let* ((entries (claude-log--parse-jsonl-file claude-log--source-file))
         (conversation (claude-log--filter-conversation entries)))
    (claude-log--extract-session-metadata entries)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (claude-log--render-header))
      (dolist (entry conversation)
        (insert (claude-log--render-entry entry)))
      (claude-log--record-offset)
      (goto-char (point-min))
      (claude-log--collapse-as-configured))))

(defun claude-log--extract-session-metadata (entries)
  "Extract project and date from ENTRIES."
  (when-let* ((first-msg (seq-find
                           (lambda (e) (member (plist-get e :type)
                                               '("user" "assistant")))
                           entries)))
    (setq claude-log--session-date
          (claude-log--format-iso-timestamp (plist-get first-msg :timestamp))))
  (when-let* ((progress (seq-find
                          (lambda (e) (equal (plist-get e :type) "progress"))
                          entries)))
    (setq claude-log--session-project
          (or (plist-get progress :cwd) ""))))

(defun claude-log--render-header ()
  "Return the Markdown header for the session."
  (let ((project (claude-log--short-project
                  (or claude-log--session-project "")))
        (date (or claude-log--session-date "unknown")))
    (format "# Session: %s — %s\n\n" project date)))

(defun claude-log--render-entry (entry)
  "Render a single conversation ENTRY to a Markdown string."
  (let* ((message (plist-get entry :message))
         (timestamp (plist-get entry :timestamp))
         (role (plist-get message :role))
         (content (plist-get message :content))
         (time-str (claude-log--format-iso-timestamp timestamp)))
    (cond
     ((equal role "user")
      (claude-log--render-user-turn content time-str))
     ((equal role "assistant")
      (claude-log--render-assistant-turn content time-str))
     (t ""))))

(defun claude-log--render-user-turn (content time-str)
  "Render a user turn with CONTENT and TIME-STR.
If CONTENT contains only tool results and no user text, render
them without a User heading."
  (if (stringp content)
      (format "---\n\n## User — %s\n\n%s\n\n" time-str content)
    (let ((text-parts (claude-log--collect-user-text content))
          (tool-parts (claude-log--collect-tool-results content)))
      (cond
       ((and text-parts (null tool-parts))
        (concat (format "---\n\n## User — %s\n\n" time-str)
                (string-join text-parts)))
       ((and text-parts tool-parts)
        (concat (format "---\n\n## User — %s\n\n" time-str)
                (string-join text-parts)
                (string-join tool-parts)))
       (tool-parts
        (string-join tool-parts))
       (t "")))))

(defun claude-log--collect-user-text (content)
  "Collect non-empty text parts from user CONTENT array.
Returns a list of formatted strings, or nil if there is no text."
  (let (parts)
    (dolist (item content)
      (when (equal (plist-get item :type) "text")
        (let ((text (plist-get item :text)))
          (when (and text (not (string-empty-p (string-trim text))))
            (push (format "%s\n\n" text) parts)))))
    (nreverse parts)))

(defun claude-log--collect-tool-results (content)
  "Collect tool result parts from user CONTENT array.
Returns a list of formatted strings, or nil if there are none or
`claude-log-show-tools' is `hidden'."
  (when (not (eq claude-log-show-tools 'hidden))
    (let (parts)
      (dolist (item content)
        (when (equal (plist-get item :type) "tool_result")
          (push (claude-log--render-tool-result item) parts)))
      (nreverse parts))))

(defun claude-log--render-assistant-turn (content time-str)
  "Render an assistant turn with CONTENT and TIME-STR.
Returns an empty string if CONTENT produces no visible output."
  (let ((body (claude-log--render-assistant-body content)))
    (if (string-empty-p body)
        ""
      (concat (format "---\n\n## Assistant — %s\n\n" time-str) body))))

(defun claude-log--render-assistant-body (content)
  "Render the body of an assistant turn from CONTENT items."
  (let ((parts '()))
    (when (listp content)
      (dolist (item content)
        (let ((item-type (plist-get item :type)))
          (cond
           ((equal item-type "thinking")
            (when (not (eq claude-log-show-thinking 'hidden))
              (push (claude-log--render-thinking item) parts)))
           ((equal item-type "text")
            (let ((text (plist-get item :text)))
              (when (and text (not (string-empty-p (string-trim text))))
                (push (format "%s\n\n" text) parts))))
           ((equal item-type "tool_use")
            (when (not (eq claude-log-show-tools 'hidden))
              (push (claude-log--render-tool-use item) parts)))))))
    (apply #'concat (nreverse parts))))

(defun claude-log--render-thinking (item)
  "Render a thinking ITEM."
  (let* ((text (or (plist-get item :thinking) ""))
         (truncated (claude-log--truncate-string text claude-log-max-tool-result-length)))
    (format "#### Thinking\n\n%s\n\n" truncated)))

(defun claude-log--render-tool-use (item)
  "Render a tool_use ITEM with a smart summary of its input."
  (let* ((name (plist-get item :name))
         (input (plist-get item :input))
         (summary (claude-log--summarize-tool-input name input)))
    (format "#### Tool: %s\n\n%s\n\n" name summary)))

(defun claude-log--render-tool-result (item)
  "Render a tool_result ITEM."
  (let* ((content (plist-get item :content))
         (text (claude-log--extract-tool-result-text content))
         (truncated (claude-log--truncate-string text claude-log-max-tool-result-length)))
    (format "#### Tool result\n\n> %s\n\n"
            (string-replace "\n" "\n> " truncated))))

(defun claude-log--extract-tool-result-text (content)
  "Extract text from tool result CONTENT, which may be a string or list."
  (cond
   ((stringp content) content)
   ((listp content)
    (mapconcat
     (lambda (item)
       (if (equal (plist-get item :type) "text")
           (plist-get item :text)
         ""))
     content
     "\n"))
   (t "")))

;;;;; Tool input summaries

(defun claude-log--summarize-tool-input (name input)
  "Return a concise summary of INPUT for tool NAME."
  (let ((summary (claude-log--summarize-tool-input-by-name name input)))
    (if (string-empty-p summary)
        (claude-log--summarize-tool-input-generic input)
      summary)))

(defun claude-log--summarize-tool-input-by-name (name input)
  "Return a summary of tool INPUT specific to tool NAME."
  (pcase name
    ("Read" (claude-log--summarize-read input))
    ("Write" (claude-log--summarize-write input))
    ("Edit" (claude-log--summarize-edit input))
    ("Bash" (claude-log--summarize-bash input))
    ("Grep" (claude-log--summarize-grep input))
    ("Glob" (claude-log--summarize-glob input))
    ("WebFetch" (claude-log--summarize-web-fetch input))
    ("WebSearch" (claude-log--summarize-web-search input))
    ("Task" (claude-log--summarize-task input))
    (_ "")))

(defun claude-log--summarize-read (input)
  "Summarize Read tool INPUT."
  (format "> **file_path**: %s" (or (plist-get input :file_path) "?")))

(defun claude-log--summarize-write (input)
  "Summarize Write tool INPUT."
  (format "> **file_path**: %s" (or (plist-get input :file_path) "?")))

(defun claude-log--summarize-edit (input)
  "Summarize Edit tool INPUT."
  (let ((file (or (plist-get input :file_path) "?"))
        (old (claude-log--truncate-string
              (or (plist-get input :old_string) "") 80)))
    (format "> **file_path**: %s\n> **old_string**: `%s`" file old)))

(defun claude-log--summarize-bash (input)
  "Summarize Bash tool INPUT."
  (let ((cmd (claude-log--truncate-string
              (or (plist-get input :command) "") claude-log-max-tool-input-length)))
    (format "> ```\n> %s\n> ```" cmd)))

(defun claude-log--summarize-grep (input)
  "Summarize Grep tool INPUT."
  (let ((pattern (or (plist-get input :pattern) "?"))
        (path (or (plist-get input :path) "")))
    (if (string-empty-p path)
        (format "> **pattern**: `%s`" pattern)
      (format "> **pattern**: `%s` in %s" pattern path))))

(defun claude-log--summarize-glob (input)
  "Summarize Glob tool INPUT."
  (format "> **pattern**: `%s`" (or (plist-get input :pattern) "?")))

(defun claude-log--summarize-web-fetch (input)
  "Summarize WebFetch tool INPUT."
  (format "> **url**: %s" (or (plist-get input :url) "?")))

(defun claude-log--summarize-web-search (input)
  "Summarize WebSearch tool INPUT."
  (format "> **query**: %s" (or (plist-get input :query) "?")))

(defun claude-log--summarize-task (input)
  "Summarize Task tool INPUT."
  (let ((desc (or (plist-get input :description) ""))
        (type (or (plist-get input :subagent_type) "")))
    (format "> **%s**: %s" type desc)))

(defun claude-log--summarize-tool-input-generic (input)
  "Return a generic summary of tool INPUT plist."
  (let ((parts '()))
    (cl-loop for (key val) on input by #'cddr
             do (let* ((k (substring (symbol-name key) 1))
                       (v (claude-log--truncate-string
                           (format "%s" val)
                           claude-log-max-tool-input-length)))
                  (push (format "> **%s**: %s" k v) parts)))
    (string-join (nreverse parts) "\n")))

;;;;; Timestamps

(defun claude-log--format-iso-timestamp (ts)
  "Format ISO 8601 timestamp TS according to `claude-log-timestamp-format'."
  (if (and (stringp ts) (not (string-empty-p ts)))
      (claude-log--parse-and-format-iso ts)
    "unknown"))

(defun claude-log--parse-and-format-iso (ts)
  "Parse ISO 8601 string TS and format it."
  (condition-case nil
      (let ((time (date-to-time ts)))
        (format-time-string claude-log-timestamp-format time))
    (error ts)))

;;;;; Utilities

(defun claude-log--preserve-order-table (collection)
  "Wrap COLLECTION in a completion table that preserves display order.
COLLECTION is a list of strings or an alist of (string . value)."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity))
      (complete-with-action action collection string pred))))

(defun claude-log--normalize-whitespace (str)
  "Collapse all whitespace in STR into single spaces and trim."
  (string-trim (replace-regexp-in-string "[\n\r\t ]+" " " (or str ""))))

(defun claude-log--truncate-string (str max)
  "Truncate STR to MAX characters, appending ellipsis if needed."
  (if (<= (length str) max)
      str
    (concat (substring str 0 max) "…")))

(defun claude-log--buffer-name (file)
  "Generate a buffer name for JSONL FILE."
  (format "*claude-log: %s*"
          (file-name-sans-extension (file-name-nondirectory file))))

;;;;; Major mode

(defvar claude-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'claude-log-next-turn)
    (define-key map "p" #'claude-log-previous-turn)
    (define-key map (kbd "TAB") #'claude-log-toggle-section)
    (define-key map "C" #'claude-log-collapse-all-tools)
    (define-key map "E" #'claude-log-expand-all)
    (define-key map "g" #'claude-log-refresh)
    (define-key map "w" #'claude-log-copy-turn)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `claude-log-mode'.")

(define-derived-mode claude-log-mode markdown-view-mode "Claude-Log"
  "Major mode for viewing Claude Code conversation logs.
\\{claude-log-mode-map}"
  (setq-local outline-regexp "##+ ")
  (setq-local outline-level #'claude-log--outline-level)
  (outline-minor-mode 1)
  (add-hook 'kill-buffer-hook #'claude-log--cleanup nil t))

(defun claude-log--outline-level ()
  "Return the outline level based on the number of `#' characters."
  (- (match-end 0) (match-beginning 0) 1))

(defun claude-log--cleanup ()
  "Clean up file watcher when buffer is killed."
  (when claude-log--watcher
    (file-notify-rm-watch claude-log--watcher)
    (setq claude-log--watcher nil)))

;;;;; Live updates

(defun claude-log--record-offset ()
  "Record the current byte size of the source file."
  (when claude-log--source-file
    (setq claude-log--file-offset
          (file-attribute-size (file-attributes claude-log--source-file)))))

(defun claude-log--start-watcher ()
  "Start watching the JSONL file for change events."
  (setq claude-log--watcher
        (file-notify-add-watch
         claude-log--source-file '(change)
         (let ((buf (current-buffer)))
           (lambda (_event)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (claude-log--handle-file-change))))))))

(defun claude-log--handle-file-change ()
  "Handle a change notification on the JSONL file."
  (let* ((new-size (file-attribute-size
                    (file-attributes claude-log--source-file)))
         (at-end (>= (point) (point-max))))
    (when (and new-size (> new-size claude-log--file-offset))
      (let ((new-text (claude-log--read-bytes-from
                       claude-log--source-file claude-log--file-offset new-size)))
        (setq claude-log--file-offset new-size)
        (claude-log--process-incremental-text new-text at-end)))))

(defun claude-log--read-bytes-from (file start end)
  "Read bytes from FILE between START and END offsets."
  (with-temp-buffer
    (insert-file-contents file nil start end)
    (buffer-string)))

(defun claude-log--process-incremental-text (text at-end)
  "Parse new TEXT from the JSONL file and append rendered entries.
If AT-END is non-nil, scroll to show new content."
  (let* ((combined (concat claude-log--partial-line text))
         (lines (split-string combined "\n")))
    (setq claude-log--partial-line (car (last lines)))
    (let ((complete-lines (butlast lines)))
      (dolist (line complete-lines)
        (unless (string-empty-p line)
          (claude-log--append-rendered-line line))))
    (when at-end
      (goto-char (point-max)))))

(defun claude-log--append-rendered-line (line)
  "Parse LINE as JSON and append its rendering if it is a conversation entry."
  (when-let* ((entry (claude-log--try-parse-json line)))
    (when (claude-log--conversation-entry-p entry)
      (let ((rendered (claude-log--render-entry entry))
            (inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert rendered)
          (claude-log--collapse-region
           (- (point-max) (length rendered)) (point-max)))))))

(defun claude-log--try-parse-json (line)
  "Parse LINE as JSON, returning nil if it is not valid JSON."
  (ignore-errors (claude-log--parse-json-line line)))

;;;;; Navigation commands

(defun claude-log-next-turn ()
  "Move point to the next User or Assistant heading."
  (interactive)
  (let ((pos (claude-log--find-turn-heading t)))
    (if pos
        (goto-char pos)
      (message "No more turns"))))

(defun claude-log-previous-turn ()
  "Move point to the previous User or Assistant heading."
  (interactive)
  (let ((pos (claude-log--find-turn-heading nil)))
    (if pos
        (goto-char pos)
      (message "No previous turns"))))

(defun claude-log--find-turn-heading (forward)
  "Find the next (if FORWARD) or previous turn heading.
Returns the position, or nil."
  (save-excursion
    (if forward (end-of-line) (beginning-of-line))
    (let ((re "^## \\(User\\|Assistant\\) — "))
      (if forward
          (when (re-search-forward re nil t)
            (match-beginning 0))
        (when (re-search-backward re nil t)
          (match-beginning 0))))))

(defun claude-log-toggle-section ()
  "Toggle the visibility of the section at point."
  (interactive)
  (when (bound-and-true-p outline-minor-mode)
    (outline-toggle-children)))

(defun claude-log-collapse-all-tools ()
  "Collapse all tool-use, tool-result, and thinking sections."
  (interactive)
  (claude-log--collapse-headings-matching "^####+ "))

(defun claude-log-expand-all ()
  "Expand all sections."
  (interactive)
  (let ((inhibit-read-only t))
    (outline-show-all)))

(defun claude-log--collapse-as-configured ()
  "Collapse sections per `claude-log-show-thinking' and `claude-log-show-tools'."
  (when (eq claude-log-show-thinking 'collapsed)
    (claude-log--collapse-headings-matching "^#### Thinking$"))
  (when (eq claude-log-show-tools 'collapsed)
    (claude-log--collapse-headings-matching "^#### Tool")))

(defun claude-log--collapse-headings-matching (regexp)
  "Collapse all outline headings matching REGEXP in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (goto-char (match-beginning 0))
      (outline-hide-subtree)
      (forward-line 1))))

(defun claude-log--collapse-region (start end)
  "Collapse sections between START and END according to configuration."
  (when (eq claude-log-show-thinking 'collapsed)
    (claude-log--collapse-headings-in-region "^#### Thinking$" start end))
  (when (eq claude-log-show-tools 'collapsed)
    (claude-log--collapse-headings-in-region "^#### Tool" start end)))

(defun claude-log--collapse-headings-in-region (regexp start end)
  "Collapse outline headings matching REGEXP between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward regexp end t)
      (goto-char (match-beginning 0))
      (outline-hide-subtree)
      (forward-line 1))))

(defun claude-log-refresh ()
  "Re-render the buffer from scratch."
  (interactive)
  (when claude-log--source-file
    (setq claude-log--partial-line "")
    (claude-log--render-full)))

(defun claude-log-copy-turn ()
  "Copy the current turn (from ## heading to next ##) to the kill ring."
  (interactive)
  (let ((start (claude-log--turn-start))
        (end (claude-log--turn-end)))
    (when (and start end)
      (kill-ring-save start end)
      (message "Turn copied"))))

(defun claude-log--turn-start ()
  "Return the start position of the current turn heading."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^## ")
        (point)
      (when (re-search-backward "^## " nil t)
        (point)))))

(defun claude-log--turn-end ()
  "Return the end position of the current turn."
  (save-excursion
    (when-let* ((start (claude-log--turn-start)))
      (goto-char start)
      (forward-line 1)
      (if (re-search-forward "^## " nil t)
          (match-beginning 0)
        (point-max)))))

(provide 'claude-log)
;;; claude-log.el ends here
