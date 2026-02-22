;;; claude-log.el --- Browse Claude Code conversation logs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/claude-log
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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
;;   `claude-log-open-file'       - open a specific JSONL file

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'filenotify)
(require 'outline)

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

(defcustom claude-log-show-tool-output t
  "Whether to show tool result content in user turns."
  :type 'boolean)

(defcustom claude-log-collapse-tools t
  "Whether to auto-fold tool and thinking sections after rendering."
  :type 'boolean)

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
  "Browse Claude Code sessions and open the selected one."
  (interactive)
  (let* ((sessions (claude-log--read-sessions))
         (candidates (claude-log--build-candidates sessions))
         (selected (completing-read "Session: " candidates nil t))
         (file (alist-get selected candidates nil nil #'equal)))
    (claude-log-open-file file)))

;;;###autoload
(defun claude-log-open-file (file)
  "Open and render the Claude Code JSONL log at FILE."
  (interactive "fJSONL file: ")
  (let* ((file (expand-file-name file))
         (buf (generate-new-buffer (claude-log--buffer-name file))))
    (with-current-buffer buf
      (claude-log-mode)
      (setq claude-log--source-file file)
      (claude-log--render-full)
      (when (and claude-log-live-update (not claude-log--watcher))
        (claude-log--start-watcher)))
    (pop-to-buffer buf)))

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

(defun claude-log--build-candidates (sessions)
  "Build an alist of (display-string . file-path) from SESSIONS."
  (mapcar
   (lambda (session)
     (let* ((meta (cdr session))
            (ts (plist-get meta :timestamp))
            (date (claude-log--format-epoch-ms ts))
            (project (claude-log--short-project (plist-get meta :project)))
            (display (string-trim (plist-get meta :display)))
            (display (claude-log--truncate-string display 60))
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
  "Filter ENTRIES to only user and assistant messages."
  (seq-filter (lambda (entry)
                (member (plist-get entry :type) '("user" "assistant")))
              entries))

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
      (when claude-log-collapse-tools
        (claude-log-collapse-all-tools)))))

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
  "Render a user turn with CONTENT and TIME-STR."
  (let ((parts '()))
    (push (format "---\n\n## User — %s\n\n" time-str) parts)
    (if (stringp content)
        (push (format "%s\n\n" content) parts)
      (dolist (item content)
        (let ((item-type (plist-get item :type)))
          (cond
           ((equal item-type "text")
            (push (format "%s\n\n" (plist-get item :text)) parts))
           ((equal item-type "tool_result")
            (when claude-log-show-tool-output
              (push (claude-log--render-tool-result item) parts)))))))
    (apply #'concat (nreverse parts))))

(defun claude-log--render-assistant-turn (content time-str)
  "Render an assistant turn with CONTENT and TIME-STR."
  (let ((parts '()))
    (push (format "---\n\n## Assistant — %s\n\n" time-str) parts)
    (when (listp content)
      (dolist (item content)
        (let ((item-type (plist-get item :type)))
          (cond
           ((equal item-type "thinking")
            (when (not (eq claude-log-show-thinking 'hidden))
              (push (claude-log--render-thinking item) parts)))
           ((equal item-type "text")
            (let ((text (plist-get item :text)))
              (when (and text (not (string-empty-p text)))
                (push (format "%s\n\n" text) parts))))
           ((equal item-type "tool_use")
            (push (claude-log--render-tool-use item) parts))))))
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
  "Return a NAME-specific summary of tool INPUT."
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

(define-derived-mode claude-log-mode special-mode "Claude-Log"
  "Major mode for viewing Claude Code conversation logs.
\\{claude-log-mode-map}"
  (setq-local outline-regexp "##+ ")
  (setq-local outline-level #'claude-log--outline-level)
  (outline-minor-mode 1)
  (claude-log--maybe-enable-markdown-fontification)
  (add-hook 'kill-buffer-hook #'claude-log--cleanup nil t))

(defun claude-log--outline-level ()
  "Return the outline level based on the number of `#' characters."
  (- (match-end 0) (match-beginning 0) 1))

(defun claude-log--maybe-enable-markdown-fontification ()
  "Enable `markdown-mode' font-lock if available."
  (when (require 'markdown-mode nil t)
    (setq-local font-lock-defaults '(markdown-mode-font-lock-keywords))
    (font-lock-mode 1)))

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
  "Start watching the JSONL file for changes."
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
  (condition-case nil
      (let ((entry (claude-log--parse-json-line line)))
        (when (member (plist-get entry :type) '("user" "assistant"))
          (let ((rendered (claude-log--render-entry entry))
                (inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (insert rendered)
              (when claude-log-collapse-tools
                (claude-log--collapse-region
                 (- (point-max) (length rendered)) (point-max)))))))
    (error nil)))

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
  (claude-log--set-subheading-visibility t))

(defun claude-log-expand-all ()
  "Expand all sections."
  (interactive)
  (let ((inhibit-read-only t))
    (outline-show-all)))

(defun claude-log--set-subheading-visibility (hide)
  "HIDE or show all `####' level subheadings."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^####+ " nil t)
      (goto-char (match-beginning 0))
      (if hide
          (outline-hide-subtree)
        (outline-show-subtree))
      (forward-line 1))))

(defun claude-log--collapse-region (start end)
  "Collapse all `####' subheadings between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "^####+ " end t)
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
