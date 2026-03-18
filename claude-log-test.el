;;; claude-log-test.el --- Tests for claude-log  -*- lexical-binding: t; -*-

;; Tests for claude-log.el: JSONL parsing, rendering, index management,
;; tool summarization, UTF-8 handling, timestamps, and session metadata.

;;; Code:

(require 'ert)
(require 'claude-log)

;;;;; Test helpers

(defvar claude-log-test--dir nil
  "Temporary directory for the current test.")

(defmacro claude-log-test--with-temp-dir (&rest body)
  "Execute BODY with a temporary directory bound to `claude-log-test--dir'.
Cleans up afterwards."
  (declare (indent 0) (debug t))
  `(let ((claude-log-test--dir (make-temp-file "claude-log-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory claude-log-test--dir t))))

(defun claude-log-test--write-file (name content)
  "Write CONTENT to file NAME under `claude-log-test--dir'."
  (let ((path (expand-file-name name claude-log-test--dir)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert content))
    path))

(defun claude-log-test--make-entry (type role content &optional timestamp)
  "Build a conversation entry plist.
TYPE is \"user\" or \"assistant\", ROLE matches, CONTENT is
the message content (string or list)."
  (let ((entry (list :type type
                     :message (list :role role :content content))))
    (when timestamp
      (plist-put entry :timestamp timestamp))
    entry))

(defun claude-log-test--user-entry (content &optional timestamp)
  "Build a user conversation entry with CONTENT."
  (claude-log-test--make-entry "user" "user" content timestamp))

(defun claude-log-test--assistant-entry (content &optional timestamp)
  "Build an assistant conversation entry with CONTENT."
  (claude-log-test--make-entry "assistant" "assistant" content timestamp))

;;;;; JSONL parsing

(ert-deftest claude-log-test-parse-json-line/simple-object ()
  "Parses a simple JSON object into a plist."
  (let ((result (claude-log--parse-json-line "{\"type\":\"user\",\"id\":1}")))
    (should (equal (plist-get result :type) "user"))
    (should (equal (plist-get result :id) 1))))

(ert-deftest claude-log-test-parse-json-line/nested-object ()
  "Parses nested JSON into nested plists."
  (let ((result (claude-log--parse-json-line
                 "{\"message\":{\"role\":\"user\",\"content\":\"hello\"}}")))
    (should (equal (plist-get (plist-get result :message) :role) "user"))
    (should (equal (plist-get (plist-get result :message) :content) "hello"))))

(ert-deftest claude-log-test-parse-json-line/array ()
  "Parses JSON arrays as lists."
  (let ((result (claude-log--parse-json-line "{\"items\":[1,2,3]}")))
    (should (equal (plist-get result :items) '(1 2 3)))))

(ert-deftest claude-log-test-parse-json-line/invalid-json ()
  "Signals an error for invalid JSON."
  (should-error (claude-log--parse-json-line "not json")))

(ert-deftest claude-log-test-parse-json-line/empty-string ()
  "Signals an error for empty string."
  (should-error (claude-log--parse-json-line "")))

(ert-deftest claude-log-test-parse-json-line/unicode ()
  "Handles Unicode characters in JSON."
  (let ((result (claude-log--parse-json-line "{\"text\":\"café ☕ 日本語\"}")))
    (should (equal (plist-get result :text) "café ☕ 日本語"))))

(ert-deftest claude-log-test-read-file-lines/normal ()
  "Reads non-empty lines from a file."
  (claude-log-test--with-temp-dir
    (let ((path (claude-log-test--write-file "test.jsonl"
                                              "line1\nline2\nline3\n")))
      (should (equal (claude-log--read-file-lines path)
                     '("line1" "line2" "line3"))))))

(ert-deftest claude-log-test-read-file-lines/empty-lines-skipped ()
  "Empty lines between content are skipped."
  (claude-log-test--with-temp-dir
    (let ((path (claude-log-test--write-file "test.jsonl"
                                              "line1\n\nline2\n")))
      (should (equal (claude-log--read-file-lines path)
                     '("line1" "line2"))))))

(ert-deftest claude-log-test-parse-jsonl-file/valid-entries ()
  "Parses multiple valid JSONL entries."
  (claude-log-test--with-temp-dir
    (let* ((content (concat "{\"type\":\"user\"}\n"
                            "{\"type\":\"assistant\"}\n"))
           (path (claude-log-test--write-file "test.jsonl" content))
           (result (claude-log--parse-jsonl-file path)))
      (should (= (length result) 2))
      (should (equal (plist-get (car result) :type) "user"))
      (should (equal (plist-get (cadr result) :type) "assistant")))))

(ert-deftest claude-log-test-parse-jsonl-file/malformed-lines-skipped ()
  "Malformed JSON lines are silently skipped."
  (claude-log-test--with-temp-dir
    (let* ((content (concat "{\"type\":\"user\"}\n"
                            "NOT VALID JSON\n"
                            "{\"type\":\"assistant\"}\n"))
           (path (claude-log-test--write-file "test.jsonl" content))
           (result (claude-log--parse-jsonl-file path)))
      (should (= (length result) 2)))))

(ert-deftest claude-log-test-parse-jsonl-file/all-malformed ()
  "Returns empty list when all lines are malformed."
  (claude-log-test--with-temp-dir
    (let* ((path (claude-log-test--write-file "test.jsonl"
                                              "bad\nalso bad\n"))
           (result (claude-log--parse-jsonl-file path)))
      (should (null result)))))

(ert-deftest claude-log-test-try-parse-json/valid ()
  "Returns parsed result for valid JSON."
  (should (equal (plist-get (claude-log--try-parse-json "{\"a\":1}") :a) 1)))

(ert-deftest claude-log-test-try-parse-json/invalid ()
  "Returns nil for invalid JSON."
  (should (null (claude-log--try-parse-json "invalid"))))

;;;;; Entry classification

(ert-deftest claude-log-test-conversation-entry-p/user ()
  "Recognizes user entries."
  (let ((entry (list :type "user" :message (list :content "hello"))))
    (should (claude-log--conversation-entry-p entry))))

(ert-deftest claude-log-test-conversation-entry-p/assistant ()
  "Recognizes assistant entries."
  (let ((entry (list :type "assistant" :message (list :content "hi"))))
    (should (claude-log--conversation-entry-p entry))))

(ert-deftest claude-log-test-conversation-entry-p/progress-excluded ()
  "Excludes progress entries."
  (let ((entry (list :type "progress")))
    (should-not (claude-log--conversation-entry-p entry))))

(ert-deftest claude-log-test-conversation-entry-p/system-entry-excluded ()
  "Excludes system-generated user entries."
  (let ((entry (list :type "user"
                     :message (list :content "<command-name>/commit</command-name>"))))
    (should-not (claude-log--conversation-entry-p entry))))

(ert-deftest claude-log-test-system-entry-p/command-name ()
  "Detects <command-name> system tag."
  (let ((entry (list :message (list :content "<command-name>/commit</command-name>"))))
    (should (claude-log--system-entry-p entry))))

(ert-deftest claude-log-test-system-entry-p/task-notification ()
  "Detects <task-notification> system tag."
  (let ((entry (list :message (list :content "<task-notification>done</task-notification>"))))
    (should (claude-log--system-entry-p entry))))

(ert-deftest claude-log-test-system-entry-p/teammate-message ()
  "Detects <teammate-message> system tag."
  (let ((entry (list :message (list :content "<teammate-message from=\"agent\">hi</teammate-message>"))))
    (should (claude-log--system-entry-p entry))))

(ert-deftest claude-log-test-system-entry-p/local-command-stdout ()
  "Detects <local-command-stdout> system tag."
  (let ((entry (list :message (list :content "<local-command-stdout>output</local-command-stdout>"))))
    (should (claude-log--system-entry-p entry))))

(ert-deftest claude-log-test-system-entry-p/normal-user-message ()
  "Does not flag normal user messages as system."
  (let ((entry (list :message (list :content "Fix the bug in main.py"))))
    (should-not (claude-log--system-entry-p entry))))

(ert-deftest claude-log-test-system-entry-p/non-string-content ()
  "Does not flag entries with non-string content as system."
  (let ((entry (list :message (list :content '((:type "text" :text "hello"))))))
    (should-not (claude-log--system-entry-p entry))))

(ert-deftest claude-log-test-system-entry-p/leading-whitespace ()
  "Detects system tags with leading whitespace."
  (let ((entry (list :message (list :content "  <command-name>/foo</command-name>"))))
    (should (claude-log--system-entry-p entry))))

(ert-deftest claude-log-test-system-entry-p/tag-with-space-after ()
  "Detects system tags that have a space after the tag name (e.g. attributes)."
  (let ((entry (list :message (list :content "<command-message type=\"info\">hello</command-message>"))))
    (should (claude-log--system-entry-p entry))))

(ert-deftest claude-log-test-filter-conversation/mixed-entries ()
  "Filters to only user and assistant entries, excluding system entries."
  (let* ((entries (list (list :type "progress" :cwd "/tmp")
                        (list :type "user" :message (list :content "hello"))
                        (list :type "user" :message (list :content "<command-name>/foo</command-name>"))
                        (list :type "assistant" :message (list :content "hi"))
                        (list :type "result")))
         (filtered (claude-log--filter-conversation entries)))
    (should (= (length filtered) 2))
    (should (equal (plist-get (car filtered) :type) "user"))
    (should (equal (plist-get (cadr filtered) :type) "assistant"))))

;;;;; Entry helpers

(ert-deftest claude-log-test-find-first-message/user-first ()
  "Finds the first user message."
  (let* ((entries (list (list :type "progress" :cwd "/tmp")
                        (list :type "user" :message (list :role "user"))
                        (list :type "assistant" :message (list :role "assistant"))))
         (result (claude-log--find-first-message entries)))
    (should (equal (plist-get result :type) "user"))))

(ert-deftest claude-log-test-find-first-message/assistant-first ()
  "Finds assistant entry when no user entry precedes it."
  (let* ((entries (list (list :type "progress")
                        (list :type "assistant" :message (list :role "assistant"))))
         (result (claude-log--find-first-message entries)))
    (should (equal (plist-get result :type) "assistant"))))

(ert-deftest claude-log-test-find-first-message/empty ()
  "Returns nil for empty entries."
  (should (null (claude-log--find-first-message nil))))

(ert-deftest claude-log-test-find-progress-entry/found ()
  "Finds progress entry."
  (let* ((entries (list (list :type "user")
                        (list :type "progress" :cwd "/home/user")))
         (result (claude-log--find-progress-entry entries)))
    (should (equal (plist-get result :cwd) "/home/user"))))

(ert-deftest claude-log-test-find-progress-entry/not-found ()
  "Returns nil when no progress entry exists."
  (let ((entries (list (list :type "user"))))
    (should (null (claude-log--find-progress-entry entries)))))

(ert-deftest claude-log-test-first-user-text/string-content ()
  "Extracts text from string content."
  (let ((entries (list (list :type "user"
                             :message (list :content "Fix the bug")))))
    (should (equal (claude-log--first-user-text entries) "Fix the bug"))))

(ert-deftest claude-log-test-first-user-text/list-content ()
  "Extracts text from list content."
  (let ((entries (list (list :type "user"
                             :message (list :content
                                            (list (list :type "text"
                                                        :text "Hello world")))))))
    (should (equal (claude-log--first-user-text entries) "Hello world"))))

(ert-deftest claude-log-test-first-user-text/skips-system-entries ()
  "Skips system entries to find first genuine user text."
  (let ((entries (list (list :type "user"
                             :message (list :content "<command-name>/foo</command-name>"))
                       (list :type "user"
                             :message (list :content "Real question")))))
    (should (equal (claude-log--first-user-text entries) "Real question"))))

(ert-deftest claude-log-test-first-user-text/no-user-entries ()
  "Returns nil when no user entries exist."
  (let ((entries (list (list :type "assistant"
                             :message (list :content "response")))))
    (should (null (claude-log--first-user-text entries)))))

(ert-deftest claude-log-test-first-user-text/mixed-content-types ()
  "Finds text in content that mixes tool_result and text items."
  (let ((entries (list (list :type "user"
                             :message (list :content
                                            (list (list :type "tool_result"
                                                        :content "result")
                                                  (list :type "text"
                                                        :text "My question")))))))
    (should (equal (claude-log--first-user-text entries) "My question"))))

;;;;; Slugification

(ert-deftest claude-log-test-slugify/normal-text ()
  "Converts normal text to a slug."
  (should (equal (claude-log--slugify "Fix the bug in main.py")
                 "fix-the-bug-in-main-py")))

(ert-deftest claude-log-test-slugify/special-characters ()
  "Replaces special characters with hyphens."
  (should (equal (claude-log--slugify "Hello, World! @#$%")
                 "hello-world")))

(ert-deftest claude-log-test-slugify/consecutive-specials ()
  "Collapses consecutive special characters into single hyphen."
  (should (equal (claude-log--slugify "a---b___c")
                 "a-b-c")))

(ert-deftest claude-log-test-slugify/empty-string ()
  "Returns \"untitled\" for empty string."
  (should (equal (claude-log--slugify "") "untitled")))

(ert-deftest claude-log-test-slugify/nil-input ()
  "Returns \"untitled\" for nil."
  (should (equal (claude-log--slugify nil) "untitled")))

(ert-deftest claude-log-test-slugify/all-special-chars ()
  "Returns \"untitled\" when all chars are special."
  (should (equal (claude-log--slugify "!!!@@@###") "untitled")))

(ert-deftest claude-log-test-slugify/truncation ()
  "Truncates to `claude-log-slug-max-length'."
  (let ((claude-log-slug-max-length 10))
    (should (equal (claude-log--slugify "this is a very long text that should be truncated")
                   "this-is-a-"))))

(ert-deftest claude-log-test-slugify/unicode ()
  "Converts non-ASCII characters to hyphens."
  (should (equal (claude-log--slugify "café résumé")
                 "caf-r-sum")))

(ert-deftest claude-log-test-slugify/leading-trailing-specials ()
  "Strips leading and trailing hyphens."
  (should (equal (claude-log--slugify "  --hello--  ") "hello")))

(ert-deftest claude-log-test-slugify/numbers-preserved ()
  "Preserves numbers in slugs."
  (should (equal (claude-log--slugify "version 2.0.1") "version-2-0-1")))

;;;;; String utilities

(ert-deftest claude-log-test-truncate-string/short-string ()
  "Returns short strings unchanged."
  (should (equal (claude-log--truncate-string "hello" 10) "hello")))

(ert-deftest claude-log-test-truncate-string/exact-length ()
  "Returns string unchanged when exactly at max."
  (should (equal (claude-log--truncate-string "hello" 5) "hello")))

(ert-deftest claude-log-test-truncate-string/long-string ()
  "Truncates long strings with ellipsis."
  (should (equal (claude-log--truncate-string "hello world" 5) "hello…")))

(ert-deftest claude-log-test-truncate-string/empty-string ()
  "Returns empty string unchanged."
  (should (equal (claude-log--truncate-string "" 10) "")))

(ert-deftest claude-log-test-normalize-whitespace/tabs-and-newlines ()
  "Collapses tabs, newlines, and multiple spaces."
  (should (equal (claude-log--normalize-whitespace "hello\n\tworld  foo")
                 "hello world foo")))

(ert-deftest claude-log-test-normalize-whitespace/nil-input ()
  "Handles nil input."
  (should (equal (claude-log--normalize-whitespace nil) "")))

(ert-deftest claude-log-test-normalize-whitespace/leading-trailing ()
  "Trims leading and trailing whitespace."
  (should (equal (claude-log--normalize-whitespace "  hello  ") "hello")))

;;;;; Timestamps

(ert-deftest claude-log-test-format-epoch-ms/valid ()
  "Formats epoch milliseconds to date string."
  (let ((result (claude-log--format-epoch-ms 1700000000000)))
    ;; Should be a date-like string.  Exact value depends on timezone.
    (should (stringp result))
    (should (string-match-p "2023-11-1[45]" result))))

(ert-deftest claude-log-test-format-epoch-ms/non-number ()
  "Returns \"unknown\" for non-numeric input."
  (should (equal (claude-log--format-epoch-ms "not a number") "unknown"))
  (should (equal (claude-log--format-epoch-ms nil) "unknown")))

(ert-deftest claude-log-test-iso-to-epoch-ms/valid ()
  "Converts ISO timestamp to epoch milliseconds."
  (let ((result (claude-log--iso-to-epoch-ms "2023-11-14T12:00:00Z")))
    (should (numberp result))
    ;; Should be approximately 1700000000000 (within a day)
    (should (< (abs (- result 1699963200000)) (* 24 60 60 1000)))))

(ert-deftest claude-log-test-iso-to-epoch-ms/invalid ()
  "Returns a number even for invalid strings (date-to-time is lenient).
Only truly unparseable inputs that signal errors return nil."
  ;; date-to-time is lenient with arbitrary strings in modern Emacs,
  ;; so these return numbers rather than nil.
  (should (numberp (claude-log--iso-to-epoch-ms "not a date")))
  (should (numberp (claude-log--iso-to-epoch-ms ""))))

(ert-deftest claude-log-test-format-iso-timestamp/valid ()
  "Formats a valid ISO timestamp."
  (let ((result (claude-log--format-iso-timestamp "2023-11-14T12:00:00Z")))
    (should (stringp result))
    (should (string-match-p "2023-11-14" result))))

(ert-deftest claude-log-test-format-iso-timestamp/nil ()
  "Returns \"unknown\" for nil."
  (should (equal (claude-log--format-iso-timestamp nil) "unknown")))

(ert-deftest claude-log-test-format-iso-timestamp/empty ()
  "Returns \"unknown\" for empty string."
  (should (equal (claude-log--format-iso-timestamp "") "unknown")))

(ert-deftest claude-log-test-parse-and-format-iso/valid ()
  "Parses and formats a valid ISO timestamp."
  (let ((result (claude-log--parse-and-format-iso "2023-11-14T12:00:00Z")))
    (should (string-match-p "2023" result))))

(ert-deftest claude-log-test-parse-and-format-iso/invalid-returns-formatted ()
  "Returns a formatted date even for invalid strings (date-to-time is lenient)."
  ;; date-to-time does not error on arbitrary strings in modern Emacs,
  ;; so the result is a formatted date string, not the original input.
  (let ((result (claude-log--parse-and-format-iso "garbage")))
    (should (stringp result))))

;;;;; Short project

(ert-deftest claude-log-test-short-project/full-path ()
  "Extracts basename from full path."
  (should (equal (claude-log--short-project "/home/user/projects/my-app")
                 "my-app")))

(ert-deftest claude-log-test-short-project/trailing-slash ()
  "Handles path with trailing slash."
  (should (equal (claude-log--short-project "/home/user/projects/my-app/")
                 "my-app")))

(ert-deftest claude-log-test-short-project/nil ()
  "Returns \"unknown\" for nil."
  (should (equal (claude-log--short-project nil) "unknown")))

(ert-deftest claude-log-test-short-project/empty ()
  "Returns \"unknown\" for empty string."
  (should (equal (claude-log--short-project "") "unknown")))

(ert-deftest claude-log-test-short-project/simple-name ()
  "Returns simple name as-is."
  (should (equal (claude-log--short-project "my-project") "my-project")))

;;;;; Encode project path

(ert-deftest claude-log-test-encode-project-path/slashes-and-dots ()
  "Replaces slashes and dots with hyphens."
  (let ((result (claude-log--encode-project-path "/home/user/my.project")))
    (should (not (string-match-p "[/.]" result)))
    (should (string-match-p "-home-user-my-project" result))))

;;;;; Index management

(ert-deftest claude-log-test-read-index/missing-file ()
  "Returns empty hash table when index file doesn't exist."
  (claude-log-test--with-temp-dir
    (let ((claude-log-rendered-directory claude-log-test--dir))
      (let ((result (claude-log--read-index)))
        (should (hash-table-p result))
        (should (= (hash-table-count result) 0))))))

(ert-deftest claude-log-test-read-index/corrupt-file ()
  "Returns empty hash table on corrupt file."
  (claude-log-test--with-temp-dir
    (let ((claude-log-rendered-directory claude-log-test--dir))
      (claude-log-test--write-file "_index.el" "not a hash table")
      (let ((result (claude-log--read-index)))
        (should (hash-table-p result))
        (should (= (hash-table-count result) 0))))))

(ert-deftest claude-log-test-write-and-read-index/roundtrip ()
  "Write then read produces the same data."
  (claude-log-test--with-temp-dir
    (let ((claude-log-rendered-directory claude-log-test--dir)
          (index (make-hash-table :test #'equal)))
      (puthash "session-1" (list :file "/tmp/a.md" :jsonl-size 100) index)
      (puthash "session-2" (list :file "/tmp/b.md" :jsonl-size 200) index)
      (claude-log--write-index index)
      (let ((loaded (claude-log--read-index)))
        (should (hash-table-p loaded))
        (should (= (hash-table-count loaded) 2))
        (should (equal (plist-get (gethash "session-1" loaded) :file) "/tmp/a.md"))
        (should (equal (plist-get (gethash "session-2" loaded) :jsonl-size) 200))))))

(ert-deftest claude-log-test-index-merge/new-entry ()
  "Merges properties into a new index entry."
  (let ((index (make-hash-table :test #'equal)))
    (claude-log--index-merge index "s1" (list :file "/a.md" :jsonl-size 100))
    (let ((entry (gethash "s1" index)))
      (should (equal (plist-get entry :file) "/a.md"))
      (should (equal (plist-get entry :jsonl-size) 100)))))

(ert-deftest claude-log-test-index-merge/update-preserves-existing ()
  "Merging preserves properties not in the new props."
  (let ((index (make-hash-table :test #'equal)))
    (puthash "s1" (list :file "/a.md" :jsonl-size 100 :summary "test") index)
    (claude-log--index-merge index "s1" (list :jsonl-size 200))
    (let ((entry (gethash "s1" index)))
      (should (equal (plist-get entry :file) "/a.md"))
      (should (equal (plist-get entry :jsonl-size) 200))
      (should (equal (plist-get entry :summary) "test")))))

(ert-deftest claude-log-test-index-merge/overwrite-value ()
  "Merging overwrites existing values for matching keys."
  (let ((index (make-hash-table :test #'equal)))
    (puthash "s1" (list :file "/old.md") index)
    (claude-log--index-merge index "s1" (list :file "/new.md"))
    (should (equal (plist-get (gethash "s1" index) :file) "/new.md"))))

;;;;; Tool result extraction

(ert-deftest claude-log-test-extract-tool-result-text/string ()
  "Extracts text from string content."
  (should (equal (claude-log--extract-tool-result-text "hello") "hello")))

(ert-deftest claude-log-test-extract-tool-result-text/list ()
  "Extracts text from list content."
  (let ((content (list (list :type "text" :text "part 1")
                       (list :type "text" :text "part 2"))))
    (should (equal (claude-log--extract-tool-result-text content)
                   "part 1\npart 2"))))

(ert-deftest claude-log-test-extract-tool-result-text/non-text-items ()
  "Handles non-text items in list content."
  (let ((content (list (list :type "image" :data "...")
                       (list :type "text" :text "hello"))))
    (should (equal (claude-log--extract-tool-result-text content) "\nhello"))))

(ert-deftest claude-log-test-extract-tool-result-text/nil ()
  "Returns empty string for nil content."
  (should (equal (claude-log--extract-tool-result-text nil) "")))

(ert-deftest claude-log-test-extract-tool-result-text/unexpected-type ()
  "Returns empty string for unexpected types."
  (should (equal (claude-log--extract-tool-result-text 42) "")))

;;;;; Tool input summarization

(ert-deftest claude-log-test-summarize-read ()
  "Summarizes Read tool input."
  (let ((result (claude-log--summarize-tool-input
                 "Read" (list :file_path "/tmp/test.el"))))
    (should (string-match-p "/tmp/test.el" result))))

(ert-deftest claude-log-test-summarize-write ()
  "Summarizes Write tool input."
  (let ((result (claude-log--summarize-tool-input
                 "Write" (list :file_path "/tmp/out.md"))))
    (should (string-match-p "/tmp/out.md" result))))

(ert-deftest claude-log-test-summarize-edit ()
  "Summarizes Edit tool input with file and old_string."
  (let ((result (claude-log--summarize-tool-input
                 "Edit" (list :file_path "/tmp/test.el"
                              :old_string "defun old-func"))))
    (should (string-match-p "/tmp/test.el" result))
    (should (string-match-p "defun old-func" result))))

(ert-deftest claude-log-test-summarize-edit/missing-fields ()
  "Handles Edit tool with missing fields."
  (let ((result (claude-log--summarize-tool-input "Edit" (list))))
    (should (string-match-p "\\?" result))))

(ert-deftest claude-log-test-summarize-bash ()
  "Summarizes Bash tool input."
  (let ((result (claude-log--summarize-tool-input
                 "Bash" (list :command "git status"))))
    (should (string-match-p "git status" result))))

(ert-deftest claude-log-test-summarize-grep/with-path ()
  "Summarizes Grep with pattern and path."
  (let ((result (claude-log--summarize-tool-input
                 "Grep" (list :pattern "defun" :path "/src"))))
    (should (string-match-p "defun" result))
    (should (string-match-p "/src" result))))

(ert-deftest claude-log-test-summarize-grep/without-path ()
  "Summarizes Grep with pattern only."
  (let ((result (claude-log--summarize-tool-input
                 "Grep" (list :pattern "TODO"))))
    (should (string-match-p "TODO" result))
    (should-not (string-match-p " in " result))))

(ert-deftest claude-log-test-summarize-glob ()
  "Summarizes Glob tool input."
  (let ((result (claude-log--summarize-tool-input
                 "Glob" (list :pattern "**/*.el"))))
    (should (string-match-p "\\*\\*/\\*\\.el" result))))

(ert-deftest claude-log-test-summarize-web-fetch ()
  "Summarizes WebFetch tool input."
  (let ((result (claude-log--summarize-tool-input
                 "WebFetch" (list :url "https://example.com"))))
    (should (string-match-p "example.com" result))))

(ert-deftest claude-log-test-summarize-web-search ()
  "Summarizes WebSearch tool input."
  (let ((result (claude-log--summarize-tool-input
                 "WebSearch" (list :query "emacs lisp testing"))))
    (should (string-match-p "emacs lisp testing" result))))

(ert-deftest claude-log-test-summarize-task/with-type ()
  "Summarizes Task tool with subagent_type."
  (let ((result (claude-log--summarize-tool-input
                 "Task" (list :subagent_type "Explore"
                              :description "Find all tests"))))
    (should (string-match-p "Explore" result))
    (should (string-match-p "Find all tests" result))))

(ert-deftest claude-log-test-summarize-task/without-type ()
  "Summarizes Task tool without subagent_type."
  (let ((result (claude-log--summarize-tool-input
                 "Task" (list :description "Find all tests"))))
    (should (string-match-p "Find all tests" result))))

(ert-deftest claude-log-test-summarize-unknown-tool/falls-back-to-generic ()
  "Falls back to generic summary for unknown tools."
  (let ((result (claude-log--summarize-tool-input
                 "CustomTool" (list :foo "bar" :baz "qux"))))
    (should (string-match-p "foo" result))
    (should (string-match-p "bar" result))))

(ert-deftest claude-log-test-summarize-generic/valid-plist ()
  "Generic summary formats plist keys and values."
  (let ((result (claude-log--summarize-tool-input-generic
                 (list :name "test" :value "42"))))
    (should (string-match-p "name" result))
    (should (string-match-p "test" result))
    (should (string-match-p "value" result))
    (should (string-match-p "42" result))))

(ert-deftest claude-log-test-summarize-generic/invalid-plist ()
  "Returns empty string for non-plist input."
  (should (equal (claude-log--summarize-tool-input-generic '(a b c)) ""))
  (should (equal (claude-log--summarize-tool-input-generic "string") ""))
  (should (equal (claude-log--summarize-tool-input-generic nil) "")))

;;;;; Rendering

(ert-deftest claude-log-test-render-entry/user-string-content ()
  "Renders user entry with string content."
  (let ((entry (list :type "user"
                     :timestamp "2023-11-14T12:00:00Z"
                     :message (list :role "user" :content "Hello"))))
    (let ((result (claude-log--render-entry entry)))
      (should (string-match-p "## User" result))
      (should (string-match-p "Hello" result)))))

(ert-deftest claude-log-test-render-entry/assistant ()
  "Renders assistant entry."
  (let ((entry (list :type "assistant"
                     :timestamp "2023-11-14T12:00:00Z"
                     :message (list :role "assistant"
                                    :content (list (list :type "text"
                                                         :text "Response"))))))
    (let ((result (claude-log--render-entry entry)))
      (should (string-match-p "## Assistant" result))
      (should (string-match-p "Response" result)))))

(ert-deftest claude-log-test-render-entry/no-message ()
  "Returns empty string for entry without message."
  (let ((entry (list :type "user")))
    (should (equal (claude-log--render-entry entry) ""))))

(ert-deftest claude-log-test-render-entry/unknown-role ()
  "Returns empty string for unknown role."
  (let ((entry (list :type "system"
                     :message (list :role "system" :content "foo"))))
    (should (equal (claude-log--render-entry entry) ""))))

(ert-deftest claude-log-test-render-user-turn/string ()
  "Renders string user content."
  (let ((result (claude-log--render-user-turn "Hello" "2023-11-14")))
    (should (string-match-p "## User — 2023-11-14" result))
    (should (string-match-p "Hello" result))
    (should (string-match-p "^---" result))))

(ert-deftest claude-log-test-render-user-turn/list-with-text ()
  "Renders list content with text."
  (let ((content (list (list :type "text" :text "My question"))))
    (let ((result (claude-log--render-user-turn content "ts")))
      (should (string-match-p "## User" result))
      (should (string-match-p "My question" result)))))

(ert-deftest claude-log-test-render-user-turn/only-tool-results ()
  "Renders tool results without User heading."
  (let ((content (list (list :type "tool_result"
                             :content "result text"))))
    (let ((result (claude-log--render-user-turn content "ts")))
      (should-not (string-match-p "## User" result))
      (should (string-match-p "Tool result" result)))))

(ert-deftest claude-log-test-render-user-turn/empty-content ()
  "Returns empty string for empty list content."
  (should (equal (claude-log--render-user-turn (list) "ts") "")))

(ert-deftest claude-log-test-render-user-turn/whitespace-only-text ()
  "Returns empty for whitespace-only text items."
  (let ((content (list (list :type "text" :text "   "))))
    (should (equal (claude-log--render-user-turn content "ts") ""))))

(ert-deftest claude-log-test-render-assistant-turn/text ()
  "Renders assistant text content."
  (let ((content (list (list :type "text" :text "Here's the fix."))))
    (let ((result (claude-log--render-assistant-turn content "ts")))
      (should (string-match-p "## Assistant — ts" result))
      (should (string-match-p "Here's the fix." result)))))

(ert-deftest claude-log-test-render-assistant-turn/empty-body ()
  "Returns empty string for assistant turn with no visible content."
  (let ((content (list)))
    (should (equal (claude-log--render-assistant-turn content "ts") ""))))

(ert-deftest claude-log-test-render-assistant-body/mixed-content ()
  "Renders mixed thinking, text, and tool_use items."
  (let ((content (list (list :type "thinking" :thinking "Let me think...")
                       (list :type "text" :text "Here's my answer.")
                       (list :type "tool_use" :name "Bash"
                             :input (list :command "ls")))))
    (let ((result (claude-log--render-assistant-body content)))
      (should (string-match-p "#### Thinking" result))
      (should (string-match-p "Here's my answer." result))
      (should (string-match-p "#### Tool: Bash" result)))))

(ert-deftest claude-log-test-render-assistant-body/skips-empty-text ()
  "Skips empty text items."
  (let ((content (list (list :type "text" :text "")
                       (list :type "text" :text "  ")
                       (list :type "text" :text "Actual content"))))
    (let ((result (claude-log--render-assistant-body content)))
      (should (string-match-p "Actual content" result))
      ;; Should only have one text block, not three
      (should (= (length (split-string result "Actual content")) 2)))))

(ert-deftest claude-log-test-render-thinking ()
  "Renders thinking block with heading."
  (let ((item (list :type "thinking" :thinking "Analyzing the code...")))
    (let ((result (claude-log--render-thinking item)))
      (should (string-match-p "^#### Thinking" result))
      (should (string-match-p "Analyzing the code..." result)))))

(ert-deftest claude-log-test-render-thinking/truncation ()
  "Truncates long thinking blocks."
  (let* ((claude-log-max-tool-result-length 20)
         (item (list :type "thinking"
                     :thinking "This is a very long thinking block that should be truncated"))
         (result (claude-log--render-thinking item)))
    (should (string-match-p "…" result))))

(ert-deftest claude-log-test-render-thinking/collapses-newlines ()
  "Collapses multiple consecutive newlines."
  (let ((item (list :type "thinking" :thinking "line1\n\n\n\nline2")))
    (let ((result (claude-log--render-thinking item)))
      (should-not (string-match-p "\n\n\n" result)))))

(ert-deftest claude-log-test-render-tool-use ()
  "Renders tool_use item with summary."
  (let ((item (list :type "tool_use" :name "Read"
                    :input (list :file_path "/tmp/test.el"))))
    (let ((result (claude-log--render-tool-use item)))
      (should (string-match-p "^#### Tool: Read" result))
      (should (string-match-p "/tmp/test.el" result)))))

(ert-deftest claude-log-test-render-tool-result ()
  "Renders tool_result as blockquote."
  (let ((item (list :type "tool_result" :content "File contents here")))
    (let ((result (claude-log--render-tool-result item)))
      (should (string-match-p "^#### Tool result" result))
      (should (string-match-p "> File contents here" result)))))

(ert-deftest claude-log-test-render-tool-result/multiline ()
  "Renders multiline tool result with blockquote continuation."
  (let ((item (list :type "tool_result" :content "line1\nline2\nline3")))
    (let ((result (claude-log--render-tool-result item)))
      (should (string-match-p "> line1" result))
      (should (string-match-p "> line2" result))
      (should (string-match-p "> line3" result)))))

(ert-deftest claude-log-test-collect-user-text/filters-text ()
  "Collects only text items from content."
  (let ((content (list (list :type "text" :text "question")
                       (list :type "tool_result" :content "result")
                       (list :type "text" :text "followup"))))
    (let ((result (claude-log--collect-user-text content)))
      (should (= (length result) 2))
      (should (string-match-p "question" (car result)))
      (should (string-match-p "followup" (cadr result))))))

(ert-deftest claude-log-test-collect-user-text/empty-text-excluded ()
  "Excludes empty and whitespace-only text items."
  (let ((content (list (list :type "text" :text "")
                       (list :type "text" :text "  \n  ")
                       (list :type "text" :text "real text"))))
    (let ((result (claude-log--collect-user-text content)))
      (should (= (length result) 1))
      (should (string-match-p "real text" (car result))))))

(ert-deftest claude-log-test-collect-tool-results ()
  "Collects only tool_result items."
  (let ((content (list (list :type "text" :text "text")
                       (list :type "tool_result" :content "result 1")
                       (list :type "tool_result" :content "result 2"))))
    (should (= (length (claude-log--collect-tool-results content)) 2))))

;;;;; UTF-8 boundary detection

(ert-deftest claude-log-test-incomplete-utf8/complete-ascii ()
  "ASCII string has no incomplete tail."
  (should (= (claude-log--incomplete-utf8-tail-length
              (encode-coding-string "hello" 'raw-text))
             0)))

(ert-deftest claude-log-test-incomplete-utf8/complete-multibyte ()
  "Complete multibyte string has no incomplete tail."
  (should (= (claude-log--incomplete-utf8-tail-length
              (encode-coding-string "café" 'utf-8))
             0)))

(ert-deftest claude-log-test-incomplete-utf8/truncated-2byte ()
  "Detects incomplete 2-byte sequence (lead byte only)."
  (let* ((full (encode-coding-string "é" 'utf-8))
         (partial (substring full 0 1)))
    (should (= (claude-log--incomplete-utf8-tail-length partial) 1))))

(ert-deftest claude-log-test-incomplete-utf8/truncated-3byte ()
  "Detects incomplete 3-byte sequence."
  (let* ((full (encode-coding-string "€" 'utf-8))  ; 3 bytes
         (partial (substring full 0 2)))
    (should (= (claude-log--incomplete-utf8-tail-length partial) 2))))

(ert-deftest claude-log-test-incomplete-utf8/truncated-4byte ()
  "Detects incomplete 4-byte sequence."
  (let* ((full (encode-coding-string "𐍈" 'utf-8))  ; 4-byte Gothic letter
         (partial (substring full 0 2)))
    (should (= (claude-log--incomplete-utf8-tail-length partial) 2))))

(ert-deftest claude-log-test-incomplete-utf8/empty-string ()
  "Empty string has no incomplete tail."
  (should (= (claude-log--incomplete-utf8-tail-length "") 0)))

(ert-deftest claude-log-test-incomplete-utf8/ascii-then-truncated ()
  "ASCII followed by truncated multibyte."
  (let* ((ascii (encode-coding-string "hello" 'raw-text))
         (full-utf8 (encode-coding-string "é" 'utf-8))
         (combined (concat ascii (substring full-utf8 0 1))))
    (should (= (claude-log--incomplete-utf8-tail-length combined) 1))))

;;;;; Front matter

(ert-deftest claude-log-test-render-front-matter ()
  "Renders front matter with session metadata."
  (let ((result (claude-log--render-front-matter "abc-123" "/tmp/test.jsonl" 5000)))
    (should (string-match-p "<!-- session: abc-123 -->" result))
    (should (string-match-p "<!-- source: /tmp/test.jsonl -->" result))
    (should (string-match-p "<!-- jsonl-size: 5000 -->" result))
    (should (string-match-p "<!-- rendered: " result))))

(ert-deftest claude-log-test-render-front-matter/nil-size ()
  "Handles nil size with fallback to 0."
  (let ((result (claude-log--render-front-matter "id" "/tmp/x.jsonl" nil)))
    (should (string-match-p "<!-- jsonl-size: 0 -->" result))))

;;;;; Session metadata extraction

(ert-deftest claude-log-test-extract-session-metadata-from-entries ()
  "Extracts project and date from entries."
  (let ((entries (list (list :type "progress" :cwd "/home/user/project")
                       (list :type "user"
                             :timestamp "2023-11-14T12:00:00Z"
                             :message (list :role "user" :content "hello")))))
    (let ((result (claude-log--extract-session-metadata-from-entries entries)))
      (should (equal (plist-get result :project) "project"))
      (should (string-match-p "2023-11-14" (plist-get result :date))))))

(ert-deftest claude-log-test-extract-session-metadata-from-entries/no-progress ()
  "Returns \"unknown\" project when no progress entry."
  (let ((entries (list (list :type "user"
                             :timestamp "2023-11-14T12:00:00Z"
                             :message (list :role "user" :content "hi")))))
    (let ((result (claude-log--extract-session-metadata-from-entries entries)))
      (should (equal (plist-get result :project) "unknown")))))

;;;;; Rendered filepath

(ert-deftest claude-log-test-rendered-filepath/generates-path ()
  "Generates a valid rendered filepath."
  (let* ((claude-log-rendered-directory "/tmp/rendered")
         (metadata (list :timestamp 1700000000000
                         :project "/home/user/my-app"
                         :display "Fix the bug"))
         (result (claude-log--rendered-filepath "session-1" metadata)))
    (should (string-match-p "/tmp/rendered/my-app/" result))
    (should (string-match-p "fix-the-bug\\.md$" result))
    (should (string-match-p "2023-11-1[45]_" result))))

(ert-deftest claude-log-test-rendered-filepath/missing-timestamp ()
  "Handles missing (non-numeric) timestamp."
  (let* ((claude-log-rendered-directory "/tmp/rendered")
         (metadata (list :timestamp nil
                         :project "/project"
                         :display "test")))
    (let ((result (claude-log--rendered-filepath "s1" metadata)))
      (should (string-match-p "unknown_test\\.md" result)))))

(ert-deftest claude-log-test-rendered-filepath/empty-display ()
  "Handles empty display text."
  (let* ((claude-log-rendered-directory "/tmp/rendered")
         (metadata (list :timestamp 1700000000000
                         :project "/project"
                         :display "")))
    (let ((result (claude-log--rendered-filepath "s1" metadata)))
      (should (string-match-p "untitled\\.md" result)))))

;;;;; Render to file (integration)

(ert-deftest claude-log-test-render-to-file/basic ()
  "Renders a JSONL file to Markdown."
  (claude-log-test--with-temp-dir
    (let* ((jsonl-content
            (concat
             "{\"type\":\"progress\",\"cwd\":\"/home/user/project\"}\n"
             "{\"type\":\"user\",\"timestamp\":\"2023-11-14T12:00:00Z\","
             "\"message\":{\"role\":\"user\",\"content\":\"Fix the bug\"}}\n"
             "{\"type\":\"assistant\",\"timestamp\":\"2023-11-14T12:00:01Z\","
             "\"message\":{\"role\":\"assistant\",\"content\":"
             "[{\"type\":\"text\",\"text\":\"I'll fix it.\"}]}}\n"))
           (jsonl-path (claude-log-test--write-file "test.jsonl" jsonl-content))
           (output-path (expand-file-name "output.md" claude-log-test--dir))
           (metadata (list :file jsonl-path
                           :timestamp 1700000000000
                           :project "/home/user/project"
                           :display "Fix the bug"))
           (result (claude-log--render-to-file "s1" metadata output-path)))
      ;; Returns (rendered-path . jsonl-size)
      (should (equal (car result) output-path))
      (should (numberp (cdr result)))
      ;; File should exist and contain expected content
      (should (file-exists-p output-path))
      (let ((content (with-temp-buffer
                       (insert-file-contents output-path)
                       (buffer-string))))
        (should (string-match-p "<!-- session: s1 -->" content))
        (should (string-match-p "# Session:" content))
        (should (string-match-p "## User" content))
        (should (string-match-p "Fix the bug" content))
        (should (string-match-p "## Assistant" content))
        (should (string-match-p "I'll fix it." content))))))

(ert-deftest claude-log-test-render-to-file/malformed-lines-handled ()
  "Renders correctly even with some malformed JSONL lines."
  (claude-log-test--with-temp-dir
    (let* ((jsonl-content
            (concat
             "CORRUPTED LINE\n"
             "{\"type\":\"user\",\"timestamp\":\"2023-11-14T12:00:00Z\","
             "\"message\":{\"role\":\"user\",\"content\":\"hello\"}}\n"
             "ANOTHER BAD LINE\n"))
           (jsonl-path (claude-log-test--write-file "test.jsonl" jsonl-content))
           (output-path (expand-file-name "output.md" claude-log-test--dir))
           (metadata (list :file jsonl-path :timestamp nil :project "" :display ""))
           (result (claude-log--render-to-file "s1" metadata output-path)))
      (should (file-exists-p output-path))
      (let ((content (with-temp-buffer
                       (insert-file-contents output-path)
                       (buffer-string))))
        (should (string-match-p "## User" content))
        (should (string-match-p "hello" content))))))

;;;;; Summary parsing

(ert-deftest claude-log-test-parse-summary-response/valid-json ()
  "Parses valid JSON summary response."
  (let ((result (claude-log--parse-summary-response
                 "{\"summary\":\"A detailed summary.\",\"oneline\":\"Short\"}")))
    (should result)
    (should (equal (car result) "A detailed summary."))
    (should (equal (cdr result) "Short"))))

(ert-deftest claude-log-test-parse-summary-response/with-code-fences ()
  "Parses JSON with markdown code fences."
  (let ((result (claude-log--parse-summary-response
                 "```json\n{\"summary\":\"test\",\"oneline\":\"short\"}\n```")))
    (should result)
    (should (equal (car result) "test"))))

(ert-deftest claude-log-test-parse-summary-response/invalid-json ()
  "Returns nil for invalid JSON."
  (should (null (claude-log--parse-summary-response "not json"))))

(ert-deftest claude-log-test-parse-summary-response/missing-fields ()
  "Returns nil when required fields are missing."
  (should (null (claude-log--parse-summary-response "{\"summary\":\"test\"}")))
  (should (null (claude-log--parse-summary-response "{\"oneline\":\"test\"}"))))

(ert-deftest claude-log-test-parse-summary-response/non-string-fields ()
  "Returns nil when fields are not strings."
  (should (null (claude-log--parse-summary-response
                 "{\"summary\":123,\"oneline\":\"test\"}"))))

(ert-deftest claude-log-test-parse-summary-response/whitespace-trimmed ()
  "Trims whitespace from response."
  (let ((result (claude-log--parse-summary-response
                 "  \n{\"summary\":\"test\",\"oneline\":\"short\"}\n  ")))
    (should result)
    (should (equal (car result) "test"))))

;;;;; Message text extraction

(ert-deftest claude-log-test-extract-message-text/string ()
  "Extracts text from string content."
  (should (equal (claude-log--extract-message-text "hello") "hello")))

(ert-deftest claude-log-test-extract-message-text/list ()
  "Extracts text from list content, ignoring non-text items."
  (let ((content (list (list :type "thinking" :thinking "...")
                       (list :type "text" :text "answer")
                       (list :type "tool_use" :name "Bash"))))
    (should (equal (claude-log--extract-message-text content) "answer"))))

(ert-deftest claude-log-test-extract-message-text/multiple-texts ()
  "Joins multiple text items."
  (let ((content (list (list :type "text" :text "part 1")
                       (list :type "text" :text "part 2"))))
    (should (equal (claude-log--extract-message-text content) "part 1\npart 2"))))

(ert-deftest claude-log-test-extract-message-text/empty ()
  "Returns empty string for nil or non-list/string."
  (should (equal (claude-log--extract-message-text nil) ""))
  (should (equal (claude-log--extract-message-text 42) "")))

(ert-deftest claude-log-test-extract-message-text/skips-empty-text ()
  "Skips empty and whitespace-only text items."
  (let ((content (list (list :type "text" :text "")
                       (list :type "text" :text "   ")
                       (list :type "text" :text "real"))))
    (should (equal (claude-log--extract-message-text content) "real"))))

;;;;; Conversation text extraction

(ert-deftest claude-log-test-extract-conversation-text/basic ()
  "Extracts conversation text from entries."
  (let ((entries (list (list :type "user"
                             :message (list :role "user" :content "question"))
                       (list :type "assistant"
                             :message (list :role "assistant"
                                            :content (list (list :type "text"
                                                                  :text "answer")))))))
    (let ((result (claude-log--extract-conversation-text entries)))
      (should (string-match-p "User: question" result))
      (should (string-match-p "Assistant: answer" result)))))

(ert-deftest claude-log-test-extract-conversation-text/truncation ()
  "Truncates to max content length."
  (let ((claude-log-summary-max-content-length 30)
        (entries (list (list :type "user"
                             :message (list :role "user"
                                            :content "This is a long message that should be truncated"))
                       (list :type "assistant"
                             :message (list :role "assistant"
                                            :content (list (list :type "text"
                                                                  :text "Another long message")))))))
    (let ((result (claude-log--extract-conversation-text entries)))
      (should (<= (length result) 30)))))

(ert-deftest claude-log-test-extract-conversation-text/filters-non-conversation ()
  "Excludes non-conversation entries."
  (let ((entries (list (list :type "progress" :cwd "/tmp")
                       (list :type "user"
                             :message (list :role "user" :content "hello")))))
    (let ((result (claude-log--extract-conversation-text entries)))
      (should (string-match-p "User: hello" result))
      (should-not (string-match-p "progress" result)))))

;;;;; Build summary prompt

(ert-deftest claude-log-test-build-summary-prompt ()
  "Builds a summary prompt wrapping conversation text."
  (let ((result (claude-log--build-summary-prompt "User: hi\nAssistant: hello")))
    (should (string-match-p "Summarize" result))
    (should (string-match-p "User: hi" result))))

;;;;; Group by project

(ert-deftest claude-log-test-group-by-project/basic ()
  "Groups sessions by project."
  (let ((sessions (list (list "s1" :project "/home/user/project-a" :timestamp 3000)
                        (list "s2" :project "/home/user/project-b" :timestamp 2000)
                        (list "s3" :project "/home/user/project-a" :timestamp 1000))))
    (let ((result (claude-log--group-by-project sessions)))
      ;; Should have two groups
      (should (= (length result) 2))
      ;; project-a should be first (most recent timestamp = 3000)
      (should (equal (car (car result)) "project-a"))
      ;; project-a should have 2 sessions
      (should (= (length (cdr (car result))) 2)))))

(ert-deftest claude-log-test-group-by-project/empty ()
  "Returns empty list for no sessions."
  (should (null (claude-log--group-by-project nil))))

;;;;; Sessions needing summary

(ert-deftest claude-log-test-sessions-needing-summary/all-need ()
  "Returns all sessions when none have summaries."
  (let ((sessions (list (list "s1" :file "/a.jsonl")
                        (list "s2" :file "/b.jsonl")))
        (index (make-hash-table :test #'equal)))
    (should (= (length (claude-log--sessions-needing-summary sessions index)) 2))))

(ert-deftest claude-log-test-sessions-needing-summary/some-summarized ()
  "Excludes sessions with existing summaries."
  (let ((sessions (list (list "s1" :file "/a.jsonl")
                        (list "s2" :file "/b.jsonl")))
        (index (make-hash-table :test #'equal)))
    (puthash "s1" (list :summary-oneline "A test") index)
    (should (= (length (claude-log--sessions-needing-summary sessions index)) 1))
    (should (equal (caar (claude-log--sessions-needing-summary sessions index)) "s2"))))

(ert-deftest claude-log-test-sessions-needing-summary/all-summarized ()
  "Returns empty when all sessions have summaries."
  (let ((sessions (list (list "s1") (list "s2")))
        (index (make-hash-table :test #'equal)))
    (puthash "s1" (list :summary-oneline "A") index)
    (puthash "s2" (list :summary-oneline "B") index)
    (should (null (claude-log--sessions-needing-summary sessions index)))))

;;;;; Outline level

(ert-deftest claude-log-test-outline-level ()
  "Returns correct outline level based on # count."
  (with-temp-buffer
    (insert "## Heading\n")
    (goto-char (point-min))
    (looking-at "##+ ")
    (should (= (claude-log--outline-level) 2)))
  (with-temp-buffer
    (insert "#### Sub-heading\n")
    (goto-char (point-min))
    (looking-at "##+ ")
    (should (= (claude-log--outline-level) 4))))

;;;;; Extract session ID from buffer

(ert-deftest claude-log-test-extract-session-id-from-buffer ()
  "Extracts session ID from front matter."
  (with-temp-buffer
    (insert "<!-- session: abc-def-123 -->\n<!-- source: /tmp/test.jsonl -->\n")
    (should (equal (claude-log--extract-session-id-from-buffer) "abc-def-123"))))

(ert-deftest claude-log-test-extract-session-id-from-buffer/not-found ()
  "Returns nil when no session ID in buffer."
  (with-temp-buffer
    (insert "No front matter here\n")
    (should (null (claude-log--extract-session-id-from-buffer)))))

;;;;; Ensure rendered (integration)

(ert-deftest claude-log-test-ensure-rendered/first-time ()
  "Creates rendered file on first call."
  (claude-log-test--with-temp-dir
    (let* ((claude-log-rendered-directory
            (expand-file-name "rendered" claude-log-test--dir))
           (jsonl-content
            (concat
             "{\"type\":\"user\",\"timestamp\":\"2023-11-14T12:00:00Z\","
             "\"message\":{\"role\":\"user\",\"content\":\"hi\"}}\n"))
           (jsonl-path (claude-log-test--write-file "test.jsonl" jsonl-content))
           (metadata (list :file jsonl-path :timestamp 1700000000000
                           :project "/project" :display "hi"))
           (result (claude-log--ensure-rendered "s1" metadata)))
      (should (file-exists-p result))
      (let ((content (with-temp-buffer
                       (insert-file-contents result)
                       (buffer-string))))
        (should (string-match-p "## User" content))))))

(ert-deftest claude-log-test-ensure-rendered/cached ()
  "Returns cached path when file is up-to-date."
  (claude-log-test--with-temp-dir
    (let* ((claude-log-rendered-directory
            (expand-file-name "rendered" claude-log-test--dir))
           (jsonl-content "{\"type\":\"user\",\"timestamp\":\"2023-11-14T12:00:00Z\",\"message\":{\"role\":\"user\",\"content\":\"hi\"}}\n")
           (jsonl-path (claude-log-test--write-file "test.jsonl" jsonl-content))
           (metadata (list :file jsonl-path :timestamp 1700000000000
                           :project "/project" :display "hi")))
      ;; First call — renders
      (let ((path1 (claude-log--ensure-rendered "s1" metadata)))
        ;; Modify the rendered file content so we can detect if it gets re-rendered
        (with-temp-file path1
          (insert "MARKER: original render"))
        ;; Second call — should use cache (same jsonl size)
        (let ((path2 (claude-log--ensure-rendered "s1" metadata)))
          (should (equal path1 path2))
          ;; Content should still be our marker (not re-rendered)
          (let ((content (with-temp-buffer
                           (insert-file-contents path2)
                           (buffer-string))))
            (should (string-match-p "MARKER: original render" content))))))))

;;;;; Incremental text processing

(ert-deftest claude-log-test-process-incremental-text/complete-lines ()
  "Processes complete JSONL lines."
  (with-temp-buffer
    (claude-log-mode)
    (let ((claude-log--partial-line "")
          (claude-log--source-file nil)
          (claude-log--rendered-file nil)
          (inhibit-read-only t))
      (insert "# Session: test — unknown\n\n")
      ;; Process a complete user entry line
      (claude-log--process-incremental-text
       (concat "{\"type\":\"user\",\"timestamp\":\"2023-11-14T12:00:00Z\","
               "\"message\":{\"role\":\"user\",\"content\":\"hello\"}}\n")
       nil)
      (should (string-match-p "## User" (buffer-string)))
      (should (string-match-p "hello" (buffer-string))))))

(ert-deftest claude-log-test-process-incremental-text/partial-line-saved ()
  "Saves incomplete line for next call."
  (with-temp-buffer
    (claude-log-mode)
    (let ((claude-log--partial-line "")
          (claude-log--source-file nil)
          (claude-log--rendered-file nil))
      (claude-log--process-incremental-text
       "{\"type\":\"user\",\"partial" nil)
      ;; Partial line should be saved
      (should (equal claude-log--partial-line "{\"type\":\"user\",\"partial")))))

(ert-deftest claude-log-test-process-incremental-text/partial-line-completed ()
  "Completes a previously partial line."
  (with-temp-buffer
    (claude-log-mode)
    (let ((claude-log--partial-line "{\"type\":\"us")
          (claude-log--source-file nil)
          (claude-log--rendered-file nil)
          (inhibit-read-only t))
      (insert "# Session: test — unknown\n\n")
      ;; Complete the partial line
      (claude-log--process-incremental-text
       (concat "er\",\"timestamp\":\"2023-11-14T12:00:00Z\","
               "\"message\":{\"role\":\"user\",\"content\":\"hello\"}}\n")
       nil)
      (should (string-match-p "hello" (buffer-string))))))

;;;;; Pending sessions

(ert-deftest claude-log-test-pending-sessions/unrendered ()
  "Detects sessions not in the index."
  (claude-log-test--with-temp-dir
    (let* ((jsonl-content "{\"type\":\"user\"}\n")
           (jsonl-path (claude-log-test--write-file "test.jsonl" jsonl-content))
           (sessions (list (list "s1" :file jsonl-path)))
           (index (make-hash-table :test #'equal))
           (pending (claude-log--pending-sessions sessions index)))
      (should (= (length pending) 1)))))

(ert-deftest claude-log-test-pending-sessions/up-to-date ()
  "Detects up-to-date sessions."
  (claude-log-test--with-temp-dir
    (let* ((jsonl-content "{\"type\":\"user\"}\n")
           (jsonl-path (claude-log-test--write-file "test.jsonl" jsonl-content))
           (jsonl-size (file-attribute-size (file-attributes jsonl-path)))
           (rendered-path (claude-log-test--write-file "rendered.md" "rendered"))
           (sessions (list (list "s1" :file jsonl-path)))
           (index (make-hash-table :test #'equal)))
      (puthash "s1" (list :file rendered-path :jsonl-size jsonl-size) index)
      (let ((pending (claude-log--pending-sessions sessions index)))
        (should (= (length pending) 0))))))

(ert-deftest claude-log-test-pending-sessions/stale ()
  "Detects sessions where JSONL has grown since rendering."
  (claude-log-test--with-temp-dir
    (let* ((jsonl-content "{\"type\":\"user\"}\n{\"type\":\"assistant\"}\n")
           (jsonl-path (claude-log-test--write-file "test.jsonl" jsonl-content))
           (rendered-path (claude-log-test--write-file "rendered.md" "rendered"))
           (sessions (list (list "s1" :file jsonl-path)))
           (index (make-hash-table :test #'equal)))
      ;; Index claims smaller size than actual
      (puthash "s1" (list :file rendered-path :jsonl-size 10) index)
      (let ((pending (claude-log--pending-sessions sessions index)))
        (should (= (length pending) 1))))))

;;;;; Append to file

(ert-deftest claude-log-test-append-to-file ()
  "Appends text to a file."
  (claude-log-test--with-temp-dir
    (let ((path (expand-file-name "test.md" claude-log-test--dir)))
      (with-temp-file path (insert "first\n"))
      (claude-log--append-to-file path "second\n")
      (let ((content (with-temp-buffer
                       (insert-file-contents path)
                       (buffer-string))))
        (should (equal content "first\nsecond\n"))))))

;;;;; Preserve order table

(ert-deftest claude-log-test-preserve-order-table/metadata ()
  "Returns metadata with identity sort functions."
  (let ((table (claude-log--preserve-order-table '("a" "b" "c"))))
    (let ((meta (funcall table "" nil 'metadata)))
      (should (equal (car meta) 'metadata))
      (should (assq 'display-sort-function (cdr meta)))
      (should (assq 'cycle-sort-function (cdr meta))))))

(provide 'claude-log-test)
;;; claude-log-test.el ends here
