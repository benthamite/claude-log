# claude-log

Browse [Claude Code](https://docs.anthropic.com/en/docs/claude-code) conversation logs in Emacs.

Claude Code stores complete conversation transcripts as JSONL files under `~/.claude/projects/`. This package renders them as readable Markdown files under `~/.claude/rendered/`, so that standard tools (`consult-ripgrep`, `dired`, `grep`) work natively on readable content.

## Features

- **Session browser**: browse all sessions, grouped by project or as a flat list, sorted most-recent-first
- **Rendered Markdown mirror**: sessions are lazily rendered to `.md` files with readable filenames, organized by project
- **Full-text search**: use `consult-ripgrep` or any grep tool directly on the rendered directory
- **Live updates**: buffers and `.md` files update in real time via `file-notify` as conversations progress
- **Outline navigation**: fold/unfold tool calls, thinking blocks, and tool results using `outline-minor-mode`
- **Smart tool summaries**: each tool call is summarized by its most relevant input (file path for Read/Write, command for Bash, pattern for Grep, etc.)

## Requirements

- Emacs 29.1 or later
- [markdown-mode](https://jblevins.org/projects/markdown-mode/) 2.6 or later

## Installation

### Manual

Clone this repository to your Emacs load path and add this to your `init.el` file:

```emacs-lisp
(require 'claude-log)
```

### With `use-package`

If you use `use-package`, add one of the following snippets to your `init.el` file:

```emacs-lisp
;; with vc
(use-package claude-log
  :vc (:url "https://github.com/benthamite/claude-log"))

;; with elpaca
(use-package claude-log
  :ensure (:host github :repo "benthamite/claude-log"))

;; with straight
(use-package claude-log
  :straight (:host github :repo "benthamite/claude-log"))

;; with quelpa
(use-package claude-log
  :quelpa (claude-log :fetcher github :repo "benthamite/claude-log"))
```

## Usage

### Browsing sessions

```
M-x claude-log-browse-sessions
```

When `claude-log-group-by-project` is non-nil (the default), this first prompts for a project, then for a session within that project. Sessions are displayed as:

```
2026-02-22 14:06  my-project  "Fix the authentication bug in..."
```

The selected session is lazily rendered to a `.md` file if needed, then opened in a buffer.

### Opening the latest session

```
M-x claude-log-open-latest
```

Opens the most recent session directly, without prompting.

### Searching sessions

Use `consult-ripgrep` (or any grep tool) on the rendered directory:

```
M-x claude-log-open-rendered-directory   ;; opens ~/.claude/rendered/ in dired
```

Since rendered files are plain Markdown, standard search tools work natively — no special integration needed.

### Bulk rendering

```
M-x claude-log-sync-all
```

Renders all unrendered or stale sessions. Uses timers to avoid blocking Emacs.

### Opening a file directly

```
M-x claude-log-open-file
```

Opens a specific JSONL file by path. Useful for automation or when you know the exact file.

### Navigation

| Key     | Command                       | Description                              |
|---------|-------------------------------|------------------------------------------|
| `n`     | `claude-log-next-turn`        | Jump to the next User/Assistant heading  |
| `p`     | `claude-log-previous-turn`    | Jump to the previous heading             |
| `TAB`   | `claude-log-toggle-section`   | Toggle fold of section at point          |
| `C`     | `claude-log-collapse-all-tools` | Collapse all tool/thinking sections    |
| `E`     | `claude-log-expand-all`       | Expand all sections                      |
| `g`     | `claude-log-refresh`          | Re-render the buffer from scratch        |
| `w`     | `claude-log-copy-turn`        | Copy the current turn to the kill ring   |
| `q`     | `quit-window`                 | Close the buffer                         |

## Customization

| Option                             | Default                | Description                                                      |
|------------------------------------|------------------------|------------------------------------------------------------------|
| `claude-log-directory`             | `"~/.claude"`          | Root directory of Claude Code configuration                      |
| `claude-log-rendered-directory`    | `"~/.claude/rendered"` | Directory where rendered Markdown files are stored               |
| `claude-log-slug-max-length`       | `50`                   | Maximum length of the slug portion of rendered filenames          |
| `claude-log-show-thinking`         | `collapsed`            | How to display thinking blocks: `hidden`, `collapsed`, `visible` |
| `claude-log-show-tools`            | `collapsed`            | How to display tool sections: `hidden`, `collapsed`, `visible`   |
| `claude-log-timestamp-format`      | `"%Y-%m-%d %H:%M:%S"` | Format string for timestamps                                     |
| `claude-log-max-tool-input-length` | `200`                  | Max characters for tool input summaries                          |
| `claude-log-max-tool-result-length`| `500`                  | Max characters for tool result content                           |
| `claude-log-live-update`           | `t`                    | Watch the JSONL file for real-time updates                       |
| `claude-log-group-by-project`      | `t`                    | Group sessions by project in the browser                         |
| `claude-log-display-width`         | `60`                   | Max width of the first-message column in the session browser     |

## How it works

Claude Code stores conversation data in two places:

1. **`~/.claude/history.jsonl`**: a global index mapping session IDs to projects and timestamps
2. **`~/.claude/projects/<encoded-path>/<uuid>.jsonl`**: one file per session, containing all messages as JSON lines

### Rendered directory

This package maintains a mirror directory of pre-rendered Markdown files:

```
~/.claude/rendered/
  tango-wiki/
    2026-02-04_14-06_fix-the-authentication-bug.md
    2026-02-05_09-22_refactor-database-schema.md
  dotfiles/
    2026-02-22_11-56_redesign-claude-log-package.md
  _index.el
```

Each `.md` file contains HTML-comment front matter (session ID, source path, rendered timestamp, JSONL size) followed by the full conversation rendered as Markdown.

### Sync strategy

- **Lazy by default**: rendering happens on first access via `claude-log-browse-sessions` or `claude-log-open-file`. The package checks whether the `.md` file exists and is up-to-date (by comparing JSONL size against the index). If missing or stale, it renders from scratch.
- **Bulk sync**: `claude-log-sync-all` renders all unrendered/stale sessions via timers (non-blocking).
- **Live updates**: for active sessions, a `file-notify` watcher detects JSONL changes and appends new entries to both the `.md` file on disk and the open buffer.

### Rendering

Each JSONL entry has a `type` field (`user`, `assistant`, `progress`, `system`, or `file-history-snapshot`). The package filters to `user` and `assistant` entries, further excluding system-generated messages (tool notifications, command output, etc.), and renders them as Markdown with `##` headings for turns and `####` headings for tool calls and thinking blocks.

The rendered buffer uses `markdown-view-mode` as its parent mode for fontification, with `outline-minor-mode` for section folding.

## License

GPL-3.0-or-later
