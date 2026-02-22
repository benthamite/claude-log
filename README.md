# claude-log

Browse [Claude Code](https://docs.anthropic.com/en/docs/claude-code) conversation logs in Emacs.

Claude Code stores complete conversation transcripts as JSONL files under `~/.claude/projects/`. This package reads those logs and renders them as readable Markdown in a dedicated buffer, with live updates as conversations progress.

## Features

- **Session browser**: browse all sessions, grouped by project or as a flat list, sorted most-recent-first
- **Full-text search**: search across all sessions with incremental ripgrep-powered search via [consult](https://github.com/minad/consult), or a simple fallback for non-consult setups
- **Markdown rendering**: conversation turns rendered as Markdown with proper headings, tool summaries, and thinking blocks
- **Live updates**: buffers update in real time via `file-notify` as conversations progress
- **Outline navigation**: fold/unfold tool calls, thinking blocks, and tool results using `outline-minor-mode`
- **Smart tool summaries**: each tool call is summarized by its most relevant input (file path for Read/Write, command for Bash, pattern for Grep, etc.)

## Requirements

- Emacs 29.1 or later
- [markdown-mode](https://jblevins.org/projects/markdown-mode/) 2.6 or later
- [ripgrep](https://github.com/BurntSushi/ripgrep) (`rg`) for session search
- (Optional) [consult](https://github.com/minad/consult) for incremental search with live preview

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

### Searching sessions

```
M-x claude-log-search-sessions
```

With consult installed, this provides an incremental ripgrep search across all session files. Results show the project name alongside matching content, and selecting a result opens the full session log.

Without consult, this prompts for a search string, finds all matching sessions, and presents them in the session browser.

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

| Option                             | Default         | Description                                                      |
|------------------------------------|-----------------|------------------------------------------------------------------|
| `claude-log-directory`             | `"~/.claude"`   | Root directory of Claude Code configuration                      |
| `claude-log-show-thinking`         | `collapsed`     | How to display thinking blocks: `hidden`, `collapsed`, `visible` |
| `claude-log-show-tool-output`      | `t`             | Whether to show tool result content in user turns                |
| `claude-log-collapse-tools`        | `t`             | Auto-fold tool and thinking sections after rendering             |
| `claude-log-timestamp-format`      | `"%Y-%m-%d %H:%M:%S"` | Format string for timestamps                              |
| `claude-log-max-tool-input-length` | `200`           | Max characters for tool input summaries                          |
| `claude-log-max-tool-result-length`| `500`           | Max characters for tool result content                           |
| `claude-log-live-update`           | `t`             | Watch the JSONL file for real-time updates                       |
| `claude-log-group-by-project`      | `t`             | Group sessions by project in the browser                         |
| `claude-log-display-width`         | `60`            | Max width of the first-message column in the session browser     |

## How it works

Claude Code stores conversation data in two places:

1. **`~/.claude/history.jsonl`**: a global index mapping session IDs to projects and timestamps
2. **`~/.claude/projects/<encoded-path>/<uuid>.jsonl`**: one file per session, containing all messages as JSON lines

Each JSONL entry has a `type` field (`user`, `assistant`, `progress`, `system`, or `file-history-snapshot`). The package filters to `user` and `assistant` entries, further excluding system-generated messages (tool notifications, command output, etc.), and renders them as Markdown with `##` headings for turns and `####` headings for tool calls and thinking blocks.

The rendered buffer uses `markdown-view-mode` as its parent mode for fontification, with `outline-minor-mode` for section folding.

For live updates, the package records the byte offset at the end of the file after the initial render, then uses `file-notify-add-watch` to detect changes. When new data arrives, only the new bytes are read and parsed, and the rendered output is appended to the buffer.

## License

GPL-3.0-or-later
