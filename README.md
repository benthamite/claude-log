# `claude-log`: Browse Claude Code conversation logs in Emacs

## Overview

[Claude Code](https://docs.anthropic.com/en/docs/claude-code) stores complete conversation transcripts as JSONL files under `~/.claude/projects/`. These files are machine-readable but not human-friendly. `claude-log` renders them as plain Markdown files under `~/.claude/rendered/`, so that standard tools -- `consult-ripgrep`, Dired, `grep` -- work natively on readable content.

The package provides a session browser that lists all your Claude Code conversations, grouped by project and sorted most-recent-first. Sessions are lazily rendered on first access: you pick a session, and `claude-log` converts its JSONL transcript into a well-structured Markdown file with headings for each turn, collapsible tool calls, and thinking blocks. For active conversations, a file watcher keeps the rendered buffer updated in real time.

Key capabilities:

- **Session browser** with project grouping and per-session preview (date, project, first message).
- **Rendered Markdown mirror** with readable filenames, organized by project.
- **Full-text search** via `consult-ripgrep` or any grep tool on the rendered directory.
- **Live updates** via `file-notify` as conversations progress.
- **Outline navigation** to fold/unfold tool calls, thinking blocks, and tool results.
- **Smart tool summaries** showing the most relevant input for each tool call (file path for Read/Write, command for Bash, pattern for Grep, etc.).

## Installation

**Requirements:** Emacs 29.1 or later, and [markdown-mode](https://jblevins.org/projects/markdown-mode/) 2.6 or later.

### package-vc (built-in since Emacs 30)

```emacs-lisp
(package-vc-install "https://github.com/benthamite/claude-log")
```

### Elpaca

```emacs-lisp
(use-package claude-log
  :ensure (claude-log :host github :repo "benthamite/claude-log"))
```

### straight.el

```emacs-lisp
(straight-use-package
 '(claude-log :type git :host github :repo "benthamite/claude-log"))
```

## Quick start

```emacs-lisp
(use-package claude-log
  :ensure (claude-log :host github :repo "benthamite/claude-log")
  :bind (("C-c l b" . claude-log-browse-sessions)
         ("C-c l l" . claude-log-open-latest)
         ("C-c l d" . claude-log-open-rendered-directory)))
```

Run `M-x claude-log-browse-sessions` to pick a project and session. The session opens as a rendered Markdown buffer with folding support. Press `n`/`p` to jump between turns, `TAB` to toggle a section, and `C` to collapse all tool sections at once.

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](README.org).

## License

`claude-log` is licensed under the GPL-3.0-or-later. See [COPYING.txt](COPYING.txt) for details.
