# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

Personal Emacs configuration. No build step, no tests, no package manager. Editing a `.el` file and reloading it in Emacs is the only workflow that matters.

To reload a changed file inside Emacs: `M-x load-file RET <path-to-file> RET`, or evaluate individual expressions with `C-x C-e`.

## Architecture

The entry point is `init.el`. It bootstraps `use-package`, sets `user-emacs-directory` to the repo root, adds all subdirectories of `site-lisp/` to `load-path`, then loads the top-level modules in order:

```
init.el
├── mhs-environment      — detects macOS vs Linux, sets browser
├── emacs-extras         — the main configuration hub: packages, hooks, project.el
├── emacs-keybinds       — all global keybindings and hydras
├── emacs-ministack      — completion stack: vertico, orderless, consult, embark, corfu, cape
├── mhs-scrum            — Jira/sprint tooling
├── mhs-org-mode         — org-mode and agenda setup (files in ~/Dropbox/orgs/)
├── mhs-org-clock-into   — org-clock helpers
├── emacs-custom-faces   — theme and face customizations
├── emacs-modeline       — doom-modeline setup
└── emacs-gpt            — gptel (Claude/Copilot) and agent-shell (Docker-based Claude Code)
```

The `site-lisp/` tree has two directories added to `load-path`:

- `site-lisp/savoie/` — the files above (environment, ministack, modeline, gpt, keybinds, extras, faces)
- `site-lisp/lisp/` — language/tool-specific modules loaded on demand from `emacs-extras`:
  - `mhs-lsp` — lsp-mode with basedpyright + ruff for Python, TypeScript LSP, corfu/cape integration
  - `mhs-javascript` — TypeScript tree-sitter modes, nodejs-repl
  - `mhs-python` — Python environment (pyenv/pyvenv)
  - `mhs-dap` — DAP debugger for Python and Node.js
  - `mhs-magit` — magit extensions and forge
  - `mhs-jira` — Jira integration
  - `mhs-map` — custom prefix keymap on `[f8]`
  - `mhs-extends` — miscellaneous extensions
  - `mhs-dblstuff` — double-stuff text utilities

## Key conventions

- All custom functions are prefixed `mhs-` or `mhs/`.
- Packages are managed via `use-package` against MELPA, MELPA-stable, GNU ELPA, and NonGNU ELPA. Straight.el is present in `straight/` but the primary mechanism is `use-package :ensure t`.
- API keys (Claude, OpenAI) are stored in `private/.authinfo.gpg` and retrieved via `auth-source`. Never put keys in `.el` files.
- `custom-file` is `.gnu-emacs-custom` (gitignored); keep `M-x customize` output out of `init.el`.
- `combobulate` is loaded from a local path (`/Users/savoie/projects/combobulate`), not from a package archive.
- `agent-shell` runs Claude Code inside Docker via `~/.claude/docker/run.sh`; the path resolver maps `/workspace/` ↔ local project root.

## LSP setup notes

- Python: basedpyright is the type checker; ruff-lsp handles linting/formatting. `pylsp` and `pyls` are explicitly disabled.
- TypeScript: `lsp-typescript-format-enable t` + `lsp-eslint-format nil` to avoid dual-formatter corruption.
- Per-project Python virtualenvs are activated via `.dir-locals.el` using `pyvenv-activate`; use `mhs-create-dir-locals-file` to scaffold one.
