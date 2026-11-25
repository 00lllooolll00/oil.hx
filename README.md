# oil.hx

A Dired & WDired-inspired file manager plugin for [Helix](https://github.com/helix-editor/helix/) written in Steel — browse directories with single-key Dired actions, mark multiple files, expand subtrees inline, preview in splits, or switch to WDired mode to edit your filesystem like a buffer.

> Inspired by Emacs [Dired / WDired](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html) and [oil.nvim](https://github.com/stevearc/oil.nvim).

![oil.hx preview](.github/assets/preview.gif)

---

## Installation

**1. Install the plugin-enabled fork of Helix** by following the instructions [here](https://github.com/mattwparas/helix/blob/steel-event-system/STEEL.md).

**2. Install oil.hx via forge (or smith.hx):**

```sh
forge pkg install --git https://github.com/Ra77a3l3-jar/oil.hx.git
```

**3. Load the plugin** in `init.scm`:

```scheme
(require "oil/oil.scm")

;; Optional configuration (defaults shown):
(oil-configure!
  #:show-hidden #false
  #:show-git-ignored #false
  #:show-metadata #false
  #:mode 'dired              ; 'dired (browse by default) | 'wdired (editable buffer by default)
  #:sort-by 'name            ; 'name | 'mtime | 'size | 'ext
  #:sort-reverse #false
  #:use-trash #true          ; trash-put / gio trash / XDG Trash fallback
  #:confirm-save 'destructive ; 'destructive | 'always | 'never
  #:default-keymaps #true)   ; install Dired/WDired buffer keymaps for *oil*

;; Legacy 2-argument form is also still supported:
;; (oil-configure! #false #false)
```

`oil.hx` depends on [notify.hx](https://github.com/chuwy/notify.hx) and pulls it in automatically via `forge`.

---

## Modes: Dired vs WDired

1. **Dired (Browse mode, default)**:
   Mutating Helix keys (`i`, `a`, `d`, `c`, `x`, `p`, `u`, ...) are locked or mapped to Dired operations inside `*oil*`, preventing accidental edits while navigating.
   - Mark multiple entries (`m` / `u` / `U` / `t` / `%`), yank/cut (`y` / `x`), paste (`p`) with conflict-safe copy naming, create (`+` / `a`), rename (`R`), delete (`D` / `d` with confirmation), toggle subtrees (`tab`), sort (`s` / `S`), and preview in splits (`o` / `O` / `v`).
   - Cursor position is remembered when jumping from a file (`:oil`), going up (`:oil-up`), refreshing, or toggling visibility/sorting.

2. **WDired (Editable mode)**:
   Press `E` (or run `:oil-wdired-enter` / `:oil-toggle-wdired`) to unlock the `*oil*` buffer and edit filenames/paths directly using normal Helix editing commands:
   - Press `C-s` or run `:oil-save` to compute an LCS-aligned rename/create/delete plan, preview changes in a confirmation modal (when destructive or multi-op), and apply atomically.
   - Press `C-q` or run `:oil-wdired-abort` to discard pending edits and return to Dired mode.
   - Navigating away while WDired has unsaved edits is guarded so edits are never silently lost.

---

## Usage

### Default `*oil*` Buffer Keybindings

**Dired mode** (installed automatically unless `#:default-keymaps #false`):

| Key | Command | Description |
|---|---|---|
| `ret` / `l` | `:oil-enter` | Enter directory or open file |
| `-` / `^` / `backspace` | `:oil-up` | Go to parent directory (cursor lands on previous subdir) |
| `H` / `L` | `:oil-history-back` / `:oil-history-forward` | Navigate directory history |
| `gr` / `~` | `:oil-root` / `:oil-home` | Jump to git root (or cwd) / `$HOME` |
| `o` / `O` | `:oil-open-vsplit` / `:oil-open-hsplit` | Open file in vertical / horizontal split |
| `v` | `:oil-preview` | Preview file in split while keeping focus in `*oil*` |
| `tab` | `:oil-toggle-subtree` | Expand / collapse directory inline as subtree |
| `m` / `u` | `:oil-mark` / `:oil-unmark` | Mark / unmark entry and move down |
| `U` / `t` / `%` | `:oil-unmark-all` / `:oil-toggle-marks` / `:oil-mark-pattern` | Clear, invert, or glob-match marks |
| `y` (`C`) / `x` / `p` / `c` | `:oil-yank` / `:oil-cut` / `:oil-paste` / `:oil-clipboard-clear` | Multi-file copy / move / paste / clear clipboard |
| `+` / `a` | `:oil-mkdir` / `:oil-create-file` | Prompt to create directory / create & open file |
| `R` | `:oil-rename` | Prompt to rename entry under cursor |
| `D` / `d` | `:oil-delete` | Trash or delete marked/current entry (with confirmation) |
| `s` / `S` | `:oil-cycle-sort` / `:oil-toggle-sort-reverse` | Cycle sort (`name`/`mtime`/`size`/`ext`) & reverse |
| `.` / `I` | `:oil-toggle-hidden` / `:oil-toggle-git-ignored` | Toggle dotfiles / git-ignored entries |
| `(` / `M` | `:oil-toggle-metadata` | Toggle permissions, links, owner, size, mtime, symlink target |
| `r` | `:oil-refresh` | Reload current directory |
| `E` | `:oil-wdired-enter` | Switch to WDired editable mode |
| `q` / `?` | `:oil-close` / `:oil-help` | Close `*oil*` / show keybinding help popup |

**WDired mode**:

| Key | Command | Description |
|---|---|---|
| `C-s` | `:oil-save` | Plan, preview, and apply filesystem edits |
| `C-q` | `:oil-wdired-abort` | Discard edits and return to Dired mode |

### Global Shortcuts (optional)

Add to `config.toml` or `init.scm` to invoke `:oil` from any buffer:

```toml
[keys.normal.space.o]
o = ":oil"
r = ":oil-root"
```

`:oil` also accepts an optional directory argument, e.g. `:oil /tmp`.

### Git support

When inside a git repository, `oil.hx` runs `git status` and renders inline status hints:

| Hint | Meaning |
|---|---|
| `~` | Modified |
| `+` | Staged (new file) |
| `→` | Renamed |
| `?` | Untracked |
| `!` | Ignored |

---

## Development & Tests

Run the headless Steel test suite and AST validation from the repository root:

```sh
sh tests/run-all.sh
```
