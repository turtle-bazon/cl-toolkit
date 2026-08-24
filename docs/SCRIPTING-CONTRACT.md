# cl-toolkit Scripting Contract

*The stable interface for scripts, CI, and harnesses driving cl-toolkit.
Everything here is tested by `test/cli-matrix.sh` and the FiveAM suite;
violations are release blockers.*

---

## Channels

| Channel | Content | Never contains |
|---|---|---|
| **exit code** | The boolean. `0` = success, `1` = failure, `64` = argument-parse error (clingon convention) | — |
| **stdout** | The payload: unified diff (write/preview), modified source (no-write edits), or JSON | diagnostics |
| **stderr** | Diagnostics: target announcements, preview stats, warnings, human-readable refusal text | payloads |

### Rules for scripts

1. **Decide from the exit code alone.** Never grep output for words like
   `error` — legitimate Lisp diffs contain `(error 'calc-error ...)`.
2. **Capture full stderr to a file first; filter for display only.**
   Diagnostics are informative, not load-bearing.
3. **Failure reasons ride BOTH channels** (since 0.4.1/0.4.2): refusal
   JSON on stdout *and* human text on stderr. A single captured channel
   always carries the diagnosis.
4. **Success with `--write --quiet`** prints nothing on stderr except
   target announcements — which are intentionally NOT suppressed by
   `--quiet` (they are the last-line defense against wrong-form writes).

---

## Positions

- All `--line`/`--col` arguments and all displayed `[line N, col M]`
  values are **0-based**, newline-delimited, LSP-style.
- A displayed position can be passed back to `--line`/`--col` verbatim.
- `grep -n` counts lines from 1 — subtract one before passing.

---

## Anchors

- `--after-anchor S`, `--contains S`, `--find-old --old S`, and
  `--match S` all require **unique** matches by default; ambiguity
  refuses with per-occurrence `[line, col]` listings.
- `--first` (match) / explicit indices opt into non-unique selection.
- Anchor matching is **literal source text**: `#'x` will not match
  `(function x)`. Prefer `--code-file`/stdin (`-`) for code-bearing
  arguments to sidestep shell quoting entirely.

---

## Guards (what can refuse a write)

| Guard | Fires when | Escape hatch |
|---|---|---|
| byte-exact anchor | `--old`/`--match` text not present exactly at the resolved position | none (fix the plan) |
| net depth-delta | substitution changes paren depth (scope shift) | `patch-span --allow-shift` |
| replacement shape | one top-level form replaced by several | `--allow-multi-forms` |
| match ambiguity | >1 subform/anchor occurrences | `--first`, or refine snippet |
| result validation | edited file no longer parses | `--no-validate-result` (last resort) |
| write-existence | `--write` target missing | none (`--write` never creates) |

All guards emit dual-channel refusals and exit 1.

---

## Match ops vs patch-span — whole node vs byte prefix

- `replace-form --match`, batch `replace-match`/`delete-match` operate on
  **whole AST nodes**: the snippet must equal the node's full (trimmed)
  source, and the replacement swaps the entire node.
- `patch-span --find-old --old/--new` splices **byte-exact prefixes** —
  e.g. replacing an opening `(let ...)` block while keeping the node's
  remaining body.

Symptom of using the wrong one: your prefix snippet matches several
*containing* nodes and ambiguity refuses (prefixes are contained by
everything they sit inside). Reach for patch-span.

## Backups

- Default: rolling `FILE.bak` (previous content) before every write.
- `--backup-dir DIR`: additionally a timestamped snapshot
  `DIR/STEM.YYYYMMDD-HHMMSS.EXT.bak` of the **pre-edit** content.
- `--no-backup`: skip (scripted/batch use).
- `*.bak` belongs in `.gitignore`.

---

## Driving builds after writes

cl-toolkit preserves the file's content but the OS may update mtime on
rename-based writes; ASDF compares source vs fasl freshness. If you
drive builds immediately after edits inside one long-lived Lisp image,
call `(asdf:clear-system :sys)` or restart — a stale in-memory fasl can
mask a fresh source. Fresh processes are always safe.

---

## Process substitution

`-f <(cmd)` works: named pipes are detected and read to EOF (0.3.1+).
