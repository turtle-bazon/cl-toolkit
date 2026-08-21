# Changelog

All notable changes to cl-toolkit. During 0.x, breaking changes are
marked `BREAKING:`.

## [0.2.0] - 2026-08-21

### BREAKING

- **Tokenizer**: symbols may now contain digits after the first character.
  `alpha2` parses as one SYMBOL, not symbol+number. `1+`/`1-`/`123abc`
  are symbols (CL reader semantics); `-5`, `-2.5e2` are negative numbers;
  `1e5` parses as 100000.0. AST shapes change for affected inputs.
- **Position targeting is exact by default** for destructive ops
  (`replace-form`, `delete-form --line/--col`): the form must *start*
  at the given position or the command fails loudly. Pass `--nearest`
  for the old containment match. Read-only `find` keeps nearest-match
  and now reports resolved line/col.
- **Line/col convention unified to 0-based everywhere** — args, JSON,
  and human-readable display. Previously `top-level --names` showed
  1-based lines while args were 0-based (source of silent off-by-one).
- **`format` default is minimal repair** (split jams + reindent only
  broken multi-line forms). Whole-file restyle moved behind `--canonical`.
- Plugin no longer generates diffs client-side; previews come from the
  CLI's own unified diff output.

### Added

- `source-of (--name|--index|--end)` — verbatim source extraction for
  safe read-modify-write of large forms.
- `replace-form --match SNIPPET` — replace smallest subform matching
  snippet inside a named/indexed form.
- `find-forms --contains [--with-source]` — structural content search.
- `split-forms` — insert newlines between jammed top-level forms only.
- `--backup-dir DIR` timestamped pre-edit snapshots; `--no-backup`.
- Non-quiet edits log target form name + resolved `[line, col]`.
- batch-replace name operations: `replace-name`, `delete-name`,
  `insert-after-name` (applied before index edits).
- FiveAM regression suite (`make test`) — 71 checks.

### Fixed

- `#'x` parsed as `(quote x)` with wrong marker name/bounds — PEG
  ordering bug in sharp-dispatch (inner rules cannot see the `#`).
- QUOTE/FUNCTION/EVAL marker symbols lacked :start/:end.
- `--write` on a nonexistent file silently created one; now errors.
- Relative paths could double up segments in backup naming; all paths
  normalized to absolute.
- Validation failures printed esrap's multi-page report; now one line:
  `Syntax error at Line L, Column C, Position N`.
- Stale-fasl phantom behavior: `make test` wipes the fasl cache first.
- batch-replace index edits apply highest-index-first (no shifting).
- `replace-form --pretty` double-indented when original form was indented.

## [0.0.1.0]

Initial release.
