# Changelog

All notable changes to cl-toolkit. During 0.x, breaking changes are
marked `BREAKING:`.

## [0.3.0] - 2026-08-22

### BREAKING

- **Replacement-shape guard**: replacing one top-level form with text
  containing several top-level forms now fails unless
  `--allow-multi-forms` is passed. Closes the whole-file-as-replacement
  corruption vector (stray in-package x4).

### Added — analysis layer (verification & surgery planning)

- `check-anchor -f F --text S` → `{count, first-offset, line, col}`;
  exit 1 unless the anchor is unique (safe-edit precondition).
- `patch-span --line L --col C --old T --new T` → byte-exact anchor
  verification + reader-aware **net depth-delta guard**: refuses ±≠0
  substitutions (scope-shifting wraps/restructures) unless
  `--allow-shift`. This is the direct antidote to the extra-closer-at-EOF
  corruption class.
- `balance --expect-delta N` — assert a fragment's net depth
  contribution using the real parser (char literals, strings, comments).
- `diff-forms -f F --name X [--against-file G]` — structural add/remove
  summary of direct children; immune to re-indentation noise.
- `lint` — flags duplicate identical top-level forms.
- `replace-form --match-exact` — never escalate to contains-match;
  fails with guidance instead.
- Fuzzy `--match` fallbacks now announce themselves:
  `Replacing in form (fuzzy contains-match) ...`.
- `--preview` prints stats line (old/new line counts) to stderr.

### Notes

- Subform deletion shipped in 0.2.1 as `--delete-match` (the gap report's
  P3 request predates it).

## [0.2.1] - 2026-08-22

### Added

- Target announcement on every destructive edit — now printed even under
  `--quiet` and includes a 60-char source preview:
  `Replacing form 'test' [line 4, col 0] "(test process-defun ...)"`.
  Closes the anonymous-sibling incident cluster (look-alike FiveAM tests).
- `top-level --names --preview-chars N` — source excerpts in listings so
  identical heads are tellable.
- `--contains SNIPPET` targeting on replace-form/delete-form: resolves to
  the *unique* top-level form containing the snippet; refuses ambiguity
  with the candidate indices.
- `replace-form --delete-match` — removes the `--match`ed subform
  (subform deletion without whole-function rewrite).
- `--match` miss error now notes that matching is literal source text.

### Fixed

- Plugin `--quiet` no longer suppresses target announcements (root cause
  of missing announcements on index writes through the plugin).

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
