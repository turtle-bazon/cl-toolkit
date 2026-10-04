# Changelog

All notable changes to cl-toolkit. During 0.x, breaking changes are
marked `BREAKING:`.

## [Unreleased]

### Verification

The properties below now hold across the 5079-file corpus and are
pinned by the test suite, so a later change cannot quietly break them.

- **Span invariants** (`check-node-spans`, `check-source-spans`): every
  node's `[start, end)` is a real range inside its parent, siblings are
  in source order, and a leaf that consumed something reads back as the
  token it parsed. 0 violations over the corpus.
- **Formatter round trip**: for both the minimal and the canonical
  formatter, the output parses, parses to the same structure, and is a
  fixed point. 0 violations over 4985 files.
- **Edit operations**: replacing a form with its own source text is a
  no-op, the same edit twice gives the same bytes, a successful edit
  leaves text that still parses, inserting cannot shorten a file and
  deleting cannot lengthen one, an out-of-range target is refused, and a
  batch equals its parts applied one at a time. 0 violations over
  48011 edits.
- **Fuzzing**: 100000 generated inputs (random token soup, random
  well-formed forms) produce no crash, no span violation, no
  nondeterminism and no format round-trip failure.
- **Bounded rejection**: a file with a syntax error is refused in time
  proportional to its size, and the reported position is inside the
  file. A truncated candidate (a cut landing mid-form, as in
  `#+sbcl (a)`) is still extended rather than treated as damage.
- **Machine output**: the parse node and lint diagnostic shapes are
  checked against the documented keys and types, rendering is
  byte-identical across runs, and deeply nested or unterminated input
  finishes in bounded time.

### Fixed

- **An unrepresentable numeric literal is located, not just reported.**
  `1e542` overflows a single float inside a rule transform, and a
  transform signals rather than failing the rule, so esrap never reports
  an offset. The error node therefore spanned the whole chunk and its
  message named no position at all -- for a long range, no help. The
  offending literal is now isolated by re-parsing each candidate token on
  its own with the same rules, so the diagnosis cannot drift from the
  grammar, and the report reads "Number has no value in this float format
  at Line 15001, Column 21, Position 529094". A file with two such
  literals reports the first.
- **The first parse error is reported, not the last.** The final range of
  a scan overwrote a failure already recorded, so a file whose line 1 has
  a stray paren and whose tail is unterminated was reported at the tail,
  sending an editor to the end of the file to fix something wrong at the
  top. This contradicted `parse-forms-from`'s own docstring.
- **Numbers follow the reader.** Only the `e` exponent marker was
  recognized, so `1d0`, `1f0`, `1s0` and `1l0` read as symbols, and
  every radix integer read as a symbol (`#xFF` was the symbol `#xFF`,
  not 255). Radix prefixes now parse as integers in any base 2-36 with
  an optional sign, and a prefix outside a number is a hard error, so
  `#xFFg` and `#10r` fail instead of degrading into a `#` symbol. The
  marker letter picks the float format (`e`/`f`/`s` follow
  `*read-default-float-format*`, `d`/`l` force double), so `1e0` is
  single and `1d0` is double. Ratios (`3/4`), leading-dot floats
  (`.5`, `.5e2`) and trailing-dot integers (`5.`) are numbers again,
  while `3/-4`, `3.5/2` and `3/0` keep the reader's spelling.
- **Dotted pairs have real structure.** `(a . (b))` is the two-list
  `(a b)` rather than four children with a `.` in the middle;
  `(a . nil)` and `(a . (b . nil))` collapse as the reader collapses
  them; `(. b)`, `(a .)`, `(a . b . c)` and `#(a . b)` are rejected.
- **Symbol escapes are decoded.** `node-name` kept the backslash, so
  `some\!thing` was named `"some\!thing"` and could never be matched
  by `find-top-level-by-name` or `rename`. Names now hold what the
  reader sees, bar segments keep their bars, and an escaped colon
  (`foo\:bar`) no longer splits as a package prefix.
- **`#()` reads as an empty vector.** It never parsed as a vector --
  it fell through to a `#` symbol plus an empty list, which read as
  success with the wrong shape -- and once `#(` was barred from the
  symbol rule it began rejecting real files.
- **A lone `.` is not a symbol.** The reader signals an error for it;
  `.b`, `..` and `a.b` remain symbols.
- **`cl-toolkit parse FILE` parses FILE.** The positional argument
  shown in `parse --help` was dropped on the floor, so the command
  printed an empty AST and exited 0 -- indistinguishable from a
  successful parse of an empty file. A bad path now fails like `-f`,
  and two bare arguments are a usage error.
- **List parsing no longer parses every list twice.** Splitting
  `list-form` into an `or` of a dotted and a proper rule made the
  dotted alternative consume a whole list, fail on the missing dot,
  and hand the list to the proper rule to re-read; nested lists paid
  that per level. `quicklisp/asdf.lisp` went from not finishing in
  45 s to 1.7 s, the same as before the split.

- An out-of-range numeric literal (`1e542`, `1e400`, `1d400`, `1e308`,
  `1.5f342`) is reported as a parse error instead of raising
  `FLOATING-POINT-OVERFLOW` out of the parser. Coercing the literal to
  the float format its exponent marker selects can overflow, and that is
  an `error` rather than an esrap parse error, so it used to escape
  `parse-chunk` and reach the caller as an unhandled condition. The
  reader rejects every one of these literals
  (`READER-IMPOSSIBLE-NUMBER-ERROR`), so the file is unreadable either
  way and the two agree; `1e308` is rejected while `1d308` parses,
  because `e` selects the default single format and `d` selects double.
- Rejecting a file with a syntax error no longer takes far longer than
  accepting one. Source is parsed in chunks whose ends esrap confirms
  are form boundaries, and a chunk that fails is retried against a
  longer range, because a cut can land mid-form. On a file that is
  genuinely broken no candidate ever succeeds, so the scan re-read the
  same growing prefix once per remaining candidate: a 174 KB file took
  25 s to reject, with 123 failed attempts over the same text. The
  scan now distinguishes the two cases by where the failure is reported
  -- moving forward with the range end means the range was too short,
  staying put means the text is broken there, and the scan steps past
  it. That file is rejected in 1.7 s, the same as it takes to accept
  `asdf.lisp`, and the reported position is more accurate (it names
  the actual stray paren). No file in the corpus is now slow to parse.

### Known divergences

- A dotted tail followed by a `#-`/`#+` form that the reader never
  reads (`(f (a b . #-no-such-feature (x y) #+no-such-feature ()))`)
  is read by SBCL as if the vanished branch contents were spliced into
  the enclosing list. cl-toolkit rejects it, which is what the
  standard's grammar for `.` requires. Two corpus files rely on it
  (SBCL's `frlock.lisp` and `ir1-translators.lisp`).

  This is not reproduced on purpose. The reader's behaviour is not
  self-consistent: a single not-taken `#-` branch contributes its
  target's elements, a following not-taken `#-` branch is an error,
  a following not-taken `#+` branch is silently discarded, and `#-`
  and `#+` disagree for the same absent feature -- so there is no rule
  to state, let alone implement. Since the file is refused either way,
  the error now names the construct instead of pointing at the
  conditional that follows the tail, which is where the offset used to
  land.
### Added

- **Lint as a rule registry with stable machine output.** Diagnostics
  are plists (`rule`, `severity`, `line`, `col`, `start`, `end`,
  `message`, `fix`); `lint --format json` emits
  `{"ok":bool,"diagnostics":[...]}` and `--rules` selects a subset.
  Rules: `duplicate-top-level`, `redefined-top-level` (same head+name,
  different bodies; byte-identical copies stay with duplicates),
  `empty-operator` (`()` in operator position only — bare and quoted
  NIL are clean), `eval-hazard` (`#.` and `(eval ...)`),
  `sharp-underscore-dispatch` (portability: this SBCL build rejects
  `#_`), and `skipped-conditional-branch` (info: branches the reader
  never reads). Legacy text output and exit behavior are unchanged.
- **`rename`, `wrap-form`, `unwrap-form` structural commands** (by
  name, index, or position) plus matching `batch-replace` operations
  (`rename-name`, `wrap-name`, `unwrap-name`). Rename touches only the
  definition/operator slot; wrap splices both halves atomically;
  unwrap accepts single-child lists only. Batch failures now report
  `Batch edit N (OPERATION) failed: cause`.
- **`format --check`** for CI: exit 0 when stable, otherwise the diff
  on stdout and exit 1; never writes.
- `[ ] { }` are constituent characters, not list delimiters
  (`[1]` is the symbol `|1|`): symbols may start with brackets, and
  balance/comma/format/chunking no longer count them as nesting.

### Tests

- FiveAM 139 -> 290 checks (reader regressions, balance/format,
  single/batch edit ops, match-ambiguity policies, move directions),
  then to 549 with lint rules (schema, portability, redefinition,
  empty-operator, eval hazards), rename/wrap/unwrap spans and batch ops,
  and batch error annotation, then to 1062 with the located-literal and first-error checks,
  formatter round trip, edit-operation properties, the machine-output
  contract and bounded rejection.
- CLI matrix 62 -> 102 checks, then to 120 with lint JSON/rules,
  format --check, rename/wrap/unwrap, and batch rename/wrap paths, then
  to 124 with the positional-FILE argument; format 21 -> 26; bugfix
  18 -> 22.
- **Reader coverage from a 455-file/17-lib sweep (alexandria, babel,
  trivia, iterate, cffi, hunchentoot, ...): 3 parse errors + 3
  balance disagreements, all fixed, sweep now fully clean.**
  - Unicode symbols (`:λlist`, `λlist`): `alpha` is now
    `alpha-char-p`-based instead of ASCII-only. Single-char literals
    keep their case (`#\a` value `"a"`, was `"A"`).
  - Backquote/comma: `` `(a ,b ,@c) `` is one `BACKQUOTE` form with
    `UNQUOTE`/`UNQUOTE-SPLICING` children (was a stray `` ` `` symbol
    plus the list).
  - `#+`/`#-`, `#S`, `#C`, `#P`, `#N A`, `#*`: single wrapped forms
    (were stray marker symbols inflating top-level counts).
  - Symbol escapes and bar segments: `\a`, `|a b|`, `|(a)|` (iterate
    test suite failed to parse before).
  - Balance `#`-dispatch consumed the char after `#`, swallowing `)`
    in `#1#` (alexandria/babel false "unbalanced").
  - Format dropped the space after block comments (`#| hi |# (a)`):
    `format-dispatch-hash` discarded the cleared indent flag.
- **`make build` always exited non-zero** (`ext:quit` package does not
  exist in SBCL): now `uiop:quit`.
- **`offset-to-line-col-inverse` clamped OOB positions to EOF**,
  silently appending on bad `--line/--col`: now signals, and edit
  commands fail loudly (insert/text still allows the exact EOF spot).

## [0.5.3] - 2026-08-24

### Fixed

- **P1: --compile-check false-positived on every multi-file-project
  file.** Compiling in cl-toolkit's image made (in-package #:proj) a
  read-time error, so files depending on sibling packages could never
  pass. Two flags, cheapest-first per field report:
  - --compile-check-package PKG: stub-creates PKG (:use CL) when
    missing — the single-file-against-project-package case.
  - --compile-check-system SYS: asdf:load-system SYS first — full
    fidelity for real systems. Flags compose.
  Rollback behavior unchanged and re-verified byte-identical.

### Corrected

- Anomaly diagnosis in 0.5.2 notes was too narrow: field evidence
  reproduces the unbound-variable failures in fresh processes,
  deterministically per script shape — not an image-hygiene artifact.
  Reproduction scripts: /tmp/opencode/int44-int62b.

## [0.5.2] - 2026-08-24

### Added

- **--compile-check** on every write-capable command (field item D4):
  after --write, the file is compile-filed in-process; on error the
  write is rolled back from the rolling backup and the reason rides
  both channels. The B1/extract-clause-P0 class — valid syntax,
  illegal code — now dies at the tool layer with no external harness.
  Warning-severity findings (e.g. undefined functions in the edited
  file) do not fail the check; error severity does.

### Notes

- The process-expression redo anomaly (unbound-variable with fboundp=T)
  remains outside toolkit charter — build/runtime introspection. The
  SCRIPTING-CONTRACT mtime/ASDF section covers the suspected class;
  rename-based writes do refresh mtime, so fresh processes are safe.

## [0.5.1] - 2026-08-24

### Fixed

- **P0: extract-clause emitted illegal code for cond clauses.** A
  (TEST BODY...) clause placed verbatim as a defun body is an illegal
  function call — invisible to placement/lint/parse gates. Shape-based
  auto-detection proved impossible (cond clause vs plain call are both
  multi-child lists), so per the report's own fallback principle the
  tool now REFUSES multi-child spans unless intent is explicit:
  --when promotes as (when TEST BODY...); --as-expression places
  verbatim; atoms refuse without --as-expression.

### Added

- **Compile gate in cli-matrix.sh**: every fixture a matrix case writes
  is compile-filed; illegal-code generation is now a release blocker.
  This is the product-level check that would have caught both the B1
  class and this P0 — mechanics tests verify the tool, compilation
  verifies its output.

## [0.5.0] - 2026-08-22

### Added

- **extract-clause** — move-clauses v1, atomic per the field post-mortem:
  clause extraction, call-splice, and new-defun placement are computed
  in memory and applied as ONE write, so the incoherent intermediate
  state of the manual two-move protocol never exists on disk. Closer
  arithmetic impossible by construction (both edits are between-sibling
  splices). Full ambiguity policy on the clause anchor.
      extract-clause -f X --name dispatch --match "(probe tok)" 
        --as handle-unknown --lambda-list "(tok)" 
        --call "(handle-unknown tok)" --write
- **source-of --select PATH** — slash-separated child-index chain
  (e.g. --select 3/0/1) returning verbatim subform source. Replaces
  most remaining AST-walking.
- **--occurrence N** on replace-form/insert-in — select the Nth
  --match occurrence directly (completes the 0.4.3 ambiguity policy).
- **docs/SCRIPTING-CONTRACT.md** — the stable scripting interface:
  channel semantics, position/anchor rules, guard table, backup
  behavior, and the ASDF freshness caveat when driving builds from
  long-lived images.

### Fixed

- split-string-on-newlines returned NIL for strings without the
  separator (loop return skipped finally); latent, benign there, but
  fatal for the new path splitter — both now use loop-finish.

## [0.4.3] - 2026-08-22

### Fixed

- **Ambiguous --match no longer silently takes the first occurrence**
  (field finding: 8 identical clauses, deterministic-first replaced one
  with exit 0). Policy now mirrors --contains: >1 matches refuse and
  list every occurrence's [line, col] + preview; --first opts into
  first-match; --match N (occurrence selection) noted as future work.
  Applies to replace-form and insert-in (shared anchor resolution).

## [0.4.2] - 2026-08-22

### Fixed

- Result-validation failures (the last stderr-only path) now emit the
  failure JSON on stdout as well. With this, every refusal/diagnosis in
  the toolkit — pre-write guards, post-write validation, and edit
  errors — rides both channels. Single-channel captures cannot lose a
  diagnosis anywhere in the contract.

## [0.4.1] - 2026-08-22

### Fixed

- patch-span refusals (anchor mismatch, depth guard) printed only to
  stderr — filtered captures saw silence after/without the Patching
  line. Refusal reasons now also emit the standard failure JSON on
  stdout, so ANY single captured channel carries the diagnosis.
  Field anomaly (exit 1 twice, reason invisible) traced to this: the
  depth guard had almost certainly fired legitimately on near-identical
  fragments whose diagnostics were filtered away.

## [0.4.0] - 2026-08-22

### Added

- **`insert-in --name F [--match CLAUSE] --insert CODE`** — scope-aware
  insertion: splices between existing siblings of F, so paren balance is
  preserved by construction. Without --match, appends after F's last
  child. The wrap problem that started the gap-report thread.
- **`--after-anchor SNIPPET`** on insert-form and append-form — position
  resolves to just past the unique occurrence of SNIPPET (ambiguity
  refuses with count). Positions survive unrelated edits; no line
  arithmetic. append-form inherits the anchor's line indentation.
- **`patch-span --find-old`** — locates `--old` uniquely anywhere in the
  file instead of requiring line/col; kills the bottom-up line-tracking
  dance entirely.
- **`source-of --child-index K`** — verbatim source of the K-th direct
  child of a named form (first slice of subform path addressing).
- **`test/cli-matrix.sh`** (also `make ci`-able via direct run) — 29
  per-command exit-code assertions covering every command's happy path
  and refusal path. Born from the B1 lesson: announced ≠ shipped, so the
  matrix is the release gate now.

### Fixed

- insert-form's own `:required` line/col flags blocked --after-anchor
  mode (B1-class bug caught by the new matrix before release).

### Deferred

- `move-clauses` (control-flow extraction with closer management):
  needs a real restructuring engine; design note pending. Today the
  patch-span + shape-guard + insert-in trio covers the same moves with
  explicit steps.

## [0.3.4] - 2026-08-22

### Fixed

- append-form/insert-at rejected `--code-file` because clingon's
  `:required` check on `--insert` fired before code-file normalization
  (0.3.2 announcement promised these). Requirement relaxed; handler-side
  guard errors with exit 1 when neither source is given.

### Noted

- Argument-parse failures exit **64** (clingon convention) — nonzero, so
  the 0.3.3 boolean contract holds across that path too.

## [0.3.3] - 2026-08-22

### Fixed

- **Exit codes are now trustworthy on every failure path**: JSON
  `{"success":false,...}` reports exited 0, forcing consumers to grep
  output — and diffs legitimately contain the word "error" (e.g.
  `(error 'calc-error ...)` clauses), producing false failures.
  All error reports now exit 1; success stays 0. Script checks should
  be `if cl-toolkit ...; then` — no text grepping.

## [0.3.2] - 2026-08-22

### Added

- Quoting-free code input on replace-form / append-form / insert-form /
  patch-span: `--code-file PATH` reads replacement from a file, and a
  value of `-` (e.g. `--replace -`) reads stdin. Sidesteps the bash
  sharp-quote trap entirely — no python subprocess layer needed:

      cl-toolkit replace-form -f X.lisp --name f \
        --code-file /tmp/new-body.lisp --write

      cat new-body.lisp | cl-toolkit replace-form ... --replace -

## [0.3.1] - 2026-08-22

### Fixed

- Preview stats banner printed a malformed label (`(""42002)`);
  now `Preview stats: 2 -> 2 lines (2 bytes)` / `(no changes)`.
- Process substitution (`diff-forms -f <(git show HEAD:x.lisp)`) read as
  empty content: named pipes have no usable FILE-LENGTH, so reading now
  falls back to stream-to-EOF.

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
