# cl-toolkit 0.2.0 — What Changed for You

*A guide for the calc project's editing workflows. Every entry traces back
to something that cost you time or data in the field reports.*

---

## TL;DR

The three things that bit you hardest are now structurally impossible:

| Old failure | Now |
|---|---|
| Position edits silently hit the wrong form | **Fail loudly** unless a form *starts* exactly at your coordinates |
| `top-level --names` line numbers didn't match `--line` args | **One 0-based convention** everywhere — display values paste back into args |
| `--write` on a bad path created stray files | Refuses if file doesn't exist; paths normalized; optional timestamped snapshots |

Plus four new capabilities built around your read-modify-write loop:
`source-of`, `--match`, `find-forms`, `split-forms`.

---

## 1. Your Habits That Must Change

### 1a. Position targeting fails instead of guessing

```bash
# Pointing INSIDE a form (old behavior: picked whatever contained it)
cl-toolkit replace-form -f evaluator.lisp --line 789 --col 22 ...
# → ERROR: No form starts exactly at line 789, col 22
#          -- pass --nearest for containment match
```

- Destructive ops (`replace-form`, `delete-form`) require the form's
  opening paren to be exactly at your line/col.
- `find` (read-only) still uses containment matching and now prints the
  resolved `[line, col]` so you can see what it locked onto.
- Truly want the old behavior? `--nearest`. But the loud error + retry is
  cheaper than a corrupted ternary.

**Every non-quiet edit also announces its target first:**
```
Replacing form 'calc-true-p' [line 790, col 24]
```
Read that line. If the name or position surprises you, abort.

### 1b. All line numbers are 0-based

`grep -n` gives you 1-based lines — subtract 1 before passing `--line`.
In exchange, anything cl-toolkit displays (`[line N]`, `top-level
--names`) can be fed straight back without conversion. No more mixed
conventions.

### 1c. `format` no longer restyles working files

```
cl-toolkit format -f X.lisp            # minimal: jams + unindented bodies only
cl-toolkit format -f X.lisp --canonical  # full restyle (explicit opt-in)
```

Your 999-line-diff problem is gone; jammed forms get split, broken
indentation gets fixed, everything else stays byte-identical.
(Limitation: fully single-line nested forms are detected but not yet
expanded — that needs a real pretty-printer.)

---

## 2. New Workflow Capabilities

### 2a. `source-of` — stop reconstructing 150-line functions from memory

The safe mechanical loop:

```bash
# 1. Extract verbatim current source
cl-toolkit source-of -f evaluator.lisp --name dispatch-token > /tmp/form.lisp

# 2. Edit /tmp/form.lisp any way you like

# 3. Write back by name (never by remembered content)
cl-toolkit replace-form -f evaluator.lisp --name dispatch-token \
  --replace "$(cat /tmp/form.lisp)" --write
```

Validation still guards step 3: if your edited fragment is unbalanced,
nothing touches disk.

### 2b. `replace-form --match` — one clause instead of whole-function rewrite

```bash
cl-toolkit replace-form -f evaluator.lisp --name dispatch-token \
  --match '(string= tok "?")' \
  --replace '(string= tok "?" :test (function equal))'
```

Finds the smallest subform inside the target whose text matches
(exact trimmed match preferred, contains-match fallback). This was the
"realistic fix size is one clause" gap.

### 2c. `find-forms --contains` — catch duplicated defects systematically

```bash
cl-toolkit find-forms -f src/evaluator.lisp --contains "(nreverse args)" --with-source
[{"index":7,"name":"expand-macro-call","line":402,"col":0,"source":"..."}]
```

Would have found your lambda-argument-reversal twin without relying on
memory of the pattern.

### 2d. `split-forms` — the two-defuns-on-one-line fix

```bash
cl-toolkit split-forms -f X.lisp --write
```

Only inserts newlines at jammed boundaries. Nothing else moves.
Idempotent.

### 2e. Backups on your terms

```bash
cl-toolkit ... --write                          # rolling X.bak (default, unchanged)
cl-toolkit ... --write --backup-dir .snapshots  # + timestamped pre-edit copies
cl-toolkit ... --write --no-backup              # scripts/batch use
```

`*.bak` is now in `.gitignore`.

### 2f. batch-replace speaks names now

```bash
cl-toolkit batch-replace -f X.lisp --edits '[
  {"operation":"delete-name","name":"dead-func"},
  {"operation":"replace-name","name":"is-stack-op","code":"..."}
]'
```

Name edits run first; index edits still apply highest-index-first (your
descending-order insight, verified).

---

## 3. Incident-by-Incident Closure

| Report # | Incident | Resolution |
|---|---|---|
| 1 | preview/write divergence | Result computed once per run; every edit logs resolved target before writing; root causes (#5 path bug, #2 line convention) eliminated |
| 2 | off-by-one line mapping | Mixed conventions unified to 0-based |
| 3 | adjacent-form collisions | Exact-match default makes this fail loudly; `--nearest` opt-in |
| 4 | plugin previews corrupt | Client-side JS diff generator deleted; previews come from CLI diff |
| 5 | relative-path `.bak` mangling | Absolute normalization + refuse-missing-file on write |
| 6 | no source extraction | `source-of` |
| 7 | no subform addressing | `--match` |
| 8 | no structural search | `find-forms --contains` |
| 9 | whitespace jams unreachable | `split-forms` + minimal `format` |
| 10 | quoting friction | Documented `(function f)` / `(quote x)` spellings; `#'` parse bug fixed |

Also fixed en route: validation failures print one compact line
(`Syntax error at Line L, Column C, Position N`) instead of esrap's
multi-page dump that garbled your TUI.

---

## 4. Quick Reference

```bash
# Discover
cl-toolkit top-level -f X.lisp --names        # indices + 0-based positions
cl-toolkit find-forms -f X.lisp --contains "..." [--with-source]
cl-toolkit source-of -f X.lisp --name FOO     # exact text

# Surgical edit
cl-toolkit replace-form -f X.lisp --name FOO --match "SNIPPET" --replace "NEW"
cl-toolkit replace-form -f X.lisp --end --replace "..." --pretty

# Bulk
cl-toolkit batch-replace -f X.lisp --edits '[{"operation":"replace-name",...}]'

# Repair
cl-toolkit split-forms -f X.lisp --write
cl-toolkit format -f X.lisp            # minimal
cl-toolkit format -f X.lisp --canonical  # full restyle

# Safety rails
--preview        # diff only
--backup-dir D   # timestamped snapshots
--no-validate-input/result   # last resort only — never needed for balanced code
```

*Version 0.2.0 · commit `24cd675` · full technical detail in the repo's
`CHANGELOG.md`*
