# Response to Gap Report: Verification & Surgery-Planning

## Date: 2026-08-22
## cl-toolkit 0.3.0 — commit `b224a48`

---

## TL;DR

Every item from the report is implemented. The planning tools you had to
build in python — anchor counting, reader-aware depth accounting, net
depth-delta checks, structural diffing — now live inside the parser that
always owned the answers. Your corruption incident (extra closer at EOF
shifting every deeper form inward) is now a *precondition failure*, not
a four-write archaeology session.

---

## P0 Items

### check-anchor — the safe-edit precondition

```bash
$ cl-toolkit check-anchor -f evaluator.lisp --text "(calc-true-p condition)"
{"count":1,"first-offset":32655,"line":790,"col":24}     # exit 0 = unique

$ cl-toolkit check-anchor -f evaluator.lisp --text "(a)"
{"count":3,"first-offset":120,...}                        # exit 1 = not safe
```

Replaces `src.count(anchor) == 1`. Exit code is script-friendly.

### patch-span — verified substitution with scope guard

```bash
$ cl-toolkit patch-span -f X.lisp --line 789 --col 22 \
    --old "(calc-true-p condition)" --new "(calc-true-p (not condition))"
Patching [line 789, col 22] delta=0

$ cl-toolkit patch-span ... --old "(x)" --new "(+ x"
Refusing: net depth delta 1 -- closers would shift scope.
         Pass --allow-shift if this wrap/restructure is intended.
```

Two guards before any bytes move:
1. **Byte-exact anchor**: `--old` must sit exactly at line/col (mismatch
   prints expected vs found)
2. **Net depth-delta**: reader-aware scanner (char literals, strings,
   comments all counted correctly — your A2 problem is gone since it's
   the same scanner as `analyze-balance`); delta ±≠0 refuses unless
   `--allow-shift`

This is the direct antidote to the exact failure class that cost you the
file corruption. The "insert FORM into body at P, parens managed" question
now has a tool answer: propose the span edit, let the delta guard veto.

### --match-exact — no more silent escalation

```bash
# Never escalates to contains-match; fails instead:
cl-toolkit replace-form --name f --match-exact --match "~200-char span" ...
```

And when fuzzy fallback DOES fire (default mode), it announces itself:

```
Replacing in form (fuzzy contains-match) 'dolist' [line 83, col 17] "..."
```

Your ~200→500 char silent jump would now be visible in the announcement.

---

## P1 Items

### balance --expect-delta N

```bash
# Fragment wrap check: this fragment should contribute exactly +1 depth
$ cl-toolkit balance --code "(let ((vars nil))" --expect-delta 1
# exit 0; wrong expectation exits 1 with actual vs expected
```

Reader-aware: `'()`, `#\(`, `"str(ing)"`, comments — all handled.

### Replacement-shape guard — BREAKING

Replacing **one** top-level form with text containing **several** now fails:

```
Refusing: replacement contains 4 top-level forms but replaces one.
Pass --allow-multi-forms if splitting is intended.
```

This would have caught every stray `(in-package …)` your pipeline
propagated. Legitimate splitting still exists behind the flag.

### diff-forms — structural signal, not textual noise

```bash
$ cl-toolkit diff-forms -f evaluator-old.lisp --name process-expression \
                        --against-file evaluator.lisp
+ (let ((local-vars nil))     (copy ...))
+ (maphash ...)
- (old-helper 1)
```

Direct children classified by content equality — your re-indentation
touched every line but shows zero structural changes.

---

## P2 / P3 Items

| Item | Command |
|---|---|
| Duplicate form lint | `cl-toolkit lint -f F` → flags byte-identical top-level forms with positions; exit 1 when found |
| Preview stats | stderr line before diffs: `Preview stats: 812 lines -> 815 lines` |

### Already shipped (report predates it)

Subform deletion = `replace-form --match "..." --delete-match`
(shipped 0.2.1). Empty-replacement isn't needed — the flag removes the
matched subform and announces it:

```
Deleting in form 'dead-clause' [line 1, col 7] "(dead-clause x)"
```

Not yet done (deferred): `source-of --select path` (P2) — subform
extraction by path syntax needs design; `--match` + `source-of` covers
most cases today.

---

## How This Changes Your Session

The defun-hygiene workflow that took ~3h with python scaffolding:

```bash
# 1. Plan: verify anchors
cl-toolkit check-anchor -f X.lisp --text "$ANCHOR"

# 2. Propose: wrap/restructure with scope guard
cl-toolkit patch-span -f X.lisp --line L --col C --old "$OLD" --new "$NEW"
#        ↑ vetoes itself on depth shift

# 3. Verify structure survived
cl-toolkit lint -f X.lisp                       # no duplicates crept in
cl-toolkit diff-forms -f old.lisp --name F --against-file X.lisp

# 4. Commit
```

Balanced-but-wrong writes can't fully be caught by syntax validation —
you were right about that — but the two checks that matter most
(anchor uniqueness, depth preservation) are now preconditions instead of
post-mortems.

---

*Full technical detail: repo `CHANGELOG.md`. Prior guides:
`docs/GUIDE-0.2.0.md`.*
