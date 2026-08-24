#!/bin/bash
# Per-command CI matrix: asserts exit codes and channel discipline for
# every user-facing command. Born from the B1 incident (append-form
# --code-file shipped announced-but-broken in 0.3.2): every command's
# happy path AND one failure path must be exercised, not just released.
set -u
BIN="${1:-./build/cl-toolkit}"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0

check() { # name expected_exit cmd...
  local name="$1" want="$2"; shift 2
  "$@" >"$TMP/out" 2>"$TMP/err"
  local got=$?
  if [ "$got" -eq "$want" ]; then PASS=$((PASS+1));
  else FAIL=$((FAIL+1)); echo "FAIL: $name (exit $got, want $want)"; fi
}

mk() { printf '%s\n' "$2" > "$TMP/$1"; }

# fixtures
mk base.lisp '(defun alpha () 1)
(defun beta ()
  (gamma))
'
mk body.lisp '(defun new-body () (function list))'

# --- read-only commands ---
check parse 0 "$BIN" parse --file "$TMP/base.lisp"
check validate 0 "$BIN" validate --file "$TMP/base.lisp"
check top-level 0 "$BIN" top-level --file "$TMP/base.lisp" --names --preview-chars 20
check find-forms 0 "$BIN" find-forms -f "$TMP/base.lisp" --contains gamma
check source-of 0 "$BIN" source-of -f "$TMP/base.lisp" --name beta
check child-index 0 "$BIN" source-of -f "$TMP/base.lisp" --name beta --child-index 0
check balance 0 "$BIN" balance --code "(let ((x 1))" --expect-delta 1
check balance-wrong-delta 1 "$BIN" balance --code "(let ((x 1))" --expect-delta 0
check check-anchor-unique 0 "$BIN" check-anchor -f "$TMP/base.lisp" --text "(gamma)"
check check-anchor-multi 1 "$BIN" check-anchor -f "$TMP/base.lisp" --text "("
check lint-clean 0 "$BIN" lint -f "$TMP/base.lisp"

# --- edit commands: success paths (exit 0) ---
cp "$TMP/base.lisp" "$TMP/e1.lisp"
check replace-name 0 "$BIN" replace-form -f "$TMP/e1.lisp" --name alpha --code-file "$TMP/body.lisp" --write --quiet
cp "$TMP/base.lisp" "$TMP/e2.lisp"
if printf '(defun tail () :ok)' | "$BIN" append-form -f "$TMP/e2.lisp" --end --insert - --write --quiet >/dev/null 2>&1; then PASS=$((PASS+1)); else FAIL=$((FAIL+1)); echo "FAIL: append-stdin"; fi
cp "$TMP/base.lisp" "$TMP/e3.lisp"
check insert-after-anchor 0 "$BIN" insert-form -f "$TMP/e3.lisp" --after-anchor "1)" --insert "(mid-clause)" --write --quiet
cp "$TMP/base.lisp" "$TMP/e4.lisp"
check insert-in 0 "$BIN" insert-in -f "$TMP/e4.lisp" --name beta --match "(gamma)" --insert "(delta)" --write --quiet
cp "$TMP/base.lisp" "$TMP/e5.lisp"
check patch-span-find-old 0 "$BIN" patch-span -f "$TMP/e5.lisp" --find-old --old "(gamma)" --new "(delta)" --write --quiet
cp "$TMP/base.lisp" "$TMP/e6.lisp"
check split-forms 0 "$BIN" split-forms -f "$TMP/e6.lisp" --write --quiet
check delete-match 0 sh -c "cp '$TMP/base.lisp' '$TMP/e7.lisp' && $BIN replace-form -f '$TMP/e7.lisp' --name beta --match '(gamma)' --delete-match --write --quiet"

# --- edit commands: failure paths MUST exit nonzero ---
cp "$TMP/base.lisp" "$TMP/f1.lisp"
check replace-missing-name 1 "$BIN" replace-form -f "$TMP/f1.lisp" --name nope --replace "(x)" --write
check replace-shape-guard 1 "$BIN" replace-form -f "$TMP/f1.lisp" --name alpha --replace "(defun a () 1)(defun b () 2)" --write
check replace-unbalanced 1 "$BIN" replace-form -f "$TMP/f1.lisp" --name alpha --replace "(unclosed" --write
check delete-missing 1 "$BIN" delete-form -f "$TMP/f1.lisp" --name nope --write
check anchor-ambiguous 1 "$BIN" patch-span -f "$TMP/f1.lisp" --find-old --old "(" --new "x" --allow-shift
check anchor-missing 1 "$BIN" patch-span -f "$TMP/f1.lisp" --find-old --old "(zzz)" --new "(x)"
check depth-guard 1 "$BIN" patch-span -f "$TMP/f1.lisp" --find-old --old "(gamma)" --new "(+ (gamma)" --write
check extract-atomic 0 sh -c "cp '$TMP/base.lisp' '$TMP/e8.lisp' && $BIN extract-clause -f '$TMP/e8.lisp' --name beta --match '(gamma)' --as gamma-helper --lambda-list '()' --call '(gamma-helper)' --write --quiet"
check extract-missing-clause 1 "$BIN" extract-clause -f "$TMP/base.lisp" --name beta --match "(zzz)" --as g --lambda-list "()" --call "(g)"
check select-path 0 sh -c "$BIN source-of -f '$TMP/base.lisp' --name beta --select '3/0' >/dev/null"
check select-bad-path 1 "$BIN" source-of -f "$TMP/base.lisp" --name beta --select "9/9"
# batch-replace match ops (0.5.5) — B1 lesson: prove features, don't announce them
check batch-replace-match 0 sh -c "cp '$TMP/base.lisp' '$TMP/bm.lisp' && $BIN batch-replace -f '$TMP/bm.lisp' --edits '[{\"operation\":\"replace-match\",\"match\":\"(gamma)\",\"code\":\"(delta)\"}]' --write --quiet && grep -c '(delta)' '$TMP/bm.lisp' | grep -q 1"
check batch-replace-match-ambiguous 1 sh -c "$BIN batch-replace -f '$TMP/base.lisp' --edits '[{\"operation\":\"replace-match\",\"match\":\"a\",\"code\":\"z\"}]' --write --quiet"
check batch-replace-delete-match 0 sh -c "cp '$TMP/base.lisp' '$TMP/bd.lisp' && $BIN batch-replace -f '$TMP/bd.lisp' --edits '[{\"operation\":\"delete-match\",\"match\":\"(gamma)\"}]' --write --quiet && ! grep -q gamma '$TMP/bd.lisp'"
check occurrence-select 0 sh -c "$BIN replace-form -f '$TMP/base.lisp' --name alpha --match '1' --occurrence 1 --replace '2' --preview >/dev/null 2>&1"
check match-ambiguous 1 "$BIN" replace-form -f "$TMP/f1.lisp" --name beta --match "a" --replace "z"
check match-ambiguous-first 0 sh -c "$BIN replace-form -f '$TMP/f1.lisp' --name beta --match 'a' --replace 'z' --first --preview >/dev/null 2>&1"
check missing-code 1 "$BIN" append-form -f "$TMP/f1.lisp" --end --write
check code-file-broken-append 1 "$BIN" append-form -f "$TMP/f1.lisp" --end --insert "(unclosed" --write
check write-nonexistent 1 "$BIN" replace-form -f "$TMP/nope-$$.lisp" --index 0 --replace "(x)" --write
# D4: compile-check rolls back illegal-code output (file must survive untouched)
check compile-check-rollback 1 sh -c "cp '$TMP/base.lisp' '$TMP/cc.lisp' && cp '$TMP/cc.lisp' '$TMP/cc.orig' && $BIN replace-form -f '$TMP/cc.lisp' --name beta --replace '(defun beta () ((string= \"a\" \"a\") 1))' --compile-check --write --quiet >/dev/null 2>&1; diff -q '$TMP/cc.lisp' '$TMP/cc.orig' >/dev/null"
check compile-check-pass 0 sh -c "cp '$TMP/base.lisp' '$TMP/cc2.lisp' && $BIN replace-form -f '$TMP/cc2.lisp' --name alpha --replace '(defun alpha () (function list))' --compile-check --write --quiet"
# project-package files: read-time in-package error without the stub, pass with it
mk proj.lisp '(in-package #:calc)

(defun calc-fn (x) (* x 2))
'
check cc-proj-no-flag 1 "$BIN" replace-form -f "$TMP/proj.lisp" --name calc-fn --replace '(defun calc-fn (x) (* x 3))' --compile-check --write --quiet
check cc-proj-package-flag 0 "$BIN" replace-form -f "$TMP/proj.lisp" --name calc-fn --replace '(defun calc-fn (x) (* x 3))' --compile-check --compile-check-package calc --write --quiet
check unknown-command 64 "$BIN" definitely-not-a-command

# --- cond-clause extraction modes (0.5.1) ---
mk cond.lisp '(defun resolve-token (upper)
  (cond
    ((string= upper "TAU") (* 2 pi))
    (t nil)))
'
check extract-cond-refused-without-mode 1 "$BIN" extract-clause -f "$TMP/cond.lisp" --name resolve-token --match '((string= upper "TAU") (* 2 pi))' --as tau-of --lambda-list '(u)' --call '(tau-of u)'
check extract-cond-when-mode 0 "$BIN" extract-clause -f "$TMP/cond.lisp" --name resolve-token --match '((string= upper "TAU") (* 2 pi))' --as tau-of --lambda-list '(u)' --call '(tau-of u)' --when --write --quiet
check extract-expr-mode 0 sh -c "cp '$TMP/base.lisp' '$TMP/expr.lisp' && $BIN extract-clause -f '$TMP/expr.lisp' --name alpha --match '1' --as one-of --lambda-list '()' --call '(one-of)' --as-expression --write --quiet"
check extract-atom-refused 1 sh -c "cp '$TMP/base.lisp' '$TMP/atom.lisp' && $BIN extract-clause -f '$TMP/atom.lisp' --name alpha --match 'alpha' --as a2 --lambda-list '()' --call '(a2)' --write --quiet"

echo "---"
# --- PRODUCT GATE: every written fixture must COMPILE ---
# Mechanics checks (placement/lint) passed B1 and the cond-clause P0;
# only compiling the output catches illegal-code generation.
if command -v sbcl >/dev/null; then
  for f in "$TMP"/e*.lisp "$TMP"/cond.lisp "$TMP"/expr.lisp; do
    [ -e "$f" ] || continue
    if F="$f" OUT="$TMP/gate.fasl" sbcl --noinform --non-interactive \
         --eval '(unless (compile-file (uiop:getenv "F") :output-file (uiop:getenv "OUT")) (uiop:quit 1))' \
         --eval '(uiop:quit 0)' >/dev/null 2>&1; then
      PASS=$((PASS+1))
    else
      FAIL=$((FAIL+1)); echo "FAIL: compile-gate $f"
    fi
    rm -f "$TMP/gate.fasl"
  done
else
  echo "WARN: sbcl not found; compile gate skipped"
fi
echo "CI matrix: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
