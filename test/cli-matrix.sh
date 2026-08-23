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
check match-ambiguous 1 "$BIN" replace-form -f "$TMP/f1.lisp" --name beta --match "a" --replace "z"
check match-ambiguous-first 0 sh -c "$BIN replace-form -f '$TMP/f1.lisp' --name beta --match 'a' --replace 'z' --first --preview >/dev/null 2>&1"
check missing-code 1 "$BIN" append-form -f "$TMP/f1.lisp" --end --write
check code-file-broken-append 1 "$BIN" append-form -f "$TMP/f1.lisp" --end --insert "(unclosed" --write
check write-nonexistent 1 "$BIN" replace-form -f "$TMP/nope-$$.lisp" --index 0 --replace "(x)" --write
check unknown-command 64 "$BIN" definitely-not-a-command

echo "---"
echo "CI matrix: $PASS passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
