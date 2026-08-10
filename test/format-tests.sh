#!/usr/bin/env bash
# Comprehensive tests for cl-toolkit format command

set -e

BIN="build/cl-toolkit"
PASS=0
FAIL=0

pass() { echo "PASS: $1"; PASS=$((PASS+1)); }
fail() { echo "FAIL: $1"; echo "  got: $2"; echo "  expected: $3"; FAIL=$((FAIL+1)); }

check() {
  local desc="$1" input="$2" expected="$3"
  result=$($BIN format --code "$input" 2>&1)
  if [ "$result" = "$expected" ]; then
    pass "$desc"
  else
    fail "$desc" "$result" "$expected"
  fi
}

# --- Whitespace normalization ---
echo "--- Whitespace normalization ---"

check "extra spaces between tokens" \
  "(defun   foo(x)  (+  x 1))" \
  "(defun foo(x) (+ x 1))"

check "tabs normalized to spaces" \
  "$(printf '(defun\tfoo\t(+\tx\t1))')" \
  "(defun foo (+ x 1))"

check "trailing whitespace removed" \
  "(defun foo (x)   )" \
  "(defun foo (x) )"

# --- Basic indentation ---
echo "--- Basic indentation ---"

check "defun body indented 2 spaces" \
  "$(printf '(defun add (a b)\n  (+ a b))')" \
  "$(printf '(defun add (a b)\n  (+ a b))')"

check "defun with no body" \
  "(defun noop nil)" \
  "(defun noop nil)"

check "nested parens" \
  "(+ (* a b) (/ c d))" \
  "(+ (* a b) (/ c d))"

# --- Multiple forms ---
echo "--- Multiple forms ---"

check "two functions" \
  "$(printf '(defun add (a b)\n  (+ a b))\n\n(defun sub (a b)\n  (- a b))')" \
  "$(printf '(defun add (a b)\n  (+ a b))\n\n(defun sub (a b)\n  (- a b))')"

# --- cond ---
echo "--- cond indentation ---"

check "cond body indented" \
  "$(printf '(defun classify (x)\n  (cond\n    ((> x 0) \"pos\")\n    ((< x 0) \"neg\")\n    (t \"zero\")))')" \
  "$(printf '(defun classify (x)\n  (cond\n    ((> x 0) \"pos\")\n    ((< x 0) \"neg\")\n    (t \"zero\")))')"

# --- let ---
echo "--- let indentation ---"

check "let body indented" \
  "$(printf '(defun example (x)\n  (let ((y (+ x 1)))\n    y))')" \
  "$(printf '(defun example (x)\n  (let ((y (+ x 1)))\n    y))')"

# --- if ---
echo "--- if indentation ---"

check "if then/else indented" \
  "$(printf '(defun abs (x)\n  (if (> x 0)\n      x\n      (- x)))')" \
  "$(printf '(defun abs (x)\n  (if (> x 0)\n    x\n    (- x)))')"

# --- #\ (char literal) ---
echo "--- Character literal ---"

check "char literal preserved" \
  "(char= ch #\()" \
  "(char= ch #\()"

check "char literal named" \
  "(position #\\Space str)" \
  "(position #\\Space str)"

check "char literal in cond" \
  "$(printf '(cond\n  ((char= ch #\\) t)\n  (t nil))')" \
  "$(printf '(cond\n  ((char= ch #\\) t)\n    (t nil))')"

# --- #\) edge case ---
echo "--- #\) edge case ---"

check "char literal then close paren" \
  "(char= ch #\\))" \
  "(char= ch #\\))"

check "char literal then close paren in list" \
  "(list #\\) #\()" \
  "(list #\\) #\()"

# --- Comments ---
echo "--- Comments ---"

check "line comment preserved" \
  "$(printf '(defun foo (x)\n  ;; comment\n  x)')" \
  "$(printf '(defun foo (x)\n  ;; comment\n  x)')"

check "inline comment preserved" \
  "(+ 1 2) ;; result is 3" \
  "(+ 1 2) ;; result is 3"

# --- Edge cases ---
echo "--- Edge cases ---"

check "single atom" \
  "foo" \
  "foo"

check "deeply nested" \
  "(a (b (c (d (e)))))" \
  "(a (b (c (d (e)))))"

# --- Balance regression ---
echo "--- Balance regression ---"

check "balance #\\(" \
  "(+ 1 (aref (aref *array* row) col))" \
  "(+ 1 (aref (aref *array* row) col))"

check "balance #\\)" \
  "(+ 1 #\\))" \
  "(+ 1 #\\))"

# --- Summary ---
echo ""
echo "Results: $PASS passed, $FAIL failed"
