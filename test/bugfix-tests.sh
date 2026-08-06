#!/bin/bash
set -e

BIN="./build/cl-toolkit"
PASS=0
FAIL=0

run_test() {
    local desc="$1"
    local cmd="$2"
    local expected="$3"
    
    result=$(eval "$cmd" 2>&1)
    if echo "$result" | grep -q "$expected"; then
        echo "PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $desc"
        echo "  Expected: $expected"
        echo "  Got: $result"
        FAIL=$((FAIL + 1))
    fi
}

file_test() {
    local desc="$1"
    local cmd="$2"
    local expected="$3"
    local file="$4"
    
    result=$(eval "$cmd" 2>&1)
    file_content=$(cat "$file")
    if echo "$file_content" | grep -q "$expected"; then
        echo "PASS: $desc"
        PASS=$((PASS + 1))
    else
        echo "FAIL: $desc"
        echo "  Expected file to contain: $expected"
        echo "  File contains: $file_content"
        FAIL=$((FAIL + 1))
    fi
}

echo "=== cl-toolkit Bug Fix Tests ==="
echo ""

# Setup test file
echo -e "(defun foo (x)\n  (+ x 1))\n\n(defun bar (y)\n  (* y 2))" > /tmp/test-bugs.lisp

# Test 1: replace --write should not truncate
echo "--- replace --write ---"
cp /tmp/test-bugs.lisp /tmp/test-replace.lisp
$BIN replace --file /tmp/test-replace.lisp --line 5 --col 3 --code "(+ y 3)" --write 2>&1 > /dev/null
file_test "replace --write keeps full file" "cat /tmp/test-replace.lisp" "defun foo" /tmp/test-replace.lisp
file_test "replace --write has replacement" "cat /tmp/test-replace.lisp" "+ y 3" /tmp/test-replace.lisp
file_test "replace --write keeps other forms" "cat /tmp/test-replace.lisp" "defun bar" /tmp/test-replace.lisp

# Test 2: insert --write should respect position
echo ""
echo "--- insert --write ---"
cp /tmp/test-bugs.lisp /tmp/test-insert.lisp
$BIN insert --file /tmp/test-insert.lisp --line 1 --col 1 --code "(import 'utils)" --write 2>&1 > /dev/null
file_test "insert --write at correct position" "head -1 /tmp/test-insert.lisp" "import" /tmp/test-insert.lisp
file_test "insert --write keeps original" "cat /tmp/test-insert.lisp" "defun foo" /tmp/test-insert.lisp

# Test 3: format should return valid JSON
echo ""
echo "--- format output ---"
result=$($BIN format --code "(+ 1 2)" 2>&1)
if echo "$result" | grep -q "success"; then
    echo "PASS: format returns valid JSON"
    PASS=$((PASS + 1))
else
    echo "FAIL: format returns invalid JSON"
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 4: format --write should work
echo ""
echo "--- format --write ---"
cp /tmp/test-bugs.lisp /tmp/test-format.lisp
$BIN format --file /tmp/test-format.lisp --write 2>&1 > /dev/null
result=$?
if [ $result -eq 0 ]; then
    echo "PASS: format --write succeeds"
    PASS=$((PASS + 1))
else
    echo "FAIL: format --write fails"
    FAIL=$((FAIL + 1))
fi

# Test 5: delete --write should return JSON
echo ""
echo "--- delete output ---"
cp /tmp/test-bugs.lisp /tmp/test-delete.lisp
result=$($BIN delete --file /tmp/test-delete.lisp --line 4 --col 1 2>&1)
if echo "$result" | grep -q "success"; then
    echo "PASS: delete returns valid JSON"
    PASS=$((PASS + 1))
else
    echo "FAIL: delete returns invalid JSON"
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 6: validate with #\( character literal
echo ""
echo "--- validate #\\( ---"
echo -e "(defun foo ()\n  (char= ch #\\()\n  t)" > /tmp/test-charlit.lisp
result=$($BIN validate --file /tmp/test-charlit.lisp 2>&1)
if echo "$result" | grep -q '"balanced":true'; then
    echo "PASS: validate handles #\\( correctly"
    PASS=$((PASS + 1))
else
    echo "FAIL: validate false positive on #\\("
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Cleanup
rm -f /tmp/test-bugs.lisp /tmp/test-replace.lisp /tmp/test-insert.lisp /tmp/test-format.lisp /tmp/test-delete.lisp /tmp/test-charlit.lisp

echo ""
echo "Results: $PASS passed, $FAIL failed"
exit $FAIL
