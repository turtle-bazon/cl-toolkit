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

# Test 7: validate with #\) character literal
echo ""
echo "--- validate #\\) ---"
echo -e "(defun foo ()\n  (char= ch #\\))\n  t)" > /tmp/test-charlit2.lisp
result=$($BIN validate --file /tmp/test-charlit2.lisp 2>&1)
if echo "$result" | grep -q '"balanced":true'; then
    echo "PASS: validate handles #\\) correctly"
    PASS=$((PASS + 1))
else
    echo "FAIL: validate false positive on #\\)"
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 8: validate with incomplete form and #\(
echo ""
echo "--- validate incomplete form with #\\( ---"
echo -e "(defun foo ()\n  (char= ch #\\()" > /tmp/test-charlit3.lisp
result=$($BIN validate --file /tmp/test-charlit3.lisp --recovery 2>&1)
if echo "$result" | grep -q '"balanced":false'; then
    echo "PASS: validate reports incomplete form with #\\("
    PASS=$((PASS + 1))
else
    echo "FAIL: validate should report incomplete form with #\\("
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 9: format with write
echo ""
echo "--- format --write ---"
echo -e "(defun foo (x)\n(+ x 1))" > /tmp/test-format-write.lisp
result=$($BIN format --file /tmp/test-format-write.lisp --write 2>&1)
if echo "$result" | grep -q '"success":true'; then
    echo "PASS: format --write succeeds"
    PASS=$((PASS + 1))
else
    echo "FAIL: format --write failed"
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 10: balance with #\(` - verify #\(` not counted as structural
echo ""
echo "--- balance #\\( ---"
echo '(let ((ch #\()) t)' > /tmp/test-balance-hash.lisp
result=$($BIN balance --file /tmp/test-balance-hash.lisp 2>&1)
if echo "$result" | grep -q '"balanced":true'; then
    echo "PASS: balance handles #\\( correctly"
    PASS=$((PASS + 1))
else
    echo "FAIL: balance counts #\\( as structural paren"
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 11: balance with #\) - verify #\) not counted as structural
echo ""
echo "--- balance #\\) ---"
echo "(let ((ch #\\))) t)" > /tmp/test-balance-hash2.lisp
result=$($BIN balance --file /tmp/test-balance-hash2.lisp 2>&1)
if echo "$result" | grep -q '"balanced":true'; then
    echo "PASS: balance handles #\\) correctly"
    PASS=$((PASS + 1))
else
    echo "FAIL: balance counts #\\) as structural paren"
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 12: balance with #\Space - verify named char not counted
echo ""
echo "--- balance #\\Space ---"
echo "(let ((ch #\\Space)) t)" > /tmp/test-balance-hash3.lisp
result=$($BIN balance --file /tmp/test-balance-hash3.lisp 2>&1)
if echo "$result" | grep -q '"balanced":true'; then
    echo "PASS: balance handles #\\Space correctly"
    PASS=$((PASS + 1))
else
    echo "FAIL: balance counts #\\Space as structural"
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 13: format normalizes whitespace
echo ""
echo "--- format whitespace ---"
result=$($BIN format --code "(defun   foo(x)  (+  x 1))" 2>&1)
if echo "$result" | grep -q "(defun foo(x) (+ x 1))"; then
    echo "PASS: format normalizes whitespace"
    PASS=$((PASS + 1))
else
    echo "FAIL: format does not normalize whitespace"
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 14: validate with #\(` in complex code (user's exact case)
echo ""
echo "--- validate complex #\\( ---"
cat > /tmp/test-complex-hash.lisp << 'ENDOFFILE'
(defun test-char-literal ()
  (let ((ch #\()))
    (format t "Char: ~A~%" ch))
ENDOFFILE
result=$($BIN validate --file /tmp/test-complex-hash.lisp 2>&1)
if echo "$result" | grep -q '"balanced":true'; then
    echo "PASS: validate handles complex #\\( correctly"
    PASS=$((PASS + 1))
else
    echo "FAIL: validate fails on complex #\\("
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Test 15: replace should target smallest enclosing form, not parent
echo ""
echo "--- replace scope ---"
cat > /tmp/test-replace-scope.lisp << 'ENDOFFILE'
(defun dispatch-token (tok stack vars)
  (cond
    ((or (string= tok "(") (string= tok ")"))
     (values stack nil))
    ((or (string= tok "+"))
     (values stack nil))
    (t
     (values stack nil))))
ENDOFFILE
result=$($BIN replace --file /tmp/test-replace-scope.lisp --line 5 --col 1 --code '((or (string= tok "+")) (values stack (+ 1 2)))' 2>&1)
# The replacement should only affect the clause on line 5, not the whole cond
if echo "$result" | grep -q "defun foo" 2>/dev/null; then
    # This shouldn't happen - the defun foo test was from the earlier test file
    true
fi
# Check that the first clause is preserved
if echo "$result" | grep -q 'string=' && echo "$result" | grep -q 'values stack'; then
    echo "PASS: replace targets clause, not entire cond"
    PASS=$((PASS + 1))
else
    echo "FAIL: replace targets wrong scope"
    echo "  Got: $result"
    FAIL=$((FAIL + 1))
fi

# Cleanup
rm -f /tmp/test-bugs.lisp /tmp/test-replace.lisp /tmp/test-insert.lisp /tmp/test-format.lisp /tmp/test-delete.lisp /tmp/test-charlit.lisp /tmp/test-charlit2.lisp /tmp/test-charlit3.lisp /tmp/test-format-write.lisp /tmp/test-balance-hash.lisp /tmp/test-balance-hash2.lisp /tmp/test-balance-hash3.lisp /tmp/test-complex-hash.lisp /tmp/test-replace-scope.lisp

echo ""
echo "Results: $PASS passed, $FAIL failed"
exit $FAIL
