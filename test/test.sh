#!/bin/bash

# Test runner for C tests using this project's compiler `jcc`.
# For each test/*.c, this script generates an assembly file with `./jcc`,
# assembles it with `gcc`, runs the resulting binary, and records pass/fail.

set -u

test_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
passed=0
failed=0

echo "Running C test suite in $test_dir"
for src in "$test_dir"/*.c; do
    [ -e "$src" ] || continue
    base="${src%.c}"
    echo "--- $src ---"

    if ! ./jcc "$src" "${base}.s"; then
        echo "[ERROR] jcc failed to compile $src"
        ((failed++))
        continue
    fi

    if ! gcc -o "$base" "${base}.s"; then
        echo "[ERROR] gcc failed for ${base}.s"
        ((failed++))
        continue
    fi

    "$base"
    rc=$?
    if [ $rc -eq 0 ]; then
        echo "[OK] $src -> exit $rc"
        ((passed++))
    else
        echo "[FAIL] $src -> exit $rc"
        ((failed++))
    fi

    # cleanup generated binary and assembly if desired
    # rm -f "$base" "${base}.s"
done

echo ""
echo "Results: $passed passed, $failed failed"
if [ $failed -gt 0 ]; then
    exit 1
fi
exit 0