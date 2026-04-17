#!/usr/bin/env bash
# Run all MxFpMul PE tests listed in test/tests.yml and print a summary.

set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
YML="$SCRIPT_DIR/test/tests.yml"

if [ ! -f "$YML" ]; then
  echo "Error: $YML not found" >&2
  exit 1
fi

pass=0
fail=0
results=()

# Parse tests.yml: extract spec/label pairs
while IFS= read -r line; do
  # spec line
  if [[ "$line" =~ ^-\ *spec:\ *(.*) ]]; then
    spec="${BASH_REMATCH[1]}"
  fi
  # label line
  if [[ "$line" =~ ^\ +label:\ *(.*) ]]; then
    label="${BASH_REMATCH[1]}"

    printf "Running %-40s ... " "$label"
    if ./mill test.testOnly "$spec" > /dev/null 2>&1; then
      printf "PASS\n"
      results+=("PASS  $label")
      ((pass++))
    else
      printf "FAIL\n"
      results+=("FAIL  $label")
      ((fail++))
    fi
  fi
done < "$YML"

total=$((pass + fail))

echo ""
echo "=============================="
echo "  Test Summary"
echo "=============================="
for r in "${results[@]}"; do
  echo "  $r"
done
echo "------------------------------"
echo "  Total: $total  Passed: $pass  Failed: $fail"
echo "=============================="

if [ "$fail" -gt 0 ]; then
  exit 1
fi
