#!/usr/bin/env bash
#
# conformance-report.sh — Run CEL conformance tests and generate a markdown
# report with baseline comparison.
#
# Usage:
#   bash scripts/conformance-report.sh [--update-baseline] [--baseline PATH]
#
# Output: markdown report on stdout
# Exit codes: 0 on success, 1 on compilation error

set -euo pipefail

# --- Argument parsing -------------------------------------------------------

UPDATE_BASELINE=false
BASELINE_PATH=".claude/context/conformance-baseline.md"

while [ $# -gt 0 ]; do
    case "$1" in
        --update-baseline)
            UPDATE_BASELINE=true
            shift
            ;;
        --baseline)
            BASELINE_PATH="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
    esac
done

# --- Phase 1: Run tests ----------------------------------------------------

BRANCH=$(git branch --show-current 2>/dev/null || echo "detached")

# Run conformance tests, capture all output (stdout+stderr merged)
TEST_OUTPUT=$(cargo test -p cel-core-conformance -- --nocapture 2>&1) || true

# Check for compilation errors (cargo emits "error[E" for compile failures)
if echo "$TEST_OUTPUT" | grep -q '^error\[E'; then
    echo "## Compilation Error" >&2
    echo "" >&2
    echo "Conformance tests failed to compile:" >&2
    echo "$TEST_OUTPUT" | grep '^error' >&2
    exit 1
fi

# --- Phase 2: Parse CONFORMANCE_RESULT lines --------------------------------

# Extract CONFORMANCE_RESULT lines. These may not start at column 0 due to
# interleaved output from parallel test threads, so we match anywhere in the
# line and use sed to strip any prefix.
RESULT_LINES=$(echo "$TEST_OUTPUT" | grep 'CONFORMANCE_RESULT' | sed 's/.*\(CONFORMANCE_RESULT\)/\1/' || true)

if [ -z "$RESULT_LINES" ]; then
    echo "No CONFORMANCE_RESULT lines found in test output." >&2
    exit 1
fi

# Compute per-type totals using awk
TOTALS=$(echo "$RESULT_LINES" | awk -F'[ /]' '{
    p[$2]+=$4; t[$2]+=$5
} END {
    for (k in p) printf "TOTAL %s %d/%d\n", k, p[k], t[k]
}')

# --- Phase 3: Extract failure details --------------------------------------

# Capture panic output blocks: lines between "panicked at" and the next
# "test test_" or "CONFORMANCE_RESULT" line
FAILURES=$(echo "$TEST_OUTPUT" | awk '
/panicked at/ {
    # Extract file and context from the panic line
    capturing = 1
    next
}
capturing && /^(test test_|CONFORMANCE_RESULT |failures:$)/ {
    capturing = 0
    next
}
capturing && /^$/ { next }
capturing { print }
')

# --- Phase 4: Load baseline ------------------------------------------------

HAS_BASELINE=false
BASELINE_TOTALS=""
BASELINE_LINES=""

if [ -f "$BASELINE_PATH" ] && [ -s "$BASELINE_PATH" ]; then
    BASELINE_LINES=$(grep '^CONFORMANCE_RESULT' "$BASELINE_PATH" || true)
    if [ -n "$BASELINE_LINES" ]; then
        HAS_BASELINE=true
        BASELINE_TOTALS=$(echo "$BASELINE_LINES" | awk -F'[ /]' '{
            p[$2]+=$4; t[$2]+=$5
        } END {
            for (k in p) printf "TOTAL %s %d/%d\n", k, p[k], t[k]
        }')
    fi
fi

# --- Phase 5: Compute deltas and generate report ---------------------------

# Helper: extract passed count for a type from TOTAL lines
get_passed() {
    echo "$1" | awk -v type="$2" '$2 == type { split($3, a, "/"); print a[1] }'
}
get_total() {
    echo "$1" | awk -v type="$2" '$2 == type { split($3, a, "/"); print a[1]+0 "/" a[2]+0 }'
}

# Extract current totals
PC_PASSED=$(get_passed "$TOTALS" "parse_check")
PC_TOTAL=$(echo "$TOTALS" | awk '$2=="parse_check" { split($3,a,"/"); print a[2] }')
TC_PASSED=$(get_passed "$TOTALS" "type_check")
TC_TOTAL=$(echo "$TOTALS" | awk '$2=="type_check" { split($3,a,"/"); print a[2] }')
EVAL_PASSED=$(get_passed "$TOTALS" "eval")
EVAL_TOTAL=$(echo "$TOTALS" | awk '$2=="eval" { split($3,a,"/"); print a[2] }')

# Default to 0 if empty
PC_PASSED=${PC_PASSED:-0}; PC_TOTAL=${PC_TOTAL:-0}
TC_PASSED=${TC_PASSED:-0}; TC_TOTAL=${TC_TOTAL:-0}
EVAL_PASSED=${EVAL_PASSED:-0}; EVAL_TOTAL=${EVAL_TOTAL:-0}

OVERALL_PASSED=$((PC_PASSED + TC_PASSED + EVAL_PASSED))
OVERALL_TOTAL=$((PC_TOTAL + TC_TOTAL + EVAL_TOTAL))

# Compute pass rates
pct() {
    if [ "$2" -eq 0 ]; then echo "N/A"; else
        awk "BEGIN { printf \"%.1f%%\", ($1/$2)*100 }"
    fi
}

PC_RATE=$(pct "$PC_PASSED" "$PC_TOTAL")
TC_RATE=$(pct "$TC_PASSED" "$TC_TOTAL")
EVAL_RATE=$(pct "$EVAL_PASSED" "$EVAL_TOTAL")
OVERALL_RATE=$(pct "$OVERALL_PASSED" "$OVERALL_TOTAL")

# Compute deltas if baseline exists
delta() {
    local curr="$1" base="$2"
    local d=$((curr - base))
    if [ "$d" -ge 0 ]; then echo "+$d"; else echo "$d"; fi
}

PC_DELTA="" TC_DELTA="" EVAL_DELTA="" OVERALL_DELTA=""
if $HAS_BASELINE; then
    B_PC=$(get_passed "$BASELINE_TOTALS" "parse_check"); B_PC=${B_PC:-0}
    B_TC=$(get_passed "$BASELINE_TOTALS" "type_check"); B_TC=${B_TC:-0}
    B_EVAL=$(get_passed "$BASELINE_TOTALS" "eval"); B_EVAL=${B_EVAL:-0}
    B_OVERALL=$((B_PC + B_TC + B_EVAL))

    PC_DELTA=$(delta "$PC_PASSED" "$B_PC")
    TC_DELTA=$(delta "$TC_PASSED" "$B_TC")
    EVAL_DELTA=$(delta "$EVAL_PASSED" "$B_EVAL")
    OVERALL_DELTA=$(delta "$OVERALL_PASSED" "$B_OVERALL")
fi

# --- Generate markdown report -----------------------------------------------

echo "## CEL Conformance Test Report"
if $HAS_BASELINE; then
    echo "Branch: \`$BRANCH\` vs baseline"
else
    echo "Branch: \`$BRANCH\` (no baseline)"
fi
echo ""

# Summary table
echo "### Summary"
echo ""
if $HAS_BASELINE; then
    echo "| Test Type | Passed | Total | Pass Rate | vs Baseline |"
    echo "|-----------|--------|-------|-----------|-------------|"
    echo "| Parse+Check | $PC_PASSED | $PC_TOTAL | $PC_RATE | $PC_DELTA |"
    if [ "$TC_TOTAL" -gt 0 ]; then
        echo "| Type Check | $TC_PASSED | $TC_TOTAL | $TC_RATE | $TC_DELTA |"
    fi
    echo "| Eval | $EVAL_PASSED | $EVAL_TOTAL | $EVAL_RATE | $EVAL_DELTA |"
    echo "| **Overall** | **$OVERALL_PASSED** | **$OVERALL_TOTAL** | **$OVERALL_RATE** | **$OVERALL_DELTA** |"
else
    echo "| Test Type | Passed | Total | Pass Rate |"
    echo "|-----------|--------|-------|-----------|"
    echo "| Parse+Check | $PC_PASSED | $PC_TOTAL | $PC_RATE |"
    if [ "$TC_TOTAL" -gt 0 ]; then
        echo "| Type Check | $TC_PASSED | $TC_TOTAL | $TC_RATE |"
    fi
    echo "| Eval | $EVAL_PASSED | $EVAL_TOTAL | $EVAL_RATE |"
    echo "| **Overall** | **$OVERALL_PASSED** | **$OVERALL_TOTAL** | **$OVERALL_RATE** |"
fi
echo ""

# Per-file breakdown table
echo "### Per-File Breakdown"
echo ""

if $HAS_BASELINE; then
    echo "| File | Parse+Check | Eval | vs Baseline (P+C / Eval) |"
    echo "|------|-------------|------|--------------------------|"
else
    echo "| File | Parse+Check | Eval |"
    echo "|------|-------------|------|"
fi

# Build per-file table using awk join of current and baseline
# Output sorted alphabetically by file
if $HAS_BASELINE; then
    awk '
    NR==FNR {
        # baseline data (from stdin fd 3)
        type=$2; file=$3; result=$4
        key=type SUBSEP file
        base[key]=result
        next
    }
    {
        # current data
        type=$2; file=$3; result=$4
        key=type SUBSEP file
        curr[key]=result
        files[file]=1
    }
    END {
        n=0
        for (f in files) { sorted[n++]=f }
        # Simple insertion sort (bash 3.2 compat)
        for (i=1; i<n; i++) {
            tmp=sorted[i]
            j=i-1
            while (j>=0 && sorted[j]>tmp) {
                sorted[j+1]=sorted[j]
                j--
            }
            sorted[j+1]=tmp
        }
        for (i=0; i<n; i++) {
            f=sorted[i]
            pc=curr["parse_check" SUBSEP f]
            ev=curr["eval" SUBSEP f]
            bpc=base["parse_check" SUBSEP f]
            bev=base["eval" SUBSEP f]
            if (pc=="") pc="-"
            if (ev=="") ev="-"

            # Compute deltas
            pc_delta=""
            ev_delta=""
            if (pc != "-" && bpc != "") {
                split(pc,a,"/"); split(bpc,b,"/")
                d=a[1]-b[1]
                pc_delta = (d>=0 ? "+" d : "" d)
            }
            if (ev != "-" && bev != "") {
                split(ev,a,"/"); split(bev,b,"/")
                d=a[1]-b[1]
                ev_delta = (d>=0 ? "+" d : "" d)
            }
            delta_col=""
            if (pc_delta != "" || ev_delta != "") {
                delta_col = pc_delta " / " ev_delta
            }
            printf "| %s | %s | %s | %s |\n", f, pc, ev, delta_col
        }
    }
    ' <(echo "$BASELINE_LINES") <(echo "$RESULT_LINES")
else
    echo "$RESULT_LINES" | awk '
    {
        type=$2; file=$3; result=$4
        key=type SUBSEP file
        data[key]=result
        files[file]=1
    }
    END {
        n=0
        for (f in files) { sorted[n++]=f }
        for (i=1; i<n; i++) {
            tmp=sorted[i]
            j=i-1
            while (j>=0 && sorted[j]>tmp) {
                sorted[j+1]=sorted[j]
                j--
            }
            sorted[j+1]=tmp
        }
        for (i=0; i<n; i++) {
            f=sorted[i]
            pc=data["parse_check" SUBSEP f]
            ev=data["eval" SUBSEP f]
            if (pc=="") pc="-"
            if (ev=="") ev="-"
            printf "| %s | %s | %s |\n", f, pc, ev
        }
    }
    '
fi

# Add type_check footnote if present
if [ "$TC_TOTAL" -gt 0 ]; then
    TC_FILES=$(echo "$RESULT_LINES" | awk '$2=="type_check" { printf "%s (%s), ", $3, $4 }')
    echo ""
    echo "_Type check tests: ${TC_FILES%, }_"
fi
echo ""

# Failures section
echo "### Failures"
echo ""

if [ -z "$FAILURES" ]; then
    echo "No failures detected."
else
    # Group failures by type using the format from test output
    # Parse failures: "{file}: {N} parse failures:"
    # Eval failures: "{file}: {p}/{t} eval tests passed, {N} failures:"
    # Type check failures: "{file}: {p}/{t} type check tests passed, {N} failures:"

    # We'll process the full failure text and group by category
    echo "$FAILURES" | awk '
    BEGIN { category=""; count=0; max_per_cat=15; total_in_cat=0 }

    /parse failures:$/ {
        if (category != "") print ""
        category = "Parse+Check"
        total_in_cat = 0
        count = 0
        print "#### " category " Failures"
        print ""
        sub(/:.*/, "", $0)
        print "**" $0 "**"
        print ""
        next
    }
    /eval tests passed,.*failures:$/ {
        if (category != "") print ""
        category = "Eval"
        total_in_cat = 0
        count = 0
        print "#### " category " Failures"
        print ""
        sub(/:.*/, "", $0)
        print "**" $0 "**"
        print ""
        next
    }
    /type check tests passed,.*failures:$/ {
        if (category != "") print ""
        category = "Type Check"
        total_in_cat = 0
        count = 0
        print "#### " category " Failures"
        print ""
        sub(/:.*/, "", $0)
        print "**" $0 "**"
        print ""
        next
    }

    /^  / && category != "" {
        total_in_cat++
        if (total_in_cat <= max_per_cat) {
            print "- `" substr($0, 3) "`"
        } else if (total_in_cat == max_per_cat + 1) {
            print "- _...and more (truncated)_"
        }
    }
    '
fi
echo ""

# Regressions / Improvements
if $HAS_BASELINE; then
    echo "### Regressions & Improvements"
    echo ""

    # Compare per-file results to find regressions and improvements
    CHANGES=$(awk '
    NR==FNR {
        key=$2 " " $3
        split($4, a, "/")
        base[key]=a[1]
        next
    }
    {
        key=$2 " " $3
        split($4, a, "/")
        curr=a[1]
        if (key in base) {
            d = curr - base[key]
            if (d < 0) printf "REGRESSION %s %s %d (was %d, now %d)\n", $2, $3, d, base[key], curr
            if (d > 0) printf "IMPROVEMENT %s %s +%d (was %d, now %d)\n", $2, $3, d, base[key], curr
        } else {
            printf "NEW %s %s %s\n", $2, $3, $4
        }
    }
    ' <(echo "$BASELINE_LINES") <(echo "$RESULT_LINES"))

    REGRESSIONS=$(echo "$CHANGES" | grep '^REGRESSION' || true)
    IMPROVEMENTS=$(echo "$CHANGES" | grep '^IMPROVEMENT' || true)
    NEW_FILES=$(echo "$CHANGES" | grep '^NEW' || true)

    if [ -n "$REGRESSIONS" ]; then
        echo "**Regressions:**"
        echo "$REGRESSIONS" | awk '{ printf "- %s %s: %s %s %s %s %s %s\n", $2, $3, $4, $5, $6, $7, $8, $9 }'
        echo ""
    fi
    if [ -n "$IMPROVEMENTS" ]; then
        echo "**Improvements:**"
        echo "$IMPROVEMENTS" | awk '{ printf "- %s %s: %s %s %s %s %s %s\n", $2, $3, $4, $5, $6, $7, $8, $9 }'
        echo ""
    fi
    if [ -n "$NEW_FILES" ]; then
        echo "**New test files:**"
        echo "$NEW_FILES" | awk '{ printf "- %s %s: %s\n", $2, $3, $4 }'
        echo ""
    fi
    if [ -z "$REGRESSIONS" ] && [ -z "$IMPROVEMENTS" ] && [ -z "$NEW_FILES" ]; then
        echo "No regressions or improvements detected."
        echo ""
    fi
fi

# --- Phase 7: Optionally update baseline -----------------------------------

if $UPDATE_BASELINE; then
    mkdir -p "$(dirname "$BASELINE_PATH")"
    echo "$RESULT_LINES" > "$BASELINE_PATH"
    # Ensure trailing newline
    echo "" >> "$BASELINE_PATH"
    echo "Baseline updated: ${BASELINE_PATH}" >&2
fi
