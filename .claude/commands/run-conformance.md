---
description: Run CEL conformance tests with regression comparison against cached baseline
---

# Run Conformance Tests

Run CEL conformance tests and produce a structured report showing pass/fail counts broken down by test type (parse+check, type check, eval) and by file. Compare results against the cached baseline from the previous run.

## Step 1: Record Current Branch

Run `git branch --show-current` to get the current branch name. You'll need this for the report header.

## Step 2: Run Conformance Tests on Current Branch

Run the conformance tests via mise (this handles submodule setup automatically):

```bash
mise run conformance:report 2>&1
```

If there are **compilation errors**, stop and report them to the user. Do not proceed to the report.

Save the full output. Extract all lines matching this exact pattern:

```
CONFORMANCE_RESULT <type> <file> <passed>/<total>
```

Where `<type>` is one of: `parse_check`, `type_check`, `eval`.

Compute per-type totals using this command (pipe the saved test output):

```bash
echo "$OUTPUT" | grep '^CONFORMANCE_RESULT' | awk -F'[ /]' '{
  p[$2]+=$4; t[$2]+=$5
} END {
  for (k in p) printf "TOTAL %s %d/%d\n", k, p[k], t[k]
}'
```

Save these TOTAL lines — use them for the summary table. Do NOT attempt to sum the numbers manually.

Also capture the failure details from any panicking tests. The cargo test output includes:
- Parse+check failures: `{file}: {count} parse failures:\n  {test_name}: {error_type}: {message}`
- Type check failures: `{file}: {passed}/{total} type check tests passed, {count} failures:\n  {details}`
- Eval failures: `{file}: {passed}/{total} eval tests passed, {count} failures:\n  {test_name}: {expr} - {reason}`

## Step 3: Load Baseline Results

Read the stored baseline from `.claude/context/conformance-baseline.md` in the repo root. This file contains the raw conformance test output from a previous run.

- If the file **does not exist or is empty**, note in the report that no baseline is available and omit delta columns.
- If the file **has content**, parse `CONFORMANCE_RESULT` lines and compute TOTAL lines using the same awk command as Step 2.

## Step 4: Produce the Report

Generate a markdown report with these exact sections:

### Header

```
## CEL Conformance Test Report
Branch: `{current_branch}` vs baseline
```

If no baseline is available, use `Branch: {current_branch} (no baseline)` instead.

### Summary Table

| Test Type | Passed | Total | Pass Rate | vs Baseline |
|-----------|--------|-------|-----------|-------------|

- Rows: Parse+Check, Type Check, Eval, **Overall** (bold the overall row)
- Use the TOTAL lines computed by the awk command above for each test type. Do NOT sum manually.
- Pass Rate = passed/total as percentage
- vs Baseline = current passed minus baseline passed (show as `+N` or `-N`). Omit column if no baseline.

### Per-File Breakdown Table

| File | Parse+Check | Eval | vs Baseline (P+C / Eval) |
|------|-------------|------|--------------------------|

- One row per .textproto file, sorted alphabetically
- Show `passed/total` for each test type
- Delta column shows change for both parse+check and eval
- If a file has a type_check result, note it in a footnote

### Failures Section

Group failures by test type. For each failure, show:
- The test file
- The test name (section/test)
- The expression
- The error or mismatch

Keep failure listings concise. If there are more than 20 failures in a category, show the first 15 and summarize the rest.

### Analysis Section

Analyze the failures and explain:
1. **Root causes**: Group failures by common patterns (e.g., "unimplemented extension", "proto handling gap", "type system limitation")
2. **Regressions**: If any tests pass in the baseline but fail on the current branch, call these out prominently with a warning
3. **Improvements**: Note tests that now pass on the current branch but failed in the baseline
4. **Impact**: Which areas would benefit most from further implementation work

## Step 5: Update Baseline

After the report is produced, write the full raw conformance test output from Step 2 into `.claude/context/conformance-baseline.md`, replacing any previous content. This makes the current run's results the new baseline for future comparisons.

$ARGUMENTS
