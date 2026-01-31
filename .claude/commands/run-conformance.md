---
description: Run CEL conformance tests with regression comparison against main
---

# Run Conformance Tests

Run CEL conformance tests and produce a structured report showing pass/fail counts broken down by test type (parse+check, type check, eval) and by file. Always compare results against the main branch.

## Step 1: Record Current Branch

Run `git branch --show-current` to get the current branch name. You'll need this for the report header and to return to it after regression testing.

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

Also capture the failure details from any panicking tests. The cargo test output includes:
- Parse+check failures: `{file}: {count} parse failures:\n  {test_name}: {error_type}: {message}`
- Type check failures: `{file}: {passed}/{total} type check tests passed, {count} failures:\n  {details}`
- Eval failures: `{file}: {passed}/{total} eval tests passed, {count} failures:\n  {test_name}: {expr} - {reason}`

## Step 3: Run Conformance Tests on Main Branch (Regression Baseline)

Use a git worktree to test main without touching the working tree:

```bash
git worktree add /tmp/cel-core-conformance-baseline main 2>&1
```

Then run the conformance report in the worktree via mise:

```bash
cd /tmp/cel-core-conformance-baseline && mise run conformance:report 2>&1
```

Parse the main branch output the same way.

Clean up the worktree when done:

```bash
git worktree remove /tmp/cel-core-conformance-baseline --force 2>&1
```

If the main branch output has no `CONFORMANCE_RESULT` lines (the println changes haven't been merged yet), note in the report that baseline data is unavailable and omit the delta column.

## Step 4: Produce the Report

Generate a markdown report with these exact sections:

### Header

```
## CEL Conformance Test Report
Branch: `{current_branch}` vs `main`
```

### Summary Table

| Test Type | Passed | Total | Pass Rate | vs Main |
|-----------|--------|-------|-----------|---------|

- Rows: Parse+Check, Type Check, Eval, **Overall** (bold the overall row)
- Sum all files' passed/total for each test type
- Pass Rate = passed/total as percentage
- vs Main = current passed minus main passed (show as `+N` or `-N`)

### Per-File Breakdown Table

| File | Parse+Check | Eval | vs Main (P+C / Eval) |
|------|-------------|------|----------------------|

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
2. **Regressions**: If any tests pass on main but fail on the current branch, call these out prominently with a warning
3. **Improvements**: Note tests that now pass on the current branch but failed on main
4. **Impact**: Which areas would benefit most from further implementation work

$ARGUMENTS
