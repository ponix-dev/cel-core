---
description: Run CEL conformance tests with regression comparison against cached baseline
---

# Run Conformance Tests

Run the conformance report script and add analysis.

## Step 1: Generate Report

Run the conformance report script:

```bash
mise run conformance:report 2>&1
```

If there are **compilation errors**, stop and report them to the user. Do not proceed.

The script handles: running tests, parsing results, loading baseline, computing deltas, and generating the full markdown report. Include the script's stdout output verbatim in your response.

## Step 2: Analysis

After the report, add an **Analysis** section with your own assessment:

1. **Root causes**: Group failures by common patterns (e.g., "unimplemented extension", "proto handling gap", "type system limitation")
2. **Regressions**: If any regressions appear in the report, call these out prominently with a warning
3. **Improvements**: Note any improvements and what likely caused them
4. **Impact**: Which areas would benefit most from further implementation work

$ARGUMENTS
