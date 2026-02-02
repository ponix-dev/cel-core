---
description: Complete work on a roadmap feature and create PR
---

# CEL Implementation: Complete Feature

You are wrapping up work on a CEL feature. This command handles running conformance tests, creating handoff notes, and submitting a pull request.

## Review Current Work

First, review the changes made on the current branch:

1. Run `git status` to see modified/added files
2. Run `git log main..HEAD` to see commits on this branch
3. Run `git diff main` to review the actual changes

## Identify the GitHub Issue

Find the GitHub issue associated with this work:

```bash
gh issue list --assignee rallinator7 --state open --json number,title
```

Also check git log and branch name for issue references. Store the issue number as `ISSUE_NUM`. If no issue is found, check open issues for related work.

## Run Conformance Tests

Run the conformance report using mise (do NOT invoke `/run-conformance` as a separate skill):

```bash
mise run conformance:report 2>&1
```

If there are **compilation errors**, stop and report them to the user. Do not proceed.

Include the script's stdout output verbatim in your response, followed by a brief analysis of regressions and improvements.

**IMPORTANT: Do not stop here.** If there are **no regressions**, continue immediately with the remaining steps below (creating handoff notes, committing, and creating the PR) without waiting for user input. Only stop and wait for user input if there are regressions in the conformance results.

## Create Handoff Notes

Create or update `.claude/handoff.md` with the following template:

```markdown
# Roadmap Handoff

## Last Updated
[Today's date]

## Just Completed
- GitHub Issue: #ISSUE_NUM
- [x] What was implemented
- Summary of changes
- Key files added/modified
- Any notable decisions or trade-offs made

## Next Up
- GitHub Issue: #NEXT_ISSUE_NUM (if known)
- What should be tackled next
- Why this is the logical next step
- Any prerequisites or dependencies

## Open Questions
- Any unresolved design decisions
- Areas that may need revisiting
- Technical debt introduced (if any)
```

## Commit Guidelines

1. **Follow conventional commits**: `type: short description`
   - Types: `feat`, `fix`, `refactor`, `test`, `docs`, `chore`
   - Keep commit messages to a single sentence
   - Do NOT add Claude co-authorship to commits
   - Save detailed explanations for the PR description

2. **Ensure all work is committed** before creating the PR

## Pull Request Creation

After all changes are committed:

1. **Push the branch to remote**:
   ```bash
   git push -u origin {branch-name}
   ```

2. **Create the PR** using this template:

```markdown
## Summary
Closes #{ISSUE_NUM}

[1-2 sentence description of what was added/changed]

## Changes
- [Key change 1]
- [Key change 2]
- [Key change 3]

## Testing
- [How changes were tested]
- Conformance test results:

| Test Type | Passed | Total | Pass Rate | vs Baseline |
|-----------|--------|-------|-----------|-------------|
| Parse+Check | X | Y | Z% | +/-N |
| Type Check | X | Y | Z% | +/-N |
| Eval | X | Y | Z% | +/-N |
| **Overall** | **X** | **Y** | **Z%** | **+/-N** |
```

3. **PR Title Format**: Brief description of the feature
   - Example: `feat: add macro expansion to parser`

## User Request

$ARGUMENTS

## Action

Review the current branch changes, create handoff notes, then create the pull request.
