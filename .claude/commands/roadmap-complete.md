---
description: Complete work on a roadmap feature and create PR
---

# CEL Implementation: Complete Feature

You are wrapping up work on a CEL implementation feature. This command handles updating documentation, creating handoff notes, and submitting a pull request.

## Review Current Work

First, review the changes made on the current branch:

1. Run `git status` to see modified/added files
2. Run `git log main..HEAD` to see commits on this branch
3. Run `git diff main` to review the actual changes

## Run Conformance Tests

Run `/run-conformance` to get a full conformance test report with regression comparison against main. Include the summary table and any regressions in the PR description under the Testing section.

## Update ROADMAP.md

Update ROADMAP.md to mark completed milestone items:

1. Find the section you were working on
2. Change `- [ ]` to `- [x]` for completed items
3. Only mark items that are fully implemented and tested

## Create Handoff Notes

Create or update `.claude/handoff.md` with the following template:

```markdown
# Roadmap Handoff

## Last Updated
[Today's date]

## Just Completed
- [x] Milestone item that was checked off
- Summary of what was implemented
- Key files added/modified
- Any notable decisions or trade-offs made

## Next Up
- [ ] Next milestone item to tackle
- Why this is the logical next step
- Any prerequisites or dependencies
- Potential challenges to watch for

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
Implements section {X.Y} ({Section Title}) from the CEL implementation roadmap.

[1-2 sentence description of what was added/changed]

## Changes
- [Key change 1]
- [Key change 2]
- [Key change 3]

## Testing
- [How changes were tested]
- Conformance test results (from `/run-conformance` report):

| Test Type | Passed | Total | Pass Rate | vs Main |
|-----------|--------|-------|-----------|---------|
| Parse+Check | X | Y | Z% | +/-N |
| Type Check | X | Y | Z% | +/-N |
| Eval | X | Y | Z% | +/-N |
| **Overall** | **X** | **Y** | **Z%** | **+/-N** |

## Roadmap
See ROADMAP.md section {X.Y} for full context.
```

3. **PR Title Format**: Brief description of the feature
   - Example: `feat: add macro expansion to parser`

## User Request

$ARGUMENTS

## Action

Review the current branch changes, update ROADMAP.md and handoff.md, then create the pull request.
