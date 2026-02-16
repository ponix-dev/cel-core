---
description: Start implementation of the next CEL feature from the roadmap
---

# CEL Implementation: Start Feature

You are beginning work on the next feature for CEL-Core. The core CEL implementation is spec-complete with 100% conformance. New work focuses on improvements, extensions, and tooling built on top of the core.

## Context

Read this file first:
- @.claude/handoff.md - Handoff notes from the previous session (if it exists)

The handoff file tracks continuity between sessions, documenting what was just completed and what should come next.

## Find the Next GitHub Issue

Query open issues to find the next work item:

```bash
gh issue list --state open --sort created --json number,title,labels
```

- If `$ARGUMENTS` specifies a topic or issue number, match it to the corresponding issue
- Otherwise, pick the lowest-numbered open issue as the next work item
- Store the issue number — reference it throughout as "Issue #N"

## Reference Sources

When creating your plan, you MUST research these authoritative sources using the Task tool with the Explore agent:

1. **cel-go implementation** (https://github.com/google/cel-go) - The reference implementation. Study how they structure code for the feature you're implementing.

2. **cel-spec** (https://github.com/google/cel-spec) - The official specification. Check the language definition and conformance tests.

3. **Local conformance tests** - Check `crates/cel-core-conformance/cel-spec/` for relevant test cases.

## Git Workflow

Before beginning any implementation work, you MUST create a feature branch:

1. **Branch Naming Convention**: `{short-description}`
   - Use kebab-case describing the feature being implemented
   - Do NOT include prefixes like `feat/`
   - Examples: `decouple-prost`, `cel-block-extension`, `recursion-limits`

2. **Create the branch immediately** after identifying the next work item:
   ```bash
   git checkout -b {short-description}
   ```

## Planning Process

1. **Identify the next work item** - Use the GitHub issue and the handoff file to understand what needs to be implemented next.

2. **Research** - Use agents to explore how cel-go or other references handle this feature if applicable.

3. **Design our implementation** - Create a plan that:
   - Fits our existing crate structure
   - Builds incrementally on previous work
   - Includes specific files to create/modify
   - Maintains 100% conformance (no regressions)

4. **Identify refactoring needs** - If current code needs restructuring to support the feature, call out necessary refactoring first.

## Plan Output Format

Your plan should include:

1. **GitHub Issue**: #N — {title}
2. **Target**: What we're implementing
3. **Reference**: Key files/patterns from cel-go or other sources
4. **Implementation Steps**: Ordered list of changes
5. **Files to Create/Modify**: Specific paths
6. **Refactoring Required**: Any structural changes needed first
7. **Success Criteria**: How we know it's done (tests, conformance, etc.)

## Important Guidelines

- Changes should build incrementally — don't skip ahead
- Always run conformance tests to verify zero regressions
## When Plan Is Accepted

After the user approves the plan:

1. **Post the plan as a comment on the GitHub issue:**
   ```bash
   gh issue comment <N> --body "<plan content>"
   ```

2. **Assign the issue:**
   ```bash
   gh issue edit <N> --add-assignee rallinator7
   ```

## User Request

$ARGUMENTS

## Action

Enter plan mode now to thoroughly research and design the implementation approach before writing any code.
