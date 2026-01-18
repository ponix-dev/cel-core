---
description: Create an implementation plan for the next CEL feature based on the roadmap
---

# CEL Implementation Planning

You are planning the next step in our CEL implementation. Our goal is to achieve full parity with cel-go, the reference implementation.

## Context

Read these files first:
- @ROADMAP.md - Our implementation roadmap with phases and milestones
- @.claude/handoff.md - Handoff notes from the previous session (if it exists)

The handoff file tracks continuity between sessions, documenting what was just completed and what should come next.

## Reference Sources

When creating your plan, you MUST research these authoritative sources using the Task tool with the Explore agent:

1. **cel-go implementation** (https://github.com/google/cel-go) - The reference implementation. Study how they structure code for the feature you're implementing.

2. **cel-spec** (https://github.com/google/cel-spec) - The official specification. Check the language definition and conformance tests.

3. **Local conformance tests** - Check `crates/cel-conformance/cel-spec/` for relevant test cases.

## Git Workflow

Before beginning any implementation work, you MUST create a feature branch for the phase:

1. **Branch Naming Convention**: `{short-description}`
   - Use kebab-case describing the feature being implemented
   - Do NOT include section numbers or prefixes like `feat/`
   - Examples:
     - Section 1.1 "Node IDs in AST" → `node-ids-in-ast`
     - Section 1.2 "Macro Expansion" → `macro-expansion`
     - Section 2.1 "Enhanced Type Representation" → `enhanced-type-representation`

2. **Create the branch immediately** after identifying the next section:
   ```bash
   git checkout -b {short-description}
   ```

3. **Commit incrementally** as you complete logical units of work.

4. **Commit message guidelines**:
   - Follow conventional commit format: `type: short description`
   - Types: `feat`, `fix`, `refactor`, `test`, `docs`, `chore`
   - Keep messages short and concise
   - Do NOT reference Claude in commit messages (PRs can reference Claude)

## Planning Process

1. **Identify the next implementation section** - Follow the numbered implementation sections in ROADMAP.md in order (1.1, 1.2, 1.3, 2.1, 2.2, etc.). Each section must be completed before moving to the next. Check the handoff file to see which section was last completed.

2. **Research the cel-go implementation** - Use agents to explore how cel-go implements this feature. Look at:
   - Package structure and file organization
   - Key types and interfaces
   - Algorithm approach

3. **Research the cel-spec** - Use agents to find:
   - Relevant specification details in `doc/langdef.md`
   - Conformance test cases that cover this feature

4. **Design our implementation** - Create a plan that:
   - Follows cel-go's patterns where appropriate
   - Fits our existing crate structure
   - Builds incrementally on previous work
   - Includes specific files to create/modify

5. **Identify refactoring needs** - If our current code diverges from cel-go's approach in ways that will cause problems, call out necessary refactoring first.

## Plan Output Format

Your plan should include:

1. **Target Milestone Item**: Which checkbox from ROADMAP.md we're implementing
2. **cel-go Reference**: Key files/patterns from cel-go to follow
3. **Specification Reference**: Relevant cel-spec sections
4. **Implementation Steps**: Ordered list of changes
5. **Files to Create/Modify**: Specific paths
6. **Refactoring Required**: Any structural changes needed first
7. **Success Criteria**: How we know it's done (conformance tests, etc.)

## Important Guidelines

- Each plan should target ONE milestone checkbox or a coherent subset
- Changes should build incrementally - don't skip ahead
- If refactoring is needed to align with cel-go, do that first
- Always check conformance tests for acceptance criteria
- Consider how this change affects the LSP functionality

## Handoff Protocol

After completing implementation work, update `.claude/handoff.md` with:

```markdown
# Roadmap Handoff

## Last Updated
[Date]

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

This handoff ensures continuity between planning sessions and helps pick up where we left off.

## Pull Request Creation

After implementation is complete and the user has confirmed the changes look good, create a pull request:

1. **Ensure all changes are committed** on the feature branch

2. **Push the branch to remote**:
   ```bash
   git push -u origin {branch-name}
   ```

3. **Create the PR** with a summary of changes:
   - Title: Brief description of the feature (e.g., "feat: add macro expansion to parser")
   - Body should include:
     - **Summary**: What was implemented and why
     - **Changes**: Key files and modifications
     - **Testing**: How the changes were verified
     - **Roadmap Reference**: Link to the section in ROADMAP.md

4. **PR Template**:
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
   - [Conformance tests added/passing]

   ## Roadmap
   See ROADMAP.md section {X.Y} for full context.
   ```

5. **Wait for user confirmation** before creating the PR - ask the user if they want to proceed with PR creation.

## User Request

$ARGUMENTS

## Action

Enter plan mode now to thoroughly research and design the implementation approach before writing any code.
