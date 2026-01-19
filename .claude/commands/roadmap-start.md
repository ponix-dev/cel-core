---
description: Start implementation of the next CEL feature from the roadmap
---

# CEL Implementation: Start Feature

You are beginning work on the next step in our CEL implementation. Our goal is to achieve full parity with cel-go, the reference implementation.

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

## User Request

$ARGUMENTS

## Action

Enter plan mode now to thoroughly research and design the implementation approach before writing any code.
