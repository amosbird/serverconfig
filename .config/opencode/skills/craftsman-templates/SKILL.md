---
name: craftsman-templates
description: "Craftsman artifact templates for specifications, plans, and task breakdowns. Load this skill when creating Craftsman planning artifacts."
license: MIT
metadata:
  source: "Ported from gsemet/Craftsman for GitHub Copilot"
  version: "0.8"
---

## Specification Template (`01-specification.md`)

Use this template when creating specifications in Phase 4:

```markdown
# Specification: [Feature/Change Name]

**ID**: [JIRA-XXXX or issue reference]

## Overview
[2-3 paragraph summary of what needs to be built and why]

## Functional Requirements
### Core Functionality
- [Requirement 1]
- [Requirement 2]

### Edge Cases
- [Edge case 1 and how to handle]

## Non-Functional Requirements
- **Performance**: [specific metrics]
- **Security**: [security considerations]
- **Compatibility**: [compatibility requirements]
- **Maintainability**: [maintainability goals]

## Integration Points
- [System/module 1]: [integration description]
- [System/module 2]: [integration description]

## Constraints and Assumptions
### Constraints
- [Constraint 1]

### Assumptions
- [Assumption 1]

## Out of Scope
- [Explicitly what will NOT be implemented]

## Success Criteria
- [Measurable criterion 1]
- [Measurable criterion 2]

## Open Questions
- [Any remaining questions for later phases]
```

## Implementation Plan Template (`02-plan.md`)

Use this template when creating plans in Phase 5:

```markdown
# Implementation Plan: [Feature/Change Name]

## Overview
[Brief summary of the technical approach]

## Architecture Changes
[Describe any architectural changes, new modules, or refactoring needed]

## Implementation Steps

### Step 1: [Component/Module Name]
**Files to modify/create**:
- `path/to/file1` - [what changes]
- `path/to/file2` - [what changes]

**Technical approach**:
[2-3 sentences on how this will be implemented]

**Dependencies**: [List any steps this depends on]

### Step 2: [Next Component]
...

## Testing Strategy
- **Unit tests**: [what needs unit testing]
- **Integration tests**: [what needs integration testing]
- **Manual testing**: [what needs manual verification]

## Risks and Mitigations
- **Risk 1**: [description] -> **Mitigation**: [approach]

## Rollout Considerations
- [Deployment considerations]
- [Backward compatibility notes]
- [Feature flags or gradual rollout needs]
```

## Task Template (`03-tasks-XX-[name].md`)

Use this template for each individual task file in Phase 6:

```markdown
# Task [N]: [Task Name]

**Depends on**: Task [M], Task [K] (or "None" if independent)
**Estimated complexity**: Low | Medium | High
**Type**: Feature | Refactoring | Testing | Documentation

## Objective
[1-2 sentences: what this task achieves]

## Important Information

Before coding, Read FIRST -> Load `03-tasks-00-READBEFORE.md`

## Files to Modify/Create
- `path/to/file1`
- `path/to/file2`

## Detailed Steps
1. Update `PROGRESS.md` to mark this task as In Progress
2. [Specific step with file and function/class references]
3. [Next specific step]
4. [Validation step: tests pass, preflight checks pass]
5. Run preflight checks and fix any issues until they pass
6. Update `PROGRESS.md` to mark this task as Completed
7. Commit with a conventional commit message

## Acceptance Criteria
- [ ] [Criterion 1]
- [ ] [Criterion 2]
- [ ] Tests pass
- [ ] Documentation updated

## Testing
- **Test file**: `tests/path/to/test_file`
- **Test cases**: [list specific test scenarios]

## Notes
[Any additional context, gotchas, or considerations]
```

## READBEFORE Template (`03-tasks-00-READBEFORE.md`)

This file provides shared context for ALL task implementations:

```markdown
# Critical Context for All Tasks

## Change Request Summary
[Brief summary of what we're building and why]

## Specification Reference
See `01-specification.md` for full requirements.

## Key Design Decisions
- [Decision 1]: [Rationale]
- [Decision 2]: [Rationale]

## Coding Standards
- [Standard 1 specific to this project]
- [Standard 2]

## Testing Requirements
- All tasks must include unit tests
- Run `[preflight command]` before marking any task complete
- [Project-specific testing guidelines]

## Common Pitfalls
- [Pitfall 1 to watch out for]
- [Pitfall 2]
```

## Commit Message Template (`04-commit-msg.md`)

Use this for generating the final squash commit message:

```markdown
type(scope): brief description of user impact

Concise explanation of what users can now do differently,
focusing on behavioral changes and benefits.

Closes ID-1234

- Bullet point of key change
- Another bullet if needed

Example:
`code example here`
```

Rules:
- Lines wrapped to 100 characters
- Do not describe files changed or tests executed
- Highlight behavioral changes for users
- Follow conventional commit format
- Focus on WHAT changed and WHY, not HOW
