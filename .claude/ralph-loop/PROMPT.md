# zOS-clone Implementation Loop - Prompt Template

## Loop Identity

```yaml
name: zOS-clone-impl-loop
version: 1.0.0
goal: Incrementally implement z/OS compatibility features
method: BMAD-guided implementation
```

## Pre-Loop Initialization

**On first iteration or new session:**

1. Read progress file: `.claude/ralph-loop/PROGRESS.md`
2. Load planning artifacts from `_bmad-output/planning-artifacts/`
3. Determine current implementation state
4. Resume from last checkpoint

---

## Loop Flow

```
┌────────────────────────────────────────────────────────────────────┐
│                                                                    │
│  ┌─────────┐    ┌─────────┐    ┌─────────────┐    ┌─────────┐     │
│  │ ASSESS  │───▶│  PLAN   │───▶│ IMPLEMENT   │───▶│  TEST   │     │
│  └─────────┘    └─────────┘    └─────────────┘    └─────────┘     │
│       │              │               │                  │          │
│       │              │               │                  │          │
│       ▼              ▼               ▼                  ▼          │
│  ┌─────────┐    ┌─────────┐    ┌─────────────┐    ┌─────────┐     │
│  │ Status  │    │ BMAD if │    │ Write Code  │    │ Verify  │     │
│  │ Report  │    │ complex │    │ Add Tests   │    │ Commit  │     │
│  └─────────┘    └─────────┘    └─────────────┘    └─────────┘     │
│                                                         │          │
│                                                         ▼          │
│                                              ┌──────────────────┐  │
│                                              │ UPDATE PROGRESS  │  │
│                                              │ Next Iteration   │  │
│                                              └──────────────────┘  │
│                                                         │          │
└─────────────────────────────────────────────────────────┼──────────┘
                                                          │
                                                          ▼
                                               ┌──────────────────┐
                                               │ COMPLETION CHECK │
                                               │ Exit if done     │
                                               └──────────────────┘
```

---

## Phase 1: ASSESS

**Purpose:** Understand current state before making changes.

### 1.1 Check Build Status
```bash
cargo build 2>&1 | tail -10
```

### 1.2 Check Test Status
```bash
cargo test 2>&1 | grep -E "^test result|running [0-9]+ test|FAILED"
```

### 1.3 Check Implementation Coverage
```bash
# Count implemented modules
find crates -name "*.rs" | wc -l

# Check what's in each crate
for crate in crates/*/; do
  echo "=== $crate ==="
  ls -la "$crate/src/" 2>/dev/null | head -10
done
```

### 1.4 Read Progress File
```bash
cat .claude/ralph-loop/PROGRESS.md
```

### 1.5 Generate Status Report

**Template:**
```markdown
## Iteration {N} - Assessment

**Build:** ✅ Passing | ❌ Failing
**Tests:** {X} passing, {Y} failing
**Last Completed:** {feature/story}
**Current Focus:** {feature/story}
**Blockers:** {none | description}
```

---

## Phase 2: PLAN

**Purpose:** Determine what to implement next and how.

### 2.1 Check Planning Artifacts

Read from `_bmad-output/planning-artifacts/`:
- `prd-v{VERSION}.md` - Requirements
- `architecture-v{VERSION}.md` - Design decisions
- `epics-v{VERSION}.md` - Stories with acceptance criteria

### 2.2 Select Next Feature

**Priority Order (v1.1):**

| Priority | Epic | Feature | Stories | Complexity |
|----------|------|---------|---------|------------|
| 1 | 15 | VSAM KSDS | 15.1-15.8 | High |
| 2 | 16 | VSAM ESDS/RRDS | 16.1-16.5 | Medium |
| 3 | 18 | GDG Support | 18.1-18.7 | Medium |
| 4 | 17 | SORT Utility | 17.1-17.9 | High |
| 5 | 19 | IDCAMS | 19.1-19.8 | Medium |
| 6 | 20 | Packaging | 20.1-20.5 | Low |

### 2.3 Planning Decision Matrix

| Scope | Action |
|-------|--------|
| Single function/fix | Implement directly |
| Single file change | Implement with inline docs |
| New module (3-5 files) | Write brief spec, then implement |
| New crate/major feature | Use BMAD: `/bmad-bmm-create-architecture` |
| New epic needed | Use BMAD: `/bmad-bmm-create-epics-and-stories` |

### 2.4 BMAD Integration Points

**When to invoke BMAD:**
- Feature not in existing epics → Create new epic
- Architecture unclear → Architecture workflow
- Requirements ambiguous → PRD edit workflow

**BMAD Commands:**
```
/bmad-bmm-create-prd          # New PRD
/bmad-bmm-edit-prd            # Extend existing PRD
/bmad-bmm-create-architecture # Architecture decisions
/bmad-bmm-create-epics-and-stories # Epic breakdown
/bmad-help                    # Get guidance
```

---

## Phase 3: IMPLEMENT

**Purpose:** Write code that fulfills the selected story.

### 3.1 Read Story Acceptance Criteria

From `epics-v{VERSION}.md`, get:
- Story description (As a... I want... So that...)
- Given/When/Then acceptance criteria
- Technical notes
- Dependencies

### 3.2 Implementation Checklist

```markdown
- [ ] Create/modify source files
- [ ] Add unit tests
- [ ] Add integration tests (if applicable)
- [ ] Update module exports
- [ ] Add documentation comments
```

### 3.3 Code Location Guidelines

| Feature Type | Location |
|--------------|----------|
| VSAM | `crates/zos-dataset/src/vsam/` |
| GDG | `crates/zos-dataset/src/gdg/` |
| IDCAMS | `crates/zos-dataset/src/idcams/` |
| SORT | `crates/zos-sort/` (new crate) |
| JCL extensions | `crates/zos-jcl/src/` |
| CLI commands | `crates/zos-clone/src/commands/` |
| COBOL runtime | `crates/zos-runtime/src/` |

### 3.4 Coding Standards

```rust
// Follow existing patterns
// Use thiserror for errors
// Use miette for diagnostics
// Add #[cfg(test)] mod tests
// Document public APIs
```

---

## Phase 4: TEST

**Purpose:** Verify implementation works correctly.

### 4.1 Run Tests
```bash
# Unit tests
cargo test --package {crate}

# All tests
cargo test

# With output
cargo test -- --nocapture
```

### 4.2 Run Quality Checks
```bash
# Linting
cargo clippy -- -D warnings

# Formatting
cargo fmt --check
```

### 4.3 Manual Verification

Test the feature manually if applicable:
```bash
# Build release
cargo build --release

# Test CLI command
./target/release/zos-clone {command} {args}
```

### 4.4 Acceptance Criteria Validation

Check each Given/When/Then from the story:
```markdown
- [ ] Given X, When Y, Then Z
- [ ] Given A, When B, Then C
```

---

## Phase 5: UPDATE PROGRESS

**Purpose:** Record progress and prepare for next iteration.

### 5.1 Update Progress File

Edit `.claude/ralph-loop/PROGRESS.md`:
- Mark completed items
- Update current focus
- Note any blockers
- Increment iteration count

### 5.2 Commit Changes

```bash
# Stage changes
git add -A

# Commit with descriptive message
git commit -m "feat({scope}): {description}

- Implements Story {X.Y}
- {additional details}

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

### 5.3 Decide Next Action

| Condition | Action |
|-----------|--------|
| Story complete, more stories in epic | Continue to next story |
| Epic complete | Move to next epic |
| Version complete | Plan next version with BMAD |
| Blocker encountered | Document and ask user |
| Tests failing | Fix before proceeding |

---

## Completion Criteria

### Version Milestones

**v1.1 Complete When:**
- [ ] VSAM KSDS fully functional
- [ ] VSAM ESDS/RRDS working
- [ ] GDG resolution working
- [ ] SORT utility processing files
- [ ] IDCAMS core commands working
- [ ] Packages building

**Signal Completion:**
```
<promise>Z/OS COMPATIBILITY MILESTONE REACHED</promise>
```

---

## Error Handling

### Build Failures
1. Read error message
2. Fix compilation issue
3. Re-run build
4. Do not proceed until build passes

### Test Failures
1. Identify failing test
2. Check if regression or new failure
3. Fix code or update test
4. Ensure all tests pass

### Blockers
1. Document in PROGRESS.md
2. If can work around, note and continue
3. If blocking, ask user for guidance

---

## Session Management

### Starting New Session
1. Read PROGRESS.md
2. Verify build status
3. Resume from current focus

### Ending Session
1. Commit all working changes
2. Update PROGRESS.md
3. Note where to resume

### Context Window Full
1. Commit current work
2. Update PROGRESS.md with detailed state
3. Signal need for new context
