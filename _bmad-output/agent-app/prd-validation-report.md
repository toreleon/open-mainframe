---
date: '2026-02-17'
status: 'validated'
prdVersion: 'v1.0'
inputDocuments:
  - prd-openmainframe-agent.md
  - product-brief-openmainframe-agent.md
  - ux-design-openmainframe-agent.md
  - research-technical-openmainframe-tools.md
---

# PRD Validation Report: OpenMainframe Agent v1.0

**BMAD Phase:** 2-Planning | **Step:** 7 of 11 | **Agent:** John 📋 Product Manager

---

## 1. Validation Summary

| Category | Status | Score |
|----------|--------|-------|
| Completeness | PASS | 9/10 |
| Consistency | PASS | 9/10 |
| Testability | PASS | 10/10 |
| Feasibility | PASS | 8/10 |
| Alignment with Brief | PASS | 10/10 |
| UX Coverage | PASS | 10/10 |
| **Overall** | **PASS** | **9.3/10** |

---

## 2. Completeness Check

### Requirements Coverage

| Product Brief Feature | PRD Requirement(s) | Status |
|----------------------|-------------------|--------|
| Assessment Agent | FR-010, FR-011, FR-012, FR-013 | ✅ Fully covered |
| Compilation Agent | FR-020, FR-021, FR-022 | ✅ Fully covered |
| Execution Agent | FR-030, FR-031, FR-032 | ✅ Fully covered |
| Code Explanation Agent | FR-040, FR-041, FR-042 | ✅ Fully covered |
| Dataset Management Agent | FR-050, FR-051 | ✅ Fully covered |
| Interactive Chat UI | FR-060, FR-061, FR-062, FR-063, FR-064 | ✅ Fully covered |
| Human-in-the-Loop | FR-070, FR-071 | ✅ Fully covered |
| State Management | FR-080, FR-081 | ✅ Fully covered |
| Infrastructure | FR-001, FR-002, FR-003, FR-004 | ✅ Fully covered |

### Missing Elements (Minor)

1. **No explicit FR for onboarding flow** — The UX design includes a Welcome Screen but no PRD requirement explicitly defines the first-time user experience.
   - **Recommendation:** Add FR-090 for onboarding wizard or first-run guidance.
   - **Severity:** Low — UX design covers it; PRD gap is formal only.

2. **No FR for error logging/observability** — Production debugging capability not specified.
   - **Recommendation:** Add to NFR section — structured logging for agent actions.
   - **Severity:** Low — not needed for MVP.

---

## 3. Consistency Check

### PRD ↔ Product Brief Alignment

| Brief Claim | PRD Support | Consistent? |
|------------|-------------|-------------|
| "First open-source end-to-end modernization agent" | All 6 agent types defined | ✅ |
| "Self-hosted, vendor-neutral" | Docker Compose deployment, configurable LLM | ✅ |
| "Built on CopilotKit + LangGraph" | FR-001, FR-002, FR-003 | ✅ |
| "Wraps OpenMainframe CLI" | FR-004 with subprocess pattern | ✅ |
| "Human-in-the-loop" | FR-070, FR-071 with LangGraph interrupt | ✅ |
| MVP excludes migration planning | Scope section explicitly excludes | ✅ |
| MVP excludes code transformation | Scope section explicitly excludes | ✅ |

### PRD ↔ Technical Research Alignment

| Research Finding | PRD Handling | Consistent? |
|-----------------|-------------|-------------|
| No assess CLI command (CRITICAL) | Section 4.2 Prerequisites lists it | ✅ |
| No --format json output | Section 4.2 Prerequisites lists it | ✅ |
| Subprocess wrapping for MVP | FR-004 specifies subprocess pattern | ✅ |
| CardDemo as test target | Section 7.2 integration tests | ✅ |
| 120s compile timeout | FR-004 specifies 120s compile, 300s JCL | ✅ |
| Output truncation needed | FR-004 specifies 20KB max | ✅ |

### Internal Consistency

- All FR numbers are unique and sequential ✅
- Data model types match state schema ✅
- Interaction flows reference correct FR numbers ✅
- Release plan weeks align with scope ✅

---

## 4. Testability Check

Every functional requirement has been verified to have testable acceptance criteria:

| FR | Acceptance Criteria Testable? | Notes |
|----|------------------------------|-------|
| FR-001 | ✅ | "npm run dev starts both" — verifiable |
| FR-002 | ✅ | "ExperimentalEmptyAdapter used" — code check |
| FR-003 | ✅ | "AgentState extends CopilotKitState" — code check |
| FR-004 | ✅ | "120s timeout", "20KB max" — measurable |
| FR-010 | ✅ | "discovers .cbl files recursively" — verifiable |
| FR-011 | ✅ | "cyclomatic complexity score" — measurable output |
| FR-012 | ✅ | "11 built-in rules checked" — countable |
| FR-013 | ✅ | "JSON output for programmatic consumption" — parseable |
| FR-020 | ✅ | "reports success/failure with exit code" — verifiable |
| FR-021 | ✅ | "returns valid/invalid status" — binary outcome |
| FR-022 | ✅ | "suggests specific code changes" — reviewable |
| FR-030 | ✅ | "requires user approval" — HITL testable |
| FR-031 | ✅ | "captures DISPLAY output" — verifiable |
| FR-032 | ✅ | "returns structured AST" — parseable |
| FR-040 | ✅ | "section-by-section explanation" — reviewable |
| FR-041 | ✅ | "explains each step" — reviewable |
| FR-042 | ✅ | "condition → action format" — structured output |
| FR-050 | ✅ | "returns dataset names, types" — verifiable |
| FR-051 | ✅ | "requires approval before DELETE" — HITL testable |
| FR-060..064 | ✅ | Visual specifications in UX design — screenshot testable |
| FR-070..071 | ✅ | "LangGraph interrupt pauses" — behavior testable |
| FR-080..081 | ✅ | "frontend reads via useCoAgent" — integration testable |

---

## 5. Feasibility Check

### High Confidence (Will Work)

- CopilotKit + LangGraph integration — well-documented, starter templates exist
- Subprocess wrapping of CLI tools — standard Python pattern
- Generative UI for cards — CopilotKit has mature support
- Human-in-the-loop via interrupt — LangGraph core feature
- Code explanation via LLM — proven pattern with strong models

### Medium Confidence (Needs Validation)

| Item | Risk | Mitigation |
|------|------|------------|
| Assess CLI command addition | Requires Rust development | Clear crate API exists; CLI is thin wrapper |
| JSON output for all commands | Requires changes to multiple crates | Start with assess only; others can use text parsing |
| Large codebase assessment performance | May be slow for 1000+ files | Set expectations; async with progress updates |
| LLM accuracy on COBOL explanation | Model may not know obscure IBM extensions | Ground in AST data; add disclaimers |

### Prerequisite Validation Status

| Prerequisite | Status | Blocker? |
|-------------|--------|----------|
| Add assess CLI command | NOT STARTED | YES — must do before agent dev |
| Add --format json flag | NOT STARTED | PARTIAL — can parse text for MVP |
| Build OpenMainframe binary | READY | No — cargo build --release works |
| CardDemo compiles | NEEDS TESTING | Verify before agent integration tests |

---

## 6. UX ↔ PRD Alignment

Every PRD UI requirement maps to a UX design component:

| PRD Requirement | UX Component | Design Section |
|-----------------|-------------|---------------|
| FR-060 | ChatPanel (CopilotSidebar) | UX 2.1, 3 |
| FR-061 | CodeViewer with syntax highlighting | UX 4.3, 6.2 |
| FR-062 | AssessmentDashboard | UX 4.2 |
| FR-063 | JobTimeline | UX 4.4 |
| FR-064 | FileTreePanel with context menu | UX 2.1, 5.2 |
| FR-070 | ApprovalCard | UX 4.5 |
| FR-022 | CompilerOutputCard | UX 4.6 |

---

## 7. Recommendations

### Before Architecture Phase

1. **Validate CardDemo compilation** — Run `open-mainframe compile` on all CardDemo .cbl files to establish baseline success rate
2. **Prototype assess CLI** — Even a minimal `open-mainframe assess scan <dir> --format json` is sufficient for agent MVP
3. **Test CopilotKit starter** — Run `npx copilotkit@latest create -f langgraph-py` to validate the scaffolding works

### PRD Amendments (Optional)

1. Add FR-090: First-run onboarding experience
2. Add NFR for structured logging of agent actions
3. Clarify whether `--format json` is a hard prerequisite or if text parsing is acceptable for MVP

---

## 8. Verdict

**PRD is VALIDATED and ready for Architecture phase.**

The document is comprehensive, internally consistent, aligned with the product brief and technical research, and has fully testable acceptance criteria. The only blocker is the OpenMainframe CLI prerequisite (assess command), which should be addressed in parallel with agent development.

Phase 2 (Planning) is now COMPLETE.
