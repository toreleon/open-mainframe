---
track: 5
status: draft
date: 2026-02-19
iteration: 5
---

# Research Cross-Reference Synthesis

**Purpose:** Cross-reference all 4 research tracks, resolve contradictions, fill critical gaps, and produce a unified set of decisions for the UX specification.

---

## 1. Contradictions Identified & Resolved

### Contradiction 1: Source Privacy vs. LLM Explanation

**Track 4, Principle 8:** "Every CLI output should be accompanied by LLM-generated explanation"
**Track 4, Principle 9:** "Source code NOT sent to LLM (configurable)"

**Resolution:** These are not contradictory — they represent a configurable spectrum:

| Privacy Setting | What Agent Sees | Explanation Quality | Use Case |
|----------------|----------------|--------------------|----|
| **Full access** (default) | CLI output + source code | Rich — agent can reference specific lines, variables, business rules | Most users, internal tools |
| **Output-only** | CLI output JSON only | Moderate — agent explains errors from diagnostic messages only, cannot reference surrounding code | Regulated environments where source is classified |
| **Minimal** | Return codes + status only | Low — agent can only report success/failure, suggest generic troubleshooting | Maximum security, compliance-only use |

**UX implication:** Show a privacy badge in the connection status: `🔒 Source shared` / `🔒 Output only` / `🔒 Minimal`. The explanation quality degrades gracefully — agent says "I can see the error is on line 42 but I don't have access to the source code to explain the specific variable issue. Enable source sharing for richer explanations."

**Decision:** Default to "Full access". Privacy setting configurable via `config` command or agent settings. Agent adapts explanation depth based on what it can see.

---

### Contradiction 2: Autonomous Compilation vs. User-Initiated

**Track 4 Autonomy Map:** `compile` and `check` marked as **Autonomous** ("non-destructive")
**Track 4 First-Five-Minutes:** User says "Compile POLCY01.cbl" → agent executes

**Resolution:** "Autonomous" means *no approval gate required*, not *proactively initiated*. The distinction:

| Autonomy Level | Meaning | Example |
|---------------|---------|---------|
| **Autonomous** | Agent executes without asking "May I?" when the operation is part of a user-requested workflow | User says "Assess codebase" → agent runs `assess scan` without asking permission |
| **Assisted** | Agent must show preview and get explicit approval before executing | User says "Run the job" → agent shows JCL preview → waits for "Yes, run it" |
| **Proactive** | Agent *suggests* an action it hasn't been asked for (future feature) | After assessment: "I found errors in POLCY01. Want me to compile it?" |

**Decision:** v1 has two levels: **Autonomous** (execute within workflow) and **Assisted** (require approval). Proactive suggestions are a v2 feature. All `interpret`, `run`, `idcams` (write ops), `gdg` (create/delete), and `cics` are Assisted. Everything else is Autonomous.

---

### Contradiction 3: Progressive Disclosure vs. Immediate Artifact Display

**Track 4, Principle 2:** "Start with summary, expand on demand (4-level disclosure)"
**Track 4, Principle 5:** "Artifacts are first-class and persistent"

**Resolution:** These work together, not against each other. The key is **where** each applies:

| UI Surface | Behavior | Rationale |
|-----------|----------|-----------|
| **Activity log** | Progressive disclosure — summary first, expand for details | Activity log is chronological narrative; too much detail buries the story |
| **Artifact panel** | Immediate display — full visualization appears when artifact is ready | Artifact panel exists *to show* the artifact; hiding it defeats the purpose |

**Example flow:**
1. Activity log: `✓ Assessment complete — 42 files, 12 issues (2 critical)` ← summary (Level 1)
2. Artifact panel: Full assessment dashboard immediately appears ← immediate
3. User expands activity log entry → sees individual file scores (Level 2)
4. User clicks issue in artifact panel → code viewer with annotation (Level 4)

**Decision:** Activity log uses progressive disclosure (4 levels). Artifact panel shows full visualization immediately when an artifact-producing command completes. The artifact panel IS the expanded view.

---

### Contradiction 4: Static Generative UI vs. Novel Situations

**Track 4, Section 1.3:** "Use Static Generative UI — frontend owns pre-built components"
**Track 4, Principle 8:** "Agent explains, not just reports" — implies agent may need custom presentation

**Resolution:** Static Generative UI handles 95% of cases. For the remaining 5%, use a **GenericCard** component:

| Situation | Component | Example |
|-----------|-----------|---------|
| `assess scan` output | AssessmentDashboard | Summary cards, charts, issue table |
| `compile` output | CompilerDiagnosticCard | Error list with severity, line refs |
| `run` output | JobTimeline | Step-by-step timeline with RCs |
| `parse-jcl` output | JobPreviewCard | Preview before execution |
| Agent explanation (no CLI output) | TextBlock (in activity log) | "The issue on line 42 is because..." |
| Unrecognized CLI output | GenericOutputCard | Monospace text with syntax highlighting |
| Mixed/novel presentation | Multiple components | Agent selects several: text + code viewer |

**Decision:** Pre-build 8-10 components (see Component Catalog below). Agent selects by output type. GenericOutputCard handles edge cases. Agent explanations live in the activity log text stream, not in artifact components.

---

### Contradiction 5: CLI-Centric Analysis vs. Workflow-Centric UX

**Track 1:** Organizes everything by CLI command (14 commands, each with schema and pain points)
**Track 2:** Organizes by interaction patterns (activity log, plan-execute, artifact preview)
**Track 4, Principle 3:** "Workflows, not commands"

**Resolution:** The research phase correctly uses CLI commands as the atomic unit of analysis (what does each command produce?). The design phase must reorganize around workflows (what does the user want to accomplish?). The UX spec should:

1. **Never expose command names** in the primary UI (no "Running `assess scan`" in the activity log header)
2. **Show command names** only in expanded tool-call details (Level 2-3 progressive disclosure)
3. **Name activities by outcome**: "Assessing codebase...", "Compiling program...", "Executing job..."
4. **Map commands to workflows** in the component selection logic, not in the user-facing UI

**Decision:** Activity log shows outcome-oriented labels. Command names visible only in expanded tool-call details for transparency. Agent references "assessment" not "`assess scan`" in its text responses.

---

## 2. Unified Component Catalog

Synthesized from Tracks 1, 2, and 4. Each component maps to specific CLI output:

| Component | Triggered By | Data Source | Key Elements |
|-----------|-------------|-------------|-------------|
| **AssessmentDashboard** | `assess scan` result | `{results[], overall_complexity, total_lines, total_issues, critical_issues, estimated_effort_hours}` | Summary stat cards (files, LOC, complexity, issues, effort), complexity distribution bar, sortable program table, issue list with severity badges |
| **ProgramDetailCard** | `assess file` result | `{file_name, program_id, metrics{}, features[], issues[], complexity, recommendations[]}` | Metrics row (LOC, complexity, paragraphs, data items), feature tags, issue list with line refs, recommendations |
| **CompilerDiagnosticCard** | `compile` or `check` result | `{status, program_id, diagnostics[], summary{errors, warnings, infos}}` | Status badge (✓/✗), summary counts, expandable diagnostic list (severity icon, message, line:col), LLM explanation per diagnostic |
| **JobPreviewCard** | `parse-jcl` result (pre-execution) | `{job_name, class, steps[]{exec, parm, dd_statements[]}}` | Job header (name, class), step list with program names, DD statement summary, "Approve & Run" / "Cancel" buttons |
| **JobTimeline** | `run` result | `{job_name, return_code, steps[]{name, return_code, success, stdout[], stderr[]}}` | Overall RC badge, vertical timeline with step nodes (green/yellow/red), expandable SYSOUT per step, re-run button |
| **ProgramOutputCard** | `interpret` result | `{output[], return_code, diagnostics[]}` | RC badge, monospace output display (DISPLAY lines), diagnostics if any |
| **CodeViewer** | File read + annotations | Source text + annotation overlay from diagnostics/assessment | Monospace COBOL source, line numbers, inline annotations (error markers, issue highlights), horizontal scroll for 80+ col |
| **ApprovalGate** | `on_interrupt` CUSTOM event | `{type, question, options[]}` | Inline card in activity log: description of pending action, what will happen, Approve/Reject buttons |
| **GenericOutputCard** | Any unrecognized output | Raw text or JSON | Monospace display with syntax highlighting, collapsible |
| **DatasetOperationCard** | `idcams`/`gdg` result (text parsed by agent) | Agent-parsed text output | Operation type badge (DEFINE/DELETE/REPRO), result status, dataset info |

**10 components total.** This covers all 14 CLI commands:

| CLI Command | Component(s) Used |
|------------|------------------|
| `assess scan` | AssessmentDashboard |
| `assess file` | ProgramDetailCard |
| `compile` | CompilerDiagnosticCard + CodeViewer |
| `check` | CompilerDiagnosticCard + CodeViewer |
| `run` | JobTimeline (preceded by JobPreviewCard) |
| `interpret` | ProgramOutputCard |
| `parse-jcl` | JobPreviewCard |
| `lex` | GenericOutputCard |
| `bms` | GenericOutputCard (+ BMS preview in v2) |
| `cics` | Deferred to v2 (GenericOutputCard as placeholder) |
| `idcams` | DatasetOperationCard |
| `gdg` | DatasetOperationCard |
| `db2 preprocess` | GenericOutputCard (SQL highlighted) |
| `config` | GenericOutputCard |

---

## 3. Unified Approval Gate Design

Referenced in Tracks 1, 2, and 4 but never fully designed. Here's the specification:

### When Approval Is Required

| Operation | Trigger | Preview Shows |
|-----------|---------|--------------|
| JCL execution (`run`) | Agent calls `parse-jcl` first, then presents preview | Job name, steps, programs, DD statements, estimated behavior |
| COBOL interpretation (`interpret`) | Agent about to execute code | Program name, what it does (if known), that it will execute |
| Dataset creation (`idcams DEFINE`, `gdg create`) | Agent about to create dataset | Dataset name, type, allocation parameters |
| Dataset deletion (`idcams DELETE`, `gdg delete`) | Agent about to delete dataset | Dataset name, ⚠️ warning that data will be lost |
| CICS session (`cics`) | Agent about to launch interactive session | Program name, transaction ID |

### Approval Gate UX

```
┌─────────────────────────────────────────────────┐
│ ⚠️  Agent wants to execute a JCL job             │
│                                                  │
│  Job: RUNCARD                                    │
│  Steps: 3                                        │
│  ┌────────────────────────────────────────────┐  │
│  │ Step 1: COMPILE  — EXEC PGM=IGYCRCTL      │  │
│  │ Step 2: LINK     — EXEC PGM=IEWL           │  │
│  │ Step 3: RUN      — EXEC PGM=HELLO          │  │
│  └────────────────────────────────────────────┘  │
│                                                  │
│  This will compile, link, and execute HELLO.     │
│                                                  │
│  [ Approve ]  [ Reject ]  [ Edit parameters ]   │
└─────────────────────────────────────────────────┘
```

**Behavior:**
- Appears **inline** in the activity log (not a modal — modals block context reading)
- No timeout — waits indefinitely for user response
- "Edit parameters" opens the JCL preview for modification (v2 feature, v1 shows view-only)
- On reject: agent acknowledges and asks what to do instead
- On approve: activity log shows "✓ Approved — executing job..." and JobTimeline appears in artifact panel

**AG-UI mapping:** Uses `CUSTOM (on_interrupt)` event with `type: "approval"`, rendered as ApprovalGate component.

---

## 4. Progress Indication Design

Gap identified in Tracks 1 and 2. Here's the specification:

### Progress Types

| Operation | Progress Type | UX Treatment |
|-----------|--------------|-------------|
| `assess scan` (many files) | **Determinate** — file count known | Progress text: "Assessing 18 of 42 files..." + percentage |
| `compile` (single file) | **Indeterminate** — unknown duration | Spinner: "Compiling HELLO.cbl..." |
| `run` (multi-step JCL) | **Step-based** — step count known from parse-jcl | Step indicator: "Running step 2 of 3: LINK..." |
| `interpret` (single file) | **Indeterminate** | Spinner: "Running HELLO.cbl..." |
| Agent thinking | **Indeterminate** | "Thinking..." with collapsible thinking stream |
| LLM response generation | **Streaming** — text appears incrementally | Text streams word-by-word in activity log |

### Progress in Activity Log

```
[thinking...] ← collapsible, dimmed
[⚙ Assessing codebase]
   Scanning 18 of 42 files... (43%)
   Current: POLCY01.cbl
[✓ Assessment complete — 42 files, 12 issues]  ← replaces progress when done
```

**Implementation:** Agent sends progress updates as `TEXT_MESSAGE_CONTENT` events. The frontend detects progress patterns (e.g., "X of Y") and renders progress bars. Alternatively, use `CUSTOM` events with `type: "progress"` carrying `{current, total, label}`.

**Decision:** Use `CUSTOM` events for structured progress. Cleaner than parsing text.

---

## 5. Error Handling & Recovery Patterns

Gap identified across all tracks. Specification:

### Error Categories

| Error Type | Source | UX Treatment | Recovery |
|-----------|--------|-------------|----------|
| **Compilation error** | `compile` diagnostics | CompilerDiagnosticCard with per-error explanation | Agent suggests fix; user can say "fix it" |
| **JCL job failure** | `run` step RC ≥ 8 | JobTimeline with red step node + expanded stderr | Agent explains failure; suggests JCL fix |
| **CLI command failure** | Non-zero exit code | Error card with command, exit code, stderr | Agent retries or suggests alternative |
| **Bridge disconnect** | WebSocket close | Connection status: red dot + "Disconnected" | Auto-reconnect with retry; manual "Reconnect" button |
| **LLM error** | `RUN_ERROR` event | "Agent encountered an error" card | "Retry" button; falls back to showing raw CLI output |
| **Timeout** | Long-running command | "Operation timed out" card | "Retry" or "Cancel" buttons |

### Error Display Pattern

```
┌─────────────────────────────────────────────────┐
│ ✗ Compilation failed — 2 errors                  │
│                                                  │
│  ┌──────────────────────────────────────────┐    │
│  │ ERROR  Line 42, Col 12                   │    │
│  │ 'WS-NAME' not defined in WORKING-STORAGE │    │
│  │                                          │    │
│  │ Explanation: The variable WS-NAME is     │    │
│  │ used in the PROCEDURE DIVISION but not   │    │
│  │ declared. Add it to WORKING-STORAGE:     │    │
│  │                                          │    │
│  │   01 WS-NAME PIC X(30).                  │    │
│  │                                          │    │
│  │ [View in code] [Apply fix]               │    │
│  └──────────────────────────────────────────┘    │
│                                                  │
│  ┌──────────────────────────────────────────┐    │
│  │ ERROR  Line 87, Col 8                    │    │
│  │ Unexpected token 'PREFORM'               │    │
│  │                                          │    │
│  │ Explanation: Likely a typo. Did you mean │    │
│  │ 'PERFORM'?                               │    │
│  │                                          │    │
│  │ [View in code] [Apply fix]               │    │
│  └──────────────────────────────────────────┘    │
│                                                  │
│  [ Fix all and recompile ]                       │
└─────────────────────────────────────────────────┘
```

**"Apply fix" behavior (v2):** Agent edits source via bridge, recompiles. In v1, agent describes the fix; user applies manually.

---

## 6. Accessibility Specifications

Gap identified in all tracks. Specification:

| Requirement | Implementation |
|-------------|---------------|
| **Color-blind safe palette** | Never rely on color alone. Severity uses icons: ✗ (error), ⚠ (warning), ℹ (info), ✓ (success). RC badges use shape + color: circle green (0), triangle yellow (4), square red (8+) |
| **Keyboard navigation** | Tab through activity log entries, artifact panel sections, approval buttons. Enter to expand/collapse. Escape to close expanded views |
| **Screen reader** | Activity log entries have aria-labels describing content. Artifact components have aria-describedby for charts/tables. Progress updates use aria-live="polite" |
| **High contrast** | Respect `prefers-contrast: more`. Ensure WCAG AA (4.5:1) for all text. WCAG AAA (7:1) for code viewer |
| **Font sizing** | Respect user font size preferences. Code viewer uses monospace at user's preferred size. Minimum 14px for body text |
| **Motion** | Respect `prefers-reduced-motion`. Disable streaming text animation, collapse/expand animations. Progress bars update discretely |

---

## 7. Key Design Decisions Summary

These decisions are binding for the UX specification:

| # | Decision | Rationale |
|---|---------|-----------|
| 1 | **Hybrid: Activity Log + Artifact Panel** | Activity log for transparency (Track 2), artifact panel for rich visualization (Track 2 recommendation) |
| 2 | **Static Generative UI** | Agent selects pre-built components, fills with CLI JSON (Track 4, CopilotKit analysis). 10 components cover all commands |
| 3 | **Activity log = progressive disclosure; Artifact panel = immediate display** | Resolution of Principles 2 vs 5 contradiction |
| 4 | **Workflow-oriented labels, command names in expanded details** | Resolution of CLI-centric vs workflow-centric contradiction (Track 4 Principle 3) |
| 5 | **Two autonomy levels in v1: Autonomous and Assisted** | Simplified from Track 4's three-level model; proactive suggestions deferred to v2 |
| 6 | **Inline approval gates, not modals** | Modals block context reading; inline cards keep activity log flow |
| 7 | **CUSTOM events for structured progress** | Better than parsing text for progress indication |
| 8 | **Full source access by default, configurable** | Resolution of privacy vs explanation contradiction |
| 9 | **10 pre-built artifact components** | Covers all 14 CLI commands; GenericOutputCard for edge cases |
| 10 | **CICS deferred to v2** | Track 1 CICS challenge analysis; xterm.js + PTY bridge is high effort |
| 11 | **Dark theme primary, light theme optional** | Mainframe engineers accustomed to dark terminals; modern agent tools use dark |
| 12 | **Desktop-first, responsive down to tablet** | Primary users work at desks; no mobile requirement for v1 |

---

## 8. Gaps Deferred to UX Specification (Phase 2)

These gaps are identified but intentionally left for the UX spec to design:

| Gap | Assigned To |
|-----|-------------|
| ASCII wireframes for all 10 screen states | UX spec section 4 |
| Exact color palette and typography | UX spec section 9 |
| Responsive breakpoints | UX spec section 10 |
| Export format specifications (PDF, Markdown, JSON) | UX spec section 8 |
| Cross-session context persistence mechanism | UX spec section 8 (interaction patterns) |
| First-visit onboarding flow | UX spec section 4 (screen 1) |
| Input modalities beyond text chat | UX spec section 8 |
| React component tree hierarchy | UX spec section 7 |

---

## 9. Gaps Deferred to v2

| Feature | Rationale |
|---------|-----------|
| CICS interactive terminal (xterm.js) | High engineering effort; Track 1 recommends deferring |
| Proactive agent suggestions ("Want me to compile?") | Need v1 data on user behavior first |
| "Apply fix" button (agent edits source) | Requires write access through bridge; v1 is read + execute only |
| BMS screen layout preview | Needs custom renderer for 24x80 field layout |
| Cross-assessment comparison ("compare to last month") | Requires persistence layer and comparison UI |
| Multi-agent parallel execution view | Track 2 mentions Mission Control; high complexity |
| Custom workflow templates | Track 3's AWS Transform job plans; v1 uses agent-driven workflows |
| Internationalization | English-only for v1 |

---

## 10. Research Completeness Assessment

| Track | Document | Status | Quality | Gaps Remaining |
|-------|----------|--------|---------|----------------|
| 1 | openmainframe-cli-ux-analysis.md | ✅ Complete | Strong — covers all 14 commands, JSON schemas, 6 workflows, CICS analysis | Minor: could add `db2 preprocess` JSON schema from source |
| 2 | agent-native-ux-patterns.md | ✅ Complete | Strong — 6 apps analyzed, 8 core patterns, anti-patterns, hybrid recommendation | Minor: generative UI implementation detail deferred to UX spec |
| 3 | modernization-ux-landscape.md | ✅ Complete | Good — 6 competitors, per-output comparison, blue-ocean features | Minor: pricing/onboarding analysis not needed for UX spec |
| 4 | agent-ux-design-principles.md | ✅ Complete | Strong — 9 principles, autonomy mapping, first-five-minutes, UX stack | Contradictions resolved in this document |
| 5 | cross-reference-synthesis.md | ✅ Complete | This document | — |

**Verdict: All research is complete. Ready for Phase 2 (UX Specification Design).**
