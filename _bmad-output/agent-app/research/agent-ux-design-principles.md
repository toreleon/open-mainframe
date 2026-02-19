---
track: 4
status: draft
date: 2026-02-19
iteration: 4
---

# Research Track 4: Agent UX Design Principles

**Purpose:** Synthesize emerging agent UX theory into concrete design principles **specific to OpenMainframe** — not generic "agent UX" but "here's how an agent that runs COBOL compilers, assesses codebases, and executes JCL jobs should present itself to a mainframe engineer."

---

## 1. Theoretical Foundations

### 1.1 From UXmatters — Designing for Autonomy

**Source:** [UXmatters — Designing for Autonomy](https://www.uxmatters.com/mt/archives/2025/12/designing-for-autonomy-ux-principles-for-agentic-ai.php)

Seven core principles and their OpenMainframe mapping:

| Principle | Definition | OpenMainframe Application |
|-----------|-----------|--------------------------|
| **Clarity of Intent** | Users express goals, limitations, exceptions, and success/failure criteria | User says "Assess my codebase" — agent should clarify: "Scan all .cbl files? Include copybooks? Focus on DB2 programs only?" |
| **Transparency as Primary Affordance** | Explain *why* decisions were made, what influenced them, confidence levels | Agent shows: "I'm running `assess scan` because you want migration readiness. I'll check 42 COBOL files for complexity and compatibility." |
| **Perceived Control** | Users need intervention capabilities even if rarely used | During assessment: pause, skip files, focus on specific directories. During compilation: cancel, retry, change flags. |
| **Visible Feedback** | Transform invisible processing into understandable narratives | "Scanning file 18/42: POLCY01.cbl — found 3 issues so far (1 critical)" |
| **Intentional Behavioral Design** | Define when the agent acts immediately vs. awaits confirmation | Assessment: act immediately. JCL execution: always confirm. Compilation: act immediately. Dataset deletion: always confirm. |
| **Embedded Ethical Constraints** | Encode boundaries through workflows, not policies | Never execute JCL without approval. Never delete datasets without confirmation. Truncate source code sent to LLM if `SEND_SOURCE_TO_LLM=false`. |
| **Collaborative Partnership** | AI complements human strengths | Agent provides speed (scan 42 files in seconds) and pattern recognition (spot IMS dependencies). Human provides judgment (which programs to prioritize) and domain knowledge (business rules the code implements). |

### 1.2 From Google A2UI — Agent-Driven Interfaces

**Source:** [Google A2UI](https://a2ui.org/), [Google Developers Blog](https://developers.googleblog.com/introducing-a2ui-an-open-project-for-agent-driven-interfaces/), [MarkTechPost](https://www.marktechpost.com/2025/12/22/google-introduces-a2ui-agent-to-user-interface-an-open-sourc-protocol-for-agent-driven-interfaces/)

Key concepts:

1. **Declarative UI from agents**: Agent generates UI descriptions (not code), frontend renders them natively. The UI is data, not executable.
2. **Component catalog**: Frontend maintains a set of trusted, pre-approved components. Agent selects and populates them.
3. **Incremental updates**: Agent can efficiently modify existing UI based on new data (e.g., update assessment progress bar).
4. **Framework-agnostic**: Same agent output renders on web, mobile, desktop.

**OpenMainframe application:**
- Define a catalog of components: AssessmentSummaryCard, CompilerDiagnosticCard, JobStepTimeline, CodeViewer, ApprovalGate, DatasetCatalog
- Agent selects which component to render based on CLI output type
- Agent incrementally updates (e.g., assessment progress: 18/42 → 19/42)
- This aligns with AG-UI's "Static Generative UI" pattern (high control, agent selects from predefined components)

### 1.3 From CopilotKit — Three Generative UI Patterns

**Source:** [CopilotKit — Generative UI Guide 2026](https://www.copilotkit.ai/blog/the-developer-s-guide-to-generative-ui-in-2026), [AG-UI and A2UI Explained](https://www.copilotkit.ai/blog/ag-ui-and-a2ui-explained-how-the-emerging-agentic-stack-fits-together)

| Pattern | Control | Description | OpenMainframe Fit |
|---------|---------|-------------|-------------------|
| **Static Generative UI** | High control | Frontend owns components; agent selects which to show and fills with data | **Best fit** — assessment dashboards, compiler cards, job timelines are pre-built React components |
| **Declarative Generative UI** | Shared control | Agent returns structured UI spec (A2UI); frontend renders with own styling | Future option for custom reports |
| **Open-ended Generative UI** | Low control | Agent returns full UI surface | Not suitable — too unpredictable for enterprise tool |

**Recommendation:** Use **Static Generative UI** for OpenMainframe. Pre-build React components for each CLI output type. Agent selects the right component and fills it with data from the CLI JSON output.

### 1.4 Variable Autonomy

**Source:** [UX Magazine — Designing for Autonomy](https://uxmag.com/articles/designing-for-autonomy-ux-principles-for-agentic-ai-systems), [Synclovis — Designing for Agentic AI](https://www.synclovis.com/articles/designing-for-agentic-ai-the-future-of-ux-ui/)

Three-mode autonomy progression:

| Mode | Description | OpenMainframe Mapping |
|------|------------|----------------------|
| **Shadow** | Agent recommends but doesn't act | "I recommend running `assess scan ./app/cbl`. Shall I proceed?" |
| **Assisted** | Agent acts with mandatory approval | JCL execution: agent parses job, shows preview, waits for approval |
| **Autonomous** | Agent acts within defined boundaries | Assessment scan: agent runs immediately after user states intent |

**OpenMainframe autonomy map:**

| Action | Autonomy Level | Why |
|--------|---------------|-----|
| Read files (cat, ls) | Autonomous | Non-destructive, informational |
| `assess scan` / `assess file` | Autonomous | Non-destructive analysis |
| `check` / `compile` | Autonomous | Non-destructive (compile produces artifact but doesn't execute) |
| `lex` / `parse-jcl` | Autonomous | Non-destructive analysis |
| `bms` | Autonomous | Generates files but doesn't execute |
| `interpret` | Assisted | Executes code — user should confirm |
| `run` (JCL) | Assisted (always) | Executes potentially side-effecting jobs |
| `idcams` (DEFINE, REPRO) | Assisted | Creates/modifies datasets |
| `idcams` (DELETE) | Assisted (always) | Destructive operation |
| `gdg create/delete` | Assisted | Creates/destroys data structures |
| `cics` | Assisted | Interactive session with side effects |
| `db2 preprocess` | Autonomous | Analysis only |

---

## 2. OpenMainframe-Specific Design Principles

Synthesized from all research tracks (CLI analysis, agent UX patterns, modernization landscape, and theory).

### Principle 1: The Agent Narrates, The Artifact Shows

The activity log (conversation) tells the **story** of what the agent is doing and why. The artifact panel shows the **result** in the best possible visualization.

- Agent text: "I scanned 42 COBOL files. Found 12 issues, 2 critical. Overall complexity: High. Estimated migration effort: 340 hours."
- Artifact panel: Assessment dashboard with summary cards, complexity chart, issue table, feature matrix.

**Why this matters for OpenMainframe:** CLI output (JSON) is rich and structured. A text-only activity log wastes this richness. A dashboard-only view hides the agent's reasoning. Both together create the best experience.

### Principle 2: Progressive Disclosure by Default

Show the minimum necessary information at each level. Expand on demand.

**Level 1 — Activity log entry:** "Compiled HELLO.cbl — 2 errors"
**Level 2 — Expand entry:** Shows error messages with severity icons
**Level 3 — Expand error:** Shows LLM explanation + fix suggestion + "Go to line" link
**Level 4 — Artifact panel:** Code viewer with inline error annotations

**Why this matters for OpenMainframe:** Mainframe engineers deal with complex output (JCL step results, COBOL diagnostics, assessment metrics). Dumping everything at once is overwhelming. Progressive disclosure respects their attention.

### Principle 3: Workflows, Not Commands

The user expresses an **outcome**, the agent orchestrates the **commands**.

- User: "Is this codebase ready for migration?"
- Agent internally: `assess scan` → analyze results → `assess file` on flagged programs → synthesize report
- User sees: progress updates → final assessment artifact

**Why this matters for OpenMainframe:** The CLI has 14 commands. Users shouldn't need to know them. The agent should select the right command sequence based on intent. This is the core differentiator from using the CLI directly.

### Principle 4: Trust Through Previews

Before any side-effecting operation, show what will happen.

- JCL execution: Show `parse-jcl` output as a job preview card → approval gate → then execute
- Dataset operations: Show what will be created/deleted → confirmation → then execute
- CICS session: Show BMS map preview → confirmation → then launch

**Why this matters for OpenMainframe:** Mainframe engineers are trained to be cautious (production systems handle billions of dollars). They will not trust an agent that executes without preview. The preview-then-approve pattern builds trust incrementally.

### Principle 5: Artifacts Are First-Class

Tool results are not chat messages to scroll past. They are **persistent artifacts** that can be:
- Referenced later ("Show me that assessment report again")
- Exported (Markdown, JSON, PDF)
- Compared (previous assessment vs. current)
- Acted upon ("Fix the 3 critical issues from the assessment")

**Why this matters for OpenMainframe:** Migration architects produce assessment reports for leadership. These reports must be exportable, shareable, and persistent — not buried in a chat scroll.

### Principle 6: Context Carries Forward

The agent remembers:
- Which files were assessed and what the results were
- Which programs compiled successfully and which failed
- Which JCL jobs were run and their return codes
- Previous conversations about specific programs

**Why this matters for OpenMainframe:** Migration is a multi-week/month process. Each session builds on previous work. The agent should know "last time we assessed, POLCY01 had 3 critical issues — let's check if they're fixed."

### Principle 7: Mainframe Metaphors Where They Help

Use mainframe concepts that users already understand:
- **Return codes** (RC=0 green, RC=4 yellow, RC=8+ red) — universal mainframe language
- **SYSOUT** — term mainframe engineers know for program output
- **Job steps** — familiar concept for batch processing
- **Condition codes** — understood by every mainframe developer

But modernize the presentation:
- Return codes as **colored badges**, not text
- SYSOUT in a **monospace panel**, not raw text dump
- Job steps as a **visual timeline**, not a list

**Why this matters for OpenMainframe:** The target user (50s, 20 years COBOL experience) should see familiar concepts presented in a modern, accessible way. Don't alienate them with unfamiliar abstractions; don't bore them with 1980s presentation.

### Principle 8: The Agent Explains, Not Just Reports

Every CLI output should be accompanied by an LLM-generated explanation:

- Compiler error → "This error means the variable WS-NAME is alphanumeric (PIC X) but you're using it in a COMPUTE which requires numeric. Change it to PIC 9 or use FUNCTION NUMVAL."
- Assessment issue → "IMS/DL1 calls are not supported by OpenMainframe. This program needs to be refactored to use DB2 or VSAM for data access."
- Job failure → "Step STEP030 failed with RC=12 because the input dataset MY.INPUT.FILE was not found. Check that the dataset exists and the DD statement SYSIN points to the correct name."

**Why this matters for OpenMainframe:** This is the #1 differentiator vs. using the CLI directly. Raw CLI output + LLM explanation = dramatically better UX than either alone.

### Principle 9: Source Stays Local, Trust Is Earned

Always communicate that source code stays on the user's machine:
- Connection status: "Connected to ~/carddemo via local bridge"
- Data flow indicator: "Commands execute locally. Only results are sent to the agent."
- Privacy controls: "Source code is NOT sent to the LLM" (configurable)

**Why this matters for OpenMainframe:** Enterprise customers (banking, government, healthcare) won't use a tool that sends COBOL source to the cloud. The local bridge architecture is a key differentiator — the UX must make this visible and trustworthy.

---

## 3. The Agent UX Stack for OpenMainframe

Based on all research, here's the technical UX stack:

```
┌─────────────────────────────────────────────────┐
│  User Intent Layer                               │
│  "Assess my codebase" / "Compile HELLO.cbl"     │
├─────────────────────────────────────────────────┤
│  Agent Reasoning (not visible by default)         │
│  Thinking → plan → tool selection                │
├─────────────────────────────────────────────────┤
│  AG-UI Event Stream                              │
│  TEXT_MESSAGE | TOOL_CALL | TOOL_RESULT | CUSTOM │
├─────────────────────────────────────────────────┤
│  Activity Log (transparent)                      │
│  Shows: thinking, commands, progress, text       │
├─────────────────────────────────────────────────┤
│  Static Generative UI Components                 │
│  Agent selects component, fills with CLI JSON    │
│  ┌──────────────┐ ┌───────────────┐ ┌─────────┐│
│  │ Assessment   │ │ Compiler      │ │ Job     ││
│  │ Dashboard    │ │ Diagnostic    │ │Timeline ││
│  │ Card         │ │ Card          │ │         ││
│  └──────────────┘ └───────────────┘ └─────────┘│
├─────────────────────────────────────────────────┤
│  Artifact Persistence                            │
│  Reports, compilation results, session history   │
└─────────────────────────────────────────────────┘
```

**Layer 1 — Intent:** User expresses outcome. Agent interprets.
**Layer 2 — Reasoning:** Agent thinks (visible via thinking_content events). Plans approach.
**Layer 3 — AG-UI:** Standardized event stream. Backend-agnostic.
**Layer 4 — Activity Log:** Every event rendered in the log. Progressive disclosure.
**Layer 5 — Generative UI Components:** Pre-built React components selected by the agent based on output type.
**Layer 6 — Persistence:** Artifacts saved, exportable, referenceable.

---

## 4. Mapping AG-UI Events to UX Behaviors

Based on the principles above, here's how each AG-UI event should manifest in the UI:

| AG-UI Event | Activity Log Entry | Artifact Panel Action | Principle |
|-------------|-------------------|----------------------|-----------|
| CUSTOM (thinking_start) | Collapsible "Thinking..." with spinner | None | Progressive Disclosure |
| CUSTOM (thinking_content) | Streaming reasoning text (collapsed by default) | None | Transparency |
| CUSTOM (thinking_end) | Thinking entry finalized, collapses | None | Progressive Disclosure |
| TOOL_CALL_START (bash) | "Running: `open-mainframe assess scan ./app/cbl`" | None (yet) | Transparency |
| TOOL_CALL_ARGS | Command text streaming (show what's being typed) | None | Transparency |
| TOOL_CALL_END | Command complete, status: "executing..." | None | Visible Feedback |
| TOOL_CALL_RESULT | "Assessment complete: 42 files, 12 issues" (summary) | **Render appropriate component** based on command type | Artifacts First-Class |
| TEXT_MESSAGE_START | Agent response beginning | None | Narration |
| TEXT_MESSAGE_CONTENT | Streaming agent explanation | None | Agent Explains |
| TEXT_MESSAGE_END | Response complete | None | — |
| CUSTOM (on_interrupt) | Approval card with operation details | Job preview in artifact panel | Trust Through Previews |
| RUN_FINISHED | Remove any lingering spinners | Artifact finalized | — |
| RUN_ERROR | Error card with explanation + recovery suggestion | None | Graceful Error Handling |

**Key insight:** The TOOL_CALL_RESULT event is the trigger for rendering the appropriate generative UI component. The agent's bash command is parsed to determine which component to show:
- Command contains `assess scan` → AssessmentDashboard
- Command contains `compile` or `check` → CompilerDiagnosticCard
- Command contains `run` → JobTimeline
- Command contains `parse-jcl` → JobPreviewCard
- Command contains `interpret` → ProgramOutputCard
- Other → GenericOutputCard (raw text with syntax highlighting)

---

## 5. The First Five Minutes

What happens when a mainframe engineer (50s, 20 years COBOL experience, never used an AI agent) opens OpenMainframe Agent for the first time?

**Minute 0-1: Arrival**
- Sees: Clean, dark-themed interface. Activity log (empty). Artifact panel with welcome message.
- Welcome message: "OpenMainframe Agent helps you assess, compile, and modernize COBOL codebases. Connect your local project to get started."
- Connection indicator: red dot — "No project connected"
- Setup instructions: "Run `openmainframe bridge connect --project ~/your-cobol-project` on your machine"

**Minute 1-2: Connection**
- Bridge daemon connects. Green dot appears: "Connected to ~/carddemo"
- Agent greets: "Connected to ~/carddemo. I found 47 COBOL files and 12 JCL files. What would you like to do?"
- Suggested actions appear: "Assess codebase" / "Compile a program" / "Explain a program" / "Run a JCL job"

**Minute 2-3: First Assessment**
- User clicks "Assess codebase" or types "assess my codebase"
- Activity log: thinking → `assess scan ./app/cbl` → progress updates (18/42...) → summary text
- Artifact panel: Assessment dashboard materializes — summary cards, complexity chart, issue table
- Agent narrates: "I found 42 COBOL programs totaling 28,450 lines. Average complexity: 6.2. 12 compatibility issues found, 2 critical. Estimated migration effort: 340 hours."

**Minute 3-4: Exploration**
- User asks: "What are the critical issues?"
- Agent: lists the 2 critical issues with file, line, description, recommendation
- User clicks an issue → artifact panel switches to code viewer at that line

**Minute 4-5: Compilation**
- User asks: "Compile POLCY01.cbl"
- Activity log: compile command → 2 errors
- Artifact panel: Code viewer with inline error annotations
- Agent explains each error in plain English with fix suggestions

**This five-minute journey demonstrates:** outcome-driven interaction, progressive disclosure, artifact-first display, agent narration + rich visualization, and trust through transparency.

---

## 6. Sources

- [UXmatters — Designing for Autonomy](https://www.uxmatters.com/mt/archives/2025/12/designing-for-autonomy-ux-principles-for-agentic-ai.php)
- [UX Magazine — Designing for Autonomy](https://uxmag.com/articles/designing-for-autonomy-ux-principles-for-agentic-ai-systems)
- [Synclovis — Designing for Agentic AI](https://www.synclovis.com/articles/designing-for-agentic-ai-the-future-of-ux-ui/)
- [Codewave — Designing Agentic AI UI](https://codewave.com/insights/designing-agentic-ai-ui/)
- [Agentic Design — UI/UX Patterns](https://agentic-design.ai/patterns/ui-ux-patterns)
- [Google A2UI](https://a2ui.org/)
- [Google Developers — Introducing A2UI](https://developers.googleblog.com/introducing-a2ui-an-open-project-for-agent-driven-interfaces/)
- [CopilotKit — Generative UI Guide 2026](https://www.copilotkit.ai/blog/the-developer-s-guide-to-generative-ui-in-2026)
- [CopilotKit — AG-UI and A2UI Explained](https://www.copilotkit.ai/blog/ag-ui-and-a2ui-explained-how-the-emerging-agentic-stack-fits-together)
- [CopilotKit — AG-UI Protocol](https://www.copilotkit.ai/ag-ui)
- [State of Design 2026](https://tejjj.medium.com/state-of-design-2026-when-interfaces-become-agents-fc967be10cba)
- [Top 10 Agentic AI Design Patterns — Enterprise Guide](https://www.aufaitux.com/blog/agentic-ai-design-patterns-enterprise-guide/)
- [MarkTechPost — Google A2UI](https://www.marktechpost.com/2025/12/22/google-introduces-a2ui-agent-to-user-interface-an-open-sourc-protocol-for-agent-driven-interfaces/)
- [AgileSoftLabs — Building Enterprise AI Agents 2026](https://www.agilesoftlabs.com/blog/2026/01/how-to-build-enterprise-ai-agents-in)
