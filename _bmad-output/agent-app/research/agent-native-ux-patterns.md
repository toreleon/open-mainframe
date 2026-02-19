---
track: 2
status: draft
date: 2026-02-19
iteration: 2
---

# Research Track 2: Agent-Native UX Patterns

**Purpose:** Analyze how leading agent-native applications present their UX, extract patterns, and evaluate which patterns apply to OpenMainframe's specific workflows (assessment, compilation, execution, explanation, CICS, dataset management).

---

## 1. Application Analysis

### 1.1 Claude Code (Anthropic)

**Source:** [Claude Code Docs](https://code.claude.com/docs/en/skills), [UX Writing Hub](https://uxwritinghub.com/claude-code-ux-writing/), [Medium — Beyond the Chatbox](https://medium.com/@vinayanand2/beyond-the-chatbox-a-non-technical-guide-to-mastering-claude-code-in-2026-8f7acd3a6e7d)

**Interaction model:** Terminal-based activity log. Linear stream of: thinking → tool calls → tool results → text responses.

| Question | Answer |
|----------|--------|
| How does the user express intent? | Natural language in a terminal prompt. "Find all error messages and make them friendlier." |
| How does the agent show plan/thinking? | Collapsible "thinking" blocks. Extended thinking (ultrathink) for complex reasoning. Plan mode writes multi-step strategy and waits for "OK". |
| How does the agent show progress? | Activity log entries: each tool call appears as a line item with status (streaming → executing → done). Thinking spinner during processing. |
| How does the user intervene/redirect? | Type a new message mid-stream. Cancel current operation. Adjust with follow-up instructions. |
| Primary artifacts vs conversation? | **Conversation IS the interface.** Artifacts are files modified on disk. The activity log shows what happened; the file system holds the result. |
| Trust/transparency? | Full visibility: every tool call shown with command + output. Thinking blocks show reasoning. Nothing hidden. |

**OpenMainframe applicability:**

| Workflow | Fit | Notes |
|----------|-----|-------|
| Assessment | Medium | Activity log can show assess commands, but rich assessment data (charts, tables) is hard to render in a terminal-style log |
| Compile loop | High | Compiler errors → agent thinks → suggests fix is a natural activity log pattern |
| JCL execution | Medium | Step timeline needs richer visualization than a text log |
| Code explanation | High | Text-heavy output works well in a log format |
| CICS | Low | Interactive TUI cannot embed in activity log |
| Dataset | Medium | Operation results are simple enough for log entries |

**Key takeaway:** The activity log model excels at transparency and simplicity but struggles with rich data visualization. OpenMainframe's assessment results (charts, matrices, tables) need more than text.

---

### 1.2 Cursor (Anysphere)

**Source:** [Prismic blog review](https://prismic.io/blog/cursor-ai), [yoDev — Cursor 2.2](https://www.yodev.dev/t/cursor-2-2-visual-editor-debug-mode-the-rise-of-ai-subagents/1348), [Cursor changelog](https://blog.promptlayer.com/cursor-changelog-whats-coming-next-in-2026/)

**Interaction model:** IDE with agent mode. Code editor + agent panel + terminal. Agent mode (Composer) operates autonomously with terminal access, browser, and subagents.

| Question | Answer |
|----------|--------|
| How does the user express intent? | Natural language in Composer panel. "Refactor the cache layer to use Redis." |
| How does the agent show plan/thinking? | Plan mode: agent writes out multi-step plan, user reviews and approves before execution. Inline diffs show proposed changes. |
| How does the agent show progress? | Agent operates in sandboxed environment. Terminal output visible. Subagents run in parallel. "Mission Control" grid view shows all active agents. |
| How does the user intervene? | Edit the plan before execution. Accept/reject individual diffs. Switch to manual editing. |
| Primary artifacts vs conversation? | **Code diffs are the primary artifact.** Conversation drives the agent; file changes are the result. |
| Trust/transparency? | Inline diffs let user review every change before accepting. Terminal output visible. Plan review before execution. |

**OpenMainframe applicability:**

| Workflow | Fit | Notes |
|----------|-----|-------|
| Assessment | Low | IDE-centric model doesn't suit assessment dashboards |
| Compile loop | High | Editor + diagnostics + inline fix suggestions is the core IDE use case |
| JCL execution | Low | IDE model doesn't naturally show job timelines |
| Code explanation | High | Inline annotations in the editor are powerful for code understanding |
| CICS | Low | IDE doesn't help with 3270 screens |
| Dataset | Low | Not relevant to IDE workflow |

**Key takeaway:** The IDE + agent model is powerful for code editing workflows but too narrow for OpenMainframe's diverse needs (assessment, execution, dataset management). The **plan-then-execute** pattern and **Mission Control** for parallel agents are worth adopting.

---

### 1.3 Devin (Cognition)

**Source:** [Qubika review](https://qubika.com/blog/devin-ai-coding-agent/), [Devin Docs](https://docs.devin.ai/), [AI Tools DevPro guide](https://aitoolsdevpro.com/ai-tools/devin-guide/)

**Interaction model:** Full autonomous agent with workspace. Chat + embedded IDE + terminal + browser + planning tools. Cloud-based sandbox.

| Question | Answer |
|----------|--------|
| How does the user express intent? | Natural language task description. "Implement user authentication with JWT." |
| How does the agent show plan/thinking? | Shows plan while implementing. Live architectural diagrams. Devin Wiki for auto-generated documentation. |
| How does the agent show progress? | Real-time workspace view: code editor updates live, terminal shows commands, browser shows test results. User can "follow along" or "take over" at any time. |
| How does the user intervene? | Take over in the embedded IDE. Edit code directly. Ask Devin questions about its approach. Modify the plan mid-execution. |
| Primary artifacts vs conversation? | **Working code is the primary artifact.** Devin Wiki provides generated documentation. The workspace is the artifact. |
| Trust/transparency? | Full workspace visibility. User can see every file change, every command, every browser action. Take-over capability for maximum control. |

**OpenMainframe applicability:**

| Workflow | Fit | Notes |
|----------|-----|-------|
| Assessment | High | Autonomous "assess the codebase" → agent scans, produces report → user reviews artifact |
| Compile loop | High | Agent compiles, sees errors, fixes, recompiles autonomously |
| JCL execution | High | Agent plans execution, user approves, agent runs and monitors |
| Code explanation | Medium | Wiki-style documentation is good, but real-time explanation is better |
| CICS | Medium | Browser widget could potentially embed a terminal |
| Dataset | Medium | Agent handles setup autonomously |

**Key takeaway:** Devin's model of **autonomous execution with workspace visibility and take-over** is the closest to what OpenMainframe needs. The "follow along or take over" pattern is excellent. Multiple parallel agents (Devin 2.0) matches OpenMainframe's need for concurrent assessment + compilation.

---

### 1.4 GitHub Copilot Workspace

**Source:** [GitHub Next](https://githubnext.com/projects/copilot-workspace), [Copilot Workspace user manual](https://github.com/githubnext/copilot-workspace-user-manual), [Coveros review](https://www.coveros.com/inside-look-github-copilot-workspace/)

**Interaction model:** Structured pipeline: Task → Spec → Plan → Code. Four-stage linear flow.

| Question | Answer |
|----------|--------|
| How does the user express intent? | Define a "Task" — describe the problem or desired feature in natural language. |
| How does the agent show plan/thinking? | Explicit Spec and Plan stages. User reviews specification and step-by-step plan before any code is generated. |
| How does the agent show progress? | Stage-by-stage progression. User sees which stage is active. Code generation shows file-by-file progress. |
| How does the user intervene? | Edit the Spec. Modify the Plan. Accept/reject individual steps. Iterate on any stage before proceeding. |
| Primary artifacts vs conversation? | **The Plan is the primary artifact.** Code is generated from the approved plan. |
| Trust/transparency? | User approves every stage. Nothing executes without explicit consent. Full visibility into what will change. |

**OpenMainframe applicability:**

| Workflow | Fit | Notes |
|----------|-----|-------|
| Assessment | High | Task: "Assess codebase" → Plan: scan steps → Execute: assess scan → Report: dashboard |
| Compile loop | Medium | Overkill for a single compile. Better for multi-file batch compilation. |
| JCL execution | High | Task: "Run batch job" → Plan: parse-jcl preview → Approve → Execute → Report |
| Code explanation | Low | Over-structured for a conversational explanation |
| CICS | Low | Not a pipeline task |
| Dataset | Medium | Setup could be a planned multi-step task |

**Key takeaway:** The **Task → Plan → Execute → Report** pipeline is excellent for OpenMainframe's structured workflows (assessment, execution). But it's over-structured for quick tasks (explain code, compile one file). Need a hybrid: pipeline for complex workflows, conversational for quick ones.

---

### 1.5 Bolt.new / v0.dev

**Source:** [Bolt.new](https://bolt.new), [Bolt intro docs](https://support.bolt.new/building/intro-bolt), [UI Bakery comparison](https://uibakery.io/blog/v0-dev-vs-bolt-new)

**Interaction model:** Generative UI. Chat prompt → live artifact preview. Split-screen: chat left, preview right.

| Question | Answer |
|----------|--------|
| How does the user express intent? | Natural language prompt. "Create a pizza ordering website." |
| How does the agent show plan/thinking? | Minimal plan display. Agent just starts building. Code appears in editor, preview renders live. |
| How does the agent show progress? | Live preview updates in real-time. Code editor shows files being created/modified. |
| How does the user intervene? | Follow-up prompts. Direct code editing. Visual editor (drag and drop). |
| Primary artifacts vs conversation? | **The live preview IS the primary artifact.** Chat is the instruction channel; the preview is what matters. |
| Trust/transparency? | Full code visibility. Preview matches code. What you see is what you get. |

**OpenMainframe applicability:**

| Workflow | Fit | Notes |
|----------|-----|-------|
| Assessment | High | Chat left + live assessment dashboard right (generated from CLI output) |
| Compile loop | Medium | Preview could show code with error annotations |
| JCL execution | Medium | Could preview job timeline as it executes |
| Code explanation | High | Chat explanation left + annotated code preview right |
| CICS | Low | Not applicable |
| Dataset | Low | Not visual enough to benefit from preview |

**Key takeaway:** The **split-screen artifact preview** pattern is powerful for OpenMainframe. Chat/agent interaction on one side, live artifact (assessment dashboard, code viewer, job timeline) on the other. This is the most transferable pattern from the generative UI world.

---

### 1.6 AWS Transform for Mainframe

**Source:** [AWS Transform blog](https://aws.amazon.com/blogs/aws/aws-transform-for-mainframe-introduces-reimagine-capabilities-and-automated-testing-functionality/), [AWS Transform page](https://aws.amazon.com/transform/mainframe/), [re:Invent 2025 blog](https://aws.amazon.com/blogs/migration-and-modernization/aws-for-mainframe-modernization-reinvent-2025-guide/)

**Interaction model:** Multi-agent pipeline with web console. AI-powered chat + assessment dashboard + transformation pipeline.

| Question | Answer |
|----------|--------|
| How does the user express intent? | Select predefined workflows (full modernization, analysis focus, business logic focus) or create custom combination. Chat interface for ad-hoc questions. |
| How does the agent show plan/thinking? | "Job plans" — customizable workflow steps. Visual dependency mapping. AI-generated insights alongside metrics. |
| How does the agent show progress? | Pipeline stage visualization. Per-component status. Metrics dashboards (LOC, component types, dependencies). |
| How does the user intervene? | Customize job plans. Review and approve transformation steps. Select which components to modernize. |
| Primary artifacts vs conversation? | **Assessment reports and transformed code are primary artifacts.** Chat is secondary. |
| Trust/transparency? | Visual dependency graphs. Component classification tables. Testing validation reports. |

**OpenMainframe applicability:**

| Workflow | Fit | Notes |
|----------|-----|-------|
| Assessment | Very High | This IS what AWS Transform does. Visual dependency mapping, component classification, metrics dashboards. |
| Compile loop | Low | AWS Transform focuses on transformation, not development iteration |
| JCL execution | Medium | Pipeline visualization could show job execution stages |
| Code explanation | Medium | Code analysis with AI-generated insights |
| CICS | Low | AWS handles differently (Blu Age transformation) |
| Dataset | Low | AWS uses different data migration approach |

**Key takeaway:** AWS Transform validates that **assessment dashboards with visual dependency graphs** are the right approach for migration architects. Their **customizable job plans** pattern (predefined workflows + custom combinations) is worth adopting. However, their UI is a traditional web console, not agent-native — it's a dashboard with AI features, not an AI agent with dashboard features.

---

## 2. Pattern Synthesis

### 2.1 Core Patterns Identified

| Pattern | Source | Description | OpenMainframe Relevance |
|---------|--------|-------------|------------------------|
| **Activity Log** | Claude Code | Linear stream of thinking → actions → results | Good for transparency; needs augmentation for rich data |
| **Plan-Then-Execute** | Cursor, Copilot Workspace | Agent proposes plan, user reviews, then executes | Perfect for assessment and JCL execution workflows |
| **Artifact Preview** | Bolt.new, v0.dev | Split-screen: conversation left, live artifact right | Strong for assessment dashboards and code explanation |
| **Autonomous Workspace** | Devin | Agent works in visible sandbox, user can follow or take over | Best for long-running autonomous tasks (full codebase assessment) |
| **Mission Control** | Cursor 2.0, AWS Transform | Multi-agent overview, parallel task tracking | Good for concurrent assessment + compilation tasks |
| **Pipeline Stages** | Copilot Workspace, AWS Transform | Task → Plan → Execute → Report linear flow | Matches assessment workflow perfectly |
| **Customizable Job Plans** | AWS Transform | Predefined + custom workflow combinations | Fits OpenMainframe's diverse command combinations |
| **HITL Approval Gates** | All (universal) | Human approval before destructive/irreversible actions | Essential for JCL execution, dataset modifications |

### 2.2 Anti-Patterns Identified

| Anti-Pattern | Source | Problem | OpenMainframe Lesson |
|-------------|--------|---------|---------------------|
| **Chat-only output** | Basic chatbots | Rich data (tables, charts) crammed into chat bubbles | Assessment results MUST be rendered as proper UI components, not text |
| **IDE-as-agent** | VS Code copilots | Agent is secondary to the IDE; user still drives everything | OpenMainframe agent should drive workflows, not be a sidebar |
| **Dashboard-with-chatbot** | Legacy modernization tools | Dashboard is primary, chat is bolted on | The agent should generate/control dashboards, not the reverse |
| **No intermediate state** | Early AI tools | User waits blindly until result appears | Show thinking, show tool calls, show progress. Always. |
| **All-or-nothing autonomy** | Binary autonomous systems | Agent either does everything or nothing | Variable autonomy: agent drives, human steers at any granularity |

---

## 3. Design Principles from Agent UX Theory

### From Agentic Design Patterns (agentic-design.ai)

**Source:** [Agentic Design Patterns — UI/UX](https://agentic-design.ai/patterns/ui-ux-patterns)

Key patterns applicable to OpenMainframe:

1. **Progressive Disclosure (PDP)** — Layer information by relevance. Assessment summary first, drill into per-program details on demand. Compiler errors summary first, expand individual diagnostics on demand.

2. **Mixed-Initiative Interface (MIP)** — Control shifts between human and agent based on context. Agent drives during assessment scan; human takes over to review specific programs. Agent proposes JCL execution; human approves; agent executes.

3. **Agent Status & Activity (ASP)** — Real-time activity indicators. Show when agent is: thinking, executing a command, waiting for approval, processing results.

4. **Monitoring and Control (MCP)** — Mission-control style oversight. For long-running tasks (codebase assessment), show a dashboard of what's happening rather than a scrolling log.

5. **Error Handling and Recovery (ERP)** — Graceful error communication. When compilation fails, don't just show the error — show the explanation, the fix suggestion, and a "fix and retry" button.

### From Codewave Insights

**Source:** [Designing Agentic AI UI](https://codewave.com/insights/designing-agentic-ai-ui/)

9 principles mapped to OpenMainframe:

| Principle | OpenMainframe Application |
|-----------|--------------------------|
| **Goal-Oriented Design** | User says "Assess my codebase" — agent handles the multi-step process |
| **Transparent Communication** | Show which CLI commands are running, what they return, why the agent chose them |
| **Adaptive Feedback Loops** | If agent's assessment approach isn't useful, user can redirect (e.g., "focus on DB2 programs only") |
| **Graceful Error Handling** | Compilation failure → undo-friendly, explanation-rich error cards |
| **Layered User Control** | Assessment runs autonomously; JCL execution requires approval; compilation is interactive |
| **Balanced Visibility** | Show summary by default; offer deep CLI output on demand |
| **Proactive Assistance** | After assessment: "I found 3 critical issues. Want me to explain the most complex program?" |
| **Multimodal Input** | Text chat + file selection + code pasting + drag-and-drop |
| **Cross-Session Memory** | Remember previous assessments, compilation results, project state |

### From State of Design 2026

**Source:** [State of Design 2026 — When Interfaces Become Agents](https://tejjj.medium.com/state-of-design-2026-when-interfaces-become-agents-fc967be10cba)

Key insight: **Generative UI** — the interface itself is created on the fly by the agent. Instead of pre-built dashboards, the agent generates a relevant, interactive dashboard based on what it found. This means:
- After `assess scan`, the agent generates an assessment dashboard tailored to what it found
- After `compile` failure, the agent generates an error card with explanations specific to these errors
- After `run`, the agent generates a job timeline matching this specific job's structure

This is the most radical departure from traditional UX: **the UI is an artifact produced by the agent, not a pre-built container the agent fills.**

---

## 4. Recommended Hybrid Model for OpenMainframe

Based on analysis of all 6 applications and theoretical patterns, the recommended model is:

### **Activity Log + Artifact Panel (Hybrid)**

```
┌──────────────────────────────────────────────────┐
│  Header: OpenMainframe Agent  [Connected ●]      │
├───────────────────────┬──────────────────────────┤
│                       │                          │
│   Activity Log        │    Artifact Panel        │
│   (primary)           │    (contextual)          │
│                       │                          │
│   [thinking...]       │    [Assessment Report]   │
│   [tool: assess scan] │    or                    │
│   [result: 42 files]  │    [Compiler Output]     │
│   [text: summary]     │    or                    │
│   [tool: compile]     │    [Job Timeline]        │
│   [approval: run?]    │    or                    │
│                       │    [Code Viewer]         │
│                       │                          │
├───────────────────────┴──────────────────────────┤
│  Input: [Type a message or describe your goal]   │
└──────────────────────────────────────────────────┘
```

**Why this model:**

1. **Activity log** (from Claude Code) provides transparency — user sees every step the agent takes
2. **Artifact panel** (from Bolt.new) provides rich visualization — assessment dashboards, code viewers, job timelines render properly
3. **HITL approval gates** (universal) appear inline in the activity log
4. **Plan-then-execute** (from Copilot Workspace) is used for complex workflows — agent proposes plan, user approves
5. **Progressive disclosure** (from Devin) — activity log shows summary; click to expand thinking, raw CLI output

**How it handles each workflow:**

| Workflow | Activity Log Shows | Artifact Panel Shows |
|----------|-------------------|---------------------|
| Assessment | thinking → assess scan command → progress → summary text | Assessment dashboard (summary cards, charts, issue table) |
| Compile | compile command → diagnostics count → error explanations | Code viewer with inline error annotations |
| JCL execution | parse-jcl → approval card → run command → step results | Job timeline with step nodes and SYSOUT |
| Code explanation | file read → explanation text with business rules | Code viewer with annotation overlays |
| CICS (v2) | bms compile → cics launch | Embedded terminal (xterm.js) |
| Dataset | idcams/gdg commands → operation results | Dataset catalog table |

**Why NOT the other models:**

| Rejected Model | Reason |
|----------------|--------|
| Pure activity log (Claude Code) | Cannot render rich assessment dashboards, charts, tables |
| Pure IDE (Cursor) | Too code-editing focused; OpenMainframe is about assessment and execution, not editing |
| Full workspace (Devin) | Over-engineered for a tool that primarily wraps CLI commands |
| Pure pipeline (Copilot Workspace) | Too structured for quick tasks like "explain this code" |
| Pure generative UI (Bolt.new) | The artifact preview is right, but needs the activity log for transparency |

---

## 5. Key Sources

- [Agentic Design — UI/UX Patterns](https://agentic-design.ai/patterns/ui-ux-patterns)
- [Codewave — Designing Agentic AI UI](https://codewave.com/insights/designing-agentic-ai-ui/)
- [State of Design 2026 — When Interfaces Become Agents](https://tejjj.medium.com/state-of-design-2026-when-interfaces-become-agents-fc967be10cba)
- [Claude Code Docs](https://code.claude.com/docs/en/skills)
- [Cursor AI Review 2026](https://prismic.io/blog/cursor-ai)
- [Cursor 2.2 — Visual Editor, Debug Mode, Subagents](https://www.yodev.dev/t/cursor-2-2-visual-editor-debug-mode-the-rise-of-ai-subagents/1348)
- [Devin AI Docs](https://docs.devin.ai/)
- [Devin AI Guide 2026](https://aitoolsdevpro.com/ai-tools/devin-guide/)
- [GitHub Copilot Workspace](https://githubnext.com/projects/copilot-workspace)
- [Bolt.new Intro](https://support.bolt.new/building/intro-bolt)
- [AWS Transform for Mainframe](https://aws.amazon.com/transform/mainframe/)
- [AWS Transform — Reimagine capabilities](https://aws.amazon.com/blogs/aws/aws-transform-for-mainframe-introduces-reimagine-capabilities-and-automated-testing-functionality/)
- [Agentic AI Design Patterns for 2026](https://medium.com/@pro.namratapanchal/what-are-the-must-know-agentive-design-patterns-for-2026-21cf34839a01)
- [Smart Interface Design Patterns — AI](https://smart-interface-design-patterns.com/articles/ai-design-patterns/)
- [Google A2UI — Agent-Driven Interfaces](https://developers.googleblog.com/introducing-a2ui-an-open-project-for-agent-driven-interfaces/)
