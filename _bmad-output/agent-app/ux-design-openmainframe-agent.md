---
version: 'v1.0'
date: '2026-02-17'
status: 'draft'
inputDocuments:
  - prd-openmainframe-agent.md
  - product-brief-openmainframe-agent.md
  - research-technical-copilotkit-langgraph.md
---

# UX Design: OpenMainframe Agent v1.0

**BMAD Phase:** 2-Planning | **Step:** 6 of 11 | **Agent:** Sally 🎨 UX Designer

---

## 1. Design Principles

1. **Code-first, chat-assisted** — The workspace shows code and results; chat is the assistant, not the center
2. **Progressive disclosure** — Start simple (chat), reveal depth (dashboards, reports) as the user progresses
3. **Familiar patterns** — IDE-like layout that COBOL developers and migration architects recognize
4. **Trust through transparency** — Always show what the agent is doing (tool calls, progress, raw output)
5. **No dead ends** — Every state has a clear next action; errors suggest what to do next

---

## 2. Layout Architecture

### 2.1 Overall Layout

```
┌──────────────────────────────────────────────────────────────────┐
│  Header Bar                                                [≡]  │
│  OpenMainframe Agent    [Project: /path/to/source]    [Settings] │
├────────────┬─────────────────────────────────┬───────────────────┤
│            │                                 │                   │
│  File Tree │         Workspace               │   Chat Panel      │
│  (240px)   │         (flexible)              │   (400px)         │
│            │                                 │                   │
│  📁 src    │  ┌─────────────────────────┐    │  ┌─────────────┐  │
│  ├─ 📄 A.cbl  │  [Tab: Code] [Tab: Report]│    │  │  Messages   │  │
│  ├─ 📄 B.cbl  │                         │    │  │             │  │
│  ├─ 📄 C.jcl  │  Content Area           │    │  │  Agent:     │  │
│  └─ 📁 cpy │  │  (code viewer,         │    │  │  "I found   │  │
│    └─ 📄 X.cpy│  │   dashboard, report,  │    │  │   42 COBOL  │  │
│            │  │   timeline)            │    │  │   files..." │  │
│            │  │                         │    │  │             │  │
│            │  │                         │    │  │  [Generative│  │
│            │  │                         │    │  │   UI cards] │  │
│            │  │                         │    │  │             │  │
│            │  └─────────────────────────┘    │  ├─────────────┤  │
│            │                                 │  │ Input       │  │
│            │  Status Bar: [operation] [prog] │  │ [Type here] │  │
├────────────┴─────────────────────────────────┴───────────────────┤
│  Footer: OpenMainframe v3.0 | Agent connected | LLM: Claude     │
└──────────────────────────────────────────────────────────────────┘
```

### 2.2 Responsive Breakpoints

| Viewport | File Tree | Workspace | Chat Panel |
|----------|-----------|-----------|------------|
| ≥1440px | 240px fixed | flexible (min 600px) | 400px fixed |
| 1280-1439px | 200px collapsible | flexible (min 500px) | 360px fixed |
| <1280px | hidden (toggle) | full width | overlay/drawer |

### 2.3 Panel Interactions

- **File Tree:** Click file → opens in Code Viewer tab; right-click → "Assess", "Compile", "Explain"
- **Workspace Tabs:** Contextual tabs appear based on agent actions (Code, Assessment, Execution, Report)
- **Chat Panel:** Collapsible via header button; maintains scroll position; auto-scrolls on new messages
- **Drag resize:** Borders between panels are draggable to adjust proportions

---

## 3. Component Hierarchy

```
App (CopilotKit Provider)
├── Header
│   ├── Logo + Title
│   ├── ProjectSelector (path input + browse button)
│   └── SettingsButton (LLM config, theme toggle)
│
├── MainLayout (3-column)
│   ├── FileTreePanel
│   │   ├── FileTreeSearch (filter input)
│   │   └── FileTree (recursive directory tree)
│   │       ├── FileTreeNode (folder — expandable)
│   │       └── FileTreeLeaf (file — clickable, with icon by type)
│   │
│   ├── WorkspacePanel
│   │   ├── TabBar
│   │   │   ├── Tab (closable, reorderable)
│   │   │   └── TabContent
│   │   │       ├── CodeViewer (COBOL/JCL syntax highlighting)
│   │   │       ├── AssessmentDashboard
│   │   │       ├── JobTimeline
│   │   │       ├── ReportViewer (markdown rendered)
│   │   │       └── WelcomeScreen (first-time instructions)
│   │   └── StatusBar (current operation, progress bar)
│   │
│   └── ChatPanel (CopilotSidebar)
│       ├── MessageList
│       │   ├── UserMessage
│       │   ├── AgentMessage
│       │   └── GenerativeUISlot
│       │       ├── AssessmentCard (inline summary)
│       │       ├── CompilerOutputCard (errors/success)
│       │       ├── ApprovalCard (HITL interrupt)
│       │       ├── ExplanationCard (annotated code)
│       │       └── ProgressCard (operation status)
│       └── ChatInput (text input + suggested actions)
│
└── Footer
    ├── VersionInfo
    ├── ConnectionStatus (agent connected/disconnected)
    └── LLMProvider indicator
```

---

## 4. Screen Designs

### 4.1 Welcome Screen (First Visit)

```
┌─────────────────────────────────────────────────┐
│                                                 │
│         🏗️ OpenMainframe Agent                  │
│                                                 │
│   Welcome! I'm your mainframe modernization     │
│   assistant. Let's get started.                 │
│                                                 │
│   ┌──────────────────────────────────────────┐  │
│   │  📂 Set Project Directory                │  │
│   │  [/path/to/your/cobol/source]   [Browse] │  │
│   └──────────────────────────────────────────┘  │
│                                                 │
│   Quick Actions:                                │
│   ┌──────────┐ ┌──────────┐ ┌──────────┐       │
│   │ 📊 Assess│ │ 🔨 Compile│ │ 📖 Explain│      │
│   │ Scan a   │ │ Build a  │ │ Understand│       │
│   │ codebase │ │ program  │ │ legacy    │       │
│   │          │ │          │ │ code      │       │
│   └──────────┘ └──────────┘ └──────────┘       │
│                                                 │
│   Or just type a message in the chat →          │
│                                                 │
└─────────────────────────────────────────────────┘
```

**Behavior:**
- Displayed when no project is set
- Quick action cards send pre-formed messages to chat
- "Assess" → `"Assess the COBOL programs in [project_path]"`
- "Compile" → `"Compile [selected file]"`
- "Explain" → `"Explain [selected file]"`

### 4.2 Assessment Dashboard

```
┌─────────────────────────────────────────────────┐
│  Assessment: /home/user/carddemo      [Export ↓]│
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌────────────┐ ┌────────────┐ ┌────────────┐  │
│  │  42 Files  │ │ 28,450 LOC │ │ Avg: 6.2   │  │
│  │  scanned   │ │  total     │ │ complexity │  │
│  └────────────┘ └────────────┘ └────────────┘  │
│  ┌────────────┐ ┌────────────┐ ┌────────────┐  │
│  │  12 Issues │ │  78%       │ │  340 hrs   │  │
│  │  found     │ │ compatible │ │ tech debt  │  │
│  └────────────┘ └────────────┘ └────────────┘  │
│                                                 │
│  Complexity Distribution          Feature Matrix│
│  ┌──────────────────┐  ┌──────────────────────┐│
│  │ ████████████ Low:18│  │ Sequential  ████ 100%││
│  │ ██████     Med:12│  │ VSAM KSDS   ███░  90%││
│  │ ████      High:8 │  │ DB2 SQL     ███░  85%││
│  │ ██      VHigh:4  │  │ CICS Cmds   ██░░  75%││
│  └──────────────────┘  │ BMS Maps    ██░░  70%││
│                        │ IMS/DL1     ░░░░   0%││
│                        └──────────────────────┘│
│                                                 │
│  Issues (12)              [All▼] [Severity ▼]   │
│  ┌─────────────────────────────────────────────┐│
│  │ 🔴 CRITICAL  POLCY01.cbl:245  IMS/DL1 call ││
│  │ 🔴 CRITICAL  CLMPR03.cbl:89   IMS/DL1 call ││
│  │ 🟡 HIGH      BATCH02.cbl:312  ALTER stmt   ││
│  │ 🟡 HIGH      RPTGEN.cbl:45    RELATIVE file││
│  │ 🟠 WARNING   CALC01.cbl:178   GO TO DEPEND ││
│  │ ...                                         ││
│  └─────────────────────────────────────────────┘│
│                                                 │
│  Programs by Complexity    [Sort: complexity ▼]  │
│  ┌──────────┬──────┬───────┬────────┬──────────┐│
│  │ Program  │ LOC  │ Cmplx │ Maint. │ Features ││
│  ├──────────┼──────┼───────┼────────┼──────────┤│
│  │ POLCY01  │ 2,450│ 12.3  │ 42/100 │ CICS,DB2 ││
│  │ CLMPR03  │ 1,890│ 9.8   │ 51/100 │ IMS,VSAM ││
│  │ BATCH02  │ 1,230│ 8.1   │ 58/100 │ VSAM,SORT││
│  │ ...      │      │       │        │          ││
│  └──────────┴──────┴───────┴────────┴──────────┘│
└─────────────────────────────────────────────────┘
```

**Behavior:**
- Opens as a workspace tab after assessment completes
- Summary cards at top are clickable (drill into detail)
- Issues list is filterable by severity, sortable by program
- Program table rows are clickable → opens Code Viewer with that file
- Export button downloads JSON or Markdown report
- Feature matrix bars are color-coded (green >80%, yellow 50-80%, red <50%)

### 4.3 Code Viewer with Annotations

```
┌─────────────────────────────────────────────────┐
│  POLCY01.cbl                    [Explain] [Compile]│
├─────────────────────────────────────────────────┤
│    1│ IDENTIFICATION DIVISION.                   │
│    2│ PROGRAM-ID. POLCY01.                       │
│    3│ AUTHOR. INSURANCE-TEAM.                    │
│    4│                                            │
│   ..│ ...                                        │
│   42│ PROCEDURE DIVISION.                        │
│   43│ MAIN-LOGIC.                                │
│   44│     PERFORM INIT-PROGRAM                   │
│   45│     PERFORM CALC-PREMIUM   ◄── 💡          │
│     │     ┌─────────────────────────────────┐    │
│     │     │ Business Rule: Premium Calculation│   │
│     │     │ Calculates annual premium based  │    │
│     │     │ on age, coverage, and risk class │    │
│     │     └─────────────────────────────────┘    │
│   46│     PERFORM WRITE-OUTPUT                   │
│   47│     STOP RUN.                              │
│   48│                                            │
│   49│ CALC-PREMIUM.                              │
│   50│     COMPUTE WS-BASE-RATE =                 │
│   51│         WS-COVERAGE-AMT * 0.015            │
│  ►52│     IF WS-AGE > 65            ⚠ HIGH      │
│     │     ┌─────────────────────────────────┐    │
│     │     │ ⚠ Compatibility: GO TO DEPENDING │   │
│     │     │ at line 58 may need review       │    │
│     │     └─────────────────────────────────┘    │
│   53│         ADD 250 TO WS-BASE-RATE            │
│   54│     END-IF                                 │
│   ..│ ...                                        │
├─────────────────────────────────────────────────┤
│  Ln 52, Col 1  │ COBOL-85  │ Complexity: 12.3   │
└─────────────────────────────────────────────────┘
```

**Behavior:**
- Syntax highlighting for COBOL keywords, divisions, paragraphs
- Annotation bubbles appear from: explanation agent, compatibility issues, compiler errors
- Click annotation → scrolls chat to the relevant explanation
- Gutter icons: 💡 = explanation available, ⚠ = compatibility issue, 🔴 = compiler error
- Header buttons trigger chat actions: "Explain POLCY01.cbl", "Compile POLCY01.cbl"

### 4.4 Job Execution Timeline

```
┌─────────────────────────────────────────────────┐
│  Execution: CARDDEMO.jcl            [Re-run]    │
├─────────────────────────────────────────────────┤
│                                                 │
│  ● STEP010 ─── ● STEP020 ─── ● STEP030 ─── ●  │
│  COBOL01       SORT01         RPTGEN01     END  │
│  RC=0 ✅       RC=0 ✅        RC=4 ⚠            │
│  2.3s          4.1s           1.2s               │
│                                                 │
│  ─────────────────────────────────────────────  │
│                                                 │
│  ▼ STEP030: RPTGEN01 (RC=4)                     │
│  ┌─────────────────────────────────────────────┐│
│  │ Program: RPTGEN01                           ││
│  │ Return Code: 4 (Warning)                    ││
│  │ Duration: 1.2 seconds                       ││
│  │                                             ││
│  │ SYSOUT:                                     ││
│  │ ┌─────────────────────────────────────────┐ ││
│  │ │ REPORT GENERATION STARTING              │ ││
│  │ │ WARNING: NO DATA FOUND FOR REGION 'NW'  │ ││
│  │ │ REPORT GENERATED: 3,450 RECORDS         │ ││
│  │ │ PROCESSING COMPLETE - RC=4              │ ││
│  │ └─────────────────────────────────────────┘ ││
│  │                                             ││
│  │ DD Statements:                              ││
│  │   SYSIN  → CARDDEMO.INPUT.DATA             ││
│  │   SYSOUT → SPOOL                           ││
│  │   REPORT → CARDDEMO.REPORT.OUTPUT          ││
│  └─────────────────────────────────────────────┘│
│                                                 │
│  Job Summary: 3 steps, Max RC=4, Total: 7.6s    │
└─────────────────────────────────────────────────┘
```

**Behavior:**
- Timeline shows steps as connected nodes, color-coded by return code
- Click a step node → expands detail panel below
- SYSOUT displayed in monospace terminal-style panel
- DD statements listed with dataset names
- Re-run button triggers new execution (with HITL approval)

### 4.5 Human-in-the-Loop Approval Card (Chat Inline)

```
┌─────────────────────────────────────────────┐
│  🤖 Agent                                   │
│                                             │
│  I'm ready to execute the JCL job.          │
│                                             │
│  ┌─────────────────────────────────────────┐│
│  │  ⚡ Execution Approval Required         ││
│  │                                         ││
│  │  File: CARDDEMO.jcl                     ││
│  │  Steps: 3 (COBOL01, SORT01, RPTGEN01)  ││
│  │  Datasets: 2 input, 1 output            ││
│  │  Estimated duration: ~10 seconds        ││
│  │                                         ││
│  │  ┌───────────┐  ┌──────────────┐       ││
│  │  │ ✅ Approve │  │ ❌ Cancel     │       ││
│  │  └───────────┘  └──────────────┘       ││
│  └─────────────────────────────────────────┘│
└─────────────────────────────────────────────┘
```

**Behavior:**
- Rendered inline in chat via `useLangGraphInterrupt`
- Shows job details extracted from JCL parsing
- Approve → agent proceeds with execution, timeline appears in workspace
- Cancel → agent acknowledges and asks what to do instead
- Card grays out after decision (cannot re-click)

### 4.6 Compiler Output Card (Chat Inline)

```
┌─────────────────────────────────────────────┐
│  🤖 Agent                                   │
│                                             │
│  Compilation of POLCY01.cbl failed with     │
│  2 errors.                                  │
│                                             │
│  ┌─────────────────────────────────────────┐│
│  │  🔨 Compilation Result: FAILED          ││
│  │                                         ││
│  │  🔴 Line 52: COMPUTE WS-RESULT =       ││
│  │     Expected numeric operand, found     ││
│  │     alphanumeric field WS-NAME          ││
│  │     → WS-NAME is PIC X(30), not numeric.││
│  │       Change to a numeric field or use  ││
│  │       FUNCTION NUMVAL(WS-NAME).         ││
│  │                                    [Go↗]││
│  │                                         ││
│  │  🔴 Line 78: PERFORM CALC-TAX          ││
│  │     Paragraph CALC-TAX not found        ││
│  │     → Check spelling. Did you mean      ││
│  │       CALC-TAXES? (found at line 120)   ││
│  │                                    [Go↗]││
│  └─────────────────────────────────────────┘│
└─────────────────────────────────────────────┘
```

**Behavior:**
- Rendered inline in chat via tool-based generative UI
- Each error is expandable with LLM-generated explanation and fix suggestion
- [Go↗] button scrolls Code Viewer to the error line
- Success variant shows green card with "Compilation successful" and binary location

---

## 5. Interaction Patterns

### 5.1 Chat Suggested Actions

At idle, the chat input shows contextual quick-action chips:

```
[📊 Assess project] [🔨 Compile selected] [📖 Explain selected] [▶️ Run JCL]
```

Chips change based on context:
- **No project set:** `[📂 Set project directory]`
- **Project set, no assessment:** `[📊 Assess project]`
- **File selected in tree:** `[🔨 Compile] [📖 Explain] [✓ Check syntax]`
- **Assessment complete:** `[📋 Export report] [🔍 Most complex programs]`
- **After compilation error:** `[🔧 Fix errors] [📖 Explain error]`

### 5.2 File Tree Context Menu

Right-click a file in the tree:

```
┌────────────────────────┐
│ 📖 Explain this file   │
│ 🔨 Compile             │
│ ✓  Check syntax        │
│ 📊 Assess metrics      │
│ ─────────────────────  │
│ 📄 View in editor      │
│ 📋 Copy path           │
└────────────────────────┘
```

Each action sends a chat message like: `"Explain /path/to/POLCY01.cbl"`

### 5.3 Progress Indicators

Long-running operations show progress in both the status bar and chat:

**Status Bar:**
```
┌──────────────────────────────────────────────────┐
│ 📊 Assessing... [████████░░░░░░░░] 18/42 files   │
└──────────────────────────────────────────────────┘
```

**Chat (via predictive state updates):**
```
┌─────────────────────────────────────────────┐
│  🤖 Agent                                   │
│                                             │
│  ┌─────────────────────────────────────────┐│
│  │  📊 Assessment in Progress              ││
│  │  ████████░░░░░░░░  18/42 files          ││
│  │                                         ││
│  │  Currently analyzing: BATCH02.cbl       ││
│  │  Found so far: 4 issues, avg cplx 5.8  ││
│  └─────────────────────────────────────────┘│
└─────────────────────────────────────────────┘
```

### 5.4 Error States

**Agent disconnected:**
```
┌─────────────────────────────────────────────┐
│  ⚠️ Agent Not Connected                     │
│                                             │
│  The LangGraph agent is not responding.     │
│  Check that the Python backend is running.  │
│                                             │
│  [Retry Connection]                         │
└─────────────────────────────────────────────┘
```

**OpenMainframe binary not found:**
```
┌─────────────────────────────────────────────┐
│  🤖 Agent                                   │
│                                             │
│  I couldn't find the OpenMainframe binary.  │
│  Please check that it's built and the path  │
│  is configured in your .env file:           │
│                                             │
│  OPEN_MAINFRAME_BIN=./target/release/open-mainframe │
│                                             │
│  To build: cargo build --release            │
└─────────────────────────────────────────────┘
```

---

## 6. Color Scheme and Theming

### 6.1 Dark Theme (Default)

| Element | Color | Usage |
|---------|-------|-------|
| Background | `#1e1e2e` | Main workspace background |
| Surface | `#282a36` | Panels, cards, elevated surfaces |
| Border | `#44475a` | Panel borders, dividers |
| Text Primary | `#f8f8f2` | Main text |
| Text Secondary | `#6272a4` | Muted text, labels |
| Accent | `#8be9fd` | Links, active items, agent name |
| Success | `#50fa7b` | RC=0, compilation success |
| Warning | `#f1fa8c` | RC=4, warnings |
| Error | `#ff5555` | RC>4, compilation errors, critical issues |
| Info | `#bd93f9` | Informational badges |

### 6.2 COBOL Syntax Highlighting

| Token | Color | Example |
|-------|-------|---------|
| Division keywords | `#ff79c6` bold | `IDENTIFICATION DIVISION.` |
| Section keywords | `#ff79c6` | `WORKING-STORAGE SECTION.` |
| Verbs | `#8be9fd` | `MOVE`, `COMPUTE`, `PERFORM` |
| Data names | `#f8f8f2` | `WS-AMOUNT` |
| Literals | `#f1fa8c` | `'HELLO WORLD'` |
| Numbers | `#bd93f9` | `100`, `PIC 9(5)` |
| Comments | `#6272a4` italic | `* This is a comment` |
| EXEC blocks | `#50fa7b` bg | `EXEC SQL ... END-EXEC` |

---

## 7. Navigation and Information Architecture

### 7.1 Workspace Tab Types

| Tab Type | Icon | Source | Content |
|----------|------|--------|---------|
| Code Viewer | 📄 | File tree click | Syntax-highlighted source |
| Assessment | 📊 | Assessment complete | Dashboard with metrics |
| Execution | ▶️ | JCL execution complete | Job timeline |
| Report | 📋 | Export action | Rendered Markdown report |
| Welcome | 🏠 | App start / no project | Getting started guide |

**Tab behavior:**
- Maximum 8 tabs open; oldest auto-closes when exceeded
- Tabs are closable (click X), reorderable (drag)
- Active tab highlighted with accent color underline
- Dirty indicator (dot) on tabs with unsaved state

### 7.2 Chat Message Types

| Type | Visual | Example |
|------|--------|---------|
| User message | Right-aligned, accent bg | "Assess the project" |
| Agent text | Left-aligned, surface bg | "I found 42 COBOL files..." |
| Tool call (in progress) | Spinner + tool name | "🔄 Running: assess_scan" |
| Tool result | Collapsed, expandable | "✅ assess_scan completed" |
| Generative UI | Inline card | Assessment card, approval card |
| Error | Red border card | "Failed to compile: ..." |

---

## 8. Responsive Behavior

### 8.1 Mobile (< 768px)

Not targeted for MVP. Display message: "OpenMainframe Agent is designed for desktop browsers (1280px+)."

### 8.2 Tablet / Small Laptop (768-1279px)

- File tree hidden by default (hamburger toggle)
- Chat panel as a drawer (slide in from right)
- Workspace takes full width
- Bottom sheet for quick actions instead of sidebar chips

### 8.3 Desktop (1280px+)

Full 3-column layout as designed.

---

## 9. Accessibility

- Keyboard navigation: Tab through panels, Enter to activate
- ARIA labels on all interactive elements
- Focus indicators visible in both themes
- Screen reader announcements for agent messages
- Color-blind safe: severity uses both color AND icon (🔴🟡🟠🔵)
- Minimum contrast ratio: 4.5:1 for text

---

## 10. Component-to-PRD Mapping

| PRD Requirement | UI Component | Section |
|-----------------|-------------|---------|
| FR-060 CopilotKit Sidebar | ChatPanel (CopilotSidebar) | 2.1 |
| FR-061 Code Viewer | CodeViewer tab | 4.3 |
| FR-062 Assessment Dashboard | AssessmentDashboard tab | 4.2 |
| FR-063 Job Execution Timeline | JobTimeline tab | 4.4 |
| FR-064 File Tree Navigation | FileTreePanel | 2.1, 5.2 |
| FR-070 Execution Approval | ApprovalCard (generative UI) | 4.5 |
| FR-071 Dataset Modification | ApprovalCard variant | 4.5 |
| FR-013 Report Generation | ReportViewer tab + Export | 4.2 |
| FR-022 Error Fix Suggestions | CompilerOutputCard | 4.6 |
| FR-040 Code Explanation | CodeViewer annotations | 4.3 |
