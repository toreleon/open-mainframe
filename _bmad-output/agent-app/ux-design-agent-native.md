---
version: 'v2.0'
date: '2026-02-19'
status: 'draft'
replaces: 'ux-design-openmainframe-agent.md'
inputDocuments:
  - research/openmainframe-cli-ux-analysis.md
  - research/agent-native-ux-patterns.md
  - research/modernization-ux-landscape.md
  - research/agent-ux-design-principles.md
  - research/cross-reference-synthesis.md
---

# UX Design: OpenMainframe Agent v2.0 — Agent-Native

**BMAD Phase:** 2-Planning | **Agent:** Sally UX Designer

---

## 1. Design Principles

Nine principles grounded in research (Track 4) and validated through cross-reference (Track 5):

### 1.1 The Agent Narrates, The Artifact Shows

The activity log tells the *story* — what the agent is doing and why. The artifact panel shows the *result* in the best visualization for that data type. Neither surface replaces the other.

**Example:** Agent says "Assessment complete — 42 files scanned, 12 issues found" (activity log) while the artifact panel shows a dashboard with summary cards, complexity chart, and sortable issue table.

### 1.2 Progressive Disclosure by Default

Four-level disclosure hierarchy in the activity log:

| Level | Shows | Triggered By |
|-------|-------|-------------|
| 1 — Summary | "Compiled HELLO.cbl — 2 errors" | Default |
| 2 — Details | Error messages with severity icons | Click to expand entry |
| 3 — Explanation | LLM explanation + fix suggestion per error | Click error in expanded entry |
| 4 — Artifact | Code viewer with inline annotations | Click "View in code" or auto in artifact panel |

The artifact panel always shows full visualization immediately when ready. Progressive disclosure applies to the activity log only.

### 1.3 Workflows, Not Commands

Users express outcomes ("Assess this codebase", "Run the job"), not commands. The agent orchestrates CLI commands internally. Activity log shows outcome-oriented labels: "Assessing codebase..." not "Running `assess scan ./app/cbl --format json`".

Command names appear only in expanded tool-call details (Level 2) for transparency.

### 1.4 Trust Through Previews

Before any side-effecting operation (JCL execution, dataset creation/deletion, code interpretation), the agent shows what will happen and waits for explicit approval. Preview cards appear inline in the activity log.

### 1.5 Artifacts Are First-Class

Tool results are persistent artifacts, not chat messages to scroll past. They can be: referenced later in conversation, exported (Markdown, JSON), compared across runs, and acted upon ("Fix these errors").

### 1.6 Context Carries Forward

The agent remembers previous assessments, compilation results, and conversations about specific programs within a session. Cross-session persistence is a v2 feature backed by local storage.

### 1.7 Mainframe Metaphors Where They Help

Use familiar concepts (return codes, SYSOUT, job steps, condition codes) but present them in modern, accessible ways: RC as colored badges, SYSOUT in monospace panels, job steps as visual timelines.

### 1.8 The Agent Explains, Not Just Reports

Every CLI output is accompanied by LLM-generated explanation. Compiler errors get plain-English descriptions + fix suggestions. Assessment issues get impact analysis. Job failures get diagnostic guidance. This is the #1 differentiator over using the CLI directly.

### 1.9 Source Stays Local, Trust Is Earned

Always communicate that source code stays on the user's machine. Show connection status prominently. Privacy level is configurable: Full (source shared), Output-only, or Minimal.

---

## 2. Interaction Model

### 2.1 Model Comparison

| Model | Assessment | Compile Loop | JCL Execution | Code Explanation | CICS | Dataset |
|-------|-----------|-------------|---------------|-----------------|------|---------|
| **Activity log** (Claude Code) | Progress visible but results crammed into text | Good iteration feel | Steps as log entries | Good conversational flow | N/A | Log entries |
| **Plan-execute** (Copilot Workspace) | Good for multi-file with review stages | Overkill for single compile | Good preview→approve | Over-structured | N/A | Over-structured |
| **Workspace** (Devin) | Good autonomous scan | Good but IDE-heavy | Good visibility | Good | Terminal embed | Good |
| **Pipeline** (AWS Transform) | Fits assessment → plan → migrate | Too structured | Good staged execution | Doesn't fit | N/A | Good catalog |
| **Hybrid: Activity Log + Artifact Panel** | **Best** — log shows progress, panel shows dashboard | **Best** — log shows cycle, panel shows code+errors | **Best** — log shows approval, panel shows timeline | **Best** — log narrates, panel shows annotated code | Panel for terminal (v2) | **Best** — log shows ops, panel shows catalog |

### 2.2 Chosen Model: Activity Log + Artifact Panel (Hybrid)

**Architecture:**

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent          [Connected ●] [⚙]   |
+---------------------------+--------------------------+
|                           |                          |
|   ACTIVITY LOG            |    ARTIFACT PANEL        |
|   (scrollable)            |    (contextual)          |
|                           |                          |
|   [thinking...]           |    Empty state:           |
|   [> Assessing codebase]  |    "Results will appear   |
|   [  Scanning 18/42...]   |     here as the agent     |
|   [v Assessment complete] |     works."               |
|   [  42 files, 12 issues] |                          |
|                           |    After assessment:      |
|   [> Compiling HELLO.cbl] |    [AssessmentDashboard]  |
|   [v 2 errors found]      |    or                     |
|                           |    [CompilerDiagnostic]   |
|                           |    or                     |
|                           |    [JobTimeline]          |
|                           |                          |
+---------------------------+--------------------------+
|  [Type a message or describe your goal...]    [Send] |
+------------------------------------------------------+
```

**Why this model:**

1. **Activity log** (from Claude Code) provides transparency — every action visible, thinking accessible, progress trackable
2. **Artifact panel** (from Bolt.new) provides rich visualization — dashboards, code viewers, timelines that would be impossible in a text stream
3. **HITL approval gates** appear inline in the activity log — preserving context flow
4. **Plan-then-execute** is used implicitly for complex workflows (assessment pipeline) without forcing explicit plan stages
5. **Progressive disclosure** — activity log shows summary by default; artifact panel shows full visualization

**Panel behavior:**
- Artifact panel shows the most recent artifact by default
- Previous artifacts accessible via clicking activity log entries
- Panel is collapsible (full-width activity log for simple conversations)
- Panel auto-opens when first artifact is produced

---

## 3. Per-Workflow UX Design

### 3.1 Assessment Workflow

**User intent:** "Assess this codebase" / "How ready is this for migration?" / "What COBOL files do we have?"

**Agent sequence:** `assess scan <dir>` → (for flagged files) `assess file <file>` → LLM synthesis

**Activity Log Flow:**

```
  [thinking...]                              ← collapsed, dimmed

  [> Assessing codebase]                     ← outcome-oriented label
     Scanning ./app/cbl...
     Progress: 18 of 42 files (43%)          ← CUSTOM progress event
     Current: POLCY01.cbl

  [v Assessment complete]                    ← green checkmark, summary
     42 programs | 28,450 lines | Avg complexity: 6.2
     12 issues found (2 critical)
     Estimated migration effort: 340 hours

  [text] "I found 42 COBOL programs totaling 28,450 lines.
   The average cyclomatic complexity is 6.2. I identified
   12 compatibility issues — 2 are critical and will need
   manual attention. Here's a breakdown..."              ← agent explanation
```

**Artifact Panel — AssessmentDashboard:**

```
+----------------------------------------------------+
|  ASSESSMENT: ./app/cbl                              |
|                                                     |
|  +----------+ +----------+ +----------+ +--------+  |
|  | 42       | | 28,450   | | 6.2      | | 340 hr |  |
|  | Programs | | Lines    | | Avg Cplx | | Effort |  |
|  +----------+ +----------+ +----------+ +--------+  |
|                                                     |
|  COMPLEXITY DISTRIBUTION                            |
|  Low  [======····] 24 (57%)                         |
|  Med  [====······] 12 (29%)                         |
|  High [==········]  6 (14%)                         |
|                                                     |
|  ISSUES BY SEVERITY                                 |
|  +--------+---------+------+---------+----------+   |
|  | Sev    | Program | Line | Issue   | Action   |   |
|  +--------+---------+------+---------+----------+   |
|  | CRIT   | POLCY01 | 142  | IMS/DL1 | [Explain]|   |
|  | CRIT   | BATCH03 | 87   | IMS/DL1 | [Explain]|   |
|  | WARN   | CALC01  | 23   | COMP-5  | [Explain]|   |
|  | WARN   | CALC01  | 45   | INSPECT | [Explain]|   |
|  | ...    | ...     | ...  | ...     | ...      |   |
|  +--------+---------+------+---------+----------+   |
|                                                     |
|  [Export Markdown]  [Export JSON]                    |
+----------------------------------------------------+
```

**Interactions:**
- Click program name → artifact switches to ProgramDetailCard for that file
- Click issue → artifact switches to CodeViewer with annotation at that line
- Click [Explain] → agent explains issue in activity log text
- "Fix the critical issues" → agent drills into each, suggests fixes
- Sort/filter issue table by severity, program, issue type

---

### 3.2 Compile-Debug Loop Workflow

**User intent:** "Compile HELLO.cbl" / "Check this file for errors" / "Fix the errors"

**Agent sequence:** `compile <file>` (or `check <file>` for quick validation) → LLM explanation → user fixes → recompile

**Activity Log Flow:**

```
  [thinking...]

  [> Compiling HELLO.cbl]                    ← spinner during compile

  [x Compilation failed — 2 errors, 1 warning]  ← red status
     ERROR  Line 42:12 — 'WS-NAME' not defined in WORKING-STORAGE
     ERROR  Line 87:8  — Unexpected token 'PREFORM'
     WARN   Line 15:1  — Unused variable WS-TEMP

  [text] "There are 2 errors preventing compilation:

   1. **Line 42** — Variable `WS-NAME` is used but not declared.
      Add to WORKING-STORAGE: `01 WS-NAME PIC X(30).`

   2. **Line 87** — `PREFORM` is a typo for `PERFORM`.

   The warning on line 15 is non-blocking but `WS-TEMP` can
   be removed to clean up the code."
```

**Artifact Panel — CompilerDiagnosticCard + CodeViewer:**

```
+----------------------------------------------------+
|  COMPILE: HELLO.cbl                    [x FAILED]   |
|                                                     |
|  Errors: 2  |  Warnings: 1  |  Info: 0             |
|                                                     |
|  +--------------------------------------------------+
|  | [x] ERROR  Line 42, Col 12                       |
|  |     'WS-NAME' not defined in WORKING-STORAGE     |
|  |                                                   |
|  |     Fix: Add `01 WS-NAME PIC X(30).` to          |
|  |     WORKING-STORAGE SECTION.                      |
|  |                                                   |
|  |     [View in code]                                |
|  +--------------------------------------------------+
|  | [x] ERROR  Line 87, Col 8                        |
|  |     Unexpected token 'PREFORM'                    |
|  |                                                   |
|  |     Fix: Change to `PERFORM`.                     |
|  |                                                   |
|  |     [View in code]                                |
|  +--------------------------------------------------+
|  | [!] WARN  Line 15, Col 1                         |
|  |     Unused variable 'WS-TEMP'                     |
|  +--------------------------------------------------+
```

**After user clicks [View in code]:**

```
+----------------------------------------------------+
|  HELLO.cbl                          [Back to errors]|
|                                                     |
|  39 |        MOVE SPACES TO WS-ADDR.               |
|  40 |        MOVE 'HELLO' TO WS-MSG.               |
|  41 |                                               |
|  42 | >>>    DISPLAY WS-NAME.               [ERROR] |
|  42 | ^^^    'WS-NAME' not defined                  |
|  43 |        DISPLAY WS-MSG.                        |
|  44 |                                               |
+----------------------------------------------------+
```

**Iteration feel:** User fixes code → says "Compile again" → agent recompiles → success:

```
  [v Compilation successful]                 ← green checkmark
     HELLO — 0 errors, 0 warnings, 12 symbols
```

---

### 3.3 JCL Job Execution Workflow

**User intent:** "Run this job" / "Execute RUNCARD.jcl" / "Test the batch process"

**Agent sequence:** `parse-jcl <file>` → approval gate → `run <file>` → results

**Activity Log Flow:**

```
  [thinking...]

  [> Analyzing job structure]
     Parsing RUNCARD.jcl...

  +------------------------------------------------------+
  | ! Agent wants to execute a JCL job                    |
  |                                                       |
  |   Job: RUNCARD    Class: A                            |
  |                                                       |
  |   Step 1: COMPILE  EXEC PGM=IGYCRCTL                 |
  |           SYSIN  = HELLO.cbl                          |
  |           SYSLIN = HELLO.obj                          |
  |                                                       |
  |   Step 2: LINK     EXEC PGM=IEWL                     |
  |           SYSLIN = HELLO.obj                          |
  |           SYSLMOD = HELLO.exe                         |
  |                                                       |
  |   Step 3: RUN      EXEC PGM=HELLO                    |
  |           SYSOUT = *                                  |
  |                                                       |
  |   This will compile, link, and execute HELLO.         |
  |                                                       |
  |   [ Approve ]  [ Reject ]                             |
  +------------------------------------------------------+

  [v Job approved — executing]                ← after user clicks Approve
     Running step 1 of 3: COMPILE...
     Running step 2 of 3: LINK...
     Running step 3 of 3: RUN...

  [v Job complete — RC=0]                     ← green overall
     Step 1: COMPILE  RC=0
     Step 2: LINK     RC=0
     Step 3: RUN      RC=0
```

**Artifact Panel — JobTimeline:**

```
+----------------------------------------------------+
|  JOB: RUNCARD                          [RC=0 (ok)]  |
|                                                     |
|  COMPILE ──────── LINK ──────── RUN                 |
|  [RC=0 ok]       [RC=0 ok]     [RC=0 ok]           |
|                                                     |
|  Step 3: RUN — Output                               |
|  +------------------------------------------------+ |
|  | HELLO, WORLD!                                   | |
|  | PROGRAM ENDED WITH RC=0                         | |
|  +------------------------------------------------+ |
|                                                     |
|  [Expand all steps]  [Re-run job]                   |
+----------------------------------------------------+
```

**When a step fails (RC >= 8):**

```
|  COMPILE ──────── LINK ──────── RUN                 |
|  [RC=0 ok]       [RC=8 FAIL]   [skipped]            |
|                                                     |
|  Step 2: LINK — Error Output                        |
|  +------------------------------------------------+ |
|  | IEW2456E UNRESOLVED EXTERNAL REFERENCE: CALCPGM | |
|  +------------------------------------------------+ |
```

Agent explains: "The linker couldn't resolve CALCPGM — this subroutine isn't in the link library. You need to compile CALCPGM first or add it to the SYSLIB DD."

---

### 3.4 Code Understanding Workflow

**User intent:** "What does CALC-PREMIUM do?" / "Explain this program" / "What are the business rules?"

**Agent sequence:** Read file → LLM analysis → optional `assess file` for metrics

**Activity Log Flow:**

```
  [thinking...]                              ← agent reads and analyzes file

  [text] "## POLCY01.cbl — Policy Premium Calculator

   **Purpose:** Calculates insurance premiums based on policy type,
   coverage amount, and risk factors.

   **Business Rules:**
   1. Base premium = coverage amount x rate table lookup
   2. Risk multiplier applied for age > 65 (1.5x) or smoker (2.0x)
   3. Multi-policy discount: 10% if customer has 2+ policies
   4. Minimum premium: $50/month regardless of calculation

   **Key Sections:**
   - `0100-INITIALIZE` — Opens files, loads rate tables
   - `0200-CALC-PREMIUM` — Core calculation logic (lines 87-142)
   - `0300-APPLY-DISCOUNTS` — Discount rules (lines 143-178)
   - `0400-WRITE-OUTPUT` — Writes premium record

   **Data Dependencies:**
   - Reads: POLICY-FILE, RATE-TABLE, CUSTOMER-FILE
   - Writes: PREMIUM-OUTPUT
   - Copybooks: POLCY-REC.cpy, RATE-TBL.cpy

   **Complexity:** 8.4 (moderate-high) — 14 paragraphs,
   6 EVALUATE statements, nested IF depth of 4"
```

**Artifact Panel — CodeViewer with annotations:**

```
+----------------------------------------------------+
|  POLCY01.cbl                      [Metrics] [Raw]   |
|                                                     |
|  85 |   0200-CALC-PREMIUM.                          |
|  86 |       MOVE ZERO TO WS-PREMIUM.                |
|  87 | [i]   EVALUATE POLICY-TYPE            Business |
|  88 |           WHEN 'AUTO'                 Rule #1: |
|  89 |               MULTIPLY COVERAGE-AMT   Base     |
|  90 |                   BY AUTO-RATE         premium  |
|  91 |                   GIVING WS-PREMIUM    calc     |
|  92 |           WHEN 'HOME'                          |
|  93 |               MULTIPLY COVERAGE-AMT            |
|  94 |                   BY HOME-RATE                  |
|  95 |                   GIVING WS-PREMIUM             |
|  96 |       END-EVALUATE.                            |
|  97 |                                                |
|  98 | [i]   IF CUST-AGE > 65                 Rule #2:|
|  99 |           MULTIPLY WS-PREMIUM BY 1.5   Risk   |
| 100 |       END-IF.                           mult   |
|                                                     |
|  Legend: [i] = annotated section                     |
+----------------------------------------------------+
```

---

### 3.5 CICS Transaction Testing Workflow (v1 — Limited)

**User intent:** "Test the CARDDEMO transaction" / "Compile the BMS maps"

**v1 scope:** BMS compilation and program explanation only. Interactive 3270 terminal deferred to v2.

**Activity Log Flow:**

```
  [thinking...]

  [> Compiling BMS maps]
     Processing COSGN00.bms...

  [v BMS compilation complete]
     Generated copybook: COSGN00.cpy
     Mapset: COSGN00  |  Map: COSGN00

  [text] "I've compiled the BMS map COSGN00. This defines
   the CICS sign-on screen with fields for:
   - User ID (row 10, col 20, 8 chars)
   - Password (row 12, col 20, 8 chars, dark)
   - Error message area (row 20, col 2, 78 chars, bright)

   Note: Interactive CICS transaction testing requires the
   terminal emulator, which is coming in a future release.
   For now, I can explain the program logic and screen flow."
```

**Artifact Panel — GenericOutputCard (v1) / BMS Preview (v2):**

v1: Monospace text showing BMS compilation output.

v2 (future): Visual 24x80 screen layout preview showing field positions, attributes, and labels.

---

### 3.6 Dataset Management Workflow

**User intent:** "Set up the VSAM files for testing" / "Create a GDG base" / "Delete the test datasets"

**Agent sequence:** `idcams` commands → `gdg` commands → confirmation for destructive ops

**Activity Log Flow:**

```
  [thinking...]

  [> Setting up VSAM datasets]
     Creating cluster CUST.MASTER...

  [v Dataset created]
     CUST.MASTER — KSDS, 100 records, 80-byte records

  [> Creating GDG base]
     MY.GDG.BASE — limit 10 generations

  [v GDG base created]

  +------------------------------------------------------+
  | ! Agent wants to delete a dataset                     |
  |                                                       |
  |   Operation: DELETE                                   |
  |   Dataset: TEST.OLD.DATA                              |
  |                                                       |
  |   Warning: This will permanently delete the dataset   |
  |   and all its contents.                               |
  |                                                       |
  |   [ Approve ]  [ Reject ]                             |
  +------------------------------------------------------+
```

**Artifact Panel — DatasetOperationCard:**

```
+----------------------------------------------------+
|  DATASET OPERATIONS                                 |
|                                                     |
|  +---+--------------------------------------------+ |
|  | v | DEFINE CLUSTER  CUST.MASTER     KSDS   OK  | |
|  | v | GDG CREATE      MY.GDG.BASE    L=10   OK  | |
|  | ? | DELETE           TEST.OLD.DATA  Pending    | |
|  +---+--------------------------------------------+ |
|                                                     |
|  CUST.MASTER Details:                               |
|  Type: KSDS  |  RecSize: 80  |  KeyLen: 8          |
|  KeyOff: 0   |  CI Size: 4096                       |
+----------------------------------------------------+
```

---

## 4. Layout Architecture — Screen Wireframes

### Screen 1: First Visit / No Project Connected

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent              [Not connected O] |
+------------------------------------------------------+
|                                                       |
|                                                       |
|     Welcome to OpenMainframe Agent                    |
|                                                       |
|     I help you assess, compile, and modernize         |
|     COBOL codebases. To get started:                  |
|                                                       |
|     1. Start the bridge daemon on your machine:       |
|                                                       |
|        $ openmainframe-bridge start                   |
|                                                       |
|     2. Point it at your project directory:             |
|                                                       |
|        $ openmainframe-bridge --project ~/carddemo    |
|                                                       |
|     3. I'll automatically connect and show what       |
|        I find.                                        |
|                                                       |
|     Waiting for connection...                         |
|                                                       |
+------------------------------------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

### Screen 2: Connected / Intent Entry

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent      [~/carddemo ●] [⚙]      |
+---------------------------+--------------------------+
|                           |                          |
|   Connected to ~/carddemo |  Results will appear     |
|   47 COBOL files          |  here as the agent       |
|   12 JCL files            |  works.                  |
|   8 copybooks             |                          |
|                           |  Suggested actions:      |
|   What would you like     |                          |
|   to do?                  |  [Assess codebase]       |
|                           |  [Compile a program]     |
|                           |  [Explain a program]     |
|                           |  [Run a JCL job]         |
|                           |                          |
+---------------------------+--------------------------+
|  [Type a message or describe your goal...]    [Send] |
+------------------------------------------------------+
```

### Screen 3: Agent Working — Assessment Scan

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent      [~/carddemo ●] [⚙]      |
+---------------------------+--------------------------+
|                           |                          |
|  [thinking...]            |  ASSESSING CODEBASE      |
|                           |                          |
|  [> Assessing codebase]   |  +-----+-----+-----+    |
|     Scanning 18/42 (43%)  |  | 18  | --  | --  |    |
|     Current: POLCY01.cbl  |  |Scand| Cplx| Iss |    |
|                           |  +-----+-----+-----+    |
|                           |                          |
|                           |  Scanning...             |
|                           |  [================----]  |
|                           |  18 of 42 files          |
|                           |                          |
+---------------------------+--------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

### Screen 4: Assessment Results

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent      [~/carddemo ●] [⚙]      |
+---------------------------+--------------------------+
|                           |                          |
|  [v Assessment complete]  |  ASSESSMENT: ./app/cbl   |
|    42 files, 12 issues    |                          |
|                           |  42     28,450  6.2  340h|
|  [text] "I found 42      |  Progs  Lines   Cplx Eff |
|  COBOL programs totaling  |                          |
|  28,450 lines. Average    |  COMPLEXITY              |
|  complexity is 6.2.       |  Low  [======--] 57%     |
|  12 issues found, 2       |  Med  [====----] 29%     |
|  critical..."             |  High [==------] 14%     |
|                           |                          |
|                           |  ISSUES (12)             |
|                           |  CRIT POLCY01:142 IMS    |
|                           |  CRIT BATCH03:87  IMS    |
|                           |  WARN CALC01:23   COMP-5 |
|                           |  ...                     |
|                           |                          |
|                           | [Export MD] [Export JSON] |
+---------------------------+--------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

### Screen 5: Agent Working — Compilation

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent      [~/carddemo ●] [⚙]      |
+---------------------------+--------------------------+
|                           |                          |
|  [> Compiling HELLO.cbl]  |  Compiling...            |
|     ...                   |                          |
|                           |  HELLO.cbl               |
|                           |  [spinning...]           |
|                           |                          |
+---------------------------+--------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

### Screen 6: Compiler Results — Errors

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent      [~/carddemo ●] [⚙]      |
+---------------------------+--------------------------+
|                           |                          |
|  [x Compile failed]       |  COMPILE: HELLO.cbl [x] |
|    2 errors, 1 warning    |                          |
|                           |  Errors: 2  Warnings: 1  |
|  [text] "Two errors:      |                          |
|  1. Line 42 — WS-NAME    |  [x] ERR L42:12          |
|     not defined...        |  'WS-NAME' not defined   |
|  2. Line 87 — typo        |  Fix: Add to WRK-STOR   |
|     PREFORM->PERFORM"     |  [View in code]          |
|                           |                          |
|                           |  [x] ERR L87:8           |
|                           |  Unexpected 'PREFORM'    |
|                           |  Fix: Change to PERFORM  |
|                           |  [View in code]          |
|                           |                          |
|                           |  [!] WARN L15:1          |
|                           |  Unused var WS-TEMP      |
+---------------------------+--------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

### Screen 7: JCL Preview + Approval

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent      [~/carddemo ●] [⚙]      |
+---------------------------+--------------------------+
|                           |                          |
|  [> Analyzing job]        |  JOB PREVIEW: RUNCARD    |
|                           |                          |
|  +---------------------+  |  Step 1: COMPILE         |
|  | ! Execute JCL job?  |  |    PGM=IGYCRCTL          |
|  |                     |  |    SYSIN=HELLO.cbl        |
|  |   Job: RUNCARD      |  |                          |
|  |   Steps: 3          |  |  Step 2: LINK            |
|  |                     |  |    PGM=IEWL               |
|  |   This will compile,|  |    SYSLIN=HELLO.obj       |
|  |   link, and execute |  |                          |
|  |   HELLO.            |  |  Step 3: RUN             |
|  |                     |  |    PGM=HELLO              |
|  |  [Approve] [Reject] |  |    SYSOUT=*               |
|  +---------------------+  |                          |
|                           |                          |
+---------------------------+--------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

### Screen 8: Job Execution Results

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent      [~/carddemo ●] [⚙]      |
+---------------------------+--------------------------+
|                           |                          |
|  [v Job complete — RC=0]  |  JOB: RUNCARD     [RC=0] |
|    Step 1: RC=0           |                          |
|    Step 2: RC=0           |  COMPILE ---LINK--- RUN  |
|    Step 3: RC=0           |  [RC=0]    [RC=0]  [RC=0]|
|                           |                          |
|  [text] "Job RUNCARD      |  Step 3: RUN — Output    |
|  completed successfully.  |  +--------------------+  |
|  All 3 steps returned     |  | HELLO, WORLD!      |  |
|  RC=0."                   |  | RC=0               |  |
|                           |  +--------------------+  |
|                           |                          |
|                           |  [Expand all] [Re-run]   |
+---------------------------+--------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

### Screen 9: Code Explanation

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent      [~/carddemo ●] [⚙]      |
+---------------------------+--------------------------+
|                           |                          |
|  [text] "## POLCY01.cbl  |  POLCY01.cbl [Metrics]   |
|                           |                          |
|  **Purpose:** Calculates  |  85 | 0200-CALC-PREMIUM. |
|  insurance premiums...    |  86 |   MOVE ZERO...     |
|                           |  87 | [i] EVALUATE       |
|  **Business Rules:**      |  88 |       WHEN 'AUTO'  |
|  1. Base premium =        |  89 |         MULTIPLY   |
|     coverage x rate       |  90 |           BY RATE  |
|  2. Risk multiplier for   |  91 |           GIVING   |
|     age > 65 (1.5x)...   |  92 |       WHEN 'HOME'  |
|                           |  ...                     |
|  **Key Sections:**        |  98 | [i] IF CUST-AGE    |
|  - 0100-INITIALIZE        |  99 |       > 65         |
|  - 0200-CALC-PREMIUM      | 100 |       MULTIPLY     |
|    (lines 87-142)..."     | 101 |         BY 1.5     |
|                           |                          |
|                           |  [i] = Business rule     |
+---------------------------+--------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

### Screen 10: Steering / Redirection

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent      [~/carddemo ●] [⚙]      |
+---------------------------+--------------------------+
|                           |                          |
|  [v Assessment complete]  |  (previous artifact)     |
|    42 files, 12 issues    |                          |
|                           |                          |
|  [text] "I found 12      |                          |
|  issues..."               |                          |
|                           |                          |
|  > "Actually, focus only  |                          |
|    on the DB2 programs"   |  ← user steers           |
|                           |                          |
|  [thinking...]            |                          |
|                           |                          |
|  [text] "Focusing on DB2  |  (new filtered artifact) |
|  programs. I found 8      |                          |
|  programs with EXEC SQL.  |                          |
|  Running db2 preprocess   |                          |
|  on each..."              |                          |
|                           |                          |
+---------------------------+--------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

---

## 5. CLI Output to UI Component Mapping

Complete mapping of every CLI command's JSON fields to UI components:

### assess scan

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| `results[]` | AssessmentDashboard — program table | Sortable table rows with program name, LOC, complexity, issue count |
| `overall_complexity` | AssessmentDashboard — stat card | Large number with label "Avg Complexity" |
| `total_lines` | AssessmentDashboard — stat card | Large number with label "Lines of Code" |
| `total_issues` | AssessmentDashboard — stat card | Large number with label "Issues Found", colored by severity |
| `critical_issues` | AssessmentDashboard — severity indicator | Red badge if > 0, appended to issues stat card |
| `estimated_effort_hours` | AssessmentDashboard — stat card | Large number with label "Est. Effort (hrs)" |
| `results[].complexity` | AssessmentDashboard — complexity chart | Horizontal bar chart: low/med/high distribution |
| `results[].issues[]` | AssessmentDashboard — issue table | Sortable table: severity badge, program, line, description, action button |

### assess file

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| `file_name`, `program_id` | ProgramDetailCard — header | Program name with ID |
| `metrics.loc`, `metrics.code_lines`, `metrics.comment_lines` | ProgramDetailCard — metrics row | Three stat badges inline |
| `metrics.cyclomatic_complexity` | ProgramDetailCard — complexity badge | Colored badge: green (<5), yellow (5-10), red (>10) |
| `metrics.paragraph_count`, `metrics.data_items` | ProgramDetailCard — metrics row | Two stat badges |
| `features[]` | ProgramDetailCard — feature tags | Tag cloud: each feature as a pill (green=supported, yellow=partial, red=unsupported) |
| `issues[]` | ProgramDetailCard — issue list | Cards with severity icon, message, line reference, [View in code] link |
| `recommendations[]` | ProgramDetailCard — recommendations | Ordered list with priority indicator |

### compile / check

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| `status` | CompilerDiagnosticCard — header badge | Green checkmark (success) or red X (failure) |
| `program_id` | CompilerDiagnosticCard — header text | Program name |
| `summary.errors` | CompilerDiagnosticCard — count badge | Red badge with count |
| `summary.warnings` | CompilerDiagnosticCard — count badge | Yellow badge with count |
| `summary.infos` | CompilerDiagnosticCard — count badge | Blue badge with count |
| `diagnostics[].severity` | CompilerDiagnosticCard — icon per diagnostic | X (error), ! (warning), i (info) |
| `diagnostics[].message` | CompilerDiagnosticCard — diagnostic text | Monospace message text |
| `diagnostics[].file` | CompilerDiagnosticCard — file reference | Filename text |
| `diagnostics[].line`, `.column` | CompilerDiagnosticCard — location + CodeViewer link | "Line N, Col M" as clickable link to CodeViewer |
| `symbols` | CompilerDiagnosticCard — footer | "N symbols" in muted text |

### run

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| `job_name` | JobTimeline — header | Job name |
| `return_code` | JobTimeline — overall RC badge | Green (0), yellow (4), red (8+) circle badge |
| `status` | JobTimeline — header badge | "Complete" or "Failed" |
| `steps[].name` | JobTimeline — step node label | Step name on timeline node |
| `steps[].return_code` | JobTimeline — step node color | Green/yellow/red node circle |
| `steps[].success` | JobTimeline — step node icon | Checkmark or X inside node |
| `steps[].stdout[]` | JobTimeline — expandable SYSOUT | Monospace text block, collapsed by default |
| `steps[].stderr[]` | JobTimeline — expandable SYSERR | Monospace text block, red-tinted, collapsed by default |

### parse-jcl

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| `job_name` | JobPreviewCard — header | Job name |
| `class`, `msgclass` | JobPreviewCard — header detail | "Class: A" text |
| `accounting`, `programmer` | JobPreviewCard — header detail | Muted text if present |
| `steps[].exec` | JobPreviewCard — step program name | Bold program name per step |
| `steps[].parm` | JobPreviewCard — step parameters | Monospace parameter text |
| `steps[].dd_statements[].name` | JobPreviewCard — DD list per step | DD name labels |
| `steps[].dd_statements[].definition` | JobPreviewCard — DD detail | Truncated definition, expandable |

### interpret

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| `output[]` | ProgramOutputCard — output display | Monospace text, dark background (terminal-style) |
| `return_code` | ProgramOutputCard — RC badge | Green (0) / red (non-zero) |
| `diagnostics[]` | ProgramOutputCard — diagnostic list | Same as CompilerDiagnosticCard diagnostics |

### db2 preprocess

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| SQL statements | GenericOutputCard — SQL blocks | Syntax-highlighted SQL in code blocks |
| Host variables | GenericOutputCard — variable table | Table: COBOL name, SQL type, usage |
| DBRM info | GenericOutputCard — footer | DBRM filename, statement count |

### lex

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| Token list | GenericOutputCard — token display | Monospace token list, collapsible |

### bms

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| Compilation output | GenericOutputCard — build output | Build status, generated copybook path |

### cics (v1)

Not rendered as artifact. Agent explains program behavior via text in activity log. v2 will embed xterm.js terminal.

### idcams

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| Command result | DatasetOperationCard — operation row | Status badge (OK/FAIL), command type, dataset name |
| Dataset info | DatasetOperationCard — detail section | Type (KSDS/ESDS/RRDS), record size, key info |

### gdg

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| Operation result | DatasetOperationCard — operation row | Status badge, operation (CREATE/LIST/DELETE), GDG base name |
| Generation list | DatasetOperationCard — generation table | Chronological list: generation number, dataset name, date |

### config

| JSON Field | UI Component | Visual Treatment |
|-----------|-------------|-----------------|
| Config values | GenericOutputCard — config display | Key-value list, monospace |

---

## 6. AG-UI Event to UI Behavior Mapping

| AG-UI Event | Activity Log Behavior | Artifact Panel Behavior | Example |
|-------------|----------------------|------------------------|---------|
| `TEXT_MESSAGE_START` | Create text entry, remove thinking spinner | No change | Agent starts explaining |
| `TEXT_MESSAGE_CONTENT` | Append text (streaming, word-by-word) | No change | "I found 42 programs..." |
| `TEXT_MESSAGE_END` | Mark text entry complete | No change | Explanation done |
| `TOOL_CALL_START` | Create tool entry: "[> Assessing codebase]" with spinner | No change | Agent starts tool |
| `TOOL_CALL_ARGS` | Update tool entry: extract command, show in expanded detail | No change | Command being constructed |
| `TOOL_CALL_END` | Update status to "executing" | No change | Command ready |
| `TOOL_CALL_RESULT` | Update tool entry: summary line + collapse raw output. Add thinking spinner. | **Render appropriate artifact component** based on command type. Auto-open panel if closed. | Assessment results → AssessmentDashboard |
| `CUSTOM (thinking_start)` | Create thinking_content entry (collapsed, streaming) | No change | Agent reasoning |
| `CUSTOM (thinking_content)` | Append delta to thinking entry | No change | "I should check for IMS..." |
| `CUSTOM (thinking_end)` | Stop streaming indicator on thinking entry | No change | Reasoning done |
| `CUSTOM (on_interrupt)` | Create ApprovalGate card inline | Show JobPreviewCard if JCL approval | "Approve job execution?" |
| `CUSTOM (progress)` | Update progress text in current tool entry: "18/42 (43%)" | Update progress indicator in artifact panel | Assessment scanning |
| `RUN_FINISHED` | Remove spinner/thinking. Mark run complete. | No change | Agent turn done |
| `RUN_ERROR` | Create error entry with message | Show error state in artifact panel | LLM or network error |

**Component selection logic on TOOL_CALL_RESULT:**

```
if command matches "assess scan"    → AssessmentDashboard
if command matches "assess file"    → ProgramDetailCard
if command matches "compile|check"  → CompilerDiagnosticCard
if command matches "run "           → JobTimeline
if command matches "parse-jcl"      → JobPreviewCard
if command matches "interpret"      → ProgramOutputCard
if command matches "idcams|gdg"     → DatasetOperationCard
else                                → GenericOutputCard
```

---

## 7. Component Hierarchy

```
App
├── Header
│   ├── Logo ("OPENMAINFRAME | agent")
│   ├── ConnectionStatusBadge
│   │   └── [dot] [path] or "Not connected"
│   └── SettingsButton
│
├── MainLayout (flex row, responsive)
│   │
│   ├── ActivityLog (left panel, scrollable)
│   │   ├── WelcomeMessage (shown when empty + not connected)
│   │   ├── ConnectedMessage (shown on first connect)
│   │   │   └── SuggestedActions (quick-start buttons)
│   │   │
│   │   └── EntryList (mapped from entries[])
│   │       ├── UserEntry
│   │       │   └── "> {user message text}"
│   │       ├── ThinkingEntry
│   │       │   └── "[thinking...]" spinner
│   │       ├── ThinkingContentEntry (collapsible)
│   │       │   └── Dimmed italic reasoning text
│   │       ├── ToolCallEntry (expandable)
│   │       │   ├── Summary: "[> Assessing codebase]"
│   │       │   ├── Progress: "18/42 (43%)" (if CUSTOM progress)
│   │       │   └── Expanded: command details, raw args
│   │       ├── ToolOutputEntry (expandable)
│   │       │   ├── Summary: "[v Assessment complete] 42 files"
│   │       │   └── Expanded: raw stdout/stderr
│   │       ├── TextEntry
│   │       │   └── Markdown-rendered agent text
│   │       ├── ErrorEntry
│   │       │   └── Red error message with recovery suggestion
│   │       ├── ApprovalGate
│   │       │   ├── Description of pending action
│   │       │   ├── Preview details (job steps, dataset info)
│   │       │   └── [Approve] [Reject] buttons
│   │       └── InterruptEntry (generic HITL)
│   │           └── Question + response buttons
│   │
│   └── ArtifactPanel (right panel, contextual)
│       ├── EmptyState ("Results will appear here...")
│       ├── AssessmentDashboard
│       │   ├── StatCardRow (programs, LOC, complexity, effort)
│       │   ├── ComplexityChart (horizontal bar)
│       │   ├── IssueTable (sortable, filterable)
│       │   └── ExportButtons
│       ├── ProgramDetailCard
│       │   ├── MetricsRow
│       │   ├── FeatureTags
│       │   ├── IssueList
│       │   └── Recommendations
│       ├── CompilerDiagnosticCard
│       │   ├── StatusBadge (success/failure)
│       │   ├── SummaryCounts (errors, warnings, infos)
│       │   └── DiagnosticList
│       │       └── DiagnosticItem (severity, message, location, explanation, [View in code])
│       ├── CodeViewer
│       │   ├── FileHeader (filename, action buttons)
│       │   ├── LineNumbers
│       │   ├── SourceCode (monospace, horizontal scroll)
│       │   └── InlineAnnotations (error markers, business rule notes)
│       ├── JobPreviewCard
│       │   ├── JobHeader (name, class)
│       │   └── StepList (program, DD statements per step)
│       ├── JobTimeline
│       │   ├── OverallRCBadge
│       │   ├── TimelineBar (horizontal step nodes)
│       │   └── StepDetail (expandable SYSOUT/SYSERR per step)
│       ├── ProgramOutputCard
│       │   ├── RCBadge
│       │   └── OutputDisplay (monospace terminal-style)
│       ├── DatasetOperationCard
│       │   ├── OperationList (status badge, command, dataset)
│       │   └── DatasetDetail (type, record size, key info)
│       └── GenericOutputCard
│           └── MonospaceDisplay (raw text with syntax highlighting)
│
├── InputBar (sticky bottom)
│   ├── TextArea (auto-resize, monospace)
│   ├── SendButton
│   └── KeyboardHint ("Enter to send, Shift+Enter for newline")
│
└── SettingsPanel (overlay, triggered by header button)
    ├── PrivacySetting (Full / Output-only / Minimal)
    ├── ThemeSetting (Dark / Light)
    └── ConnectionInfo (bridge address, project path)
```

---

## 8. Interaction Patterns

### 8.1 Intent Entry

Users express goals in natural language. The input bar supports:

- **Free text:** "Assess this codebase", "What does POLCY01 do?"
- **File references:** "Compile HELLO.cbl" (agent resolves path via bridge)
- **Follow-ups:** "Fix those errors", "Show me the DB2 programs" (context from previous entries)
- **Redirection:** "Actually, focus on the JCL files instead" (agent adjusts)

**Suggested actions** appear in the artifact panel on first connect. Clicking a suggestion fills the input bar.

### 8.2 Steering and Redirection

Users can redirect the agent mid-task:

| Scenario | User Says | Agent Behavior |
|----------|----------|---------------|
| During assessment | "Skip the copybooks" | Filters current scan |
| After assessment | "Focus on DB2 programs" | Re-analyzes subset |
| After compile error | "Fix it" | Suggests fix with code change |
| After job failure | "What went wrong?" | Explains failure diagnosis |
| Mid-conversation | "Actually, run the job instead" | Switches workflow |

### 8.3 Approval Gates

Side-effecting operations require explicit approval:

1. Agent presents ApprovalGate inline in activity log
2. Artifact panel shows preview (JobPreviewCard for JCL, DatasetOperationCard for datasets)
3. User clicks [Approve] or [Reject]
4. If approved: agent proceeds, activity log shows progress
5. If rejected: agent acknowledges, asks what to do instead
6. No timeout — waits indefinitely

### 8.4 Artifact Navigation

- **Auto-display:** Artifact panel shows the latest artifact component automatically
- **History:** Clicking a tool entry in the activity log switches artifact panel to that result
- **Collapse:** Artifact panel has a collapse toggle (< / >) to maximize activity log
- **Export:** Each artifact has export buttons (format depends on component type)

### 8.5 Error Recovery

| Error Type | Activity Log | Artifact Panel | User Options |
|-----------|-------------|---------------|-------------|
| Compile failure | Error summary + explanation | CompilerDiagnosticCard with fixes | "Fix it", "Explain more", manual fix + recompile |
| Job failure | Failed step summary + diagnosis | JobTimeline with red step | "What went wrong?", "Fix and re-run" |
| CLI crash | Error entry with stderr | GenericOutputCard with raw error | "Retry", "Try different approach" |
| Bridge disconnect | "Connection lost" banner | Grayed out | Auto-reconnect, manual "Reconnect" |
| LLM error | "Agent error" entry | Previous artifact preserved | "Retry" button |

---

## 9. Color Scheme and Theming

### Primary Theme: Dark (Default)

```
Background:     #0d1117  (om-bg)      — page background
Surface:        #161b22  (om-surface)  — panels, cards, code blocks
Surface Hover:  #1c2129  (om-surface-hover) — interactive hover
Border:         #30363d  (om-border)   — panel dividers, card edges
Text:           #e6edf3  (om-text)     — primary text
Text Secondary: #7d8590  (om-muted)    — labels, metadata, timestamps
Accent:         #58a6ff  (om-accent)   — links, active states, prompts
Success:        #3fb950  (om-success)  — RC=0, compilation success, connected
Warning:        #d29922  (om-warning)  — RC=4, warnings, pending approval
Error:          #f85149  (om-error)    — RC>=8, errors, disconnected, failures
Info:           #a371f7  (om-info)     — thinking, informational badges
```

### Severity Palette

| Severity | Icon | Color | Badge BG |
|----------|------|-------|----------|
| Critical | X (circle) | om-error (#f85149) | #f8514920 (10% opacity) |
| Error | X | om-error (#f85149) | #f8514920 |
| Warning | ! (triangle) | om-warning (#d29922) | #d2992220 |
| Info | i (circle) | om-info (#a371f7) | #a371f720 |
| Success | checkmark | om-success (#3fb950) | #3fb95020 |

### Return Code Badges

| RC Value | Color | Shape | Label |
|----------|-------|-------|-------|
| 0 | om-success | Circle | "RC=0" |
| 4 | om-warning | Triangle | "RC=4" |
| 8+ | om-error | Square | "RC=N" |

### Typography

```
Font Family:     'JetBrains Mono', 'Fira Code', 'Cascadia Code', monospace
Body Size:       14px
Code Size:       13px
Heading Size:    16px (h2), 14px bold (h3)
Line Height:     1.6 (body), 1.4 (code)
Letter Spacing:  -0.01em
```

### Optional: Light Theme

```
Background:     #ffffff
Surface:        #f6f8fa
Border:         #d1d9e0
Text:           #1f2328
Muted:          #636c76
Accent:         #0969da
Success:        #1a7f37
Warning:        #9a6700
Error:          #d1242f
Info:           #8250df
```

---

## 10. Responsive Behavior

### Breakpoints

| Viewport | Layout | Activity Log | Artifact Panel |
|----------|--------|-------------|---------------|
| >= 1280px | Side-by-side | 50% width (min 400px) | 50% width (min 400px) |
| 1024-1279px | Side-by-side narrow | 55% width | 45% width |
| 768-1023px | Stacked / tabbed | Full width (default tab) | Full width (tab: "Results") |
| < 768px | Single column | Full width | Overlay drawer (swipe up) |

### Tablet Mode (768-1023px)

Activity log and artifact panel become tabs:

```
+------------------------------------------------------+
|  OPENMAINFRAME | agent          [Connected ●] [⚙]   |
+------------------------------------------------------+
|  [Activity]  [Results]                               |
+------------------------------------------------------+
|                                                       |
|   (shows selected tab content full-width)             |
|                                                       |
+------------------------------------------------------+
|  [Type a message...]                          [Send] |
+------------------------------------------------------+
```

Tab indicator shows unread badge when artifact updates while on other tab.

### Panel Resize

On desktop, the divider between activity log and artifact panel is draggable. Double-click to reset to 50/50. Collapse arrow (<) hides artifact panel entirely.

---

## 11. Accessibility

### Color Independence

Never rely on color alone to convey meaning:

| Information | Color | Icon/Shape | Text |
|------------|-------|-----------|------|
| Error | Red | X in circle | "ERROR" label |
| Warning | Yellow | ! in triangle | "WARNING" label |
| Success | Green | Checkmark | "OK" / "RC=0" |
| In progress | Blue | Spinner animation | "Running..." |
| Approval needed | Yellow | ! in shield | "Approval required" |

### Keyboard Navigation

| Key | Action |
|-----|--------|
| Tab | Move between activity log entries, artifact panel sections, input bar |
| Enter | Expand/collapse entry; activate button |
| Escape | Collapse expanded entry; close overlay; cancel approval |
| Up/Down | Navigate entries in activity log |
| Ctrl+Shift+A | Toggle artifact panel |
| Ctrl+Enter | Send message (alternative: Enter when input focused) |

### Screen Reader Support

- Activity log entries: `role="log"`, `aria-live="polite"` for new entries
- Artifact panel: `role="complementary"`, `aria-label="Results panel"`
- Approval gates: `role="alertdialog"`, `aria-describedby` with full context
- Progress updates: `aria-live="polite"`, announce "Scanning file 18 of 42"
- Status badges: `aria-label="Return code 0, success"`
- Charts: `aria-describedby` with text description ("57% low complexity, 29% medium, 14% high")

### Motion and Animation

- Respect `prefers-reduced-motion`: disable streaming text animation, collapse animations
- Progress bars update discretely (jump, not animate) when reduced motion preferred
- Spinner uses CSS animation (can be disabled)

### Contrast

- All text meets WCAG AA (4.5:1 contrast ratio)
- Code viewer meets WCAG AAA (7:1) — high-contrast content area
- Focus indicators use 2px solid om-accent outline with 2px offset

---

## 12. Comparison to Previous UX Spec (v1.0)

### What Changed

| Aspect | v1.0 (Old) | v2.0 (New) | Why |
|--------|-----------|-----------|-----|
| **Layout** | 3-panel IDE (file tree + workspace + chat) | 2-panel hybrid (activity log + artifact) | IDE layout is tool-centric. Agent-native needs the agent to drive, not be a sidebar. File tree unnecessary when agent browses files. |
| **Primary surface** | Workspace (code viewer, dashboards) | Activity log (conversation + actions) | In agent-native UX, the conversation IS the workflow. Artifacts supplement but don't replace the narrative. |
| **Chat role** | Right sidebar assistant | Left panel activity log (primary) | Chat was secondary in v1.0. In v2.0 the agent's narration is the primary interaction surface. |
| **File browsing** | Manual file tree | Agent discovers and presents files | Users shouldn't need to browse files when they can say "show me the COBOL files" or "explain POLCY01". |
| **Artifact display** | Workspace tabs (code, report, execution) | Contextual artifact panel | v1.0 required users to switch tabs manually. v2.0 auto-shows the right artifact based on agent output. |
| **Generative UI** | CopilotKit inline UI in chat | Static Generative UI in artifact panel | v1.0 put cards inline in chat (cramped). v2.0 gives artifacts a dedicated panel with proper layout. |
| **Navigation** | Tab-based workspace switching | Activity log history + artifact auto-switch | v1.0 required explicit tab clicks. v2.0 shows the right thing automatically and allows history navigation. |
| **Approval gates** | Not designed | Inline ApprovalGate component | v1.0 had no HITL approval pattern. v2.0 makes it a first-class interaction for JCL execution and dataset ops. |
| **Progress** | Status bar text | Structured progress in activity log + artifact | v1.0 used a static footer status bar. v2.0 streams progress inline with the workflow. |
| **Responsive** | Fixed breakpoints, chat as overlay | Tab-based on tablet, drawer on mobile | v1.0 required 1280px minimum. v2.0 works down to 768px with tab layout. |
| **Theming** | Dark terminal theme | Dark default + light option | Same base palette, now with light theme support. |
| **Design philosophy** | "Code-first, chat-assisted" | "Agent narrates, artifact shows" | Fundamental shift: from IDE with AI assistant to AI agent with rich output. |

### What Stayed the Same

1. **Dark terminal aesthetic** — Monospace fonts, GitHub-dark-inspired colors, terminal feel
2. **Progressive disclosure** — Start simple, reveal detail on demand
3. **Trust through transparency** — Show what the agent is doing
4. **Connection status** — Bridge connectivity prominently displayed
5. **Tailwind CSS + React** — Same tech stack, same design token system

### Why the Change

The v1.0 spec was designed around a **tool-centric** model (IDE with AI features). Research revealed that:

1. **Competitors all use tool-centric dashboards** (Track 3) — and they're losing to agent-native tools
2. **Agent-native apps** like Claude Code, Devin, and Bolt.new put the AI in the driver's seat (Track 2)
3. **Mainframe engineers** need the agent to orchestrate complex multi-command workflows, not present a code editor (Track 1, workflows)
4. **The file tree is obsolete** when the agent can discover, analyze, and present files conversationally (Track 4, Principle 3)
5. **The workspace tabs model** forces users to manually track which tab has which result — the agent should handle this (Track 2, anti-pattern: "IDE-as-agent")

The v2.0 spec inverts the model: the agent is primary, the artifacts are its output, and the human steers rather than drives.
