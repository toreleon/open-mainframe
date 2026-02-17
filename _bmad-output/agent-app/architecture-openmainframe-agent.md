---
version: 'v1.0'
date: '2026-02-17'
status: 'draft'
inputDocuments:
  - prd-openmainframe-agent.md
  - ux-design-openmainframe-agent.md
  - research-technical-copilotkit-langgraph.md
  - research-technical-openmainframe-tools.md
---

# Architecture Document: OpenMainframe Agent v1.0

**BMAD Phase:** 3-Solutioning | **Step:** 8 of 11 | **Agent:** Winston 🏗️ Architect

---

## 1. System Overview

### 1.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        Browser                               │
│  ┌───────────────────────────────────────────────────────┐  │
│  │              Next.js Frontend (Port 3000)              │  │
│  │  ┌─────────────┐  ┌──────────────┐  ┌─────────────┐  │  │
│  │  │ CopilotKit  │  │  Workspace   │  │  File Tree   │  │  │
│  │  │ Sidebar     │  │  (Code,Dash) │  │  Panel       │  │  │
│  │  │ (Chat UI)   │  │              │  │              │  │  │
│  │  └──────┬──────┘  └──────┬───────┘  └──────┬───────┘  │  │
│  │         │ useCoAgent     │ state            │ actions   │  │
│  │         └────────────────┼──────────────────┘          │  │
│  └──────────────────────────┼─────────────────────────────┘  │
│                             │ HTTP POST /api/copilotkit       │
└─────────────────────────────┼────────────────────────────────┘
                              │
┌─────────────────────────────┼────────────────────────────────┐
│              Next.js API Route (Server-Side)                  │
│  ┌──────────────────────────┼─────────────────────────────┐  │
│  │         CopilotRuntime (Agent-Lock Mode)                │  │
│  │         LangGraphHttpAgent → http://localhost:8123       │  │
│  └──────────────────────────┼─────────────────────────────┘  │
└─────────────────────────────┼────────────────────────────────┘
                              │ AG-UI Protocol (SSE)
┌─────────────────────────────┼────────────────────────────────┐
│              Python Agent Server (Port 8123)                  │
│  ┌──────────────────────────┼─────────────────────────────┐  │
│  │          FastAPI + ag_ui_langgraph                       │  │
│  │  ┌──────────────────────────────────────────────────┐   │  │
│  │  │            LangGraph StateGraph                   │   │  │
│  │  │                                                   │   │  │
│  │  │  [entry] → [router] → [assess]  → [tools] ──┐   │   │  │
│  │  │                     → [compile] → [tools] ──┤   │   │  │
│  │  │                     → [execute] → [tools] ──┤   │   │  │
│  │  │                     → [explain]  ───────────┤   │   │  │
│  │  │                     → [dataset] → [tools] ──┤   │   │  │
│  │  │                     → [chat]    ────────────┤   │   │  │
│  │  │                                    ↓         │   │  │
│  │  │                              [__end__]       │   │   │  │
│  │  └──────────────────────────────────────────────────┘   │  │
│  └─────────────────────────────────────────────────────────┘  │
│                              │                                 │
│  ┌───────────────────────────┼─────────────────────────────┐  │
│  │          Tool Layer (subprocess wrapper)                 │  │
│  │  assess_scan | compile_cobol | check_cobol               │  │
│  │  run_jcl | interpret_cobol | parse_jcl                   │  │
│  │  list_catalog | idcams_command                            │  │
│  └───────────────────────────┼─────────────────────────────┘  │
└──────────────────────────────┼────────────────────────────────┘
                               │ subprocess.run()
┌──────────────────────────────┼────────────────────────────────┐
│              OpenMainframe CLI (Rust Binary)                   │
│              open-mainframe compile|check|run|interpret|assess │
│              |parse-jcl|idcams|lex                             │
└───────────────────────────────────────────────────────────────┘
```

### 1.2 Technology Decisions Summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| AD-01 Frontend Framework | Next.js 14+ App Router | Native CopilotKit support |
| AD-02 Agent Framework | LangGraph (Python) | Richest tool ecosystem, subprocess support |
| AD-03 Agent-UI Protocol | AG-UI via CopilotKit | Standard streaming, bi-directional state |
| AD-04 CLI Integration | subprocess.run() | Simplest MVP path; no Rust changes needed beyond assess CLI |
| AD-05 LLM Provider | Configurable (OpenAI/Anthropic) | Environment variable switches provider |
| AD-06 Graph Pattern | Router → Capability Nodes | Clear separation of concerns per agent type |
| AD-07 State Sync | useCoAgent + CopilotKitState | CopilotKit's native pattern |
| AD-08 HITL Pattern | LangGraph interrupt() | Native support, renders in chat |
| AD-09 Persistence | MemorySaver (dev) / PostgreSQL (prod) | LangGraph checkpointing |
| AD-10 Deployment | Docker Compose | Single-command local setup |

---

## 2. Frontend Architecture

### 2.1 Next.js Project Structure

```
src/
├── app/
│   ├── layout.tsx              # AD-01: CopilotKit provider + global styles
│   ├── page.tsx                # Main application shell (3-column layout)
│   └── api/
│       └── copilotkit/
│           └── route.ts        # AD-03: CopilotRuntime endpoint
│
├── components/
│   ├── layout/
│   │   ├── Header.tsx          # App header with project selector
│   │   ├── FileTreePanel.tsx   # Left panel: directory tree
│   │   ├── WorkspacePanel.tsx  # Center panel: tabbed content area
│   │   ├── StatusBar.tsx       # Bottom status with operation progress
│   │   └── Footer.tsx          # Version, connection status
│   │
│   ├── workspace/
│   │   ├── CodeViewer.tsx      # COBOL/JCL syntax highlighting
│   │   ├── AssessmentDashboard.tsx  # Metrics cards, charts, tables
│   │   ├── JobTimeline.tsx     # JCL execution step visualization
│   │   ├── ReportViewer.tsx    # Markdown report rendering
│   │   ├── WelcomeScreen.tsx   # First-time onboarding
│   │   └── TabManager.tsx      # Tab state management (max 8)
│   │
│   ├── chat/
│   │   ├── ApprovalCard.tsx    # HITL interrupt rendering
│   │   ├── CompilerOutputCard.tsx  # Compilation results
│   │   ├── AssessmentCard.tsx  # Inline assessment summary
│   │   ├── ProgressCard.tsx    # Operation progress indicator
│   │   └── ExplanationCard.tsx # Code explanation rendering
│   │
│   └── shared/
│       ├── MetricCard.tsx      # Single metric display
│       ├── SeverityBadge.tsx   # Issue severity indicator
│       └── CobolHighlighter.tsx # Syntax highlighting rules
│
├── hooks/
│   ├── useAgentState.ts        # Typed wrapper around useCoAgent
│   ├── useProjectFiles.ts     # File discovery and tree state
│   └── useWorkspaceTabs.ts    # Tab management logic
│
├── lib/
│   ├── types.ts                # Shared TypeScript types matching Python state
│   └── constants.ts            # Theme colors, defaults
│
└── styles/
    └── globals.css             # Tailwind + CopilotKit overrides
```

### 2.2 CopilotKit Runtime Configuration

```typescript
// src/app/api/copilotkit/route.ts
// AD-02, AD-03: Agent-lock mode with LangGraph HTTP agent

import {
  CopilotRuntime,
  ExperimentalEmptyAdapter,
  copilotRuntimeNextJSAppRouterEndpoint,
} from "@copilotkit/runtime";
import { LangGraphHttpAgent } from "@copilotkit/runtime/langgraph";

const runtime = new CopilotRuntime({
  agents: {
    modernization_agent: new LangGraphHttpAgent({
      url: process.env.AGENT_URL || "http://localhost:8123",
    }),
  },
});

export const POST = async (req: NextRequest) => {
  const { handleRequest } = copilotRuntimeNextJSAppRouterEndpoint({
    runtime,
    serviceAdapter: new ExperimentalEmptyAdapter(),
    endpoint: "/api/copilotkit",
  });
  return handleRequest(req);
};
```

### 2.3 State Synchronization (AD-07)

```typescript
// src/hooks/useAgentState.ts
import { useCoAgent } from "@copilotkit/react-core";

export interface AgentState {
  project_path: string | null;
  source_files: SourceFile[];
  assessment_results: AssessmentReport | null;
  compilation_results: CompilationResult[];
  execution_results: ExecutionResult[];
  current_operation: string | null;
  operation_progress: number;
}

export function useAgentState() {
  return useCoAgent<AgentState>({
    name: "modernization_agent",
    initialState: {
      project_path: null,
      source_files: [],
      assessment_results: null,
      compilation_results: [],
      execution_results: [],
      current_operation: null,
      operation_progress: 0,
    },
  });
}
```

### 2.4 Generative UI Registration

```typescript
// src/app/page.tsx — Register tool renderers

// Assessment tool renderer (AD-08 pattern: tool-based generative UI)
useCopilotAction({
  name: "assess_scan",
  available: "disabled",  // render-only, agent calls the real tool
  render: ({ status, result }) => {
    if (status === "executing") return <ProgressCard label="Scanning..." />;
    if (status === "complete") return <AssessmentCard data={result} />;
    return null;
  },
});

// Compilation tool renderer
useCopilotAction({
  name: "compile_cobol",
  available: "disabled",
  render: ({ status, args, result }) => {
    if (status === "executing") return <ProgressCard label={`Compiling ${args.source_file}`} />;
    if (status === "complete") return <CompilerOutputCard data={result} />;
    return null;
  },
});

// HITL interrupt renderer (AD-08)
useLangGraphInterrupt({
  render: ({ event, resolve }) => (
    <ApprovalCard
      details={event.value}
      onApprove={() => resolve({ approved: true })}
      onReject={(reason) => resolve({ approved: false, reason })}
    />
  ),
});
```

### 2.5 Frontend Actions (Agent → Browser)

```typescript
// Actions the agent can invoke on the frontend

useCopilotAction({
  name: "open_file_in_viewer",
  description: "Open a source file in the code viewer",
  available: "remote",
  parameters: [
    { name: "file_path", type: "string", required: true },
    { name: "line", type: "number", required: false },
  ],
  handler: async ({ file_path, line }) => {
    openTab({ type: "code", path: file_path, scrollToLine: line });
  },
});

useCopilotAction({
  name: "show_assessment_dashboard",
  description: "Open the assessment dashboard in the workspace",
  available: "remote",
  handler: async () => {
    openTab({ type: "assessment" });
  },
});

useCopilotAction({
  name: "show_execution_timeline",
  description: "Open the job execution timeline in the workspace",
  available: "remote",
  parameters: [
    { name: "jcl_file", type: "string", required: true },
  ],
  handler: async ({ jcl_file }) => {
    openTab({ type: "execution", jclFile: jcl_file });
  },
});
```

---

## 3. Agent Architecture

### 3.1 Python Project Structure

```
agent/
├── main.py                     # FastAPI entry point
├── pyproject.toml              # Dependencies
├── .env                        # OPENAI_API_KEY, OPEN_MAINFRAME_BIN
│
└── src/
    ├── __init__.py
    ├── agent.py                # Graph definition + compilation
    ├── state.py                # AgentState definition
    ├── config.py               # Environment config loading
    │
    ├── nodes/
    │   ├── __init__.py
    │   ├── router.py           # Intent classification → route to capability
    │   ├── chat.py             # General conversation (fallback)
    │   ├── assess.py           # Assessment workflow node
    │   ├── compile.py          # Compilation workflow node
    │   ├── execute.py          # JCL execution workflow node
    │   ├── explain.py          # Code explanation node
    │   └── dataset.py          # Dataset management node
    │
    ├── tools/
    │   ├── __init__.py
    │   ├── base.py             # Shared subprocess wrapper
    │   ├── assess_tools.py     # assess_scan, assess_file
    │   ├── compile_tools.py    # compile_cobol, check_cobol
    │   ├── execute_tools.py    # run_jcl, interpret_cobol
    │   ├── parse_tools.py      # parse_jcl, lex_cobol
    │   └── dataset_tools.py    # list_catalog, idcams_command
    │
    └── util.py                 # Tool routing helper
```

### 3.2 Agent State (AD-07)

```python
# agent/src/state.py

from typing import TypedDict, Annotated, Optional
from copilotkit import CopilotKitState
from langgraph.graph import add_messages

class SourceFile(TypedDict):
    path: str
    type: str          # "cobol", "jcl", "copybook", "bms", "data"
    size_bytes: int
    line_count: int

class AssessmentReport(TypedDict):
    total_files: int
    total_loc: int
    average_complexity: float
    programs: list[dict]
    issues: list[dict]
    recommendations: list[str]
    feature_support: dict[str, float]

class CompilationResult(TypedDict):
    file_path: str
    success: bool
    output: str
    errors: str
    timestamp: str

class ExecutionResult(TypedDict):
    jcl_file: str
    steps: list[dict]
    max_return_code: int
    output: str
    timestamp: str

class AgentState(CopilotKitState):
    """Main agent state. Inherits messages + copilotkit from CopilotKitState."""

    # Project context
    project_path: Optional[str]
    source_files: list[SourceFile]

    # Assessment
    assessment_results: Optional[AssessmentReport]

    # Compilation
    compilation_results: list[CompilationResult]

    # Execution
    execution_results: list[ExecutionResult]

    # Operation tracking
    current_operation: Optional[str]
    operation_progress: float  # 0.0 - 1.0
```

### 3.3 LangGraph Graph Definition (AD-06)

```python
# agent/src/agent.py

from langgraph.graph import StateGraph, END
from langgraph.checkpoint.memory import MemorySaver
from langgraph.prebuilt import ToolNode

from .state import AgentState
from .nodes.router import router_node
from .nodes.chat import chat_node
from .nodes.assess import assess_node
from .nodes.compile import compile_node
from .nodes.execute import execute_node
from .nodes.explain import explain_node
from .nodes.dataset import dataset_node
from .tools import ALL_TOOLS
from .util import should_route_to_tool_node

# Build graph
workflow = StateGraph(AgentState)

# Add nodes
workflow.add_node("router", router_node)
workflow.add_node("chat", chat_node)
workflow.add_node("assess", assess_node)
workflow.add_node("compile", compile_node)
workflow.add_node("execute", execute_node)
workflow.add_node("explain", explain_node)
workflow.add_node("dataset", dataset_node)
workflow.add_node("tools", ToolNode(tools=ALL_TOOLS))

# Entry point
workflow.set_entry_point("router")

# Router dispatches to capability nodes
# (routing logic inside router_node returns Command)

# Tool node returns to the originating capability node
# (handled via Command routing in each capability node)

# All capability nodes can exit to __end__
# (via Command(goto="__end__"))

# Compile graph
graph = workflow.compile(checkpointer=MemorySaver())
```

### 3.4 Graph Flow Diagram

```
                        ┌──────────┐
                        │  ENTRY   │
                        └────┬─────┘
                             │
                        ┌────▼─────┐
                   ┌────│  router  │────┐
                   │    └────┬─────┘    │
                   │         │          │
          ┌────────┼─────────┼──────────┼────────┐
          │        │         │          │        │
     ┌────▼───┐ ┌──▼────┐ ┌─▼──────┐ ┌─▼─────┐ ┌▼───────┐
     │ assess │ │compile│ │execute │ │explain│ │dataset │
     └───┬────┘ └──┬────┘ └───┬────┘ └──┬────┘ └───┬────┘
         │         │          │         │          │
         │    ┌────▼──────────▼─────────▼──────────▼───┐
         │    │              tools                      │
         │    │    (ToolNode — subprocess execution)    │
         │    └────┬──────────┬─────────┬──────────┬───┘
         │         │          │         │          │
         │    ┌────▼───┐ ┌───▼────┐   │     ┌────▼───┐
         │    │compile │ │execute │   │     │dataset │
         │    │(resume)│ │(resume)│   │     │(resume)│
         │    └───┬────┘ └───┬────┘   │     └───┬────┘
         │        │          │        │         │
         └────────┼──────────┼────────┼─────────┘
                  │          │        │
             ┌────▼──────────▼────────▼────┐
             │        __end__               │
             │   (response to user)         │
             └──────────────────────────────┘

  Special flows:
  - router → chat (for general conversation, no tools)
  - execute: interrupt() before tool call (HITL)
  - dataset: interrupt() before DELETE (HITL)
  - explain: no tools needed (LLM-only analysis)
```

### 3.5 Router Node (AD-06)

```python
# agent/src/nodes/router.py

from langchain_core.messages import SystemMessage
from langgraph.types import Command
from typing import Literal

ROUTER_SYSTEM = """You are a routing agent for mainframe modernization.
Classify the user's intent into exactly one category:

- ASSESS: scanning codebases, getting metrics, compatibility reports
- COMPILE: compiling COBOL files, syntax checking, fixing errors
- EXECUTE: running JCL jobs, interpreting COBOL programs
- EXPLAIN: explaining code, extracting business rules, understanding legacy code
- DATASET: managing datasets, browsing catalogs, IDCAMS operations
- CHAT: general conversation, greetings, questions about the tool itself

Respond with ONLY the category name."""

async def router_node(state: AgentState, config) -> Command[
    Literal["assess", "compile", "execute", "explain", "dataset", "chat"]
]:
    model = get_model(config)
    last_message = state["messages"][-1]

    response = await model.ainvoke([
        SystemMessage(content=ROUTER_SYSTEM),
        last_message,
    ])

    intent = response.content.strip().upper()

    route_map = {
        "ASSESS": "assess",
        "COMPILE": "compile",
        "EXECUTE": "execute",
        "EXPLAIN": "explain",
        "DATASET": "dataset",
        "CHAT": "chat",
    }

    target = route_map.get(intent, "chat")
    return Command(goto=target)
```

### 3.6 Capability Node Pattern

Each capability node follows the same pattern:

```python
# Pattern: agent/src/nodes/<capability>.py

async def capability_node(state: AgentState, config) -> Command:
    model = get_model(config)

    # Get frontend tools from CopilotKit
    fe_tools = state.get("copilotkit", {}).get("actions", [])

    # Bind capability-specific backend tools + frontend tools
    capability_tools = [tool_a, tool_b]
    model_with_tools = model.bind_tools([*fe_tools, *capability_tools])

    # Build context-aware system message
    system = SystemMessage(content=CAPABILITY_SYSTEM_PROMPT)

    # Invoke LLM
    response = await model_with_tools.ainvoke(
        [system, *state["messages"]], config
    )

    # Check for tool calls
    tool_calls = getattr(response, "tool_calls", [])
    if tool_calls:
        if should_route_to_tool_node(tool_calls, fe_tools):
            return Command(goto="tools", update={"messages": response})
        # Frontend tool — let CopilotKit handle it
        return Command(goto="__end__", update={"messages": response})

    # No tool calls — direct response
    return Command(goto="__end__", update={
        "messages": response,
        "current_operation": None,
    })
```

### 3.7 Execute Node with HITL (AD-08)

```python
# agent/src/nodes/execute.py

from langgraph.types import interrupt, Command

async def execute_node(state: AgentState, config) -> Command:
    model = get_model(config)
    fe_tools = state.get("copilotkit", {}).get("actions", [])

    execute_tools = [run_jcl, interpret_cobol, parse_jcl]
    model_with_tools = model.bind_tools([*fe_tools, *execute_tools])

    response = await model_with_tools.ainvoke(
        [SystemMessage(content=EXECUTE_SYSTEM), *state["messages"]], config
    )

    tool_calls = getattr(response, "tool_calls", [])

    for tc in tool_calls:
        tool_name = tc.get("name") if isinstance(tc, dict) else tc.name
        # HITL: interrupt before execution
        if tool_name in ("run_jcl", "interpret_cobol"):
            args = tc.get("args", {}) if isinstance(tc, dict) else tc.args
            approval = interrupt({
                "action": f"Execute {tool_name}",
                "file": args.get("jcl_file") or args.get("source_file"),
                "description": f"Run {tool_name} on {args}",
            })
            if not approval.get("approved", False):
                return Command(goto="__end__", update={
                    "messages": [AIMessage(content="Execution cancelled.")],
                })

    # Approved — route to tools
    if tool_calls and should_route_to_tool_node(tool_calls, fe_tools):
        return Command(goto="tools", update={"messages": response})

    return Command(goto="__end__", update={"messages": response})
```

---

## 4. Tool Layer Architecture (AD-04)

### 4.1 Base Subprocess Wrapper

```python
# agent/src/tools/base.py

import subprocess
import os
import json
from typing import Optional

OPEN_MAINFRAME_BIN = os.getenv(
    "OPEN_MAINFRAME_BIN",
    "./target/release/open-mainframe"
)

MAX_OUTPUT_BYTES = 20_000  # 20KB truncation limit

def run_cli(
    args: list[str],
    timeout: int = 120,
    cwd: Optional[str] = None,
) -> dict:
    """Execute an OpenMainframe CLI command and return structured result."""
    try:
        result = subprocess.run(
            [OPEN_MAINFRAME_BIN, *args],
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=cwd,
            env={**os.environ, "LANG": "C.UTF-8"},
        )
        return {
            "success": result.returncode == 0,
            "stdout": result.stdout[:MAX_OUTPUT_BYTES],
            "stderr": result.stderr[:MAX_OUTPUT_BYTES],
            "return_code": result.returncode,
        }
    except subprocess.TimeoutExpired:
        return {
            "success": False,
            "stdout": "",
            "stderr": f"Command timed out after {timeout} seconds",
            "return_code": -1,
        }
    except FileNotFoundError:
        return {
            "success": False,
            "stdout": "",
            "stderr": f"OpenMainframe binary not found at {OPEN_MAINFRAME_BIN}",
            "return_code": -1,
        }
```

### 4.2 Tool Definitions

```python
# agent/src/tools/assess_tools.py
from langchain.tools import tool
from .base import run_cli

@tool
def assess_scan(directory: str) -> dict:
    """Scan a directory of COBOL source files and return assessment metrics
    including complexity scores, feature inventory, and compatibility issues."""
    return run_cli(["assess", "scan", directory, "--format", "json"], timeout=300)

@tool
def assess_file(file_path: str) -> dict:
    """Assess a single COBOL source file for metrics and compatibility."""
    return run_cli(["assess", "file", file_path, "--format", "json"])
```

```python
# agent/src/tools/compile_tools.py
@tool
def compile_cobol(source_file: str) -> dict:
    """Compile a COBOL source file to a native executable."""
    return run_cli(["compile", source_file])

@tool
def check_cobol(source_file: str) -> dict:
    """Syntax check a COBOL source file without full compilation."""
    return run_cli(["check", source_file], timeout=30)
```

```python
# agent/src/tools/execute_tools.py
@tool
def run_jcl(jcl_file: str) -> dict:
    """Execute a JCL job file. Returns step-by-step execution results."""
    return run_cli(["run", jcl_file], timeout=300)

@tool
def interpret_cobol(source_file: str) -> dict:
    """Run a COBOL program through the tree-walking interpreter."""
    return run_cli(["interpret", source_file])
```

```python
# agent/src/tools/parse_tools.py
@tool
def parse_jcl(jcl_file: str) -> dict:
    """Parse a JCL file and return its AST structure."""
    return run_cli(["parse-jcl", jcl_file], timeout=30)

@tool
def lex_cobol(source_file: str) -> dict:
    """Tokenize a COBOL source file and return the token stream."""
    return run_cli(["lex", source_file], timeout=30)
```

```python
# agent/src/tools/dataset_tools.py
@tool
def list_catalog(pattern: str = "*") -> dict:
    """List datasets in the catalog matching the given pattern."""
    return run_cli(["idcams", "LISTCAT", f"ENTRIES({pattern})"])

@tool
def idcams_command(command: str) -> dict:
    """Execute an IDCAMS command for dataset management.
    Supports: DEFINE CLUSTER, DELETE, REPRO, LISTCAT."""
    ALLOWED = ["DEFINE", "DELETE", "REPRO", "LISTCAT", "PRINT"]
    verb = command.strip().split()[0].upper()
    if verb not in ALLOWED:
        return {"success": False, "stderr": f"Command '{verb}' not allowed"}
    return run_cli(["idcams", command])
```

### 4.3 Tool Aggregation

```python
# agent/src/tools/__init__.py

from .assess_tools import assess_scan, assess_file
from .compile_tools import compile_cobol, check_cobol
from .execute_tools import run_jcl, interpret_cobol
from .parse_tools import parse_jcl, lex_cobol
from .dataset_tools import list_catalog, idcams_command

ALL_TOOLS = [
    assess_scan, assess_file,
    compile_cobol, check_cobol,
    run_jcl, interpret_cobol,
    parse_jcl, lex_cobol,
    list_catalog, idcams_command,
]
```

### 4.4 Tool Routing Utility

```python
# agent/src/util.py

def should_route_to_tool_node(tool_calls, fe_tools) -> bool:
    """Check if tool calls should go to ToolNode or are frontend tools."""
    fe_tool_names = {
        tool.get("name") for tool in fe_tools
    } if fe_tools else set()

    for tc in tool_calls:
        name = tc.get("name") if isinstance(tc, dict) else getattr(tc, "name", None)
        if name in fe_tool_names:
            return False  # Frontend tool — CopilotKit handles it
    return True
```

---

## 5. FastAPI Server (AD-03)

```python
# agent/main.py

import os
from dotenv import load_dotenv
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
import uvicorn
from copilotkit import LangGraphAGUIAgent
from ag_ui_langgraph import add_langgraph_fastapi_endpoint
from src.agent import graph

load_dotenv()

app = FastAPI(title="OpenMainframe Agent")

app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:3000"],
    allow_methods=["*"],
    allow_headers=["*"],
)

add_langgraph_fastapi_endpoint(
    app=app,
    agent=LangGraphAGUIAgent(
        name="modernization_agent",
        description="AI-powered mainframe modernization assistant",
        graph=graph,
    ),
    path="/",
)

def main():
    port = int(os.getenv("PORT", "8123"))
    uvicorn.run("main:app", host="0.0.0.0", port=port, reload=True)

if __name__ == "__main__":
    main()
```

---

## 6. Deployment Architecture (AD-10)

### 6.1 Docker Compose

```yaml
# docker-compose.yml
version: "3.9"

services:
  frontend:
    build:
      context: .
      dockerfile: Dockerfile.frontend
    ports:
      - "3000:3000"
    environment:
      - AGENT_URL=http://agent:8123
    depends_on:
      - agent

  agent:
    build:
      context: ./agent
      dockerfile: Dockerfile.agent
    ports:
      - "8123:8123"
    environment:
      - OPENAI_API_KEY=${OPENAI_API_KEY}
      - OPEN_MAINFRAME_BIN=/usr/local/bin/open-mainframe
    volumes:
      - ./workspaces:/workspaces  # User source files
      - open-mainframe-bin:/usr/local/bin  # Pre-built binary

volumes:
  open-mainframe-bin:
```

### 6.2 Development Setup

```bash
# Prerequisites
cargo build --release  # Build OpenMainframe binary
node -v                # Verify Node.js 20+
python3 --version      # Verify Python 3.11+

# Setup
npm install                          # Frontend dependencies
cd agent && pip install -e .         # Agent dependencies

# Configure
cp .env.example .env                 # Set API keys
cp agent/.env.example agent/.env

# Run
npm run dev                          # Starts both servers
```

---

## 7. Security Architecture

### 7.1 Input Sanitization

```python
# agent/src/tools/base.py

import re

def sanitize_path(path: str) -> str:
    """Prevent directory traversal and command injection."""
    # Remove null bytes
    path = path.replace('\x00', '')
    # Resolve to absolute and check it's under allowed root
    resolved = os.path.realpath(path)
    allowed_root = os.getenv("WORKSPACE_ROOT", os.getcwd())
    if not resolved.startswith(os.path.realpath(allowed_root)):
        raise ValueError(f"Path {path} is outside allowed workspace")
    return resolved

def sanitize_idcams(command: str) -> str:
    """Restrict IDCAMS to allowed verbs only."""
    ALLOWED_VERBS = {"DEFINE", "DELETE", "REPRO", "LISTCAT", "PRINT"}
    verb = command.strip().split()[0].upper()
    if verb not in ALLOWED_VERBS:
        raise ValueError(f"IDCAMS verb '{verb}' not allowed")
    # No shell metacharacters
    if re.search(r'[;&|`$]', command):
        raise ValueError("Shell metacharacters not allowed in IDCAMS commands")
    return command
```

### 7.2 LLM Data Handling

- Source code is sent to the LLM for explanation (user must consent)
- Assessment metrics are structured data — no source code in assessment reports
- Tool outputs are truncated to 20KB before entering LLM context
- Configurable: `SEND_SOURCE_TO_LLM=true/false` environment variable

---

## 8. Error Handling Strategy

### 8.1 Error Categories

| Category | Example | Handling |
|----------|---------|----------|
| Tool Error | OpenMainframe binary not found | Return structured error, agent explains |
| Timeout | Large codebase assessment > 5min | Timeout message, suggest smaller scope |
| LLM Error | API rate limit / network failure | Retry with backoff, user notification |
| State Error | Invalid state transition | Log warning, fall back to chat node |
| User Error | Invalid file path | Agent asks for correction |

### 8.2 Error Flow

```
Tool execution fails
    → subprocess returns error dict
    → ToolNode passes error to capability node
    → Capability node's LLM sees error in tool result
    → LLM generates user-friendly explanation
    → Response includes: what happened, why, what to do next
```

---

## 9. Architectural Decision Records

### AD-01: Next.js App Router over Pages Router

**Context:** CopilotKit supports both. App Router is the modern Next.js pattern.
**Decision:** Use App Router with server components for layout, client components for interactive panels.
**Consequence:** Requires `"use client"` directive on all CopilotKit-consuming components.

### AD-02: Python LangGraph over JavaScript LangGraph

**Context:** LangGraph supports both Python and JS.
**Decision:** Python — subprocess.run() is more natural, richer ML/AI library ecosystem, most CopilotKit examples use Python.
**Consequence:** Two languages in the project (TS frontend, Python backend). Acceptable trade-off.

### AD-03: AG-UI over Direct WebSocket

**Context:** Could use raw WebSocket or REST polling for agent communication.
**Decision:** AG-UI via CopilotKit's standard protocol — SSE-based, supports streaming, state sync, tool calls.
**Consequence:** Locked to CopilotKit's protocol version. Acceptable — it's the standard.

### AD-04: Subprocess over HTTP API for CLI

**Context:** Could wrap OpenMainframe as HTTP server (using deploy crate) or call via subprocess.
**Decision:** Subprocess for MVP — no Rust changes needed (except assess CLI), simpler deployment.
**Trade-off:** ~100ms overhead per call, text parsing for non-JSON commands.
**Migration path:** Add HTTP API mode in v1.1 using open-mainframe-deploy's HTTP server.

### AD-05: Configurable LLM Provider

**Context:** CopilotKit supports OpenAI, Anthropic, Google, etc.
**Decision:** Default to Anthropic Claude (best code understanding), configurable via environment variable.
**Implementation:** `LLM_PROVIDER=anthropic|openai`, `LLM_MODEL=claude-sonnet-4-5-20250929|gpt-4o`.

### AD-06: Router Pattern over Flat Tool Dispatch

**Context:** Could give one LLM all tools and let it decide, or route first then specialize.
**Decision:** Router node classifies intent, then dispatches to specialized capability nodes with focused tool sets and system prompts.
**Rationale:** Better prompt engineering per capability. Each node's system prompt can be tuned for its specific domain (COBOL compilation vs JCL execution vs code explanation). Reduces tool confusion.
**Trade-off:** Extra LLM call for routing (~500ms). Acceptable for better quality.

### AD-07: CopilotKit Shared State over Custom WebSocket

**Context:** Need real-time state sync between agent and UI.
**Decision:** Use CopilotKit's native `useCoAgent` + `CopilotKitState` pattern.
**Consequence:** State schema must be JSON-serializable. Large state (>100KB) may impact performance.

### AD-08: LangGraph interrupt() for HITL

**Context:** Multiple HITL patterns available (interrupt, frontend tools, custom events).
**Decision:** Use `interrupt()` — cleanest pattern, renders natively in CopilotKit chat, supports rich data in interrupt payload.
**Consequence:** Requires checkpointer to be set (state must be persisted at interrupt boundary).

### AD-09: MemorySaver for Dev, PostgreSQL for Prod

**Context:** LangGraph requires a checkpointer for HITL and state persistence.
**Decision:** MemorySaver for development (zero setup), AsyncPostgresSaver for production.
**Migration:** Swap via environment variable `DATABASE_URL`. If set, use Postgres; else, MemorySaver.

### AD-10: Docker Compose for Local Deployment

**Context:** Users need a simple setup process.
**Decision:** Docker Compose with 2 services (frontend, agent) + volume mount for OpenMainframe binary.
**Alternative considered:** Single container — rejected because Python + Node.js + Rust in one image is complex.
