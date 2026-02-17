"""
Execute node — run JCL jobs and interpret COBOL with HITL approval.
Uses run_jcl, interpret_cobol, and parse_jcl tools.
Requires human approval before executing run_jcl or interpret_cobol.
"""

from langchain_core.messages import AIMessage, SystemMessage
from langgraph.types import Command, interrupt

from ..config import get_model
from ..tools.execute_tools import run_jcl, interpret_cobol
from ..tools.parse_tools import parse_jcl
from ..util import should_route_to_tool_node

EXECUTE_SYSTEM = """You are a JCL execution and COBOL runtime expert. Help users run mainframe jobs using OpenMainframe.

You have these tools:
- run_jcl: Execute a JCL job file (returns step-by-step results with return codes)
- interpret_cobol: Run a COBOL program through the tree-walking interpreter
- parse_jcl: Parse JCL to understand job structure before execution

IMPORTANT RULES:
1. Before executing run_jcl or interpret_cobol, ALWAYS explain what the job/program will do.
2. Execution requires human approval — the system will ask the user before running.
3. After execution, explain the results clearly:
   - For JCL: summarize each step's return code and SYSOUT content
   - For COBOL: show DISPLAY output and final return code
4. If execution fails (non-zero return code), explain the error and suggest fixes.
5. Use parse_jcl first if the user wants to understand a JCL job before running it.
6. Say "I'm not sure" when uncertain — never hallucinate.

All file paths must be within the configured workspace."""

EXECUTE_TOOLS = [run_jcl, interpret_cobol, parse_jcl]

# Tools that require human approval before execution
HITL_TOOLS = {"run_jcl", "interpret_cobol"}


async def execute_node(state, config) -> Command:
    """Handle JCL execution and COBOL interpretation requests."""
    model = get_model(config)

    # Get frontend tools from CopilotKit for generative UI
    fe_tools = state.get("copilotkit", {}).get("actions", [])

    # Bind execute tools + any frontend tools
    model_with_tools = model.bind_tools([*fe_tools, *EXECUTE_TOOLS])

    response = await model_with_tools.ainvoke(
        [SystemMessage(content=EXECUTE_SYSTEM), *state["messages"]],
        config,
    )

    tool_calls = getattr(response, "tool_calls", [])

    # HITL: interrupt before executing dangerous tools
    for tc in tool_calls:
        tool_name = tc.get("name") if isinstance(tc, dict) else getattr(tc, "name", None)

        if tool_name in HITL_TOOLS:
            args = tc.get("args", {}) if isinstance(tc, dict) else getattr(tc, "args", {})
            file_arg = args.get("jcl_file") or args.get("source_file") or "unknown"

            approval = interrupt({
                "action": f"Execute {tool_name}",
                "file": file_arg,
                "description": f"Run {tool_name} on {file_arg}",
            })

            if not approval.get("approved", False):
                reason = approval.get("reason", "User declined execution.")
                return Command(goto="__end__", update={
                    "messages": [response, AIMessage(content=f"Execution cancelled: {reason}")],
                    "current_operation": None,
                })

    # Approved (or no HITL-requiring tools) — route to tools
    if tool_calls:
        if should_route_to_tool_node(tool_calls, fe_tools):
            return Command(goto="tools", update={
                "messages": [response],
                "active_node": "execute",
                "current_operation": "execute",
            })
        # Frontend tool — CopilotKit handles it
        return Command(goto="__end__", update={
            "messages": [response],
        })

    # No tool calls — direct response
    return Command(goto="__end__", update={
        "messages": [response],
        "current_operation": None,
    })
