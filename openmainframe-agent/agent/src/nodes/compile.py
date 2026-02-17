"""
Compile node — COBOL compilation with error explanation.
Uses compile_cobol and check_cobol tools.
"""

from langchain_core.messages import SystemMessage
from langgraph.types import Command

from ..config import get_model
from ..tools.compile_tools import compile_cobol, check_cobol
from ..util import should_route_to_tool_node

COMPILE_SYSTEM = """You are a COBOL compilation expert. Help users compile COBOL programs using OpenMainframe.

You have these tools:
- compile_cobol: Compile a COBOL source file to a native executable
- check_cobol: Quick syntax check without full compilation

When compilation fails:
1. Parse the error output to identify line numbers and error types
2. Explain each error in plain English
3. Suggest specific code fixes when possible
4. Say "I'm not sure about the fix" when uncertain — never hallucinate

When compilation succeeds, congratulate and mention the output binary location.

All file paths must be within the configured workspace."""

COMPILE_TOOLS = [compile_cobol, check_cobol]


async def compile_node(state, config) -> Command:
    """Handle COBOL compilation requests."""
    model = get_model(config)

    # Get frontend tools from CopilotKit for generative UI
    fe_tools = state.get("copilotkit", {}).get("actions", [])

    # Bind compile tools + any frontend tools
    model_with_tools = model.bind_tools([*fe_tools, *COMPILE_TOOLS])

    response = await model_with_tools.ainvoke(
        [SystemMessage(content=COMPILE_SYSTEM), *state["messages"]],
        config,
    )

    tool_calls = getattr(response, "tool_calls", [])

    if tool_calls:
        if should_route_to_tool_node(tool_calls, fe_tools):
            return Command(goto="tools", update={
                "messages": [response],
                "active_node": "compile",
                "current_operation": "compile",
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
