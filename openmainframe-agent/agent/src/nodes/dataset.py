"""
Dataset node — browse catalogs and manage datasets via IDCAMS.
Uses list_catalog and idcams_command tools.
Requires human approval before DELETE operations.
"""

from langchain_core.messages import AIMessage, SystemMessage
from langgraph.types import Command, interrupt

from ..config import get_model
from ..tools.dataset_tools import list_catalog, idcams_command
from ..util import should_route_to_tool_node

DATASET_SYSTEM = """You are a VSAM dataset management expert. Help users browse catalogs and manage datasets using OpenMainframe IDCAMS.

You have these tools:
- list_catalog: List datasets matching a pattern (e.g., "USER.*", "ACCT.DATA.*")
- idcams_command: Execute IDCAMS commands (DEFINE, DELETE, REPRO, LISTCAT, PRINT)

IMPORTANT RULES:
1. DELETE operations require human approval — the system will ask the user first.
2. When defining datasets, explain the parameters (record format, key length, etc.).
3. When listing datasets, present results in a clear, organized format.
4. For REPRO (copy) operations, confirm source and target with the user.
5. Explain IDCAMS concepts in plain language when the user seems unfamiliar.
6. Say "I'm not sure" when uncertain — never hallucinate.

Common IDCAMS patterns:
- DEFINE CLUSTER(NAME(ds.name) RECORDS(n) RECORDSIZE(avg max) KEYS(len offset))
- DELETE ds.name
- REPRO INFILE(source) OUTFILE(target)
- LISTCAT ENTRIES(pattern)
- PRINT INFILE(ds.name) COUNT(n)

All dataset names should follow mainframe naming conventions."""

DATASET_TOOLS = [list_catalog, idcams_command]


async def dataset_node(state, config) -> Command:
    """Handle dataset management requests."""
    model = get_model(config)

    # Get frontend tools from CopilotKit for generative UI
    fe_tools = state.get("copilotkit", {}).get("actions", [])

    # Bind dataset tools + any frontend tools
    model_with_tools = model.bind_tools([*fe_tools, *DATASET_TOOLS])

    response = await model_with_tools.ainvoke(
        [SystemMessage(content=DATASET_SYSTEM), *state["messages"]],
        config,
    )

    tool_calls = getattr(response, "tool_calls", [])

    # HITL: interrupt before DELETE operations
    for tc in tool_calls:
        tool_name = tc.get("name") if isinstance(tc, dict) else getattr(tc, "name", None)

        if tool_name == "idcams_command":
            args = tc.get("args", {}) if isinstance(tc, dict) else getattr(tc, "args", {})
            command_str = args.get("command", "")

            if command_str.strip().upper().startswith("DELETE"):
                approval = interrupt({
                    "action": "Delete dataset",
                    "command": command_str,
                    "description": f"Execute IDCAMS DELETE: {command_str}",
                })

                if not approval.get("approved", False):
                    reason = approval.get("reason", "User declined deletion.")
                    return Command(goto="__end__", update={
                        "messages": [response, AIMessage(content=f"Delete cancelled: {reason}")],
                        "current_operation": None,
                    })

    # Approved (or no DELETE) — route to tools
    if tool_calls:
        if should_route_to_tool_node(tool_calls, fe_tools):
            return Command(goto="tools", update={
                "messages": [response],
                "active_node": "dataset",
                "current_operation": "dataset",
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
