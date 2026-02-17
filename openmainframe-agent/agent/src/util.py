"""
Utility functions for the agent graph.
"""


def should_route_to_tool_node(tool_calls, fe_tools) -> bool:
    """Check if tool calls should go to ToolNode (backend) or are frontend tools.

    Frontend tools are handled by CopilotKit directly, so we route to __end__
    and let the framework handle them. Backend tools go through ToolNode for
    subprocess execution.
    """
    fe_tool_names = set()
    if fe_tools:
        for tool in fe_tools:
            name = (
                tool.get("name")
                if isinstance(tool, dict)
                else getattr(tool, "name", None)
            )
            if name:
                fe_tool_names.add(name)

    for tc in tool_calls:
        name = (
            tc.get("name")
            if isinstance(tc, dict)
            else getattr(tc, "name", None)
        )
        if name and name in fe_tool_names:
            return False  # Frontend tool — CopilotKit handles it

    return True
