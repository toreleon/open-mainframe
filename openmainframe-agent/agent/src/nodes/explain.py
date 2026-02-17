"""
Explain node — COBOL/JCL code explanation and business rule extraction.
Primarily LLM-driven analysis; optionally uses lex_cobol and parse_jcl
for structural grounding.
"""

from langchain_core.messages import SystemMessage
from langgraph.types import Command

from ..config import get_model
from ..tools.parse_tools import lex_cobol, parse_jcl
from ..util import should_route_to_tool_node

EXPLAIN_SYSTEM = """You are a COBOL and JCL code explanation expert. When asked to explain code:

1. Provide section-by-section explanations for each COBOL division
2. Identify key data structures and their purposes
3. Extract business rules in "When [condition], then [action]" format
4. Highlight external dependencies (CALL, COPY, EXEC SQL/CICS)
5. Use clear, non-technical language where possible

You have optional tools for deeper analysis:
- lex_cobol: Tokenize a COBOL source file to see its structural elements
- parse_jcl: Parse a JCL file to understand job structure

Use these tools when you need to ground your explanation in the actual code
structure, especially for complex programs. For simple explanations where
the user has already shared the code, you can respond directly.

When you're uncertain about the purpose of a section or business rule,
say so clearly rather than guessing."""

EXPLAIN_TOOLS = [lex_cobol, parse_jcl]


async def explain_node(state, config) -> Command:
    """Handle code explanation and business rule extraction requests."""
    model = get_model(config)

    # Get frontend tools from CopilotKit for generative UI
    fe_tools = state.get("copilotkit", {}).get("actions", [])

    # Bind explain tools + any frontend tools
    model_with_tools = model.bind_tools([*fe_tools, *EXPLAIN_TOOLS])

    response = await model_with_tools.ainvoke(
        [SystemMessage(content=EXPLAIN_SYSTEM), *state["messages"]],
        config,
    )

    tool_calls = getattr(response, "tool_calls", [])

    if tool_calls:
        if should_route_to_tool_node(tool_calls, fe_tools):
            return Command(goto="tools", update={
                "messages": [response],
                "active_node": "explain",
                "current_operation": "explain",
            })
        # Frontend tool — CopilotKit handles it
        return Command(goto="__end__", update={
            "messages": [response],
        })

    # No tool calls — direct LLM response (most common for explain)
    return Command(goto="__end__", update={
        "messages": [response],
        "current_operation": None,
    })
