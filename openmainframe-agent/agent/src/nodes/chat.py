"""
Chat node — general conversation fallback.
Handles greetings, tool questions, and unrouted intents.
"""

from langchain_core.messages import SystemMessage
from langgraph.types import Command

from ..config import get_model

CHAT_SYSTEM = """You are the OpenMainframe Agent — an AI-powered mainframe modernization assistant.

You help users assess, compile, execute, and understand COBOL and JCL code running on the OpenMainframe platform.

Your capabilities include:
- **Compilation:** Compile COBOL programs and explain errors
- **Execution:** Run JCL jobs and interpret COBOL programs (with your approval)
- **Explanation:** Explain COBOL/JCL code and extract business rules
- **Assessment:** Scan codebases for metrics and compatibility
- **Dataset Management:** Browse catalogs and manage VSAM datasets

For this conversation, answer general questions, explain how the tool works,
and guide users toward the right command. Be concise, helpful, and honest.
If you don't know something, say so."""


async def chat_node(state, config) -> Command:
    """Handle general conversation without tool calls."""
    model = get_model(config)

    response = await model.ainvoke(
        [SystemMessage(content=CHAT_SYSTEM), *state["messages"]],
        config,
    )

    return Command(goto="__end__", update={
        "messages": [response],
        "current_operation": None,
    })
