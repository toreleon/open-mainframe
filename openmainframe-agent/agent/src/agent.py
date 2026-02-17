"""
LangGraph agent definition.
Batch 1: chat-only skeleton.
Batch 2: all 10 CLI tools wired in.
Batch 3: router → compile/chat nodes with StateGraph.
"""

from langgraph.graph import StateGraph
from langgraph.checkpoint.memory import MemorySaver
from langgraph.prebuilt import ToolNode

from .state import AgentState
from .nodes import router_node, chat_node, compile_node
from .tools import ALL_TOOLS


def route_after_tools(state: AgentState) -> str:
    """Route back to the capability node that invoked tools."""
    return state.get("active_node", "chat")


# Build graph
workflow = StateGraph(AgentState)

# Add nodes
workflow.add_node("router", router_node)
workflow.add_node("chat", chat_node)
workflow.add_node("compile", compile_node)
workflow.add_node("tools", ToolNode(tools=ALL_TOOLS))

# Entry point
workflow.set_entry_point("router")

# Router dispatches via Command (goto="compile" or goto="chat")
# Capability nodes dispatch via Command (goto="tools" or goto="__end__")

# After tools execute, route back to the originating capability node
workflow.add_conditional_edges("tools", route_after_tools, {
    "compile": "compile",
    "chat": "chat",
})

# Compile graph with memory checkpointer (required for HITL in later batches)
graph = workflow.compile(checkpointer=MemorySaver())
