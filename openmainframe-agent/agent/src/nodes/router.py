"""
Router node — classifies user intent and dispatches to capability nodes.
"""

from typing import Literal

from langchain_core.messages import SystemMessage
from langgraph.types import Command

from ..config import get_model

ROUTER_SYSTEM = """You are a routing agent for mainframe modernization.
Classify the user's intent into exactly one category:

- ASSESS: scanning codebases, getting metrics, compatibility reports
- COMPILE: compiling COBOL files, syntax checking, fixing errors, building programs
- EXECUTE: running JCL jobs, interpreting COBOL programs
- EXPLAIN: explaining code, extracting business rules, understanding legacy code
- DATASET: managing datasets, browsing catalogs, IDCAMS operations
- CHAT: general conversation, greetings, questions about the tool itself

Respond with ONLY the category name."""


async def router_node(state, config) -> Command[Literal["compile", "execute", "chat"]]:
    """Classify user intent and route to the appropriate capability node."""
    model = get_model(config)
    last_message = state["messages"][-1]

    response = await model.ainvoke([
        SystemMessage(content=ROUTER_SYSTEM),
        last_message,
    ])

    intent = response.content.strip().upper()

    # Batch 3: COMPILE node
    if intent == "COMPILE":
        return Command(goto="compile")

    # Batch 4: EXECUTE node (with HITL)
    if intent == "EXECUTE":
        return Command(goto="execute")

    # Future batches add: assess, explain, dataset
    return Command(goto="chat")
