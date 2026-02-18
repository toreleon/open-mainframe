"""
Agent state definition.
Plain dataclass — no LangGraph/CopilotKit dependency.
Keep field names in sync with frontend types in src/lib/types.ts.
"""

from dataclasses import dataclass


@dataclass
class AgentState:
    """Agent state emitted as STATE_SNAPSHOT events for frontend consumption."""

    project_path: str | None = None
    current_operation: str | None = None

    def to_dict(self) -> dict:
        return {
            "project_path": self.project_path,
            "current_operation": self.current_operation,
        }
