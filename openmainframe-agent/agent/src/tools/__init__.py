"""
CLI coding agent tools.
Single bash tool — all operations go through shell commands.
"""

from .bash_tool import bash

# name → async callable
TOOL_REGISTRY: dict = {
    "bash": bash,
}

# Anthropic tool format: {name, description, input_schema}
TOOL_SCHEMAS: list[dict] = [
    {
        "name": "bash",
        "description": (
            "Execute a shell command and return stdout, stderr, and return code. "
            "Use this for everything: reading files (cat), writing files (tee, heredoc), "
            "editing files (sed, patch), searching (find, grep), listing (ls, tree), "
            "running builds, tests, git, package managers, and any other CLI operation."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "command": {
                    "type": "string",
                    "description": "The shell command to execute.",
                },
                "timeout": {
                    "type": "integer",
                    "description": "Max execution time in seconds (default: 120).",
                    "default": 120,
                },
            },
            "required": ["command"],
        },
    },
]

# Tools requiring human-in-the-loop approval before execution.
HITL_TOOLS: set[str] = set()

__all__ = [
    "TOOL_REGISTRY",
    "TOOL_SCHEMAS",
    "HITL_TOOLS",
    "bash",
]
