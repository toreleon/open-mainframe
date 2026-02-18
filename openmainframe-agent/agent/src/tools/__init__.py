"""
General-purpose CLI coding agent tools.
Provides TOOL_REGISTRY (name → function), TOOL_SCHEMAS (Anthropic format),
and HITL_TOOLS (tools requiring human approval before execution).
"""

from .bash_tool import bash
from .file_tools import read_file, write_file, edit_file, list_directory
from .search_tools import grep, glob

# name → async callable
TOOL_REGISTRY: dict = {
    "bash": bash,
    "read_file": read_file,
    "write_file": write_file,
    "edit_file": edit_file,
    "list_directory": list_directory,
    "grep": grep,
    "glob": glob,
}

# Anthropic tool format: {name, description, input_schema}
TOOL_SCHEMAS: list[dict] = [
    {
        "name": "bash",
        "description": (
            "Execute a shell command and return stdout, stderr, and return code. "
            "Use this for running builds, tests, git operations, installing packages, "
            "and any other terminal commands."
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
    {
        "name": "read_file",
        "description": (
            "Read a file and return its contents with line numbers. "
            "Use offset and limit to read specific line ranges in large files."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "file_path": {
                    "type": "string",
                    "description": "Path to the file to read.",
                },
                "offset": {
                    "type": "integer",
                    "description": "Line offset to start reading from (0-based, default: 0).",
                    "default": 0,
                },
                "limit": {
                    "type": "integer",
                    "description": "Maximum number of lines to read (default: 2000).",
                    "default": 2000,
                },
            },
            "required": ["file_path"],
        },
    },
    {
        "name": "write_file",
        "description": (
            "Create or overwrite a file with the given content. "
            "Parent directories are created automatically."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "file_path": {
                    "type": "string",
                    "description": "Path to the file to write.",
                },
                "content": {
                    "type": "string",
                    "description": "The full content to write to the file.",
                },
            },
            "required": ["file_path", "content"],
        },
    },
    {
        "name": "edit_file",
        "description": (
            "Find and replace an exact string in a file. "
            "The old_string must appear exactly once in the file. "
            "Provide enough surrounding context to make the match unique."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "file_path": {
                    "type": "string",
                    "description": "Path to the file to edit.",
                },
                "old_string": {
                    "type": "string",
                    "description": "The exact string to find (must be unique in the file).",
                },
                "new_string": {
                    "type": "string",
                    "description": "The replacement string.",
                },
            },
            "required": ["file_path", "old_string", "new_string"],
        },
    },
    {
        "name": "list_directory",
        "description": (
            "List the contents of a directory. "
            "Returns file names, types (file/dir), and sizes."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "path": {
                    "type": "string",
                    "description": 'Directory path to list (default: ".").',
                    "default": ".",
                },
            },
            "required": [],
        },
    },
    {
        "name": "grep",
        "description": (
            "Search file contents by regex pattern. "
            "Returns matching lines with file paths and line numbers."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "pattern": {
                    "type": "string",
                    "description": "Regular expression pattern to search for.",
                },
                "path": {
                    "type": "string",
                    "description": 'File or directory to search in (default: ".").',
                    "default": ".",
                },
                "include": {
                    "type": "string",
                    "description": 'Glob pattern to filter files (e.g., "*.py", "*.ts").',
                    "default": "",
                },
            },
            "required": ["pattern"],
        },
    },
    {
        "name": "glob",
        "description": (
            "Find files by glob pattern. "
            "Returns matching file paths. Use '**/' for recursive matching."
        ),
        "input_schema": {
            "type": "object",
            "properties": {
                "pattern": {
                    "type": "string",
                    "description": 'Glob pattern to match (e.g., "**/*.py", "src/**/*.ts").',
                },
                "path": {
                    "type": "string",
                    "description": 'Base directory for the search (default: ".").',
                    "default": ".",
                },
            },
            "required": ["pattern"],
        },
    },
]

# Tools requiring human-in-the-loop approval before execution.
# General-purpose tools run freely (like Claude Code).
HITL_TOOLS: set[str] = set()

__all__ = [
    "TOOL_REGISTRY",
    "TOOL_SCHEMAS",
    "HITL_TOOLS",
    "bash",
    "read_file",
    "write_file",
    "edit_file",
    "list_directory",
    "grep",
    "glob",
]
