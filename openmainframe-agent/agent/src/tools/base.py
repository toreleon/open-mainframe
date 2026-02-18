"""
Base tool utilities.
Provides workspace sandboxing (sanitize_path), output truncation, JSON parsing,
and bridge connection detection.
"""

import json
import os


def _get_bridge_manager():
    """Lazy import to avoid circular-import issues at module load time."""
    from ..bridge_client import bridge_manager
    return bridge_manager

MAX_OUTPUT_BYTES = 20_000  # 20KB truncation limit
WORKSPACE_ROOT = os.getenv("WORKSPACE_ROOT", os.getcwd())


def is_bridge_connected() -> bool:
    """Return True if a bridge daemon is currently connected."""
    try:
        return _get_bridge_manager().has_connection
    except Exception:
        return False


def sanitize_path(path: str) -> str:
    """Prevent directory traversal and null byte injection.

    Resolves the path against WORKSPACE_ROOT and ensures it stays within bounds.
    """
    path = path.replace("\x00", "")

    # Make relative paths relative to WORKSPACE_ROOT
    if not os.path.isabs(path):
        path = os.path.join(WORKSPACE_ROOT, path)

    resolved = os.path.realpath(path)
    allowed_root = os.path.realpath(WORKSPACE_ROOT)
    if not resolved.startswith(allowed_root + os.sep) and resolved != allowed_root:
        raise ValueError(
            f"Path '{path}' resolves outside the allowed workspace '{allowed_root}'"
        )
    return resolved


def try_parse_json(text: str) -> dict | str:
    """Attempt to parse text as JSON; return raw string on failure."""
    try:
        return json.loads(text)
    except (json.JSONDecodeError, ValueError):
        return text
