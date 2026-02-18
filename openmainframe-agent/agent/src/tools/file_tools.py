"""
File operation tools: read, write, edit, list directory.
"""

import os

from .base import sanitize_path


async def read_file(file_path: str, offset: int = 0, limit: int = 2000) -> dict:
    """Read a file with optional line range. Returns numbered lines."""
    resolved = sanitize_path(file_path)

    if not os.path.isfile(resolved):
        return {"error": f"File not found: {file_path}"}

    try:
        with open(resolved, "r", encoding="utf-8", errors="replace") as f:
            all_lines = f.readlines()

        total_lines = len(all_lines)
        selected = all_lines[offset : offset + limit]

        # Number lines (1-based)
        numbered = ""
        for i, line in enumerate(selected, start=offset + 1):
            numbered += f"{i:>6}\t{line}"

        truncated = total_lines > offset + limit

        return {
            "content": numbered,
            "total_lines": total_lines,
            "truncated": truncated,
        }
    except Exception as e:
        return {"error": str(e)}


async def write_file(file_path: str, content: str) -> dict:
    """Create or overwrite a file."""
    resolved = sanitize_path(file_path)

    try:
        os.makedirs(os.path.dirname(resolved), exist_ok=True)
        with open(resolved, "w", encoding="utf-8") as f:
            f.write(content)

        return {
            "success": True,
            "bytes_written": len(content.encode("utf-8")),
        }
    except Exception as e:
        return {"error": str(e)}


async def edit_file(file_path: str, old_string: str, new_string: str) -> dict:
    """Replace exact string match in a file.

    Fails if old_string is not found or appears more than once.
    """
    resolved = sanitize_path(file_path)

    if not os.path.isfile(resolved):
        return {"error": f"File not found: {file_path}"}

    try:
        with open(resolved, "r", encoding="utf-8", errors="replace") as f:
            content = f.read()

        count = content.count(old_string)
        if count == 0:
            return {"error": "old_string not found in file"}
        if count > 1:
            return {
                "error": f"old_string appears {count} times — must be unique. "
                "Provide more surrounding context to make it unique."
            }

        new_content = content.replace(old_string, new_string, 1)

        with open(resolved, "w", encoding="utf-8") as f:
            f.write(new_content)

        return {"success": True}
    except Exception as e:
        return {"error": str(e)}


async def list_directory(path: str = ".") -> dict:
    """List directory contents with file types and sizes."""
    resolved = sanitize_path(path)

    if not os.path.isdir(resolved):
        return {"error": f"Not a directory: {path}"}

    try:
        entries = []
        for name in sorted(os.listdir(resolved)):
            full = os.path.join(resolved, name)
            try:
                stat = os.stat(full)
                entries.append({
                    "name": name,
                    "type": "dir" if os.path.isdir(full) else "file",
                    "size": stat.st_size,
                })
            except OSError:
                entries.append({"name": name, "type": "unknown", "size": 0})

        return {"entries": entries, "total": len(entries)}
    except Exception as e:
        return {"error": str(e)}
