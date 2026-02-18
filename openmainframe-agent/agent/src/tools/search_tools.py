"""
Code search tools: grep (content search) and glob (file pattern matching).
"""

import asyncio
import pathlib

from .base import sanitize_path, MAX_OUTPUT_BYTES


async def grep(pattern: str, path: str = ".", include: str = "") -> dict:
    """Search file contents by regex. Returns matching lines with file paths."""
    resolved = sanitize_path(path)

    # Build command — prefer rg if available, fallback to grep
    cmd_parts = ["grep", "-rn", "--color=never"]
    if include:
        cmd_parts.extend(["--include", include])
    cmd_parts.extend(["--", pattern, resolved])

    try:
        proc = await asyncio.create_subprocess_exec(
            *cmd_parts,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        stdout_bytes, stderr_bytes = await asyncio.wait_for(
            proc.communicate(), timeout=30
        )

        stdout = stdout_bytes.decode("utf-8", errors="replace")[:MAX_OUTPUT_BYTES]

        matches = []
        for line in stdout.splitlines():
            # Format: file:line_number:content
            parts = line.split(":", 2)
            if len(parts) >= 3:
                matches.append({
                    "file": parts[0],
                    "line": int(parts[1]) if parts[1].isdigit() else 0,
                    "content": parts[2],
                })

        return {
            "matches": matches,
            "total_matches": len(matches),
        }
    except asyncio.TimeoutError:
        return {"error": "Search timed out after 30 seconds"}
    except Exception as e:
        return {"error": str(e)}


async def glob(pattern: str, path: str = ".") -> dict:
    """Find files by glob pattern. Returns matching file paths."""
    resolved = sanitize_path(path)

    try:
        base = pathlib.Path(resolved)
        files = sorted(str(p) for p in base.glob(pattern) if p.is_file())

        return {
            "files": files,
            "total": len(files),
        }
    except Exception as e:
        return {"error": str(e)}
