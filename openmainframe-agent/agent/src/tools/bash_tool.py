"""
Shell command execution tool.
"""

import asyncio

from .base import WORKSPACE_ROOT, MAX_OUTPUT_BYTES


async def bash(command: str, timeout: int = 120) -> dict:
    """Execute a shell command and return stdout, stderr, return_code."""
    try:
        proc = await asyncio.create_subprocess_shell(
            command,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            cwd=WORKSPACE_ROOT,
        )
        try:
            stdout_bytes, stderr_bytes = await asyncio.wait_for(
                proc.communicate(), timeout=timeout
            )
        except asyncio.TimeoutError:
            proc.kill()
            await proc.communicate()
            return {
                "stdout": "",
                "stderr": f"Command timed out after {timeout} seconds",
                "return_code": -1,
            }

        stdout = stdout_bytes.decode("utf-8", errors="replace")[:MAX_OUTPUT_BYTES]
        stderr = stderr_bytes.decode("utf-8", errors="replace")[:MAX_OUTPUT_BYTES]

        return {
            "stdout": stdout,
            "stderr": stderr,
            "return_code": proc.returncode,
        }
    except Exception as e:
        return {
            "stdout": "",
            "stderr": f"Error executing command: {str(e)}",
            "return_code": -1,
        }
