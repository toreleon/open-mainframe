#!/usr/bin/env python3
"""
OpenMainframe Local Bridge Daemon.

A lightweight WebSocket client that runs on the customer's machine.
It connects to the OpenMainframe Agent server, receives commands,
executes the open-mainframe CLI locally, and streams results back.

Usage:
    python bridge.py --server ws://localhost:8123/ws/bridge \
                     --project ~/carddemo \
                     --token <session-token>
"""

from __future__ import annotations

import argparse
import asyncio
import glob as globmod
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import websockets

from protocol import (
    MessageType,
    ResultMessage,
    PongMessage,
    ErrorMessage,
    serialize,
    deserialize,
    MAX_OUTPUT_BYTES,
)
from security import (
    validate_project_path,
    sandbox_path,
    validate_command,
    validate_token,
)


def find_cli_binary() -> str:
    """Locate the open-mainframe CLI binary."""
    # Check common locations
    candidates = [
        shutil.which("open-mainframe"),
        os.path.expanduser("~/.cargo/bin/open-mainframe"),
        "./target/release/open-mainframe",
        "../target/release/open-mainframe",
        "../../target/release/open-mainframe",
    ]
    for candidate in candidates:
        if candidate and os.path.isfile(candidate) and os.access(candidate, os.X_OK):
            return str(Path(candidate).resolve())

    # If not found, return the name and let PATH resolve it
    return "open-mainframe"


def get_cli_version(cli_path: str) -> str:
    """Get the CLI version string."""
    try:
        result = subprocess.run(
            [cli_path, "--version"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        return result.stdout.strip() or "unknown"
    except Exception:
        return "unknown"


class BridgeDaemon:
    """Local bridge daemon that connects to the agent server."""

    def __init__(
        self,
        server_url: str,
        project_path: str,
        token: str,
        cli_path: str | None = None,
    ):
        self.server_url = server_url
        self.project_root = validate_project_path(project_path)
        self.token = token
        self.cli_path = cli_path or find_cli_binary()
        self.cli_version = get_cli_version(self.cli_path)
        self.ws = None

    async def connect(self):
        """Connect to the agent server and process messages."""
        url = f"{self.server_url}?token={self.token}"

        print(f"Bridge v1.0.0")
        print(f"Project: {self.project_root}")
        print(f"CLI:     {self.cli_path} ({self.cli_version})")
        print(f"Server:  {self.server_url}")
        print()

        while True:
            try:
                print("Connecting to agent server...")
                async with websockets.connect(url) as ws:
                    self.ws = ws
                    print("Connected! Waiting for commands...\n")
                    await self._message_loop(ws)
            except websockets.ConnectionClosed:
                print("Connection closed. Reconnecting in 3s...")
            except ConnectionRefusedError:
                print("Connection refused. Is the agent server running? Retrying in 5s...")
                await asyncio.sleep(5)
                continue
            except Exception as e:
                print(f"Error: {e}. Reconnecting in 3s...")

            await asyncio.sleep(3)

    async def _message_loop(self, ws):
        """Process incoming messages from the server."""
        async for raw_message in ws:
            try:
                msg = deserialize(raw_message)
                msg_type = msg.get("type")
                msg_id = msg.get("id")

                if msg_type == MessageType.PING:
                    await self._handle_ping(ws)
                elif msg_type == MessageType.EXEC:
                    await self._handle_exec(ws, msg_id, msg.get("command", ""))
                elif msg_type == MessageType.LIST_FILES:
                    await self._handle_list_files(
                        ws, msg_id, msg.get("directory", "."), msg.get("pattern", "*")
                    )
                elif msg_type == MessageType.READ_FILE:
                    await self._handle_read_file(
                        ws, msg_id, msg.get("path", ""), msg.get("lines")
                    )
                else:
                    await ws.send(serialize(ErrorMessage(
                        error=f"Unknown message type: {msg_type}",
                        id=msg_id,
                    )))
            except ValueError as e:
                await ws.send(serialize(ErrorMessage(error=str(e))))
            except Exception as e:
                await ws.send(serialize(ErrorMessage(error=f"Internal error: {e}")))

    async def _handle_ping(self, ws):
        """Respond to ping with bridge info."""
        await ws.send(serialize(PongMessage(
            project_path=str(self.project_root),
            cli_version=self.cli_version,
        )))

    async def _handle_exec(self, ws, msg_id: str, command: str):
        """Execute a CLI command and return the result."""
        try:
            parts = validate_command(command, self.project_root)

            # Ensure the binary path is absolute
            if not parts[0].endswith("open-mainframe") and "/" not in parts[0]:
                parts = [self.cli_path] + parts
            elif parts[0].endswith("open-mainframe") or "/" in parts[0]:
                parts[0] = self.cli_path

            print(f"  EXEC [{msg_id[:8]}]: {' '.join(parts)}")

            result = await asyncio.get_running_loop().run_in_executor(
                None,
                lambda: subprocess.run(
                    parts,
                    capture_output=True,
                    text=True,
                    timeout=120,
                    cwd=str(self.project_root),
                ),
            )

            output = result.stdout or ""
            stderr = result.stderr or ""

            # Truncate if too large
            if len(output) > MAX_OUTPUT_BYTES:
                output = output[:MAX_OUTPUT_BYTES] + "\n... (output truncated)"
            if len(stderr) > MAX_OUTPUT_BYTES:
                stderr = stderr[:MAX_OUTPUT_BYTES] + "\n... (stderr truncated)"

            status = "ok" if result.returncode == 0 else "error"
            data = {
                "stdout": output,
                "stderr": stderr,
                "returncode": result.returncode,
            }

            await ws.send(serialize(ResultMessage(
                id=msg_id,
                status=status,
                data=data,
                error=stderr if result.returncode != 0 else None,
            )))

            print(f"  DONE [{msg_id[:8]}]: rc={result.returncode}")

        except ValueError as e:
            await ws.send(serialize(ResultMessage(
                id=msg_id,
                status="error",
                error=str(e),
            )))
        except subprocess.TimeoutExpired:
            await ws.send(serialize(ResultMessage(
                id=msg_id,
                status="error",
                error="Command timed out after 120 seconds",
            )))
        except FileNotFoundError:
            await ws.send(serialize(ResultMessage(
                id=msg_id,
                status="error",
                error=f"CLI binary not found: {self.cli_path}. "
                      "Build it with: cargo build --release",
            )))

    async def _handle_list_files(self, ws, msg_id: str, directory: str, pattern: str):
        """List files matching a glob pattern within the project."""
        try:
            dir_path = sandbox_path(directory, self.project_root)
            if not dir_path.is_dir():
                raise ValueError(f"Not a directory: {directory}")

            search_pattern = str(dir_path / "**" / pattern)
            files = []
            for f in globmod.glob(search_pattern, recursive=True):
                fp = Path(f)
                if fp.is_file():
                    rel = fp.relative_to(self.project_root)
                    try:
                        size = fp.stat().st_size
                        if size < 5_000_000:
                            with open(fp, "rb") as fh:
                                line_count = sum(1 for _ in fh)
                        else:
                            line_count = 0
                    except Exception:
                        size = 0
                        line_count = 0

                    # Determine file type
                    ext = fp.suffix.lower()
                    file_type = {
                        ".cbl": "cobol", ".cob": "cobol", ".cobol": "cobol",
                        ".jcl": "jcl",
                        ".cpy": "copybook",
                        ".bms": "bms",
                    }.get(ext, "data")

                    files.append({
                        "path": str(rel),
                        "type": file_type,
                        "size_bytes": size,
                        "line_count": line_count,
                    })

            await ws.send(serialize(ResultMessage(
                id=msg_id,
                status="ok",
                data={"files": files, "count": len(files)},
            )))
        except ValueError as e:
            await ws.send(serialize(ResultMessage(
                id=msg_id,
                status="error",
                error=str(e),
            )))

    async def _handle_read_file(self, ws, msg_id: str, path: str, lines: list | None):
        """Read a file's content, optionally a range of lines."""
        try:
            file_path = sandbox_path(path, self.project_root)
            if not file_path.is_file():
                raise ValueError(f"File not found: {path}")

            # Size check — don't read files > 5MB
            if file_path.stat().st_size > 5_000_000:
                raise ValueError(f"File too large to read: {path} ({file_path.stat().st_size} bytes)")

            content = file_path.read_text(errors="replace")

            if lines and len(lines) == 2:
                start, end = lines
                all_lines = content.split("\n")
                content = "\n".join(all_lines[max(0, start - 1):end])

            if len(content) > MAX_OUTPUT_BYTES:
                content = content[:MAX_OUTPUT_BYTES] + "\n... (content truncated)"

            await ws.send(serialize(ResultMessage(
                id=msg_id,
                status="ok",
                data={"content": content, "path": path},
            )))
        except ValueError as e:
            await ws.send(serialize(ResultMessage(
                id=msg_id,
                status="error",
                error=str(e),
            )))


def main():
    parser = argparse.ArgumentParser(
        description="OpenMainframe Local Bridge — connects your environment to the agent"
    )
    parser.add_argument(
        "--server",
        default="ws://localhost:8123/ws/bridge",
        help="Agent server WebSocket URL (default: ws://localhost:8123/ws/bridge)",
    )
    parser.add_argument(
        "--project",
        required=True,
        help="Path to your COBOL/JCL project directory",
    )
    parser.add_argument(
        "--token",
        default="",
        help="Session token for authentication",
    )
    parser.add_argument(
        "--cli",
        default=None,
        help="Path to open-mainframe CLI binary (auto-detected if not set)",
    )

    args = parser.parse_args()

    daemon = BridgeDaemon(
        server_url=args.server,
        project_path=args.project,
        token=args.token,
        cli_path=args.cli,
    )

    try:
        asyncio.run(daemon.connect())
    except KeyboardInterrupt:
        print("\nBridge stopped.")
        sys.exit(0)


if __name__ == "__main__":
    main()
