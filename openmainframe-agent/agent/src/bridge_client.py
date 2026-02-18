"""
Agent-side bridge client.

Manages WebSocket connections from local bridge daemons.
The agent server runs a WebSocket endpoint at /ws/bridge.
Bridge daemons connect with a session token. Tools then
send commands to the bridge via this client.
"""

from __future__ import annotations

import asyncio
import json
import uuid
from typing import Any, Optional

from fastapi import WebSocket, WebSocketDisconnect


class BridgeConnection:
    """Represents a single bridge daemon connection."""

    def __init__(self, ws: WebSocket, token: str):
        self.ws = ws
        self.token = token
        self.project_path: Optional[str] = None
        self.cli_version: Optional[str] = None
        self.connected = True
        self._pending: dict[str, asyncio.Future] = {}

    async def send_command(self, msg: dict) -> dict:
        """Send a command and wait for the result."""
        msg_id = msg.get("id", str(uuid.uuid4()))
        msg["id"] = msg_id

        future = asyncio.get_running_loop().create_future()
        self._pending[msg_id] = future

        try:
            await self.ws.send_json(msg)
            result = await asyncio.wait_for(future, timeout=180)
            return result
        except asyncio.TimeoutError:
            self._pending.pop(msg_id, None)
            return {
                "status": "error",
                "error": "Bridge command timed out after 180 seconds",
            }
        except Exception as e:
            self._pending.pop(msg_id, None)
            return {"status": "error", "error": str(e)}

    def handle_response(self, msg: dict):
        """Route an incoming response to the waiting future.

        Handles both ID-matched responses (exec, list_files, read_file)
        and non-ID messages (pong).
        """
        msg_type = msg.get("type")

        # Pong messages have no ID — route to the dedicated future
        if msg_type == "pong" and "_pong" in self._pending:
            future = self._pending.pop("_pong")
            if not future.done():
                future.set_result(msg)
            return

        msg_id = msg.get("id")
        if msg_id and msg_id in self._pending:
            future = self._pending.pop(msg_id)
            if not future.done():
                future.set_result(msg)

    async def ping(self) -> dict:
        """Ping the bridge and get info.

        Pong messages have no ID, so we use a special '_pong' key
        instead of the normal send_command ID-matching flow.
        """
        future = asyncio.get_running_loop().create_future()
        self._pending["_pong"] = future

        try:
            await self.ws.send_json({"type": "ping"})
            result = await asyncio.wait_for(future, timeout=10)
            if result.get("type") == "pong":
                self.project_path = result.get("project_path")
                self.cli_version = result.get("cli_version")
            return result
        except asyncio.TimeoutError:
            self._pending.pop("_pong", None)
            return {"status": "error", "error": "Ping timed out"}
        except Exception as e:
            self._pending.pop("_pong", None)
            return {"status": "error", "error": str(e)}

    async def close(self):
        """Close the connection."""
        self.connected = False
        for msg_id, future in self._pending.items():
            if not future.done():
                future.set_exception(ConnectionError("Bridge disconnected"))
        self._pending.clear()


class BridgeManager:
    """Manages all bridge connections, keyed by session token."""

    def __init__(self):
        self._connections: dict[str, BridgeConnection] = {}
        # Default connection for single-user mode
        self._default: Optional[BridgeConnection] = None

    @property
    def has_connection(self) -> bool:
        return self._default is not None and self._default.connected

    @property
    def default_connection(self) -> Optional[BridgeConnection]:
        return self._default

    async def register(self, ws: WebSocket, token: str) -> BridgeConnection:
        """Register a new bridge connection.

        NOTE: Does NOT ping here. The caller must start a message-reading
        loop first, then call conn.ping() so the pong can actually be received.
        """
        conn = BridgeConnection(ws, token)
        self._connections[token] = conn
        if self._default is None:
            self._default = conn
        return conn

    def unregister(self, token: str):
        """Remove a bridge connection."""
        conn = self._connections.pop(token, None)
        if conn:
            conn.connected = False
            if self._default is conn:
                # Promote another connection or set to None
                if self._connections:
                    self._default = next(iter(self._connections.values()))
                else:
                    self._default = None

    def get(self, token: str = "") -> Optional[BridgeConnection]:
        """Get a bridge connection by token, or the default."""
        if token and token in self._connections:
            return self._connections[token]
        return self._default

    def status(self) -> dict:
        """Get bridge status for the /health endpoint."""
        conn = self._default
        if conn and conn.connected:
            return {
                "bridge_connected": True,
                "project_path": conn.project_path,
                "cli_version": conn.cli_version,
            }
        return {"bridge_connected": False}


# Singleton instance
bridge_manager = BridgeManager()


async def execute_via_bridge(
    command_args: list[str],
    timeout: int = 120,
) -> dict:
    """Execute a CLI command via the connected bridge.

    This is the bridge-aware replacement for run_cli().
    Returns the same dict format: {success, stdout, stderr, return_code}

    Args:
        command_args: CLI arguments (without binary name).
        timeout: Max execution time in seconds.
    """
    conn = bridge_manager.get()
    if not conn or not conn.connected:
        return {
            "success": False,
            "stdout": "",
            "stderr": (
                "No local environment connected. "
                "Start the bridge with:\n"
                "  python bridge.py --project ~/your-project --server ws://<agent-host>:8123/ws/bridge"
            ),
            "return_code": -1,
        }

    command_str = " ".join(command_args)
    msg = {
        "type": "exec",
        "id": str(uuid.uuid4()),
        "command": command_str,
    }

    result = await conn.send_command(msg)

    if result.get("status") == "ok":
        data = result.get("data", {})
        return {
            "success": True,
            "stdout": data.get("stdout", ""),
            "stderr": data.get("stderr", ""),
            "return_code": data.get("returncode", 0),
        }
    else:
        return {
            "success": False,
            "stdout": "",
            "stderr": result.get("error", "Unknown bridge error"),
            "return_code": -1,
        }


async def list_files_via_bridge(
    directory: str = ".",
    pattern: str = "*",
) -> dict:
    """List files in the project via the bridge."""
    conn = bridge_manager.get()
    if not conn or not conn.connected:
        return {"files": [], "count": 0, "error": "No bridge connected"}

    msg = {
        "type": "list_files",
        "id": str(uuid.uuid4()),
        "directory": directory,
        "pattern": pattern,
    }

    result = await conn.send_command(msg)
    if result.get("status") == "ok":
        return result.get("data", {"files": [], "count": 0})
    return {"files": [], "count": 0, "error": result.get("error")}


async def read_file_via_bridge(
    path: str,
    lines: list[int] | None = None,
) -> dict:
    """Read a file from the project via the bridge."""
    conn = bridge_manager.get()
    if not conn or not conn.connected:
        return {"content": "", "error": "No bridge connected"}

    msg = {
        "type": "read_file",
        "id": str(uuid.uuid4()),
        "path": path,
        "lines": lines,
    }

    result = await conn.send_command(msg)
    if result.get("status") == "ok":
        return result.get("data", {"content": ""})
    return {"content": "", "error": result.get("error")}
