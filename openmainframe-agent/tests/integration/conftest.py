"""
Shared fixtures for integration tests.
Tests run against the CardDemo sample codebase.
"""

import os
import sys
import pytest

# Add agent source to path
AGENT_SRC = os.path.join(os.path.dirname(__file__), "..", "..", "agent")
sys.path.insert(0, AGENT_SRC)

# CardDemo paths relative to repo root
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
CARDDEMO_DIR = os.path.join(REPO_ROOT, "aws-mainframe-modernization-carddemo")
CARDDEMO_CBL = os.path.join(CARDDEMO_DIR, "app", "cbl")
CARDDEMO_JCL = os.path.join(CARDDEMO_DIR, "app", "jcl")
CARDDEMO_CPY = os.path.join(CARDDEMO_DIR, "app", "cpy")


@pytest.fixture
def carddemo_dir():
    """Path to CardDemo source root."""
    return CARDDEMO_DIR


@pytest.fixture
def carddemo_cbl():
    """Path to CardDemo COBOL source directory."""
    return CARDDEMO_CBL


@pytest.fixture
def carddemo_jcl():
    """Path to CardDemo JCL directory."""
    return CARDDEMO_JCL


@pytest.fixture
def carddemo_cpy():
    """Path to CardDemo copybook directory."""
    return CARDDEMO_CPY


@pytest.fixture
def sample_cobol_file(carddemo_cbl):
    """Path to a representative CardDemo COBOL file."""
    path = os.path.join(carddemo_cbl, "CBACT01C.cbl")
    if not os.path.exists(path):
        pytest.skip(f"CardDemo COBOL file not found: {path}")
    return path


@pytest.fixture
def sample_jcl_file(carddemo_jcl):
    """Path to a representative CardDemo JCL file."""
    path = os.path.join(carddemo_jcl, "ACCTFILE.jcl")
    if not os.path.exists(path):
        pytest.skip(f"CardDemo JCL file not found: {path}")
    return path


@pytest.fixture(autouse=True)
def set_workspace_root(monkeypatch):
    """Set WORKSPACE_ROOT to repo root so path sanitization allows CardDemo files."""
    monkeypatch.setenv("WORKSPACE_ROOT", REPO_ROOT)


@pytest.fixture
def mock_bridge(monkeypatch):
    """Mock the bridge client so tools route commands through a fake bridge.

    Returns a MockBridgeConnection that records all commands and can be
    configured with custom responses.
    """
    import asyncio
    from unittest.mock import MagicMock, AsyncMock
    from src.bridge_client import bridge_manager, BridgeConnection

    class MockBridgeConnection:
        def __init__(self):
            self.ws = MagicMock()
            self.token = "test-token"
            self.project_path = "/mock/project"
            self.cli_version = "1.0.0-test"
            self.connected = True
            self._commands: list[dict] = []
            self._responses: dict[str, dict] = {}
            self._default_response = {
                "status": "ok",
                "data": {"stdout": "mock output", "stderr": "", "returncode": 0},
            }

        def set_response(self, command_prefix: str, response: dict):
            """Configure a custom response for commands starting with prefix."""
            self._responses[command_prefix] = response

        async def send_command(self, msg: dict) -> dict:
            self._commands.append(msg)
            cmd = msg.get("command", "")
            for prefix, resp in self._responses.items():
                if cmd.startswith(prefix):
                    return {**resp, "id": msg.get("id")}
            return {**self._default_response, "id": msg.get("id")}

        def handle_response(self, msg: dict):
            pass

        async def ping(self) -> dict:
            return {
                "type": "pong",
                "project_path": self.project_path,
                "cli_version": self.cli_version,
            }

        async def close(self):
            self.connected = False

        @property
        def commands(self):
            """List of all commands sent through the bridge."""
            return self._commands

    mock_conn = MockBridgeConnection()
    monkeypatch.setattr(bridge_manager, "_default", mock_conn)
    monkeypatch.setattr(bridge_manager, "_connections", {"test-token": mock_conn})
    return mock_conn
