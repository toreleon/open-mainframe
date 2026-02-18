"""
Tests for the bridge daemon's security and protocol components.

Tests run against the bridge's protocol and security modules directly,
without requiring a WebSocket connection.
"""

import os
import sys
import json
import tempfile
from pathlib import Path

import pytest

# Add bridge source to path
BRIDGE_SRC = os.path.join(os.path.dirname(__file__), "..")
sys.path.insert(0, BRIDGE_SRC)

from protocol import (
    MessageType,
    ExecMessage,
    ListFilesMessage,
    ReadFileMessage,
    PingMessage,
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
    ALLOWED_SUBCOMMANDS,
)


class TestProtocolSerialization:
    """Test protocol message serialization/deserialization."""

    def test_exec_message_serializes(self):
        msg = ExecMessage(command="assess scan /path")
        raw = serialize(msg)
        data = json.loads(raw)
        assert data["type"] == "exec"
        assert data["command"] == "assess scan /path"
        assert "id" in data

    def test_list_files_message_serializes(self):
        msg = ListFilesMessage(directory="/path", pattern="*.cbl")
        raw = serialize(msg)
        data = json.loads(raw)
        assert data["type"] == "list_files"
        assert data["directory"] == "/path"
        assert data["pattern"] == "*.cbl"

    def test_read_file_message_serializes(self):
        msg = ReadFileMessage(path="/path/file.cbl", lines=[1, 50])
        raw = serialize(msg)
        data = json.loads(raw)
        assert data["type"] == "read_file"
        assert data["path"] == "/path/file.cbl"
        assert data["lines"] == [1, 50]

    def test_ping_message_serializes(self):
        msg = PingMessage()
        raw = serialize(msg)
        data = json.loads(raw)
        assert data["type"] == "ping"

    def test_result_message_serializes(self):
        msg = ResultMessage(id="abc", status="ok", data={"stdout": "hello"})
        raw = serialize(msg)
        data = json.loads(raw)
        assert data["type"] == "result"
        assert data["id"] == "abc"
        assert data["status"] == "ok"
        assert data["data"]["stdout"] == "hello"

    def test_pong_message_serializes(self):
        msg = PongMessage(project_path="/my/project", cli_version="1.0.0")
        raw = serialize(msg)
        data = json.loads(raw)
        assert data["type"] == "pong"
        assert data["project_path"] == "/my/project"
        assert data["cli_version"] == "1.0.0"

    def test_error_message_serializes(self):
        msg = ErrorMessage(error="something went wrong", id="xyz")
        raw = serialize(msg)
        data = json.loads(raw)
        assert data["type"] == "error"
        assert data["error"] == "something went wrong"

    def test_deserialize_valid_json(self):
        raw = '{"type": "exec", "command": "assess scan /path", "id": "123"}'
        data = deserialize(raw)
        assert data["type"] == "exec"
        assert data["command"] == "assess scan /path"

    def test_deserialize_invalid_json(self):
        with pytest.raises(ValueError, match="Invalid JSON"):
            deserialize("not json at all")

    def test_deserialize_missing_type(self):
        with pytest.raises(ValueError, match="missing 'type'"):
            deserialize('{"command": "assess"}')


class TestProtocolConstants:
    """Test protocol constants."""

    def test_output_limit(self):
        assert MAX_OUTPUT_BYTES == 50 * 1024

    def test_message_types(self):
        assert MessageType.EXEC == "exec"
        assert MessageType.LIST_FILES == "list_files"
        assert MessageType.READ_FILE == "read_file"
        assert MessageType.PING == "ping"
        assert MessageType.RESULT == "result"
        assert MessageType.PONG == "pong"


class TestSecuritySandbox:
    """Test path sandboxing."""

    def test_sandbox_allows_relative_path(self, tmp_path):
        (tmp_path / "test.cbl").touch()
        result = sandbox_path("test.cbl", tmp_path)
        assert result == tmp_path / "test.cbl"

    def test_sandbox_allows_nested_path(self, tmp_path):
        subdir = tmp_path / "src"
        subdir.mkdir()
        (subdir / "test.cbl").touch()
        result = sandbox_path("src/test.cbl", tmp_path)
        assert result == subdir / "test.cbl"

    def test_sandbox_blocks_traversal(self, tmp_path):
        with pytest.raises(ValueError, match="outside the project"):
            sandbox_path("../../etc/passwd", tmp_path)

    def test_sandbox_blocks_absolute_escape(self, tmp_path):
        with pytest.raises(ValueError, match="outside the project"):
            sandbox_path("/etc/passwd", tmp_path)

    def test_project_path_validation(self, tmp_path):
        result = validate_project_path(str(tmp_path))
        assert result == tmp_path

    def test_project_path_nonexistent(self):
        with pytest.raises(ValueError, match="does not exist"):
            validate_project_path("/nonexistent/path/12345")

    def test_project_path_file_not_dir(self, tmp_path):
        f = tmp_path / "file.txt"
        f.touch()
        with pytest.raises(ValueError, match="not a directory"):
            validate_project_path(str(f))


class TestSecurityCommandValidation:
    """Test command validation and allowlisting."""

    def test_allowed_subcommands(self, tmp_path):
        for cmd in ALLOWED_SUBCOMMANDS:
            # Should not raise
            validate_command(f"{cmd} dummy_arg", tmp_path)

    def test_disallowed_subcommand(self, tmp_path):
        with pytest.raises(ValueError, match="not allowed"):
            validate_command("rm -rf /", tmp_path)

    def test_shell_metacharacters_blocked(self, tmp_path):
        with pytest.raises(ValueError, match="disallowed characters"):
            validate_command("assess scan /path; rm -rf /", tmp_path)

    def test_pipe_blocked(self, tmp_path):
        with pytest.raises(ValueError, match="disallowed characters"):
            validate_command("assess scan /path | cat", tmp_path)

    def test_backtick_blocked(self, tmp_path):
        with pytest.raises(ValueError, match="disallowed characters"):
            validate_command("assess `whoami`", tmp_path)

    def test_empty_command(self, tmp_path):
        with pytest.raises(ValueError, match="Empty command"):
            validate_command("", tmp_path)

    def test_binary_prefix_stripped(self, tmp_path):
        """Commands starting with binary path should still work."""
        result = validate_command("open-mainframe assess scan", tmp_path)
        assert "assess" in result

    def test_file_paths_sandboxed(self, tmp_path):
        """File path args should be sandboxed within project root."""
        with pytest.raises(ValueError, match="outside the project"):
            validate_command("assess file /etc/passwd", tmp_path)


class TestTokenValidation:
    """Test token comparison."""

    def test_matching_tokens(self):
        assert validate_token("abc123", "abc123") is True

    def test_mismatched_tokens(self):
        assert validate_token("abc123", "xyz789") is False

    def test_empty_tokens(self):
        assert validate_token("", "") is True

    def test_partial_match(self):
        assert validate_token("abc", "abcdef") is False
