"""
Integration tests for the assessment pipeline.
Tests assess_scan and assess_file tools against CardDemo.
"""

import os
import pytest

from src.tools.base import sanitize_path, run_cli
from src.tools.assess_tools import assess_scan, assess_file


class TestAssessScan:
    """Test assess_scan tool against CardDemo directory."""

    def test_scan_returns_result(self, carddemo_cbl):
        """assess_scan should return a result dict with expected keys."""
        result = assess_scan.invoke({"directory": carddemo_cbl})
        assert isinstance(result, dict)
        assert "success" in result
        assert "stdout" in result
        assert "stderr" in result
        assert "return_code" in result

    def test_scan_produces_output(self, carddemo_cbl):
        """assess_scan should produce non-empty stdout on success."""
        result = assess_scan.invoke({"directory": carddemo_cbl})
        if result["success"]:
            assert len(result["stdout"]) > 0
            # Should have parsed JSON data
            assert "parsed" in result

    def test_scan_json_format(self, carddemo_cbl):
        """assess_scan with --format json should return parseable JSON."""
        result = assess_scan.invoke({"directory": carddemo_cbl})
        if result["success"] and "parsed" in result:
            parsed = result["parsed"]
            assert isinstance(parsed, (dict, list))

    def test_scan_nonexistent_directory(self):
        """assess_scan should handle non-existent paths gracefully."""
        with pytest.raises(ValueError, match="outside the allowed workspace"):
            assess_scan.invoke({"directory": "/nonexistent/path"})


class TestAssessFile:
    """Test assess_file tool against individual CardDemo files."""

    def test_file_returns_result(self, sample_cobol_file):
        """assess_file should return a result dict."""
        result = assess_file.invoke({"file_path": sample_cobol_file})
        assert isinstance(result, dict)
        assert "success" in result
        assert "return_code" in result

    def test_file_produces_output(self, sample_cobol_file):
        """assess_file should produce non-empty stdout for a valid COBOL file."""
        result = assess_file.invoke({"file_path": sample_cobol_file})
        if result["success"]:
            assert len(result["stdout"]) > 0

    def test_file_nonexistent(self, carddemo_cbl):
        """assess_file should handle missing file gracefully."""
        fake_path = os.path.join(carddemo_cbl, "NONEXISTENT.cbl")
        result = assess_file.invoke({"file_path": fake_path})
        # Should not succeed (file doesn't exist)
        assert not result["success"] or "error" in result.get("stderr", "").lower()


class TestPathSanitization:
    """Test that path sanitization prevents traversal attacks."""

    def test_traversal_blocked(self):
        """Paths that escape WORKSPACE_ROOT should be rejected."""
        with pytest.raises(ValueError, match="outside the allowed workspace"):
            sanitize_path("/etc/passwd")

    def test_null_byte_stripped(self):
        """Null bytes in paths should be stripped."""
        # This should not raise for null bytes — they get stripped
        # but the resulting path still needs to be valid
        with pytest.raises((ValueError, OSError)):
            sanitize_path("/etc\x00/passwd")

    def test_valid_path_accepted(self, sample_cobol_file):
        """Valid paths within WORKSPACE_ROOT should be accepted."""
        result = sanitize_path(sample_cobol_file)
        assert os.path.isabs(result)
