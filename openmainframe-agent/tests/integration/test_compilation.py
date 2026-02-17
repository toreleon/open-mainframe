"""
Integration tests for the compilation pipeline.
Tests compile_cobol and check_cobol tools against CardDemo.
"""

import os
import pytest

from src.tools.compile_tools import compile_cobol, check_cobol


class TestCompileCobol:
    """Test compile_cobol tool against CardDemo files."""

    def test_compile_returns_result(self, sample_cobol_file):
        """compile_cobol should return a result dict with expected keys."""
        result = compile_cobol.invoke({"source_file": sample_cobol_file})
        assert isinstance(result, dict)
        assert "success" in result
        assert "stdout" in result
        assert "stderr" in result
        assert "return_code" in result

    def test_compile_produces_output(self, sample_cobol_file):
        """compile_cobol should produce output (success or error messages)."""
        result = compile_cobol.invoke({"source_file": sample_cobol_file})
        # Whether success or failure, there should be some output
        has_output = len(result["stdout"]) > 0 or len(result["stderr"]) > 0
        assert has_output

    def test_compile_nonexistent_file(self, carddemo_cbl):
        """compile_cobol should handle non-existent files."""
        fake_path = os.path.join(carddemo_cbl, "NONEXISTENT.cbl")
        result = compile_cobol.invoke({"source_file": fake_path})
        assert not result["success"]

    def test_compile_path_traversal_blocked(self):
        """compile_cobol should reject paths outside workspace."""
        with pytest.raises(ValueError, match="outside the allowed workspace"):
            compile_cobol.invoke({"source_file": "/etc/passwd"})


class TestCheckCobol:
    """Test check_cobol syntax checker against CardDemo files."""

    def test_check_returns_result(self, sample_cobol_file):
        """check_cobol should return a result dict."""
        result = check_cobol.invoke({"source_file": sample_cobol_file})
        assert isinstance(result, dict)
        assert "success" in result
        assert "return_code" in result

    def test_check_produces_output(self, sample_cobol_file):
        """check_cobol should produce some output."""
        result = check_cobol.invoke({"source_file": sample_cobol_file})
        has_output = len(result["stdout"]) > 0 or len(result["stderr"]) > 0
        assert has_output

    def test_check_multiple_files(self, carddemo_cbl):
        """check_cobol should work across multiple CardDemo files."""
        cbl_files = [
            f for f in os.listdir(carddemo_cbl)
            if f.lower().endswith((".cbl", ".cob"))
        ][:5]  # Test first 5 files

        for filename in cbl_files:
            filepath = os.path.join(carddemo_cbl, filename)
            result = check_cobol.invoke({"source_file": filepath})
            assert isinstance(result, dict), f"Failed for {filename}"
            assert "success" in result, f"Missing 'success' key for {filename}"
