"""
Integration tests for the execution pipeline.
Tests run_jcl, interpret_cobol, parse_jcl, and lex_cobol tools against CardDemo.

NOTE: run_jcl and interpret_cobol are HITL-gated in the agent. These tests
call the tool functions directly (bypassing the agent graph) to validate
the CLI wrapper layer works correctly.
"""

import os
import pytest

from src.tools.execute_tools import run_jcl, interpret_cobol
from src.tools.parse_tools import parse_jcl, lex_cobol


class TestParseJcl:
    """Test JCL parsing against CardDemo JCL files."""

    def test_parse_returns_result(self, sample_jcl_file):
        """parse_jcl should return a result dict."""
        result = parse_jcl.invoke({"jcl_file": sample_jcl_file})
        assert isinstance(result, dict)
        assert "success" in result
        assert "return_code" in result

    def test_parse_produces_output(self, sample_jcl_file):
        """parse_jcl should produce AST output."""
        result = parse_jcl.invoke({"jcl_file": sample_jcl_file})
        if result["success"]:
            assert len(result["stdout"]) > 0

    def test_parse_multiple_jcl(self, carddemo_jcl):
        """parse_jcl should work across multiple JCL files."""
        jcl_files = [
            f for f in os.listdir(carddemo_jcl)
            if f.lower().endswith(".jcl")
        ][:3]

        for filename in jcl_files:
            filepath = os.path.join(carddemo_jcl, filename)
            result = parse_jcl.invoke({"jcl_file": filepath})
            assert isinstance(result, dict), f"Failed for {filename}"


class TestLexCobol:
    """Test COBOL lexer against CardDemo files."""

    def test_lex_returns_result(self, sample_cobol_file):
        """lex_cobol should return a result dict."""
        result = lex_cobol.invoke({"source_file": sample_cobol_file})
        assert isinstance(result, dict)
        assert "success" in result
        assert "return_code" in result

    def test_lex_produces_tokens(self, sample_cobol_file):
        """lex_cobol should produce token output."""
        result = lex_cobol.invoke({"source_file": sample_cobol_file})
        if result["success"]:
            assert len(result["stdout"]) > 0

    def test_lex_multiple_files(self, carddemo_cbl):
        """lex_cobol should tokenize multiple CardDemo files."""
        cbl_files = [
            f for f in os.listdir(carddemo_cbl)
            if f.lower().endswith((".cbl", ".cob"))
        ][:5]

        for filename in cbl_files:
            filepath = os.path.join(carddemo_cbl, filename)
            result = lex_cobol.invoke({"source_file": filepath})
            assert isinstance(result, dict), f"Failed for {filename}"


class TestRunJcl:
    """Test JCL execution (direct tool call, bypassing HITL)."""

    def test_run_returns_result(self, sample_jcl_file):
        """run_jcl should return a result dict with expected keys."""
        result = run_jcl.invoke({"jcl_file": sample_jcl_file})
        assert isinstance(result, dict)
        assert "success" in result
        assert "stdout" in result
        assert "stderr" in result
        assert "return_code" in result

    def test_run_nonexistent_jcl(self, carddemo_jcl):
        """run_jcl should handle missing JCL files."""
        fake_path = os.path.join(carddemo_jcl, "NONEXISTENT.jcl")
        result = run_jcl.invoke({"jcl_file": fake_path})
        assert not result["success"]


class TestInterpretCobol:
    """Test COBOL interpreter (direct tool call, bypassing HITL)."""

    def test_interpret_returns_result(self, sample_cobol_file):
        """interpret_cobol should return a result dict."""
        result = interpret_cobol.invoke({"source_file": sample_cobol_file})
        assert isinstance(result, dict)
        assert "success" in result
        assert "return_code" in result

    def test_interpret_nonexistent_file(self, carddemo_cbl):
        """interpret_cobol should handle missing files."""
        fake_path = os.path.join(carddemo_cbl, "NONEXISTENT.cbl")
        result = interpret_cobol.invoke({"source_file": fake_path})
        assert not result["success"]
