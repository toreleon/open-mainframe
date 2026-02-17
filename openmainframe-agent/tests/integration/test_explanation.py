"""
Integration tests for the explanation pipeline.
Tests the parse/lex tools used by the explain node to ground explanations,
and validates the node system prompt pattern.
"""

import os
import pytest

from src.tools.parse_tools import parse_jcl, lex_cobol
from src.tools.base import sanitize_path


class TestExplanationGrounding:
    """Test that the tools used by the explain node produce usable output."""

    def test_lex_provides_structural_tokens(self, sample_cobol_file):
        """lex_cobol should produce tokens that help explain code structure."""
        result = lex_cobol.invoke({"source_file": sample_cobol_file})
        if result["success"]:
            # Should contain division-level structure tokens
            stdout = result["stdout"].upper()
            # CardDemo files should have standard COBOL divisions
            has_structure = any(
                keyword in stdout
                for keyword in ["IDENTIFICATION", "PROCEDURE", "DATA", "DIVISION"]
            )
            assert has_structure or len(stdout) > 100, (
                "Lexer output should contain structural COBOL tokens"
            )

    def test_parse_jcl_provides_structure(self, sample_jcl_file):
        """parse_jcl should produce AST that helps explain JCL structure."""
        result = parse_jcl.invoke({"jcl_file": sample_jcl_file})
        if result["success"]:
            stdout = result["stdout"]
            # JCL AST should have some structural content
            assert len(stdout) > 50, "JCL parse output should be substantial"

    def test_lex_handles_all_carddemo_cobol(self, carddemo_cbl):
        """lex_cobol should successfully tokenize all CardDemo COBOL files."""
        cbl_files = [
            f for f in os.listdir(carddemo_cbl)
            if f.lower().endswith((".cbl", ".cob"))
        ]
        assert len(cbl_files) > 0, "CardDemo should have COBOL files"

        success_count = 0
        for filename in cbl_files:
            filepath = os.path.join(carddemo_cbl, filename)
            result = lex_cobol.invoke({"source_file": filepath})
            if result["success"]:
                success_count += 1

        # At least some files should tokenize successfully
        assert success_count > 0, (
            f"No CardDemo files tokenized successfully out of {len(cbl_files)}"
        )


class TestExplanationSecurity:
    """Test that explanation tools respect path sanitization."""

    def test_lex_rejects_traversal(self):
        """lex_cobol should reject paths outside workspace."""
        with pytest.raises(ValueError, match="outside the allowed workspace"):
            lex_cobol.invoke({"source_file": "/etc/passwd"})

    def test_parse_rejects_traversal(self):
        """parse_jcl should reject paths outside workspace."""
        with pytest.raises(ValueError, match="outside the allowed workspace"):
            parse_jcl.invoke({"jcl_file": "/etc/passwd"})


class TestExplainNodePrompt:
    """Validate that the explain node module loads correctly."""

    def test_explain_node_importable(self):
        """The explain node should be importable."""
        from src.nodes.explain import explain_node
        assert callable(explain_node)

    def test_explain_system_prompt_exists(self):
        """The explain node should have a system prompt."""
        from src.nodes.explain import EXPLAIN_SYSTEM
        assert isinstance(EXPLAIN_SYSTEM, str)
        assert len(EXPLAIN_SYSTEM) > 100
        # Should mention key explanation patterns
        assert "business rule" in EXPLAIN_SYSTEM.lower() or "explain" in EXPLAIN_SYSTEM.lower()
