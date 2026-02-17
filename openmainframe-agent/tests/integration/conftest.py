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
