"""
Agent state definition.
Extends CopilotKitState for message handling and frontend state sync.
Keep in sync with frontend types in src/lib/types.ts
"""

from typing import TypedDict, Optional

from copilotkit import CopilotKitState


class SourceFile(TypedDict):
    path: str
    type: str  # "cobol", "jcl", "copybook", "bms", "data"
    size_bytes: int
    line_count: int


class AssessmentReport(TypedDict):
    total_files: int
    total_loc: int
    average_complexity: float
    programs: list[dict]
    issues: list[dict]
    recommendations: list[str]
    feature_support: dict[str, float]


class CompilationResult(TypedDict):
    file_path: str
    success: bool
    output: str
    errors: str
    timestamp: str


class ExecutionResult(TypedDict):
    jcl_file: str
    steps: list[dict]
    max_return_code: int
    output: str
    timestamp: str


class AgentState(CopilotKitState):
    """Main agent state. Inherits messages + copilotkit from CopilotKitState."""

    # Project context
    project_path: Optional[str]
    source_files: list[SourceFile]

    # Assessment
    assessment_results: Optional[AssessmentReport]

    # Compilation
    compilation_results: list[CompilationResult]

    # Execution
    execution_results: list[ExecutionResult]

    # Operation tracking
    current_operation: Optional[str]
    operation_progress: float  # 0.0 - 1.0

    # Internal routing (set by capability nodes before routing to tools)
    active_node: Optional[str]
