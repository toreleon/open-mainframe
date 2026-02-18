"use client";

import { useState } from "react";
import { CopilotChat } from "@copilotkit/react-ui";
import { useRenderToolCall } from "@copilotkit/react-core";
import { useAgentState } from "@/hooks/useAgentState";
import { useInterruptHandler } from "@/hooks/useInterruptHandler";
import { useConnectionStatus } from "@/hooks/useConnectionStatus";
import { ConnectionStatusBadge } from "@/components/chat/ConnectionStatusBadge";
import { AssessmentCard } from "@/components/chat/AssessmentCard";
import { CompilerOutputCard } from "@/components/chat/CompilerOutputCard";
import { ProgressCard } from "@/components/chat/ProgressCard";
import { ExplanationCard } from "@/components/chat/ExplanationCard";
import { JobResultCard } from "@/components/chat/JobResultCard";
import { AssessmentDrawer } from "@/components/drawers/AssessmentDrawer";
import { JobTimelineDrawer } from "@/components/drawers/JobTimelineDrawer";
import type { ExecutionResult } from "@/lib/types";

export default function HomeContent() {
  const { state } = useAgentState();
  const connectionStatus = useConnectionStatus();

  // Drawer state
  const [assessmentDrawerOpen, setAssessmentDrawerOpen] = useState(false);
  const [jobDrawerOpen, setJobDrawerOpen] = useState(false);
  const [jobDrawerResult, setJobDrawerResult] = useState<ExecutionResult | null>(null);

  // Dynamic initial message based on connection status
  const initialMessage = connectionStatus.connected
    ? `Connected to **${connectionStatus.projectPath || "your project"}**${connectionStatus.cliVersion ? ` (CLI v${connectionStatus.cliVersion})` : ""}. How can I help you today?\n\nI can help you:\n- **Assess** your COBOL programs for modernization readiness\n- **Compile** and syntax-check COBOL source files\n- **Execute** JCL jobs and COBOL programs\n- **Explain** code structure and logic\n\nJust ask!`
    : "Hello! I'm your mainframe modernization assistant. I can help you assess, compile, execute, and explain COBOL and JCL code.\n\nTo get started, connect your local environment by running:\n```\npip install openmainframe-bridge\nopenmainframe bridge connect --project ~/your-cobol-project\n```\n\nYour source code stays on your machine — only command results flow through the agent.";

  // HITL — render LangGraph interrupt() approvals inline in chat
  useInterruptHandler();

  // ── Generative UI — render tool calls inline in chat ──

  // Assessment tools
  useRenderToolCall({
    name: "assess_scan",
    render: ({ status }) => {
      if (status === "inProgress") return <ProgressCard label="Scanning codebase..." />;
      return (
        <AssessmentCard
          report={state.assessment_results}
          onViewFullReport={() => setAssessmentDrawerOpen(true)}
        />
      );
    },
  });

  useRenderToolCall({
    name: "assess_file",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Assessing ${args?.path || "file"}...`} />;
      return (
        <AssessmentCard
          report={state.assessment_results}
          onViewFullReport={() => setAssessmentDrawerOpen(true)}
        />
      );
    },
  });

  // Compilation tools
  useRenderToolCall({
    name: "compile_cobol",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Compiling ${args?.source_file || ""}...`} />;
      const latest = state.compilation_results[state.compilation_results.length - 1];
      return (
        <CompilerOutputCard
          filePath={latest?.file_path || args?.source_file}
          success={latest?.success}
          errorCount={latest?.errors?.length}
          warningCount={latest?.warnings?.length}
          errors={latest?.errors}
          warnings={latest?.warnings}
        />
      );
    },
  });

  useRenderToolCall({
    name: "check_cobol",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Checking ${args?.source_file || ""}...`} />;
      const latest = state.compilation_results[state.compilation_results.length - 1];
      return (
        <CompilerOutputCard
          filePath={latest?.file_path || args?.source_file}
          success={latest?.success}
          errorCount={latest?.errors?.length}
          warningCount={latest?.warnings?.length}
          errors={latest?.errors}
          warnings={latest?.warnings}
        />
      );
    },
  });

  // Explanation/parse tools
  useRenderToolCall({
    name: "lex_cobol",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Analyzing ${args?.source_file || ""}...`} />;
      return <ExplanationCard filePath={args?.source_file} language="cobol" />;
    },
  });

  useRenderToolCall({
    name: "parse_jcl",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Parsing ${args?.jcl_file || ""}...`} />;
      return <ExplanationCard filePath={args?.jcl_file} language="jcl" />;
    },
  });

  // Execution tools — show actual results
  useRenderToolCall({
    name: "run_jcl",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Running ${args?.jcl_file || ""}...`} />;
      const latest = state.execution_results.length > 0
        ? state.execution_results[state.execution_results.length - 1]
        : null;
      return (
        <JobResultCard
          result={latest}
          jclFile={args?.jcl_file}
        />
      );
    },
  });

  useRenderToolCall({
    name: "interpret_cobol",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Interpreting ${args?.source_file || ""}...`} />;
      const latest = state.execution_results.length > 0
        ? state.execution_results[state.execution_results.length - 1]
        : null;
      return (
        <JobResultCard
          result={latest}
          jclFile={args?.source_file}
        />
      );
    },
  });

  // Dataset tools
  useRenderToolCall({
    name: "list_catalog",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Listing catalog ${args?.pattern || "*"}...`} />;
      return <ProgressCard label={`Catalog listing complete (${args?.pattern || "*"})`} />;
    },
  });

  useRenderToolCall({
    name: "idcams_command",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label="Executing IDCAMS command..." />;
      return <ProgressCard label="IDCAMS command complete" />;
    },
  });

  return (
    <main className="flex flex-col h-screen bg-om-bg">
      {/* Minimal header */}
      <header className="h-14 bg-om-surface border-b border-om-border flex items-center px-6 shrink-0">
        <div className="flex items-center gap-3">
          <span className="text-om-accent font-bold text-sm tracking-wide">
            OPENMAINFRAME
          </span>
          <span className="text-om-muted text-xs">|</span>
          <span className="text-om-muted text-xs">Agent</span>
        </div>
        <div className="flex-1" />
        <ConnectionStatusBadge
          connected={connectionStatus.connected}
          projectPath={connectionStatus.projectPath}
          loading={connectionStatus.loading}
        />
      </header>

      {/* Full-screen chat area */}
      <div className="chat-container flex-1 min-h-0">
        <CopilotChat
          className="h-full"
          instructions="You are the OpenMainframe Agent, an AI-powered mainframe modernization assistant. You help users assess, compile, execute, and explain COBOL and JCL code. The user's code stays on their local machine — you operate through a local bridge connection. Be concise, helpful, and proactive."
          labels={{
            title: "OpenMainframe Agent",
            initial: initialMessage,
          }}
        />
      </div>

      {/* Drawers (progressive disclosure) */}
      {state.assessment_results && (
        <AssessmentDrawer
          open={assessmentDrawerOpen}
          onClose={() => setAssessmentDrawerOpen(false)}
          report={state.assessment_results}
        />
      )}
      {jobDrawerResult && (
        <JobTimelineDrawer
          open={jobDrawerOpen}
          onClose={() => {
            setJobDrawerOpen(false);
            setJobDrawerResult(null);
          }}
          result={jobDrawerResult}
        />
      )}
    </main>
  );
}
