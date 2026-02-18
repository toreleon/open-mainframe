"use client";

import { CopilotChat } from "@copilotkit/react-ui";
import { useRenderToolCall } from "@copilotkit/react-core";
import { useAgentState } from "@/hooks/useAgentState";
import { useInterruptHandler } from "@/hooks/useInterruptHandler";
import { useConnectionStatus } from "@/hooks/useConnectionStatus";
import { ConnectionStatusBadge } from "@/components/chat/ConnectionStatusBadge";
import { ProgressCard } from "@/components/chat/ProgressCard";

export default function HomeContent() {
  const { state } = useAgentState();
  const connectionStatus = useConnectionStatus();

  const initialMessage =
    "Hello! I'm your AI coding assistant. I can help you with any software engineering task — writing code, debugging, refactoring, running builds and tests, and more.\n\nJust describe what you need!";

  // HITL — render interrupt() approvals inline in chat
  useInterruptHandler();

  // ── Generative UI — render tool calls inline in chat ──

  useRenderToolCall({
    name: "bash",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Running: ${args?.command?.slice(0, 60) || "command"}...`} />;
      return <ProgressCard label={`Ran: ${args?.command?.slice(0, 80) || "command"}`} />;
    },
  });

  useRenderToolCall({
    name: "read_file",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Reading ${args?.file_path || "file"}...`} />;
      return <ProgressCard label={`Read ${args?.file_path || "file"}`} />;
    },
  });

  useRenderToolCall({
    name: "write_file",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Writing ${args?.file_path || "file"}...`} />;
      return <ProgressCard label={`Wrote ${args?.file_path || "file"}`} />;
    },
  });

  useRenderToolCall({
    name: "edit_file",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Editing ${args?.file_path || "file"}...`} />;
      return <ProgressCard label={`Edited ${args?.file_path || "file"}`} />;
    },
  });

  useRenderToolCall({
    name: "list_directory",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Listing ${args?.path || "."}...`} />;
      return <ProgressCard label={`Listed ${args?.path || "."}`} />;
    },
  });

  useRenderToolCall({
    name: "grep",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Searching for "${args?.pattern || "pattern"}"...`} />;
      return <ProgressCard label={`Searched for "${args?.pattern || "pattern"}"`} />;
    },
  });

  useRenderToolCall({
    name: "glob",
    render: ({ status, args }) => {
      if (status === "inProgress")
        return <ProgressCard label={`Finding files: ${args?.pattern || "pattern"}...`} />;
      return <ProgressCard label={`Found files: ${args?.pattern || "pattern"}`} />;
    },
  });

  return (
    <main className="flex flex-col h-screen bg-om-bg">
      {/* Header */}
      <header className="h-14 bg-om-surface border-b border-om-border flex items-center px-6 shrink-0">
        <div className="flex items-center gap-3">
          <span className="text-om-accent font-bold text-sm tracking-wide">
            CODING AGENT
          </span>
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
          instructions="You are a general-purpose AI coding agent. You help users with any software engineering task — writing code, debugging, refactoring, explaining, running builds and tests, and managing files. Be concise, helpful, and proactive."
          labels={{
            title: "Coding Agent",
            initial: initialMessage,
          }}
        />
      </div>
    </main>
  );
}
