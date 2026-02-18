"use client";

import { useCoAgent } from "@copilotkit/react-core";
import type { AgentState } from "@/lib/types";

const INITIAL_STATE: AgentState = {
  project_path: null,
  current_operation: null,
};

export function useAgentState() {
  const coAgent = useCoAgent<AgentState>({
    name: "default",
    initialState: INITIAL_STATE,
  });

  const raw = coAgent.state;
  const state: AgentState = {
    project_path: raw?.project_path ?? INITIAL_STATE.project_path,
    current_operation: raw?.current_operation ?? INITIAL_STATE.current_operation,
  };

  /** Check if any operation is currently running. */
  const isOperating = state.current_operation !== null;

  return {
    ...coAgent,
    state,
    isOperating,
  };
}
