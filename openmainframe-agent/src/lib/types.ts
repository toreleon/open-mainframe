/**
 * Shared TypeScript types matching the Python AgentState.
 * Keep in sync with agent/src/state.py
 */

export interface AgentState {
  project_path: string | null;
  current_operation: string | null;
}
