/**
 * Thin SSE client for the AG-UI protocol.
 * POST to the Python agent and yield parsed events.
 */

import type { AguiEvent, RunAgentInput } from "./agui-types";

/**
 * Stream AG-UI events from the Python agent.
 *
 * @param agentUrl - Base URL of the Python agent (e.g. "http://localhost:8123")
 * @param input    - RunAgentInput payload (camelCase)
 * @param signal   - Optional AbortSignal for cancellation
 */
export async function* streamAgui(
  agentUrl: string,
  input: RunAgentInput,
  signal?: AbortSignal,
): AsyncGenerator<AguiEvent> {
  let res: Response;
  try {
    res = await fetch(agentUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Accept: "text/event-stream",
      },
      body: JSON.stringify(input),
      signal,
    });
  } catch (err) {
    if (signal?.aborted) throw err;
    throw new Error(
      `Cannot reach agent at ${agentUrl} — is it running? (${err instanceof Error ? err.message : err})`,
    );
  }

  if (!res.ok) {
    let detail = "";
    try {
      detail = await res.text();
    } catch {
      /* ignore */
    }
    throw new Error(
      `Agent returned ${res.status}: ${res.statusText}${detail ? `\n${detail.slice(0, 200)}` : ""}`,
    );
  }

  const reader = res.body?.getReader();
  if (!reader) throw new Error("No response body");

  const decoder = new TextDecoder();
  let buffer = "";

  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;

      buffer += decoder.decode(value, { stream: true });

      // SSE format: "data: {json}\n\n"
      const parts = buffer.split("\n\n");
      // Keep the last (possibly incomplete) chunk in the buffer
      buffer = parts.pop() ?? "";

      for (const part of parts) {
        for (const line of part.split("\n")) {
          if (line.startsWith("data: ")) {
            const jsonStr = line.slice(6).trim();
            if (jsonStr === "[DONE]") return;
            if (!jsonStr) continue;
            try {
              yield JSON.parse(jsonStr) as AguiEvent;
            } catch {
              // Skip malformed JSON
            }
          }
        }
      }
    }
  } finally {
    reader.releaseLock();
  }
}
