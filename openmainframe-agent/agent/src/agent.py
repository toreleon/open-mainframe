"""
Anthropic agent with AG-UI streaming.

Replaces the LangGraph StateGraph with a single Anthropic tool-use loop.
Emits AG-UI protocol events (SSE) for CopilotKit consumption.
"""

import json
import uuid
from dataclasses import dataclass, field
from typing import AsyncGenerator

from ag_ui.core import events as ev
from ag_ui.core.events import EventType

from .config import get_client, get_model_name
from .prompts import SYSTEM_PROMPT
from .state import AgentState
from .tools import TOOL_REGISTRY, TOOL_SCHEMAS, HITL_TOOLS


@dataclass
class SessionState:
    """Persisted session state for HITL interrupt/resume."""

    messages: list = field(default_factory=list)
    pending_tool_calls: list = field(default_factory=list)
    state: dict = field(default_factory=dict)


class AnthropicAgent:
    """Streaming agent that uses the Anthropic Messages API with tool use."""

    def __init__(self):
        # In-memory session store for HITL interrupt persistence.
        # Keyed by thread_id. Lost on server restart (acceptable for
        # single-user bridge model).
        self.sessions: dict[str, SessionState] = {}

    async def run(self, input_data) -> AsyncGenerator[ev.BaseEvent, None]:
        """Main entry point — yields AG-UI events."""
        thread_id = input_data.thread_id
        run_id = input_data.run_id

        # ── RunStarted ──────────────────────────────────────────────
        yield ev.RunStartedEvent(
            type=EventType.RUN_STARTED,
            thread_id=thread_id,
            run_id=run_id,
        )

        # ── Check for HITL resume ───────────────────────────────────
        forwarded_props = input_data.forwarded_props or {}
        if isinstance(forwarded_props, dict):
            resume = forwarded_props.get("command", {}).get("resume")
        else:
            resume = None

        if resume and thread_id in self.sessions:
            async for event in self._handle_resume(
                thread_id, run_id, resume, input_data
            ):
                yield event
            return

        # ── Convert AG-UI messages → Anthropic format ───────────────
        system_parts, anthropic_messages = self._convert_messages(
            input_data.messages
        )

        system = SYSTEM_PROMPT
        if system_parts:
            system = system + "\n\n" + "\n".join(system_parts)

        # ── Build tool list (backend + frontend) ────────────────────
        tools = list(TOOL_SCHEMAS)
        frontend_tool_names: set[str] = set()
        for tool in input_data.tools or []:
            frontend_tool_names.add(tool.name)
            tools.append({
                "name": tool.name,
                "description": tool.description,
                "input_schema": tool.parameters,
            })

        # ── Agent state ─────────────────────────────────────────────
        agent_state = AgentState()

        # ── Agentic loop ────────────────────────────────────────────
        async for event in self._agentic_loop(
            thread_id=thread_id,
            run_id=run_id,
            system=system,
            messages=anthropic_messages,
            tools=tools,
            frontend_tool_names=frontend_tool_names,
            agent_state=agent_state,
        ):
            yield event

    # ─────────────────────────────────────────────────────────────────
    # Agentic loop
    # ─────────────────────────────────────────────────────────────────

    async def _agentic_loop(
        self,
        *,
        thread_id: str,
        run_id: str,
        system: str,
        messages: list[dict],
        tools: list[dict],
        frontend_tool_names: set[str],
        agent_state: AgentState,
    ) -> AsyncGenerator[ev.BaseEvent, None]:
        client = get_client()
        model = get_model_name()

        while True:
            message_id = f"msg_{uuid.uuid4().hex[:12]}"
            text_started = False

            # Track content blocks by stream index
            block_types: dict[int, str] = {}
            tool_call_ids: dict[int, str] = {}
            tool_call_names: dict[int, str] = {}
            collected_tool_calls: list[dict] = []

            # ── Stream LLM response ────────────────────────────────
            try:
                async with client.messages.stream(
                    model=model,
                    system=system,
                    messages=messages,
                    tools=tools if tools else [],
                    max_tokens=4096,
                ) as stream:
                    async for event in stream:
                        if event.type == "content_block_start":
                            idx = event.index
                            block = event.content_block

                            if block.type == "text":
                                block_types[idx] = "text"
                                if not text_started:
                                    text_started = True
                                    yield ev.TextMessageStartEvent(
                                        type=EventType.TEXT_MESSAGE_START,
                                        message_id=message_id,
                                        role="assistant",
                                    )

                            elif block.type == "tool_use":
                                block_types[idx] = "tool_use"
                                tool_call_ids[idx] = block.id
                                tool_call_names[idx] = block.name
                                yield ev.ToolCallStartEvent(
                                    type=EventType.TOOL_CALL_START,
                                    tool_call_id=block.id,
                                    tool_call_name=block.name,
                                    parent_message_id=message_id,
                                )

                        elif event.type == "content_block_delta":
                            idx = event.index
                            if block_types.get(idx) == "text":
                                yield ev.TextMessageContentEvent(
                                    type=EventType.TEXT_MESSAGE_CONTENT,
                                    message_id=message_id,
                                    delta=event.delta.text,
                                )
                            elif block_types.get(idx) == "tool_use":
                                yield ev.ToolCallArgsEvent(
                                    type=EventType.TOOL_CALL_ARGS,
                                    tool_call_id=tool_call_ids[idx],
                                    delta=event.delta.partial_json,
                                )

                        elif event.type == "content_block_stop":
                            idx = event.index
                            if block_types.get(idx) == "tool_use":
                                yield ev.ToolCallEndEvent(
                                    type=EventType.TOOL_CALL_END,
                                    tool_call_id=tool_call_ids[idx],
                                )

                    # Stream consumed — get final message
                    final_message = stream.get_final_message()

            except Exception as exc:
                # Close any open text message before error
                if text_started:
                    yield ev.TextMessageEndEvent(
                        type=EventType.TEXT_MESSAGE_END,
                        message_id=message_id,
                    )
                yield ev.RunErrorEvent(
                    type=EventType.RUN_ERROR,
                    message=str(exc),
                )
                return

            # ── Close text message ──────────────────────────────────
            if text_started:
                yield ev.TextMessageEndEvent(
                    type=EventType.TEXT_MESSAGE_END,
                    message_id=message_id,
                )

            # ── Collect tool calls from final message ───────────────
            for block in final_message.content:
                if block.type == "tool_use":
                    collected_tool_calls.append({
                        "id": block.id,
                        "name": block.name,
                        "input": block.input,
                    })

            # ── No tool calls → done ────────────────────────────────
            if not collected_tool_calls:
                # Emit state snapshot and finish
                yield ev.StateSnapshotEvent(
                    type=EventType.STATE_SNAPSHOT,
                    snapshot=agent_state.to_dict(),
                )
                yield ev.RunFinishedEvent(
                    type=EventType.RUN_FINISHED,
                    thread_id=thread_id,
                    run_id=run_id,
                )
                return

            # ── Frontend tool calls → emit and stop ─────────────────
            has_frontend_tool = any(
                tc["name"] in frontend_tool_names
                for tc in collected_tool_calls
            )
            if has_frontend_tool:
                yield ev.StateSnapshotEvent(
                    type=EventType.STATE_SNAPSHOT,
                    snapshot=agent_state.to_dict(),
                )
                yield ev.RunFinishedEvent(
                    type=EventType.RUN_FINISHED,
                    thread_id=thread_id,
                    run_id=run_id,
                )
                return

            # ── Check for HITL tools ────────────────────────────────
            for tc in collected_tool_calls:
                needs_hitl = False

                if tc["name"] in HITL_TOOLS:
                    needs_hitl = True

                if needs_hitl:
                    # Add assistant message to history before saving
                    messages.append({
                        "role": "assistant",
                        "content": final_message.content,
                    })

                    # Save session for resume
                    self.sessions[thread_id] = SessionState(
                        messages=messages,
                        pending_tool_calls=collected_tool_calls,
                        state=agent_state.to_dict(),
                    )

                    file_arg = (
                        tc["input"].get("file_path")
                        or tc["input"].get("command")
                        or tc["input"].get("path")
                        or "unknown"
                    )

                    yield ev.CustomEvent(
                        type=EventType.CUSTOM,
                        name="on_interrupt",
                        value={
                            "action": f"Execute {tc['name']}",
                            "file": file_arg,
                            "description": f"Run {tc['name']} on {file_arg}",
                        },
                    )

                    yield ev.StateSnapshotEvent(
                        type=EventType.STATE_SNAPSHOT,
                        snapshot=agent_state.to_dict(),
                    )
                    yield ev.RunFinishedEvent(
                        type=EventType.RUN_FINISHED,
                        thread_id=thread_id,
                        run_id=run_id,
                    )
                    return

            # ── Execute backend tools ───────────────────────────────
            # Add assistant message to conversation history
            messages.append({
                "role": "assistant",
                "content": final_message.content,
            })

            tool_results = []
            for tc in collected_tool_calls:
                tool_name = tc["name"]
                tool_input = tc["input"]
                tool_call_id = tc["id"]

                agent_state.current_operation = tool_name

                # Execute
                if tool_name in TOOL_REGISTRY:
                    try:
                        result = await TOOL_REGISTRY[tool_name](**tool_input)
                        result_str = (
                            json.dumps(result)
                            if isinstance(result, dict)
                            else str(result)
                        )
                    except Exception as exc:
                        result_str = json.dumps({
                            "error": str(exc),
                            "tool": tool_name,
                        })
                else:
                    result_str = json.dumps({
                        "error": f"Unknown tool: {tool_name}",
                    })

                # Emit tool result event
                result_msg_id = f"res_{uuid.uuid4().hex[:12]}"
                yield ev.ToolCallResultEvent(
                    type=EventType.TOOL_CALL_RESULT,
                    tool_call_id=tool_call_id,
                    message_id=result_msg_id,
                    content=result_str,
                    role="tool",
                )

                tool_results.append({
                    "type": "tool_result",
                    "tool_use_id": tool_call_id,
                    "content": result_str,
                })

            agent_state.current_operation = None

            # Add tool results as user message and continue loop
            messages.append({"role": "user", "content": tool_results})

    # ─────────────────────────────────────────────────────────────────
    # HITL resume handler
    # ─────────────────────────────────────────────────────────────────

    async def _handle_resume(
        self,
        thread_id: str,
        run_id: str,
        resume_value,
        input_data,
    ) -> AsyncGenerator[ev.BaseEvent, None]:
        """Handle HITL resume — execute approved tool or cancel."""
        session = self.sessions.pop(thread_id)
        messages = session.messages
        pending = session.pending_tool_calls
        agent_state = AgentState(**session.state)

        # Parse resume value
        if isinstance(resume_value, str):
            try:
                approval = json.loads(resume_value)
            except json.JSONDecodeError:
                approval = {"approved": False, "reason": resume_value}
        elif isinstance(resume_value, dict):
            approval = resume_value
        else:
            approval = {"approved": False, "reason": "Invalid resume value"}

        if not approval.get("approved", False):
            # Rejected — emit cancellation message
            reason = approval.get("reason", "User declined execution.")
            cancel_id = f"msg_{uuid.uuid4().hex[:12]}"

            yield ev.TextMessageStartEvent(
                type=EventType.TEXT_MESSAGE_START,
                message_id=cancel_id,
                role="assistant",
            )
            yield ev.TextMessageContentEvent(
                type=EventType.TEXT_MESSAGE_CONTENT,
                message_id=cancel_id,
                delta=f"Execution cancelled: {reason}",
            )
            yield ev.TextMessageEndEvent(
                type=EventType.TEXT_MESSAGE_END,
                message_id=cancel_id,
            )

            yield ev.StateSnapshotEvent(
                type=EventType.STATE_SNAPSHOT,
                snapshot=agent_state.to_dict(),
            )
            yield ev.RunFinishedEvent(
                type=EventType.RUN_FINISHED,
                thread_id=thread_id,
                run_id=run_id,
            )
            return

        # Approved — execute all pending tool calls
        tool_results = []
        for tc in pending:
            tool_name = tc["name"]
            tool_input = tc["input"]
            tool_call_id = tc["id"]

            if tool_name in TOOL_REGISTRY:
                try:
                    result = await TOOL_REGISTRY[tool_name](**tool_input)
                    result_str = (
                        json.dumps(result)
                        if isinstance(result, dict)
                        else str(result)
                    )
                except Exception as exc:
                    result_str = json.dumps({
                        "error": str(exc),
                        "tool": tool_name,
                    })
            else:
                result_str = json.dumps({
                    "error": f"Unknown tool: {tool_name}",
                })

            result_msg_id = f"res_{uuid.uuid4().hex[:12]}"
            yield ev.ToolCallResultEvent(
                type=EventType.TOOL_CALL_RESULT,
                tool_call_id=tool_call_id,
                message_id=result_msg_id,
                content=result_str,
                role="tool",
            )

            tool_results.append({
                "type": "tool_result",
                "tool_use_id": tool_call_id,
                "content": result_str,
            })

        # Feed results back and continue the agentic loop
        messages.append({"role": "user", "content": tool_results})

        # Build tool list (backend + frontend)
        tools = list(TOOL_SCHEMAS)
        frontend_tool_names: set[str] = set()
        for tool in input_data.tools or []:
            frontend_tool_names.add(tool.name)
            tools.append({
                "name": tool.name,
                "description": tool.description,
                "input_schema": tool.parameters,
            })

        async for event in self._agentic_loop(
            thread_id=thread_id,
            run_id=run_id,
            system=SYSTEM_PROMPT,
            messages=messages,
            tools=tools,
            frontend_tool_names=frontend_tool_names,
            agent_state=agent_state,
        ):
            yield event

    # ─────────────────────────────────────────────────────────────────
    # Message conversion
    # ─────────────────────────────────────────────────────────────────

    @staticmethod
    def _convert_messages(
        ag_ui_messages,
    ) -> tuple[list[str], list[dict]]:
        """Convert AG-UI messages to Anthropic format.

        Returns:
            (system_parts, anthropic_messages) where system_parts are
            extracted system/developer messages to prepend to the system
            prompt, and anthropic_messages are user/assistant/tool messages
            in Anthropic's format.
        """
        system_parts: list[str] = []
        anthropic_msgs: list[dict] = []

        i = 0
        msgs = list(ag_ui_messages or [])

        while i < len(msgs):
            msg = msgs[i]
            role = msg.role

            if role in ("system", "developer"):
                system_parts.append(msg.content)
                i += 1
                continue

            if role == "user":
                content = msg.content
                if isinstance(content, str):
                    anthropic_msgs.append({
                        "role": "user",
                        "content": content,
                    })
                else:
                    # List of InputContent (text, image, etc.)
                    blocks = []
                    for part in content:
                        if hasattr(part, "text"):
                            blocks.append({
                                "type": "text",
                                "text": part.text,
                            })
                    anthropic_msgs.append({
                        "role": "user",
                        "content": blocks if blocks else "",
                    })
                i += 1
                continue

            if role == "assistant":
                content_blocks = []
                if hasattr(msg, "content") and msg.content:
                    content_blocks.append({
                        "type": "text",
                        "text": msg.content,
                    })
                if hasattr(msg, "tool_calls") and msg.tool_calls:
                    for tc in msg.tool_calls:
                        args = tc.function.arguments
                        content_blocks.append({
                            "type": "tool_use",
                            "id": tc.id,
                            "name": tc.function.name,
                            "input": (
                                json.loads(args)
                                if isinstance(args, str)
                                else args
                            ),
                        })
                anthropic_msgs.append({
                    "role": "assistant",
                    "content": content_blocks if content_blocks else "",
                })
                i += 1
                continue

            if role == "tool":
                # Collect consecutive tool messages into one user message
                tool_results = []
                while i < len(msgs) and msgs[i].role == "tool":
                    tool_msg = msgs[i]
                    tool_results.append({
                        "type": "tool_result",
                        "tool_use_id": tool_msg.tool_call_id,
                        "content": tool_msg.content,
                    })
                    i += 1
                anthropic_msgs.append({
                    "role": "user",
                    "content": tool_results,
                })
                continue

            # Skip unknown roles (activity, reasoning, etc.)
            i += 1

        # ── Merge consecutive same-role messages ────────────────────
        merged: list[dict] = []
        for msg in anthropic_msgs:
            if merged and merged[-1]["role"] == msg["role"]:
                prev = merged[-1]
                # Normalize both to lists
                prev_content = prev["content"]
                msg_content = msg["content"]

                if isinstance(prev_content, str):
                    prev_content = [{"type": "text", "text": prev_content}]
                if isinstance(msg_content, str):
                    msg_content = [{"type": "text", "text": msg_content}]
                if isinstance(prev_content, list) and isinstance(
                    msg_content, list
                ):
                    prev["content"] = prev_content + msg_content
                else:
                    merged.append(msg)
            else:
                merged.append(msg)

        return system_parts, merged


# Module-level singleton
agent = AnthropicAgent()
