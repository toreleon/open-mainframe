"""
FastAPI entry point for the OpenMainframe Agent.
Serves the Anthropic agent via AG-UI SSE protocol for CopilotKit.
Accepts local bridge connections via WebSocket at /ws/bridge.
"""

import os
from dotenv import load_dotenv

load_dotenv()

from fastapi import FastAPI, Request, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse, StreamingResponse
import uvicorn

from ag_ui.core.types import RunAgentInput
from ag_ui.encoder import EventEncoder

from src.agent import agent
from src.bridge_client import bridge_manager

app = FastAPI(
    title="Coding Agent",
    description="General-purpose AI coding agent",
    version="2.0.0",
)

_frontend_url = os.getenv("FRONTEND_URL", "")
_allowed_origins = [
    "http://localhost:3000",
    "http://127.0.0.1:3000",
]
if _frontend_url and _frontend_url not in _allowed_origins:
    _allowed_origins.append(_frontend_url)

app.add_middleware(
    CORSMiddleware,
    allow_origins=_allowed_origins,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/health")
async def health():
    """Health check endpoint — includes bridge connection status."""
    bridge_status = bridge_manager.status()
    return JSONResponse(
        content={
            "status": "ok",
            "agent": "coding_agent",
            "version": "2.0.0",
            **bridge_status,
        }
    )


@app.websocket("/ws/bridge")
async def bridge_websocket(ws: WebSocket, token: str = ""):
    """WebSocket endpoint for local bridge daemon connections."""
    import asyncio
    import json as _json

    await ws.accept()
    key = token or "default"
    print(f"Bridge connected (token: {key[:8]}...)")

    conn = await bridge_manager.register(ws, key)

    # Background reader — routes all incoming messages to pending futures
    async def _reader():
        try:
            while True:
                raw = await ws.receive_text()
                try:
                    msg = _json.loads(raw)
                    conn.handle_response(msg)
                except Exception as e:
                    print(f"  Bridge message error: {e}")
        except WebSocketDisconnect:
            pass
        except Exception as e:
            print(f"  Bridge reader error: {e}")

    read_task = asyncio.create_task(_reader())

    # Now that the reader is active, ping to get project info
    try:
        await conn.ping()
        print(f"  Project: {conn.project_path}")
        print(f"  CLI:     {conn.cli_version}")
    except Exception as e:
        print(f"  Bridge ping failed: {e}")

    # Wait for the reader to finish (bridge disconnect)
    try:
        await read_task
    except Exception:
        pass
    finally:
        print(f"Bridge disconnected (token: {key[:8]}...)")
        bridge_manager.unregister(key)


@app.post("/")
async def agent_endpoint(input_data: RunAgentInput, request: Request):
    """AG-UI streaming endpoint — replaces ag_ui_langgraph."""
    encoder = EventEncoder(accept=request.headers.get("accept"))

    async def event_generator():
        async for event in agent.run(input_data):
            yield encoder.encode(event)

    return StreamingResponse(
        event_generator(),
        media_type=encoder.get_content_type(),
    )


def main():
    port = int(os.getenv("PORT", "8123"))
    uvicorn.run("main:app", host="0.0.0.0", port=port, reload=True)


if __name__ == "__main__":
    main()
