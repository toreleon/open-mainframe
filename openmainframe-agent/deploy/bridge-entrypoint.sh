#!/usr/bin/env bash
# Entrypoint for the CardDemo bridge container.
# Verifies the CLI binary is available, then starts the bridge daemon.
set -euo pipefail

echo "=== OpenMainframe CardDemo Bridge ==="

# Verify CLI binary
if ! open-mainframe --version; then
    echo "ERROR: open-mainframe binary not found or not working" >&2
    exit 1
fi

echo "Bridge server: ${BRIDGE_SERVER_URL}"
echo ""

exec python /bridge/bridge.py \
    --server "${BRIDGE_SERVER_URL}" \
    --project /workspace/aws-mainframe-modernization-carddemo \
    --cli /usr/local/bin/open-mainframe \
    --mode "${BRIDGE_MODE:-general}"
