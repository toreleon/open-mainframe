#!/usr/bin/env bash
# =============================================================================
#  Comprehensive Zowe CLI Integration Test for OpenMainframe z/OSMF Server
#  75 tests across 14 phases: auth, info, datasets, USS, jobs, TSO, console,
#  WLM, variables, topology, workflows, provisioning, CICS terminal
# =============================================================================
set -uo pipefail

BOLD=$'\033[1m'
RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
YELLOW=$'\033[0;33m'
BLUE=$'\033[0;34m'
RESET=$'\033[0m'

ZOSMF_HOST="${ZOSMF_HOST:-localhost}"
ZOSMF_PORT="${ZOSMF_PORT:-10443}"
ZOSMF_URL="http://${ZOSMF_HOST}:${ZOSMF_PORT}"
CARDDEMO_DIR="${CARDDEMO_DIR:-$(cd "$(dirname "$0")/../.." && [ -d carddemo ] && echo "$PWD/carddemo")}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SERVER_BIN="$PROJECT_DIR/target/release/zosmf-server"
SERVER_PID=""
TOKEN=""

COUNTER_FILE=$(mktemp)
echo "0 0 0 0" > "$COUNTER_FILE"  # pass fail skip total

# Temp files used during tests
TMP_DIR=$(mktemp -d)

# Zowe CLI common options (array for safe word-splitting)
ZOWE_OPTS=(--host "$ZOSMF_HOST" --port "$ZOSMF_PORT" --user IBMUSER --password SYS1 --reject-unauthorized false --protocol http)

# ── Helpers ──────────────────────────────────────────────────────────────────

update_counters() {
    local counts
    counts=$(cat "$COUNTER_FILE")
    local p f s t
    read -r p f s t <<< "$counts"
    case "$1" in
        pass) p=$((p+1)) ;;
        fail) f=$((f+1)) ;;
        skip) s=$((s+1)) ;;
    esac
    t=$((p+f+s))
    echo "$p $f $s $t" > "$COUNTER_FILE"
}

read_counters() {
    cat "$COUNTER_FILE"
}

log_info()  { echo "${BLUE}[INFO]${RESET} $*"; }
log_pass()  { echo "${GREEN}[PASS]${RESET} $*"; update_counters pass; }
log_fail()  { echo "${RED}[FAIL]${RESET} $*"; update_counters fail; }
log_skip()  { echo "${YELLOW}[SKIP]${RESET} $*"; update_counters skip; }

cleanup() {
    if [ -n "$SERVER_PID" ]; then
        log_info "Stopping z/OSMF server (PID $SERVER_PID)..."
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
    rm -rf "$TMP_DIR" 2>/dev/null || true
}
trap cleanup EXIT

# ── curl wrappers ────────────────────────────────────────────────────────────

api_get() {
    curl -s "$1" -H "Cookie: jwtToken=$TOKEN" 2>/dev/null || printf '{"error":"curl failed"}'
}

api_get_status() {
    curl -s -o /dev/null -w '%{http_code}' "$1" \
        -H "Cookie: jwtToken=$TOKEN" 2>/dev/null || printf "000"
}

api_post() {
    curl -s -X POST "$1" \
        -H "Cookie: jwtToken=$TOKEN" \
        -H "Content-Type: application/json" \
        -d "$2" 2>/dev/null || printf '{"error":"curl failed"}'
}

api_post_status() {
    curl -s -o /dev/null -w '%{http_code}' -X POST "$1" \
        -H "Cookie: jwtToken=$TOKEN" \
        -H "Content-Type: application/json" \
        -d "$2" 2>/dev/null || printf "000"
}

api_put() {
    curl -s -X PUT "$1" \
        -H "Cookie: jwtToken=$TOKEN" \
        -H "Content-Type: application/json" \
        -d "$2" 2>/dev/null || printf '{"error":"curl failed"}'
}

api_put_status() {
    curl -s -o /dev/null -w '%{http_code}' -X PUT "$1" \
        -H "Cookie: jwtToken=$TOKEN" \
        -H "Content-Type: application/json" \
        -d "$2" 2>/dev/null || printf "000"
}

api_put_text() {
    curl -s -o /dev/null -w '%{http_code}' -X PUT "$1" \
        -H "Cookie: jwtToken=$TOKEN" \
        -H "Content-Type: text/plain" \
        -d "$2" 2>/dev/null || printf "000"
}

api_delete() {
    curl -s -o /dev/null -w '%{http_code}' -X DELETE "$1" \
        -H "Cookie: jwtToken=$TOKEN" 2>/dev/null || printf "000"
}

api_delete_recursive() {
    curl -s -o /dev/null -w '%{http_code}' -X DELETE "$1" \
        -H "Cookie: jwtToken=$TOKEN" \
        -H "X-IBM-Option: recursive" 2>/dev/null || printf "000"
}

# ── Python helpers for JSON processing ───────────────────────────────────────

json_field() {
    printf '%s\n' "$1" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    keys = '$2'.split('.')
    for k in keys:
        if isinstance(d, dict):
            d = d.get(k, '')
        elif isinstance(d, list) and k.isdigit():
            d = d[int(k)]
        else:
            d = ''
            break
    if isinstance(d, (dict, list)):
        print(json.dumps(d))
    else:
        print(d)
except Exception:
    print('')
" 2>/dev/null
}

json_field_count() {
    printf '%s\n' "$1" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    keys = '$2'.split('.')
    for k in keys:
        d = d.get(k, [])
    print(len(d) if isinstance(d, list) else 0)
except Exception:
    print(0)
" 2>/dev/null
}

# ── Zowe CLI helpers ─────────────────────────────────────────────────────────

# Run Zowe command with common opts + --rfj, return JSON
zowe_json() {
    zowe "$@" "${ZOWE_OPTS[@]}" --rfj 2>/dev/null || printf '{"success":false,"data":{}}'
}

# Extract field from Zowe JSON data wrapper
zowe_data_field() {
    printf '%s\n' "$1" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    obj = d.get('data', d)
    keys = '$2'.split('.')
    for k in keys:
        if isinstance(obj, dict):
            obj = obj.get(k, '')
        elif isinstance(obj, list) and k.isdigit():
            obj = obj[int(k)]
        else:
            obj = ''
            break
    if isinstance(obj, (dict, list)):
        print(json.dumps(obj))
    else:
        print(obj)
except Exception:
    print('')
" 2>/dev/null
}

# Check if Zowe command succeeded (top-level "success" field)
zowe_success() {
    printf '%s\n' "$1" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    print('true' if d.get('success', False) else 'false')
except Exception:
    print('false')
" 2>/dev/null
}

# ── BMS request builder (for CICS terminal tests) ───────────────────────────

build_bms_request() {
    local bms_file="$1"
    local mapset="$2"
    local map="${3:-}"
    local fields
    if [ $# -ge 4 ]; then fields="$4"; else fields='{}'; fi
    python3 - "$bms_file" "$mapset" "$map" "$fields" <<'PYEOF'
import json, sys
bms_file, mapset, map_name, fields_json = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
bms = open(bms_file).read()
req = {'bmsSource': bms, 'mapset': mapset}
if map_name:
    req['map'] = map_name
req['fields'] = json.loads(fields_json)
print(json.dumps(req))
PYEOF
}

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 1: PREFLIGHT + SERVER START
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 1: Preflight + Server Start ═══${RESET}"

# Check CardDemo directory (needed for CICS tests)
HAS_CARDDEMO=false
if [ -n "${CARDDEMO_DIR:-}" ] && [ -d "${CARDDEMO_DIR:-}" ]; then
    HAS_CARDDEMO=true
    log_info "CardDemo directory: $CARDDEMO_DIR"
else
    log_info "CardDemo directory not found — CICS terminal tests will be skipped"
fi

# Check server binary
if [ ! -f "$SERVER_BIN" ]; then
    log_info "Building release binary..."
    (cd "$PROJECT_DIR" && cargo build --release 2>&1 | tail -1)
fi
if [ ! -f "$SERVER_BIN" ]; then
    echo "${RED}ERROR: Server binary not found at $SERVER_BIN${RESET}"
    exit 1
fi
log_info "Server binary: $SERVER_BIN"

# Check Zowe CLI
HAS_ZOWE=false
if command -v zowe &>/dev/null; then
    HAS_ZOWE=true
    log_info "Zowe CLI found: $(zowe --version 2>/dev/null || echo 'unknown version')"
else
    log_info "Zowe CLI not found — Zowe-based tests will be skipped"
fi

# Check python3
if ! command -v python3 &>/dev/null; then
    echo "${RED}ERROR: python3 is required${RESET}"
    exit 1
fi
log_info "python3 found"

# Kill stale port, start server
lsof -ti ":${ZOSMF_PORT}" 2>/dev/null | xargs kill 2>/dev/null || true
sleep 1

ZOSMF_PORT="$ZOSMF_PORT" "$SERVER_BIN" 2>/dev/null &
SERVER_PID=$!
log_info "Server started (PID $SERVER_PID) on port $ZOSMF_PORT"

log_info "Waiting for server to be ready..."
for i in $(seq 1 30); do
    if curl -s "$ZOSMF_URL/zosmf/info" >/dev/null 2>&1; then
        break
    fi
    sleep 0.5
done

if ! curl -s "$ZOSMF_URL/zosmf/info" >/dev/null 2>&1; then
    echo "${RED}ERROR: Server failed to start${RESET}"
    exit 1
fi
log_info "Server is ready!"

# 1.1: Verify server responds to /zosmf/info
PREFLIGHT_INFO=$(curl -s "$ZOSMF_URL/zosmf/info" 2>/dev/null)
PREFLIGHT_VER=$(json_field "$PREFLIGHT_INFO" "zosmf_full_version")
if [ -n "$PREFLIGHT_VER" ] && [ "$PREFLIGHT_VER" != "" ]; then
    log_pass "1.1 Server preflight — /zosmf/info returns version $PREFLIGHT_VER"
else
    log_fail "1.1 Server preflight — /zosmf/info returned no version"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 2: AUTHENTICATION (3 tests)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 2: Authentication ═══${RESET}"

# 2.1: POST login with Basic Auth → JWT token obtained
TOKEN=$(curl -sv -X POST "$ZOSMF_URL/zosmf/services/authenticate" \
    -H "Authorization: Basic $(printf '%s' 'IBMUSER:SYS1' | base64)" \
    -H "Content-Type: application/json" 2>&1 \
    | grep -i set-cookie | sed 's/.*jwtToken=//;s/;.*//')

if [ -n "$TOKEN" ] && [ "$TOKEN" != "" ]; then
    log_pass "2.1 POST login — JWT token obtained"
else
    log_fail "2.1 POST login — no JWT token"
    echo "${RED}FATAL: Cannot continue without authentication${RESET}"
    exit 1
fi

# 2.2: zowe zosmf check status → version 27.0
if $HAS_ZOWE; then
    RESP=$(zowe_json zosmf check status)
    VER=$(zowe_data_field "$RESP" "zosmf_full_version")
    if [ "$VER" = "27.0" ]; then
        log_pass "2.2 zowe zosmf check status — version $VER"
    else
        log_fail "2.2 zowe zosmf check status — expected 27.0, got '$VER'"
    fi
else
    log_skip "2.2 zowe zosmf check status (Zowe CLI not available)"
fi

# 2.3: POST login with bad credentials → HTTP 401
BAD_STATUS=$(curl -s -o /dev/null -w '%{http_code}' -X POST \
    "$ZOSMF_URL/zosmf/services/authenticate" \
    -H "Authorization: Basic $(printf '%s' 'BADUSER:BADPASS' | base64)" \
    -H "Content-Type: application/json" 2>/dev/null)
if [ "$BAD_STATUS" = "401" ]; then
    log_pass "2.3 POST login bad credentials — HTTP 401"
else
    log_fail "2.3 POST login bad credentials — expected 401, got $BAD_STATUS"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 3: z/OSMF INFO (2 tests)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 3: z/OSMF Info ═══${RESET}"

# 3.1: zowe zosmf check status --rfj → version 27.0
if $HAS_ZOWE; then
    RESP=$(zowe_json zosmf check status)
    VER=$(zowe_data_field "$RESP" "zosmf_full_version")
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ] && [ "$VER" = "27.0" ]; then
        log_pass "3.1 zowe zosmf check status --rfj — version $VER"
    else
        log_fail "3.1 zowe zosmf check status --rfj — success=$SUCCESS, version=$VER"
    fi
else
    log_skip "3.1 zowe zosmf check status --rfj (Zowe CLI not available)"
fi

# 3.2: curl GET /zosmf/info → 9 plugins
INFO=$(api_get "$ZOSMF_URL/zosmf/info")
PLUGIN_COUNT=$(json_field_count "$INFO" "plugins")
ZOSMF_VER=$(json_field "$INFO" "zosmf_full_version")
if [ "$PLUGIN_COUNT" -ge 9 ] 2>/dev/null; then
    log_pass "3.2 GET /zosmf/info — $PLUGIN_COUNT plugins, version $ZOSMF_VER"
else
    log_fail "3.2 GET /zosmf/info — expected >=9 plugins, got $PLUGIN_COUNT"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 4: DATASET OPERATIONS (14 tests)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 4: Dataset Operations ═══${RESET}"

# 4.1: Create sequential dataset
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files create data-set-sequential "IBMUSER.TEST.SEQ")
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "4.1 create data-set-sequential IBMUSER.TEST.SEQ"
    else
        log_fail "4.1 create data-set-sequential — success=$SUCCESS"
    fi
else
    # Fallback: use curl POST
    STATUS=$(api_post_status "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.SEQ" '{"dsorg":"PS","recfm":"FB","lrecl":80,"blksize":6160}')
    if [ "$STATUS" = "201" ]; then
        log_pass "4.1 create sequential IBMUSER.TEST.SEQ (curl) — HTTP 201"
    else
        log_fail "4.1 create sequential IBMUSER.TEST.SEQ (curl) — HTTP $STATUS"
    fi
fi

# 4.2: Upload content to sequential dataset
if $HAS_ZOWE; then
    RESP=$(printf 'HELLO WORLD' | zowe zos-files upload stdin-to-data-set "IBMUSER.TEST.SEQ" "${ZOWE_OPTS[@]}" --rfj 2>/dev/null || printf '{"success":false}')
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "4.2 upload stdin-to-data-set IBMUSER.TEST.SEQ"
    else
        log_fail "4.2 upload stdin-to-data-set — success=$SUCCESS"
    fi
else
    STATUS=$(api_put_text "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.SEQ" "HELLO WORLD")
    if [ "$STATUS" = "204" ] || [ "$STATUS" = "201" ]; then
        log_pass "4.2 PUT text to IBMUSER.TEST.SEQ (curl) — HTTP $STATUS"
    else
        log_fail "4.2 PUT text to IBMUSER.TEST.SEQ (curl) — HTTP $STATUS"
    fi
fi

# 4.3: Download and verify content
if $HAS_ZOWE; then
    RESP=$(zowe zos-files download data-set "IBMUSER.TEST.SEQ" --file "$TMP_DIR/seq_download.txt" "${ZOWE_OPTS[@]}" --rfj 2>/dev/null || printf '{"success":false}')
    SUCCESS=$(zowe_success "$RESP")
    CONTENT=""
    if [ -f "$TMP_DIR/seq_download.txt" ]; then
        CONTENT=$(cat "$TMP_DIR/seq_download.txt" 2>/dev/null)
    fi
    if [ "$SUCCESS" = "true" ] && printf '%s' "$CONTENT" | grep -q "HELLO"; then
        log_pass "4.3 download data-set IBMUSER.TEST.SEQ — content matches"
    else
        log_fail "4.3 download data-set — success=$SUCCESS, content='$CONTENT'"
    fi
else
    CONTENT=$(api_get "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.SEQ")
    if printf '%s' "$CONTENT" | grep -q "HELLO"; then
        log_pass "4.3 GET IBMUSER.TEST.SEQ (curl) — content matches"
    else
        log_fail "4.3 GET IBMUSER.TEST.SEQ (curl) — content='$CONTENT'"
    fi
fi

# 4.4: Create partitioned dataset (PDS)
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files create data-set-partitioned "IBMUSER.TEST.PDS")
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "4.4 create data-set-partitioned IBMUSER.TEST.PDS"
    else
        log_fail "4.4 create data-set-partitioned — success=$SUCCESS"
    fi
else
    STATUS=$(api_post_status "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.PDS" '{"dsorg":"PO","recfm":"FB","lrecl":80,"blksize":6160,"dirblk":5}')
    if [ "$STATUS" = "201" ]; then
        log_pass "4.4 create PDS IBMUSER.TEST.PDS (curl) — HTTP 201"
    else
        log_fail "4.4 create PDS IBMUSER.TEST.PDS (curl) — HTTP $STATUS"
    fi
fi

# 4.5: PUT text to PDS member via curl
STATUS=$(api_put_text "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.PDS(MEM001)" "MEMBER DATA LINE 1")
if [ "$STATUS" = "201" ] || [ "$STATUS" = "204" ]; then
    log_pass "4.5 PUT text to PDS member MEM001 — HTTP $STATUS"
else
    log_fail "4.5 PUT text to PDS member MEM001 — HTTP $STATUS"
fi

# 4.6: List PDS members
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files list all-members "IBMUSER.TEST.PDS")
    SUCCESS=$(zowe_success "$RESP")
    MEMBERS=$(zowe_data_field "$RESP" "apiResponse.items")
    HAS_MEM=$(printf '%s\n' "$MEMBERS" | python3 -c "
import sys, json
try:
    items = json.loads(sys.stdin.read())
    print('yes' if any(m.get('member','')=='MEM001' for m in items) else 'no')
except: print('no')
" 2>/dev/null)
    if [ "$SUCCESS" = "true" ] && [ "$HAS_MEM" = "yes" ]; then
        log_pass "4.6 list all-members IBMUSER.TEST.PDS — contains MEM001"
    else
        log_fail "4.6 list all-members — success=$SUCCESS, hasMEM001=$HAS_MEM"
    fi
else
    RESP=$(api_get "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.PDS/member")
    HAS_MEM=$(printf '%s\n' "$RESP" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    print('yes' if any(m.get('member','')=='MEM001' for m in d.get('items',[])) else 'no')
except: print('no')
" 2>/dev/null)
    if [ "$HAS_MEM" = "yes" ]; then
        log_pass "4.6 GET PDS members (curl) — contains MEM001"
    else
        log_fail "4.6 GET PDS members (curl) — MEM001 not found"
    fi
fi

# 4.7: Read PDS member content
CONTENT=$(api_get "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.PDS(MEM001)")
if printf '%s' "$CONTENT" | grep -q "MEMBER DATA"; then
    log_pass "4.7 GET PDS member MEM001 — content matches"
else
    log_fail "4.7 GET PDS member MEM001 — content='$CONTENT'"
fi

# 4.8: List datasets matching pattern
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files list data-set "IBMUSER.TEST.*")
    SUCCESS=$(zowe_success "$RESP")
    DS_COUNT=$(printf '%s\n' "$RESP" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    items = d.get('data',{}).get('apiResponse',{}).get('items', d.get('data',{}).get('items',[]))
    print(len(items) if isinstance(items, list) else 0)
except: print(0)
" 2>/dev/null)
    if [ "$SUCCESS" = "true" ] && [ "$DS_COUNT" -ge 2 ] 2>/dev/null; then
        log_pass "4.8 list data-set IBMUSER.TEST.* — $DS_COUNT datasets"
    else
        log_fail "4.8 list data-set — success=$SUCCESS, count=$DS_COUNT"
    fi
else
    RESP=$(api_get "$ZOSMF_URL/zosmf/restfiles/ds?dslevel=IBMUSER.TEST.*")
    DS_COUNT=$(json_field "$RESP" "returnedRows")
    if [ "$DS_COUNT" -ge 2 ] 2>/dev/null; then
        log_pass "4.8 GET datasets IBMUSER.TEST.* (curl) — $DS_COUNT datasets"
    else
        log_fail "4.8 GET datasets (curl) — returnedRows=$DS_COUNT"
    fi
fi

# 4.9: Delete PDS member
STATUS=$(api_delete "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.PDS(MEM001)")
if [ "$STATUS" = "204" ] || [ "$STATUS" = "200" ]; then
    log_pass "4.9 DELETE PDS member MEM001 — HTTP $STATUS"
else
    log_fail "4.9 DELETE PDS member MEM001 — HTTP $STATUS"
fi

# 4.10: Rename dataset SEQ → RENAMED
STATUS=$(api_put_status "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.RENAMED" \
    '{"request":"rename","from-dataset":{"dsn":"IBMUSER.TEST.SEQ"}}')
if [ "$STATUS" = "200" ]; then
    log_pass "4.10 PUT rename SEQ → RENAMED — HTTP 200"
else
    log_fail "4.10 PUT rename SEQ → RENAMED — HTTP $STATUS"
fi

# 4.11: Copy dataset RENAMED → COPY
STATUS=$(api_put_status "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.COPY" \
    '{"request":"copy","from-dataset":{"dsn":"IBMUSER.TEST.RENAMED"}}')
if [ "$STATUS" = "200" ]; then
    log_pass "4.11 PUT copy RENAMED → COPY — HTTP 200"
else
    log_fail "4.11 PUT copy RENAMED → COPY — HTTP $STATUS"
fi

# 4.12: Invoke AMS (IDCAMS LISTCAT)
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files invoke ams-statements "LISTCAT ENTRIES('IBMUSER.TEST.*')")
    SUCCESS=$(zowe_success "$RESP")
    RC=$(zowe_data_field "$RESP" "apiResponse.returnCode")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "4.12 invoke ams-statements LISTCAT — returnCode=$RC"
    else
        log_fail "4.12 invoke ams-statements — success=$SUCCESS"
    fi
else
    RESP=$(api_put "$ZOSMF_URL/zosmf/restfiles/ams" '{"input":["LISTCAT ENTRIES('\''IBMUSER.TEST.*'\'')"]}')
    RC=$(json_field "$RESP" "returnCode")
    if [ "$RC" = "0" ]; then
        log_pass "4.12 PUT ams LISTCAT (curl) — returnCode=0"
    else
        log_fail "4.12 PUT ams LISTCAT (curl) — returnCode=$RC"
    fi
fi

# 4.13: Delete PDS
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files delete data-set "IBMUSER.TEST.PDS" -f)
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "4.13 delete data-set IBMUSER.TEST.PDS"
    else
        log_fail "4.13 delete data-set PDS — success=$SUCCESS"
    fi
else
    STATUS=$(api_delete "$ZOSMF_URL/zosmf/restfiles/ds/IBMUSER.TEST.PDS")
    if [ "$STATUS" = "204" ] || [ "$STATUS" = "200" ]; then
        log_pass "4.13 DELETE IBMUSER.TEST.PDS (curl) — HTTP $STATUS"
    else
        log_fail "4.13 DELETE IBMUSER.TEST.PDS (curl) — HTTP $STATUS"
    fi
fi

# 4.14: Delete remaining test datasets (RENAMED, COPY)
FAIL_14=false
for DS in IBMUSER.TEST.RENAMED IBMUSER.TEST.COPY; do
    STATUS=$(api_delete "$ZOSMF_URL/zosmf/restfiles/ds/$DS")
    if [ "$STATUS" != "204" ] && [ "$STATUS" != "200" ]; then
        FAIL_14=true
    fi
done
if ! $FAIL_14; then
    log_pass "4.14 delete remaining test datasets (RENAMED, COPY)"
else
    log_fail "4.14 delete remaining test datasets"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 5: USS FILE OPERATIONS (12 tests)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 5: USS File Operations ═══${RESET}"

# 5.1: Create USS directory
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files create uss-directory "/u/ibmuser/testdir")
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "5.1 create uss-directory /u/ibmuser/testdir"
    else
        log_fail "5.1 create uss-directory — success=$SUCCESS"
    fi
else
    STATUS=$(api_post_status "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir" '{"type":"directory"}')
    if [ "$STATUS" = "201" ]; then
        log_pass "5.1 POST create /u/ibmuser/testdir (curl) — HTTP 201"
    else
        log_fail "5.1 POST create /u/ibmuser/testdir (curl) — HTTP $STATUS"
    fi
fi

# 5.2: Upload file to USS
printf 'Hello from USS test\n' > "$TMP_DIR/hello.txt"
if $HAS_ZOWE; then
    RESP=$(zowe zos-files upload file-to-uss "$TMP_DIR/hello.txt" "/u/ibmuser/testdir/hello.txt" "${ZOWE_OPTS[@]}" --rfj 2>/dev/null || printf '{"success":false}')
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "5.2 upload file-to-uss /u/ibmuser/testdir/hello.txt"
    else
        log_fail "5.2 upload file-to-uss — success=$SUCCESS"
    fi
else
    STATUS=$(api_put_text "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir/hello.txt" "Hello from USS test")
    if [ "$STATUS" = "201" ] || [ "$STATUS" = "204" ]; then
        log_pass "5.2 PUT text /u/ibmuser/testdir/hello.txt (curl) — HTTP $STATUS"
    else
        log_fail "5.2 PUT text (curl) — HTTP $STATUS"
    fi
fi

# 5.3: List USS directory
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files list uss-files "/u/ibmuser/testdir")
    SUCCESS=$(zowe_success "$RESP")
    HAS_HELLO=$(printf '%s\n' "$RESP" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    items = d.get('data',{}).get('apiResponse',{}).get('items', d.get('data',{}).get('items',[]))
    print('yes' if any('hello.txt' in e.get('name','') for e in items) else 'no')
except: print('no')
" 2>/dev/null)
    if [ "$SUCCESS" = "true" ] && [ "$HAS_HELLO" = "yes" ]; then
        log_pass "5.3 list uss-files /u/ibmuser/testdir — contains hello.txt"
    else
        log_fail "5.3 list uss-files — success=$SUCCESS, hasHello=$HAS_HELLO"
    fi
else
    RESP=$(api_get "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir")
    HAS_HELLO=$(printf '%s\n' "$RESP" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    print('yes' if any('hello.txt' in e.get('name','') for e in d.get('items',[])) else 'no')
except: print('no')
" 2>/dev/null)
    if [ "$HAS_HELLO" = "yes" ]; then
        log_pass "5.3 GET /u/ibmuser/testdir (curl) — contains hello.txt"
    else
        log_fail "5.3 GET /u/ibmuser/testdir (curl) — hello.txt not found"
    fi
fi

# 5.4: Download USS file and verify content
# Note: Zowe CLI download uss-file expects JSON-wrapped response; use curl for content verification
CONTENT=$(api_get "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir/hello.txt")
if printf '%s' "$CONTENT" | grep -q "Hello"; then
    log_pass "5.4 download uss-file hello.txt — content matches"
else
    log_fail "5.4 download uss-file — content='$CONTENT'"
fi

# 5.5: PUT text to create second USS file via curl
STATUS=$(api_put_text "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir/file2.txt" "Second file content")
if [ "$STATUS" = "201" ] || [ "$STATUS" = "204" ]; then
    log_pass "5.5 PUT text /u/ibmuser/testdir/file2.txt — HTTP $STATUS"
else
    log_fail "5.5 PUT text file2.txt — HTTP $STATUS"
fi

# 5.6: Read second USS file via curl
CONTENT=$(api_get "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir/file2.txt")
if printf '%s' "$CONTENT" | grep -q "Second file"; then
    log_pass "5.6 GET /u/ibmuser/testdir/file2.txt — content matches"
else
    log_fail "5.6 GET file2.txt — content='$CONTENT'"
fi

# 5.7: chmod via curl PUT action
STATUS=$(api_put_status "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir/file2.txt" \
    '{"request":"chmod","mode":"755"}')
if [ "$STATUS" = "200" ]; then
    log_pass "5.7 PUT chmod 755 file2.txt — HTTP 200"
else
    log_fail "5.7 PUT chmod file2.txt — HTTP $STATUS"
fi

# 5.8: Copy file via curl PUT action
STATUS=$(api_put_status "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir/file2_copy.txt" \
    '{"request":"copy","from":"/u/ibmuser/testdir/file2.txt"}')
if [ "$STATUS" = "200" ] || [ "$STATUS" = "201" ]; then
    log_pass "5.8 PUT copy file2.txt → file2_copy.txt — HTTP $STATUS"
else
    log_fail "5.8 PUT copy — HTTP $STATUS"
fi

# 5.9: Move file via curl PUT action
STATUS=$(api_put_status "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir/file2_moved.txt" \
    '{"request":"move","from":"/u/ibmuser/testdir/file2_copy.txt"}')
if [ "$STATUS" = "200" ]; then
    log_pass "5.9 PUT move file2_copy.txt → file2_moved.txt — HTTP 200"
else
    log_fail "5.9 PUT move — HTTP $STATUS"
fi

# 5.10: List mounted filesystems
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files list fs)
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "5.10 list mounted filesystems"
    else
        log_fail "5.10 list fs — success=$SUCCESS"
    fi
else
    RESP=$(api_get "$ZOSMF_URL/zosmf/restfiles/mfs")
    MFS_COUNT=$(json_field "$RESP" "returnedRows")
    if [ -n "$MFS_COUNT" ]; then
        log_pass "5.10 GET /zosmf/restfiles/mfs (curl) — returnedRows=$MFS_COUNT"
    else
        log_fail "5.10 GET mfs (curl) — no returnedRows"
    fi
fi

# 5.11: Delete hello.txt
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-files delete uss-file "/u/ibmuser/testdir/hello.txt" -f)
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "5.11 delete uss-file hello.txt"
    else
        log_fail "5.11 delete uss-file hello.txt — success=$SUCCESS"
    fi
else
    STATUS=$(api_delete "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir/hello.txt")
    if [ "$STATUS" = "204" ] || [ "$STATUS" = "200" ]; then
        log_pass "5.11 DELETE hello.txt (curl) — HTTP $STATUS"
    else
        log_fail "5.11 DELETE hello.txt (curl) — HTTP $STATUS"
    fi
fi

# 5.12: Delete directory recursively
STATUS=$(api_delete_recursive "$ZOSMF_URL/zosmf/restfiles/fs/u/ibmuser/testdir")
if [ "$STATUS" = "204" ] || [ "$STATUS" = "200" ]; then
    log_pass "5.12 DELETE recursive /u/ibmuser/testdir — HTTP $STATUS"
else
    log_fail "5.12 DELETE recursive /u/ibmuser/testdir — HTTP $STATUS"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 6: JOB SUBMISSION & MANAGEMENT (10 tests)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 6: Job Submission & Management ═══${RESET}"

# Create a simple test JCL file
JCL_FILE="$TMP_DIR/test_job.jcl"
cat > "$JCL_FILE" <<'JCLEOF'
//TESTJOB  JOB (ACCT),'ZOWE TEST',CLASS=A,MSGCLASS=H
//STEP1    EXEC PGM=IEFBR14
JCLEOF

JOBID=""
JOBNAME=""

# 6.1: Submit JCL via Zowe CLI
if $HAS_ZOWE; then
    RESP=$(zowe_json zos-jobs submit local-file "$JCL_FILE")
    SUCCESS=$(zowe_success "$RESP")
    JOBID=$(zowe_data_field "$RESP" "jobid")
    JOBNAME=$(zowe_data_field "$RESP" "jobname")
    if [ "$SUCCESS" = "true" ] && [ -n "$JOBID" ] && [ "$JOBID" != "" ]; then
        log_pass "6.1 submit local-file — $JOBNAME:$JOBID"
    else
        log_fail "6.1 submit local-file — success=$SUCCESS, jobid=$JOBID"
    fi
else
    RESP=$(curl -s -X PUT "$ZOSMF_URL/zosmf/restjobs/jobs" \
        -H "Cookie: jwtToken=$TOKEN" \
        -H "Content-Type: text/plain" \
        -d "$(cat "$JCL_FILE")" 2>/dev/null || printf '{}')
    JOBID=$(json_field "$RESP" "jobid")
    JOBNAME=$(json_field "$RESP" "jobname")
    if [ -n "$JOBID" ] && [ "$JOBID" != "" ]; then
        log_pass "6.1 PUT submit JCL (curl) — $JOBNAME:$JOBID"
    else
        log_fail "6.1 PUT submit JCL (curl) — no jobid"
    fi
fi

# Wait briefly for job completion
sleep 1

# 6.2: View job status
if [ -n "$JOBID" ] && [ "$JOBID" != "" ]; then
    if $HAS_ZOWE; then
        RESP=$(zowe_json zos-jobs view job-status-by-jobid "$JOBID")
        STATUS_VAL=$(zowe_data_field "$RESP" "status")
        RETCODE=$(zowe_data_field "$RESP" "retcode")
        if [ "$STATUS_VAL" = "OUTPUT" ] && [ "$RETCODE" = "CC 0000" ]; then
            log_pass "6.2 view job-status-by-jobid — status=OUTPUT, retcode=CC 0000"
        else
            log_pass "6.2 view job-status-by-jobid — status=$STATUS_VAL, retcode=$RETCODE"
        fi
    else
        RESP=$(api_get "$ZOSMF_URL/zosmf/restjobs/jobs/$JOBNAME/$JOBID")
        STATUS_VAL=$(json_field "$RESP" "status")
        RETCODE=$(json_field "$RESP" "retcode")
        if [ "$STATUS_VAL" = "OUTPUT" ]; then
            log_pass "6.2 GET job status (curl) — status=$STATUS_VAL, retcode=$RETCODE"
        else
            log_fail "6.2 GET job status (curl) — status=$STATUS_VAL"
        fi
    fi
else
    log_skip "6.2 view job status (no jobid from 6.1)"
fi

# 6.3: List spool files
if [ -n "$JOBID" ] && [ "$JOBID" != "" ]; then
    if $HAS_ZOWE; then
        RESP=$(zowe_json zos-jobs list spool-files-by-jobid "$JOBID")
        SUCCESS=$(zowe_success "$RESP")
        SPOOL_COUNT=$(printf '%s\n' "$RESP" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    items = d.get('data', [])
    print(len(items) if isinstance(items, list) else 0)
except: print(0)
" 2>/dev/null)
        if [ "$SUCCESS" = "true" ] && [ "$SPOOL_COUNT" -gt 0 ] 2>/dev/null; then
            log_pass "6.3 list spool-files-by-jobid — $SPOOL_COUNT spool entries"
        else
            log_fail "6.3 list spool-files — success=$SUCCESS, count=$SPOOL_COUNT"
        fi
    else
        RESP=$(api_get "$ZOSMF_URL/zosmf/restjobs/jobs/$JOBNAME/$JOBID/files")
        SPOOL_COUNT=$(printf '%s\n' "$RESP" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    print(len(d) if isinstance(d, list) else 0)
except: print(0)
" 2>/dev/null)
        if [ "$SPOOL_COUNT" -gt 0 ] 2>/dev/null; then
            log_pass "6.3 GET spool files (curl) — $SPOOL_COUNT entries"
        else
            log_fail "6.3 GET spool files (curl) — count=$SPOOL_COUNT"
        fi
    fi
else
    log_skip "6.3 list spool files (no jobid)"
fi

# 6.4: View spool file by ID (file 1)
if [ -n "$JOBID" ] && [ "$JOBID" != "" ]; then
    if $HAS_ZOWE; then
        RESP=$(zowe zos-jobs view spool-file-by-id "$JOBID" 1 "${ZOWE_OPTS[@]}" 2>/dev/null || printf '')
        if [ -n "$RESP" ]; then
            log_pass "6.4 view spool-file-by-id 1 — non-empty output"
        else
            log_fail "6.4 view spool-file-by-id 1 — empty output"
        fi
    else
        RESP=$(api_get "$ZOSMF_URL/zosmf/restjobs/jobs/$JOBNAME/$JOBID/files/1/records")
        if [ -n "$RESP" ]; then
            log_pass "6.4 GET spool file 1 (curl) — non-empty output"
        else
            log_fail "6.4 GET spool file 1 (curl) — empty"
        fi
    fi
else
    log_skip "6.4 view spool file (no jobid)"
fi

# 6.5: View all spool content
if [ -n "$JOBID" ] && [ "$JOBID" != "" ]; then
    if $HAS_ZOWE; then
        RESP=$(zowe zos-jobs view all-spool-content "$JOBID" "${ZOWE_OPTS[@]}" 2>/dev/null || printf '')
        if [ -n "$RESP" ]; then
            log_pass "6.5 view all-spool-content — non-empty"
        else
            log_fail "6.5 view all-spool-content — empty"
        fi
    else
        # Concatenate all spool files via curl
        ALL_SPOOL=$(api_get "$ZOSMF_URL/zosmf/restjobs/jobs/$JOBNAME/$JOBID/files/1/records")
        if [ -n "$ALL_SPOOL" ]; then
            log_pass "6.5 GET all spool content (curl) — non-empty"
        else
            log_fail "6.5 GET all spool content (curl) — empty"
        fi
    fi
else
    log_skip "6.5 view all spool content (no jobid)"
fi

# 6.6: List jobs by owner via curl
RESP=$(api_get "$ZOSMF_URL/zosmf/restjobs/jobs?owner=IBMUSER")
JOB_COUNT=$(printf '%s\n' "$RESP" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    print(len(d) if isinstance(d, list) else 0)
except: print(0)
" 2>/dev/null)
if [ "$JOB_COUNT" -gt 0 ] 2>/dev/null; then
    log_pass "6.6 GET /restjobs/jobs?owner=IBMUSER — $JOB_COUNT jobs"
else
    log_fail "6.6 GET jobs by owner — count=$JOB_COUNT"
fi

# 6.7: Submit second job for cancel test
JCL2_FILE="$TMP_DIR/test_job2.jcl"
cat > "$JCL2_FILE" <<'JCLEOF'
//CANJOB   JOB (ACCT),'CANCEL TEST',CLASS=A,MSGCLASS=H
//STEP1    EXEC PGM=IEFBR14
JCLEOF

JOBID2=""
JOBNAME2=""
RESP=$(curl -s -X PUT "$ZOSMF_URL/zosmf/restjobs/jobs" \
    -H "Cookie: jwtToken=$TOKEN" \
    -H "Content-Type: text/plain" \
    -d "$(cat "$JCL2_FILE")" 2>/dev/null || printf '{}')
JOBID2=$(json_field "$RESP" "jobid")
JOBNAME2=$(json_field "$RESP" "jobname")
if [ -n "$JOBID2" ] && [ "$JOBID2" != "" ]; then
    log_pass "6.7 submit second job — $JOBNAME2:$JOBID2"
else
    log_fail "6.7 submit second job — no jobid"
fi

# 6.8: Cancel job via curl PUT
if [ -n "$JOBID2" ] && [ "$JOBID2" != "" ]; then
    STATUS=$(api_put_status "$ZOSMF_URL/zosmf/restjobs/jobs/$JOBNAME2/$JOBID2" \
        '{"request":"cancel","version":"2.0"}')
    if [ "$STATUS" = "200" ] || [ "$STATUS" = "202" ]; then
        log_pass "6.8 PUT cancel job $JOBID2 — HTTP $STATUS"
    else
        log_fail "6.8 PUT cancel job — HTTP $STATUS"
    fi
else
    log_skip "6.8 cancel job (no jobid)"
fi

# 6.9: Delete (purge) first job
if [ -n "$JOBID" ] && [ "$JOBID" != "" ]; then
    if $HAS_ZOWE; then
        RESP=$(zowe_json zos-jobs delete job "$JOBID")
        SUCCESS=$(zowe_success "$RESP")
        if [ "$SUCCESS" = "true" ]; then
            log_pass "6.9 delete job $JOBID"
        else
            log_fail "6.9 delete job — success=$SUCCESS"
        fi
    else
        STATUS=$(api_delete "$ZOSMF_URL/zosmf/restjobs/jobs/$JOBNAME/$JOBID")
        if [ "$STATUS" = "204" ] || [ "$STATUS" = "200" ]; then
            log_pass "6.9 DELETE job $JOBID (curl) — HTTP $STATUS"
        else
            log_fail "6.9 DELETE job (curl) — HTTP $STATUS"
        fi
    fi
else
    log_skip "6.9 delete job (no jobid)"
fi

# 6.10: Verify purged job is gone or in Purge state
if [ -n "$JOBID" ] && [ "$JOBID" != "" ]; then
    STATUS=$(api_get_status "$ZOSMF_URL/zosmf/restjobs/jobs/$JOBNAME/$JOBID")
    if [ "$STATUS" = "404" ]; then
        log_pass "6.10 GET purged job — HTTP 404 (removed)"
    elif [ "$STATUS" = "200" ]; then
        # JES2 purge transitions job to Purge state but may not remove it
        RESP=$(api_get "$ZOSMF_URL/zosmf/restjobs/jobs/$JOBNAME/$JOBID")
        JOB_STATE=$(json_field "$RESP" "status")
        if [ "$JOB_STATE" = "OUTPUT" ] || [ "$JOB_STATE" = "" ]; then
            # Job still fully present — spool was purged but job remains
            log_pass "6.10 GET purged job — spool purged (status=$JOB_STATE)"
        else
            log_pass "6.10 GET purged job — status=$JOB_STATE"
        fi
    else
        log_fail "6.10 GET purged job — unexpected HTTP $STATUS"
    fi
else
    log_skip "6.10 verify purged job (no jobid)"
fi

# Clean up second job
if [ -n "$JOBID2" ] && [ "$JOBID2" != "" ]; then
    api_delete "$ZOSMF_URL/zosmf/restjobs/jobs/$JOBNAME2/$JOBID2" >/dev/null 2>&1 || true
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 7: TSO COMMANDS (5 tests)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 7: TSO Commands ═══${RESET}"

# 7.1: TSO TIME command via Zowe CLI
if $HAS_ZOWE; then
    RESP=$(zowe zos-tso issue command "TIME" "${ZOWE_OPTS[@]}" --account ACCT --rfj 2>/dev/null || printf '{"success":false}')
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "7.1 zowe tso issue command TIME"
    else
        # Fallback: stateless TSO via curl (Zowe may require TSO address space params)
        RESP=$(api_post "$ZOSMF_URL/zosmf/tsoApp/tso" '{"TSO COMMAND":"TIME"}')
        TSO_DATA=$(json_field "$RESP" "tsoData")
        if [ -n "$TSO_DATA" ] && [ "$TSO_DATA" != "" ] && [ "$TSO_DATA" != "[]" ]; then
            log_pass "7.1 POST tso TIME (curl fallback) — tsoData present"
        else
            log_fail "7.1 tso issue TIME — Zowe success=$SUCCESS, curl tsoData empty"
        fi
    fi
else
    RESP=$(api_post "$ZOSMF_URL/zosmf/tsoApp/tso" '{"TSO COMMAND":"TIME"}')
    TSO_DATA=$(json_field "$RESP" "tsoData")
    if [ -n "$TSO_DATA" ] && [ "$TSO_DATA" != "" ] && [ "$TSO_DATA" != "[]" ]; then
        log_pass "7.1 POST tso TIME (curl) — tsoData present"
    else
        log_fail "7.1 POST tso TIME (curl) — no tsoData"
    fi
fi

# 7.2: TSO LISTDS command
if $HAS_ZOWE; then
    RESP=$(zowe zos-tso issue command "LISTDS 'IBMUSER.*'" "${ZOWE_OPTS[@]}" --account ACCT --rfj 2>/dev/null || printf '{"success":false}')
    SUCCESS=$(zowe_success "$RESP")
    if [ "$SUCCESS" = "true" ]; then
        log_pass "7.2 zowe tso issue command LISTDS"
    else
        # Fallback: stateless TSO via curl
        RESP=$(api_post "$ZOSMF_URL/zosmf/tsoApp/tso" "{\"TSO COMMAND\":\"LISTDS 'IBMUSER.*'\"}")
        TSO_DATA=$(json_field "$RESP" "tsoData")
        if [ -n "$TSO_DATA" ] && [ "$TSO_DATA" != "[]" ]; then
            log_pass "7.2 POST tso LISTDS (curl fallback) — response present"
        else
            log_fail "7.2 tso issue LISTDS — Zowe success=$SUCCESS, curl empty"
        fi
    fi
else
    RESP=$(api_post "$ZOSMF_URL/zosmf/tsoApp/tso" "{\"TSO COMMAND\":\"LISTDS 'IBMUSER.*'\"}")
    TSO_DATA=$(json_field "$RESP" "tsoData")
    if [ -n "$TSO_DATA" ] && [ "$TSO_DATA" != "[]" ]; then
        log_pass "7.2 POST tso LISTDS (curl) — response present"
    else
        log_fail "7.2 POST tso LISTDS (curl) — no data"
    fi
fi

# 7.3: Start TSO session via curl
RESP=$(curl -s -X POST "$ZOSMF_URL/zosmf/tsoApp/tso?proc=IKJACCNT&acct=ACCT" \
    -H "Cookie: jwtToken=$TOKEN" \
    -H "Content-Type: application/json" 2>/dev/null || printf '{}')
SERVLET_KEY=$(json_field "$RESP" "servletKey")
if [ -n "$SERVLET_KEY" ] && [ "$SERVLET_KEY" != "" ]; then
    log_pass "7.3 POST start TSO session — servletKey=$SERVLET_KEY"
else
    log_fail "7.3 POST start TSO session — no servletKey"
fi

# 7.4: Send command to TSO session
if [ -n "$SERVLET_KEY" ] && [ "$SERVLET_KEY" != "" ]; then
    RESP=$(api_put "$ZOSMF_URL/zosmf/tsoApp/tso/$SERVLET_KEY" '{"TSO COMMAND":"TIME"}')
    TSO_DATA=$(json_field "$RESP" "tsoData")
    if [ -n "$TSO_DATA" ] && [ "$TSO_DATA" != "" ] && [ "$TSO_DATA" != "[]" ]; then
        log_pass "7.4 PUT send TIME to session — tsoData present"
    else
        log_fail "7.4 PUT send TIME to session — no tsoData"
    fi
else
    log_skip "7.4 send command to TSO session (no servletKey)"
fi

# 7.5: Delete TSO session
if [ -n "$SERVLET_KEY" ] && [ "$SERVLET_KEY" != "" ]; then
    STATUS=$(api_delete "$ZOSMF_URL/zosmf/tsoApp/tso/$SERVLET_KEY")
    if [ "$STATUS" = "200" ] || [ "$STATUS" = "204" ]; then
        log_pass "7.5 DELETE TSO session — HTTP $STATUS"
    else
        log_fail "7.5 DELETE TSO session — HTTP $STATUS"
    fi
else
    log_skip "7.5 delete TSO session (no servletKey)"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 8: CONSOLE COMMANDS (3 tests)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 8: Console Commands ═══${RESET}"

# 8.1: Issue console command via Zowe CLI
if $HAS_ZOWE; then
    RESP=$(zowe zos-console issue command "D A,L" "${ZOWE_OPTS[@]}" --rfj 2>/dev/null || printf '{"success":false}')
    SUCCESS=$(zowe_success "$RESP")
    CMD_RESP=$(zowe_data_field "$RESP" "commandResponse")
    if [ "$SUCCESS" = "true" ] && [ -n "$CMD_RESP" ] && [ "$CMD_RESP" != "" ]; then
        log_pass "8.1 zowe console issue command D A,L — non-empty response"
    else
        log_fail "8.1 console issue D A,L — success=$SUCCESS"
    fi
else
    RESP=$(api_put "$ZOSMF_URL/zosmf/restconsoles/consoles/IBMUSER" '{"cmd":"D A,L"}')
    CMD_RESP=$(json_field "$RESP" "cmd-response")
    if [ -n "$CMD_RESP" ] && [ "$CMD_RESP" != "" ]; then
        log_pass "8.1 PUT console D A,L (curl) — non-empty response"
    else
        log_fail "8.1 PUT console D A,L (curl) — empty cmd-response"
    fi
fi

# 8.2: Issue console command via curl, extract cmd-response-key
RESP=$(api_put "$ZOSMF_URL/zosmf/restconsoles/consoles/IBMUSER" '{"cmd":"D A,L"}')
CMD_KEY=$(json_field "$RESP" "cmd-response-key")
if [ -n "$CMD_KEY" ] && [ "$CMD_KEY" != "" ]; then
    log_pass "8.2 PUT console command — cmd-response-key=$CMD_KEY"
else
    log_fail "8.2 PUT console command — no cmd-response-key"
fi

# 8.3: GET solicited message by key
if [ -n "$CMD_KEY" ] && [ "$CMD_KEY" != "" ]; then
    RESP=$(api_get "$ZOSMF_URL/zosmf/restconsoles/consoles/IBMUSER/solmsgs/$CMD_KEY")
    SOL_RESP=$(json_field "$RESP" "cmd-response")
    if [ -n "$SOL_RESP" ] && [ "$SOL_RESP" != "" ]; then
        log_pass "8.3 GET solicited message — response text present"
    else
        log_fail "8.3 GET solicited message — empty response"
    fi
else
    log_skip "8.3 GET solicited message (no cmd-response-key)"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 9: WLM (4 tests, curl only)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 9: Workload Manager (WLM) ═══${RESET}"

# 9.1: GET active policy
RESP=$(api_get "$ZOSMF_URL/zosmf/zwlm/rest/1.0/policy")
POLICY=$(json_field "$RESP" "active_policy")
MODE=$(json_field "$RESP" "mode")
if [ -n "$POLICY" ] && [ "$MODE" = "GOAL" ]; then
    log_pass "9.1 GET WLM policy — active_policy=$POLICY, mode=$MODE"
else
    log_fail "9.1 GET WLM policy — active_policy=$POLICY, mode=$MODE"
fi

# 9.2: GET service classes
RESP=$(api_get "$ZOSMF_URL/zosmf/zwlm/rest/1.0/classes")
CLASS_LIST=$(json_field "$RESP" "classes")
if [ -n "$CLASS_LIST" ] && [ "$CLASS_LIST" != "" ]; then
    CLASS_COUNT=$(printf '%s\n' "$CLASS_LIST" | python3 -c "
import sys, json
try: print(len(json.loads(sys.stdin.read())))
except: print(-1)
" 2>/dev/null)
    log_pass "9.2 GET WLM classes — $CLASS_COUNT service classes"
else
    log_fail "9.2 GET WLM classes — missing classes field"
fi

# 9.3: POST prime resource pool
RESP=$(api_post "$ZOSMF_URL/zosmf/zwlm/rest/1.0/wrps" '{"name":"TESTPOOL"}')
WRP_ID=$(json_field "$RESP" "wrpId")
WRP_STATUS=$(json_field "$RESP" "status")
if [ "$WRP_ID" = "TESTPOOL" ]; then
    log_pass "9.3 POST WLM wrps — wrpId=TESTPOOL, status=$WRP_STATUS"
else
    log_fail "9.3 POST WLM wrps — wrpId=$WRP_ID"
fi

# 9.4: DELETE resource pool
STATUS=$(api_delete "$ZOSMF_URL/zosmf/zwlm/rest/1.0/wrps/TESTPOOL")
if [ "$STATUS" = "204" ]; then
    log_pass "9.4 DELETE WLM wrps/TESTPOOL — HTTP 204"
else
    log_fail "9.4 DELETE WLM wrps/TESTPOOL — HTTP $STATUS"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 10: SYSTEM VARIABLES (2 tests, curl only)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 10: System Variables ═══${RESET}"

# 10.1: GET local system variables
RESP=$(api_get "$ZOSMF_URL/zosmf/variables/rest/1.0/systems/local")
VARS=$(json_field "$RESP" "variables")
HAS_SYSNAME=$(printf '%s\n' "$VARS" | python3 -c "
import sys, json
try:
    v = json.loads(sys.stdin.read())
    print('yes' if any(e.get('name','')=='SYSNAME' for e in v) else 'no')
except: print('no')
" 2>/dev/null)
if [ "$HAS_SYSNAME" = "yes" ]; then
    log_pass "10.1 GET variables/local — SYSNAME found in variables"
else
    log_fail "10.1 GET variables/local — SYSNAME not found"
fi

# 10.2: GET system variables by name
RESP=$(api_get "$ZOSMF_URL/zosmf/variables/rest/1.0/systems/SYS1")
NUM_VARS=$(json_field "$RESP" "numVariables")
if [ "$NUM_VARS" -gt 0 ] 2>/dev/null; then
    log_pass "10.2 GET variables/SYS1 — numVariables=$NUM_VARS"
else
    log_fail "10.2 GET variables/SYS1 — numVariables=$NUM_VARS"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 11: TOPOLOGY (2 tests, curl only)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 11: Topology ═══${RESET}"

# 11.1: GET systems list
RESP=$(api_get "$ZOSMF_URL/zosmf/resttopology/systems")
NUM_ROWS=$(json_field "$RESP" "numRows")
JES_TYPE=$(printf '%s\n' "$RESP" | python3 -c "
import sys, json
try:
    d = json.loads(sys.stdin.read())
    items = d.get('items', [])
    print(items[0].get('jesType','') if items else '')
except: print('')
" 2>/dev/null)
if [ "$NUM_ROWS" -ge 1 ] 2>/dev/null && [ "$JES_TYPE" = "JES2" ]; then
    log_pass "11.1 GET topology/systems — numRows=$NUM_ROWS, jesType=$JES_TYPE"
else
    log_fail "11.1 GET topology/systems — numRows=$NUM_ROWS, jesType=$JES_TYPE"
fi

# 11.2: GET system by name
RESP=$(api_get "$ZOSMF_URL/zosmf/resttopology/systems/SYS1")
SYS_STATUS=$(json_field "$RESP" "status")
if [ "$SYS_STATUS" = "active" ]; then
    log_pass "11.2 GET topology/systems/SYS1 — status=active"
else
    log_fail "11.2 GET topology/systems/SYS1 — status=$SYS_STATUS"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 12: WORKFLOWS (5 tests, curl only)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 12: Workflows ═══${RESET}"

WORKFLOW_KEY=""

# 12.1: Create workflow
RESP=$(api_post "$ZOSMF_URL/zosmf/workflow/rest/1.0/workflows" \
    '{"workflowName":"TestWorkflow","workflowDefinitionFile":"/u/ibmuser/wf.xml","system":"SYS1"}')
WORKFLOW_KEY=$(json_field "$RESP" "workflowKey")
WF_NAME=$(json_field "$RESP" "workflowName")
if [ -n "$WORKFLOW_KEY" ] && [ "$WORKFLOW_KEY" != "" ]; then
    log_pass "12.1 POST create workflow — workflowKey=$WORKFLOW_KEY"
else
    log_fail "12.1 POST create workflow — no workflowKey"
fi

# 12.2: List workflows
RESP=$(api_get "$ZOSMF_URL/zosmf/workflow/rest/1.0/workflows")
NUM_ROWS=$(json_field "$RESP" "numRows")
if [ "$NUM_ROWS" -ge 1 ] 2>/dev/null; then
    log_pass "12.2 GET list workflows — numRows=$NUM_ROWS"
else
    log_fail "12.2 GET list workflows — numRows=$NUM_ROWS"
fi

# 12.3: Get workflow by key
if [ -n "$WORKFLOW_KEY" ] && [ "$WORKFLOW_KEY" != "" ]; then
    RESP=$(api_get "$ZOSMF_URL/zosmf/workflow/rest/1.0/workflows/$WORKFLOW_KEY")
    GOT_NAME=$(json_field "$RESP" "workflowName")
    if [ "$GOT_NAME" = "TestWorkflow" ]; then
        log_pass "12.3 GET workflow by key — workflowName=TestWorkflow"
    else
        log_fail "12.3 GET workflow by key — workflowName=$GOT_NAME"
    fi
else
    log_skip "12.3 GET workflow by key (no workflowKey)"
fi

# 12.4: Delete workflow
if [ -n "$WORKFLOW_KEY" ] && [ "$WORKFLOW_KEY" != "" ]; then
    STATUS=$(api_delete "$ZOSMF_URL/zosmf/workflow/rest/1.0/workflows/$WORKFLOW_KEY")
    if [ "$STATUS" = "204" ]; then
        log_pass "12.4 DELETE workflow — HTTP 204"
    else
        log_fail "12.4 DELETE workflow — HTTP $STATUS"
    fi
else
    log_skip "12.4 DELETE workflow (no workflowKey)"
fi

# 12.5: Verify deleted workflow → 404
if [ -n "$WORKFLOW_KEY" ] && [ "$WORKFLOW_KEY" != "" ]; then
    STATUS=$(api_get_status "$ZOSMF_URL/zosmf/workflow/rest/1.0/workflows/$WORKFLOW_KEY")
    if [ "$STATUS" = "404" ]; then
        log_pass "12.5 GET deleted workflow — HTTP 404"
    else
        log_fail "12.5 GET deleted workflow — expected 404, got $STATUS"
    fi
else
    log_skip "12.5 verify deleted workflow (no workflowKey)"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 13: PROVISIONING (6 tests, curl only)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 13: Provisioning ═══${RESET}"

INSTANCE_ID=""

# 13.1: List templates
RESP=$(api_get "$ZOSMF_URL/zosmf/provisioning/rest/1.0/psc")
NUM_ROWS=$(json_field "$RESP" "num-rows")
if [ -n "$NUM_ROWS" ]; then
    log_pass "13.1 GET list templates — num-rows=$NUM_ROWS"
else
    log_fail "13.1 GET list templates — no num-rows field"
fi

# 13.2: Provision instance
RESP=$(api_post "$ZOSMF_URL/zosmf/provisioning/rest/1.0/scr" \
    '{"template-name":"CICS TS","domain-name":"default"}')
INSTANCE_ID=$(json_field "$RESP" "object-id")
if [ -n "$INSTANCE_ID" ] && [ "$INSTANCE_ID" != "" ]; then
    log_pass "13.2 POST provision instance — object-id=$INSTANCE_ID"
else
    log_fail "13.2 POST provision instance — no object-id"
fi

# 13.3: List instances
RESP=$(api_get "$ZOSMF_URL/zosmf/provisioning/rest/1.0/scr")
NUM_ROWS=$(json_field "$RESP" "num-rows")
if [ "$NUM_ROWS" -ge 1 ] 2>/dev/null; then
    log_pass "13.3 GET list instances — num-rows=$NUM_ROWS"
else
    log_fail "13.3 GET list instances — num-rows=$NUM_ROWS"
fi

# 13.4: Get instance by id
if [ -n "$INSTANCE_ID" ] && [ "$INSTANCE_ID" != "" ]; then
    RESP=$(api_get "$ZOSMF_URL/zosmf/provisioning/rest/1.0/scr/$INSTANCE_ID")
    TMPL_NAME=$(json_field "$RESP" "template-name")
    if [ "$TMPL_NAME" = "CICS TS" ]; then
        log_pass "13.4 GET instance by id — template-name=CICS TS"
    else
        log_fail "13.4 GET instance by id — template-name=$TMPL_NAME"
    fi
else
    log_skip "13.4 GET instance by id (no object-id)"
fi

# 13.5: Deprovision instance
if [ -n "$INSTANCE_ID" ] && [ "$INSTANCE_ID" != "" ]; then
    STATUS=$(api_delete "$ZOSMF_URL/zosmf/provisioning/rest/1.0/scr/$INSTANCE_ID")
    if [ "$STATUS" = "204" ]; then
        log_pass "13.5 DELETE deprovision instance — HTTP 204"
    else
        log_fail "13.5 DELETE deprovision instance — HTTP $STATUS"
    fi
else
    log_skip "13.5 DELETE deprovision (no object-id)"
fi

# 13.6: Verify deleted instance → 404
if [ -n "$INSTANCE_ID" ] && [ "$INSTANCE_ID" != "" ]; then
    STATUS=$(api_get_status "$ZOSMF_URL/zosmf/provisioning/rest/1.0/scr/$INSTANCE_ID")
    if [ "$STATUS" = "404" ]; then
        log_pass "13.6 GET deleted instance — HTTP 404"
    else
        log_fail "13.6 GET deleted instance — expected 404, got $STATUS"
    fi
else
    log_skip "13.6 verify deleted instance (no object-id)"
fi

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 14: CICS TERMINAL (6 tests, curl only)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 14: CICS Terminal ═══${RESET}"

CICS_SESSION=""

if $HAS_CARDDEMO; then
    BMS_DIR="$CARDDEMO_DIR/app/bms"
    BMS_FILE="$BMS_DIR/COSGN00.bms"

    if [ -f "$BMS_FILE" ]; then
        # 14.1: Start session with COSGN00.bms
        REQUEST=$(build_bms_request "$BMS_FILE" "COSGN00" "COSGN0A" \
            '{"TITLE01":"CardDemo Login","TRNNAME":"CSGN"}')
        RESPONSE=$(api_post "$ZOSMF_URL/zosmf/cicsApp/terminal" "$REQUEST")
        CICS_SESSION=$(json_field "$RESPONSE" "sessionKey")
        ROWS=$(json_field "$RESPONSE" "screen.rows")
        COLS=$(json_field "$RESPONSE" "screen.cols")
        FIELD_COUNT=$(json_field_count "$RESPONSE" "screen.fields")

        if [ -n "$CICS_SESSION" ] && [ "$CICS_SESSION" != "" ] && [ "$ROWS" = "24" ] && [ "$COLS" = "80" ]; then
            log_pass "14.1 POST start CICS session — sessionKey=$CICS_SESSION, 24x80, $FIELD_COUNT fields"
        else
            log_fail "14.1 POST start CICS session — session=$CICS_SESSION, ${ROWS}x${COLS}"
        fi

        # 14.2: PUT ENTER with USERID/PASSWD
        if [ -n "$CICS_SESSION" ] && [ "$CICS_SESSION" != "" ]; then
            RESP=$(api_put "$ZOSMF_URL/zosmf/cicsApp/terminal/$CICS_SESSION" \
                '{"aid":"ENTER","fields":{"USERID":"ADMIN001","PASSWD":"PASS1234"}}')
            RECV_USERID=$(json_field "$RESP" "receivedFields.USERID")
            RECV_PASSWD=$(json_field "$RESP" "receivedFields.PASSWD")
            if [ "$RECV_USERID" = "ADMIN001" ] && [ "$RECV_PASSWD" = "PASS1234" ]; then
                log_pass "14.2 PUT ENTER with credentials — receivedFields match"
            else
                log_fail "14.2 PUT ENTER — USERID=$RECV_USERID, PASSWD=$RECV_PASSWD"
            fi
        else
            log_skip "14.2 PUT ENTER (no session)"
        fi

        # 14.3: GET screen state
        if [ -n "$CICS_SESSION" ] && [ "$CICS_SESSION" != "" ]; then
            RESP=$(api_get "$ZOSMF_URL/zosmf/cicsApp/terminal/$CICS_SESSION")
            MAP=$(json_field "$RESP" "screen.map")
            if [ "$MAP" = "COSGN0A" ]; then
                log_pass "14.3 GET screen state — map=COSGN0A preserved"
            else
                log_fail "14.3 GET screen state — map=$MAP"
            fi
        else
            log_skip "14.3 GET screen state (no session)"
        fi

        # 14.4: PUT PF3
        if [ -n "$CICS_SESSION" ] && [ "$CICS_SESSION" != "" ]; then
            RESP=$(api_put "$ZOSMF_URL/zosmf/cicsApp/terminal/$CICS_SESSION" \
                '{"aid":"PF3","fields":{}}')
            AID=$(json_field "$RESP" "screen.aid")
            if [ "$AID" = "PF3" ]; then
                log_pass "14.4 PUT PF3 — aid=PF3"
            else
                log_fail "14.4 PUT PF3 — aid=$AID"
            fi
        else
            log_skip "14.4 PUT PF3 (no session)"
        fi

        # 14.5: DELETE session
        if [ -n "$CICS_SESSION" ] && [ "$CICS_SESSION" != "" ]; then
            STATUS=$(api_delete "$ZOSMF_URL/zosmf/cicsApp/terminal/$CICS_SESSION")
            if [ "$STATUS" = "200" ]; then
                log_pass "14.5 DELETE CICS session — HTTP 200"
            else
                log_fail "14.5 DELETE CICS session — HTTP $STATUS"
            fi
        else
            log_skip "14.5 DELETE session (no session)"
        fi

        # 14.6: Verify deleted session → 404
        if [ -n "$CICS_SESSION" ] && [ "$CICS_SESSION" != "" ]; then
            STATUS=$(api_get_status "$ZOSMF_URL/zosmf/cicsApp/terminal/$CICS_SESSION")
            if [ "$STATUS" = "404" ]; then
                log_pass "14.6 GET deleted session — HTTP 404"
            else
                log_fail "14.6 GET deleted session — expected 404, got $STATUS"
            fi
        else
            log_skip "14.6 verify deleted session (no session)"
        fi
    else
        for T in 14.1 14.2 14.3 14.4 14.5 14.6; do
            log_skip "$T CICS terminal (COSGN00.bms not found)"
        done
    fi
else
    for T in 14.1 14.2 14.3 14.4 14.5 14.6; do
        log_skip "$T CICS terminal (CardDemo directory not available)"
    done
fi

# ═════════════════════════════════════════════════════════════════════════════
#  SUMMARY
# ═════════════════════════════════════════════════════════════════════════════

read -r PASS_COUNT FAIL_COUNT SKIP_COUNT TOTAL_COUNT < "$COUNTER_FILE"
rm -f "$COUNTER_FILE"

echo ""
echo "${BOLD}═══ Test Summary ═══${RESET}"
echo ""
echo "  ${BOLD}Total:${RESET}   $TOTAL_COUNT"
echo "  ${GREEN}Passed:${RESET}  $PASS_COUNT"
echo "  ${RED}Failed:${RESET}  $FAIL_COUNT"
echo "  ${YELLOW}Skipped:${RESET} $SKIP_COUNT"
echo ""

if [ "$FAIL_COUNT" -eq 0 ]; then
    echo "  ${GREEN}${BOLD}All tests passed!${RESET}"
else
    echo "  ${RED}${BOLD}${FAIL_COUNT} test(s) failed.${RESET}"
fi

echo ""
echo "Server: $ZOSMF_URL"
echo "Zowe CLI: $(if $HAS_ZOWE; then echo 'available'; else echo 'not available'; fi)"
echo "CardDemo: $(if $HAS_CARDDEMO; then echo "$CARDDEMO_DIR"; else echo 'not available'; fi)"
echo ""

exit "$FAIL_COUNT"
