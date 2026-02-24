#!/usr/bin/env bash
# =============================================================================
#  CardDemo Full Integration Test
#  Tests both JCL batch processing AND CICS screen interaction via z/OSMF REST
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

# ── Helpers ──────────────────────────────────────────────────────────────────

update_counters() {
    # $1 = which counter (1=pass, 2=fail, 3=skip)
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
}
trap cleanup EXIT

# JSON-safe curl wrapper — returns HTTP body
api_get() {
    curl -s "$1" -H "Cookie: jwtToken=$TOKEN" 2>/dev/null || echo '{"error":"curl failed"}'
}

api_post() {
    curl -s -X POST "$1" \
        -H "Cookie: jwtToken=$TOKEN" \
        -H "Content-Type: application/json" \
        -d "$2" 2>/dev/null || echo '{"error":"curl failed"}'
}

api_put() {
    curl -s -X PUT "$1" \
        -H "Cookie: jwtToken=$TOKEN" \
        -H "Content-Type: application/json" \
        -d "$2" 2>/dev/null || echo '{"error":"curl failed"}'
}

api_delete() {
    curl -s -o /dev/null -w '%{http_code}' -X DELETE "$1" \
        -H "Cookie: jwtToken=$TOKEN" 2>/dev/null || echo "000"
}

# ── Python helper for JSON processing ────────────────────────────────────────

# json_field <json> <field>  — extract a top-level or dotted field from JSON
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

# json_field_count <json> <array_field> — count elements in a JSON array
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

# build_bms_request <bms_file> <mapset> [map] [fields_json]
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
#  PHASE 0: PREFLIGHT
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Preflight Checks ═══${RESET}"

if [ ! -d "$CARDDEMO_DIR" ]; then
    echo "${RED}ERROR: CardDemo directory not found at $CARDDEMO_DIR${RESET}"
    echo "Set CARDDEMO_DIR to the carddemo repo root."
    exit 1
fi
log_info "CardDemo directory: $CARDDEMO_DIR"

if [ ! -f "$SERVER_BIN" ]; then
    log_info "Building release binary..."
    (cd "$PROJECT_DIR" && cargo build --release 2>&1 | tail -1)
fi
log_info "Server binary: $SERVER_BIN"

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 1: START SERVER
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Starting z/OSMF Server ═══${RESET}"

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

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 2: AUTHENTICATION
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Authentication ═══${RESET}"

TOKEN=$(curl -sv -X POST "$ZOSMF_URL/zosmf/services/authenticate" \
    -H "Authorization: Basic $(echo -n 'IBMUSER:SYS1' | base64)" \
    -H "Content-Type: application/json" 2>&1 \
    | grep -i set-cookie | sed 's/.*jwtToken=//;s/;.*//')

if [ -z "$TOKEN" ]; then
    echo "${RED}ERROR: Failed to obtain JWT token${RESET}"
    exit 1
fi
log_info "JWT token obtained"

# Quick API health check
INFO=$(api_get "$ZOSMF_URL/zosmf/info")
ZOSMF_VER=$(json_field "$INFO" "zosmf_full_version")
log_info "z/OSMF version: $ZOSMF_VER"

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 3: BMS SCREEN PARSE TEST (all 17 CardDemo screens)
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 3: BMS Screen Parse & Render ═══${RESET}"

BMS_DIR="$CARDDEMO_DIR/app/bms"

# Test each BMS screen: MAPSET|LABEL
BMS_LIST="
COADM01|Admin Menu
COACTUP|Account Update
COACTVW|Account View
COBIL00|Bill Payment
COCRDLI|Credit Card List
COCRDSL|Credit Card Detail
COCRDUP|Credit Card Update
COMEN01|Main Menu
CORPT00|Reports Menu
COSGN00|Login Screen
COTRN00|Transaction List
COTRN01|Transaction Detail
COTRN02|Transaction Add
COUSR00|User List
COUSR01|User Add
COUSR02|User Update
COUSR03|User Delete
"

echo "$BMS_LIST" | while IFS='|' read -r MAPSET LABEL; do
    [ -z "$MAPSET" ] && continue
    BMS_FILE="$BMS_DIR/${MAPSET}.bms"

    if [ ! -f "$BMS_FILE" ]; then
        log_skip "$MAPSET — $LABEL (file not found)"
        continue
    fi

    REQUEST=$(build_bms_request "$BMS_FILE" "$MAPSET")
    RESPONSE=$(api_post "$ZOSMF_URL/zosmf/cicsApp/terminal" "$REQUEST")

    SESSION_KEY=$(json_field "$RESPONSE" "sessionKey")
    MAP_NAME=$(json_field "$RESPONSE" "screen.map")
    FIELD_COUNT=$(json_field_count "$RESPONSE" "screen.fields")
    ROWS=$(json_field "$RESPONSE" "screen.rows")
    COLS=$(json_field "$RESPONSE" "screen.cols")

    if [ -n "$SESSION_KEY" ] && [ "$SESSION_KEY" != "" ] && [ "$ROWS" = "24" ]; then
        log_pass "$MAPSET — $LABEL (map=$MAP_NAME, ${FIELD_COUNT} fields, ${ROWS}x${COLS})"
        # Clean up session
        api_delete "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION_KEY" >/dev/null
    else
        ERROR=$(json_field "$RESPONSE" "message")
        log_fail "$MAPSET — $LABEL ($ERROR)"
    fi
done

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 4: CICS SCREEN INTERACTION FLOW
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 4: CICS Screen Interaction Flow ═══${RESET}"

# Re-authenticate (TOKEN may be lost if subshell was involved)
TOKEN=$(curl -sv -X POST "$ZOSMF_URL/zosmf/services/authenticate" \
    -H "Authorization: Basic $(echo -n 'IBMUSER:SYS1' | base64)" \
    -H "Content-Type: application/json" 2>&1 \
    | grep -i set-cookie | sed 's/.*jwtToken=//;s/;.*//')

# --- Test 1: Login screen — render, input, PF3 ---
REQUEST=$(build_bms_request "$BMS_DIR/COSGN00.bms" "COSGN00" "COSGN0A" \
    '{"TITLE01":"CardDemo - Credit Card Application","TRNNAME":"CSGN","CURDATE":"02/24/26"}')
RESPONSE=$(api_post "$ZOSMF_URL/zosmf/cicsApp/terminal" "$REQUEST")
SESSION=$(json_field "$RESPONSE" "sessionKey")

# Verify login screen fields
USERID_PROT=$(printf '%s\n' "$RESPONSE" | python3 -c "
import sys, json
d = json.loads(sys.stdin.read())
for f in d.get('screen',{}).get('fields',[]):
    if f['name'] == 'USERID':
        print('yes' if f['protected'] else 'no')
        break
" 2>/dev/null)
PASSWD_HIDDEN=$(printf '%s\n' "$RESPONSE" | python3 -c "
import sys, json
d = json.loads(sys.stdin.read())
for f in d.get('screen',{}).get('fields',[]):
    if f['name'] == 'PASSWD':
        print('yes' if f['hidden'] else 'no')
        break
" 2>/dev/null)
CURSOR_ROW=$(json_field "$RESPONSE" "screen.cursorRow")
CURSOR_COL=$(json_field "$RESPONSE" "screen.cursorCol")

if [ "$USERID_PROT" = "no" ] && [ "$PASSWD_HIDDEN" = "yes" ] && \
   [ "$CURSOR_ROW" = "19" ] && [ "$CURSOR_COL" = "43" ]; then
    log_pass "Login screen: USERID=input, PASSWD=hidden, cursor at (19,43)"
else
    log_fail "Login screen: USERID_prot=$USERID_PROT, PASSWD_hidden=$PASSWD_HIDDEN, cursor=($CURSOR_ROW,$CURSOR_COL)"
fi

# --- Test 2: Send credentials via ENTER ---
INPUT_RESP=$(api_put "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION" \
    '{"aid":"ENTER","fields":{"USERID":"ADMIN001","PASSWD":"PASS1234"}}')
RECV_USERID=$(json_field "$INPUT_RESP" "receivedFields.USERID")
RECV_PASSWD=$(json_field "$INPUT_RESP" "receivedFields.PASSWD")
RECV_AID=$(json_field "$INPUT_RESP" "aid")

if [ "$RECV_USERID" = "ADMIN001" ] && [ "$RECV_PASSWD" = "PASS1234" ] && [ "$RECV_AID" = "ENTER" ]; then
    log_pass "Login submit: USERID=ADMIN001, PASSWD=PASS1234, AID=ENTER"
else
    log_fail "Login submit: USERID=$RECV_USERID, PASSWD=$RECV_PASSWD, AID=$RECV_AID"
fi

# --- Test 3: PF3 exit key ---
PF3_RESP=$(api_put "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION" \
    '{"aid":"PF3","fields":{}}')
PF3_AID=$(json_field "$PF3_RESP" "screen.aid")
if [ "$PF3_AID" = "PF3" ]; then
    log_pass "PF3 key: AID correctly set to PF3"
else
    log_fail "PF3 key: expected PF3, got $PF3_AID"
fi

# --- Test 4: GET screen state ---
GET_RESP=$(api_get "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION")
GET_MAP=$(json_field "$GET_RESP" "screen.map")
if [ "$GET_MAP" = "COSGN0A" ]; then
    log_pass "GET screen: map=$GET_MAP preserved across requests"
else
    log_fail "GET screen: expected COSGN0A, got $GET_MAP"
fi

# --- Test 5: DELETE session ---
DEL_STATUS=$(api_delete "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION")
if [ "$DEL_STATUS" = "200" ]; then
    log_pass "DELETE session: HTTP 200"
else
    log_fail "DELETE session: HTTP $DEL_STATUS"
fi

# --- Test 6: Verify 404 after delete ---
GONE_STATUS=$(curl -s -o /dev/null -w '%{http_code}' \
    "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION" \
    -H "Cookie: jwtToken=$TOKEN" 2>/dev/null)
if [ "$GONE_STATUS" = "404" ]; then
    log_pass "Session gone: HTTP 404 after delete"
else
    log_fail "Session gone: expected 404, got $GONE_STATUS"
fi

# --- Test 7: Account View screen with field data ---
REQUEST=$(build_bms_request "$BMS_DIR/COACTVW.bms" "COACTVW" "" \
    '{"ACCESSION":"0000000001","APTS":"S","ACTSTS":"Y","OPNDT":"20200115","EXPDT":"20301231","ADDR1":"123 Main St","CITY":"New York","STATE":"NY","CNTRY":"US"}')
RESPONSE=$(api_post "$ZOSMF_URL/zosmf/cicsApp/terminal" "$REQUEST")
SESSION=$(json_field "$RESPONSE" "sessionKey")
SCREEN_TEXT=$(json_field "$RESPONSE" "screen.text")

HAS_TEXT=$(printf '%s\n' "$SCREEN_TEXT" | python3 -c "
import sys
text = sys.stdin.read()
# Verify some field data appears in screen
print('yes' if '123 Main St' in text or 'New York' in text else 'no')
" 2>/dev/null)

FIELD_COUNT=$(json_field_count "$RESPONSE" "screen.fields")
if [ -n "$SESSION" ] && [ "$FIELD_COUNT" -gt "10" ]; then
    log_pass "Account View: $FIELD_COUNT fields rendered"
else
    log_fail "Account View: session=$SESSION, fields=$FIELD_COUNT"
fi
[ -n "$SESSION" ] && api_delete "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION" >/dev/null

# --- Test 8: Transaction List with multi-field input ---
REQUEST=$(build_bms_request "$BMS_DIR/COTRN00.bms" "COTRN00" "" '{}')
RESPONSE=$(api_post "$ZOSMF_URL/zosmf/cicsApp/terminal" "$REQUEST")
SESSION=$(json_field "$RESPONSE" "sessionKey")
FIELD_COUNT=$(json_field_count "$RESPONSE" "screen.fields")

# Simulate scrolling with PF8
SCROLL_RESP=$(api_put "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION" \
    '{"aid":"PF8","fields":{}}')
SCROLL_AID=$(json_field "$SCROLL_RESP" "screen.aid")

if [ -n "$SESSION" ] && [ "$SCROLL_AID" = "PF8" ]; then
    log_pass "Transaction List: $FIELD_COUNT fields, PF8 scroll processed"
else
    log_fail "Transaction List: session=$SESSION, scroll_aid=$SCROLL_AID"
fi
[ -n "$SESSION" ] && api_delete "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION" >/dev/null

# --- Test 9: Credit Card List screen ---
REQUEST=$(build_bms_request "$BMS_DIR/COCRDLI.bms" "COCRDLI" "" '{}')
RESPONSE=$(api_post "$ZOSMF_URL/zosmf/cicsApp/terminal" "$REQUEST")
SESSION=$(json_field "$RESPONSE" "sessionKey")
FIELD_COUNT=$(json_field_count "$RESPONSE" "screen.fields")

if [ -n "$SESSION" ] && [ "$FIELD_COUNT" -gt "20" ]; then
    log_pass "Credit Card List: $FIELD_COUNT fields"
else
    log_fail "Credit Card List: session=$SESSION, fields=$FIELD_COUNT"
fi
[ -n "$SESSION" ] && api_delete "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION" >/dev/null

# --- Test 10: User Management CRUD screens ---
for SCREEN in COUSR00 COUSR01 COUSR02 COUSR03; do
    REQUEST=$(build_bms_request "$BMS_DIR/${SCREEN}.bms" "$SCREEN" "" '{}')
    RESPONSE=$(api_post "$ZOSMF_URL/zosmf/cicsApp/terminal" "$REQUEST")
    SESSION=$(json_field "$RESPONSE" "sessionKey")
    MAP=$(json_field "$RESPONSE" "screen.map")
    FC=$(json_field_count "$RESPONSE" "screen.fields")

    LABEL="User Mgmt"
    case $SCREEN in
        COUSR00) LABEL="User List" ;;
        COUSR01) LABEL="User Add" ;;
        COUSR02) LABEL="User Update" ;;
        COUSR03) LABEL="User Delete" ;;
    esac

    if [ -n "$SESSION" ] && [ "$FC" -gt "5" ]; then
        log_pass "$SCREEN — $LABEL (map=$MAP, $FC fields)"
    else
        log_fail "$SCREEN — $LABEL (session=$SESSION, fields=$FC)"
    fi
    [ -n "$SESSION" ] && api_delete "$ZOSMF_URL/zosmf/cicsApp/terminal/$SESSION" >/dev/null
done

# ═════════════════════════════════════════════════════════════════════════════
#  PHASE 5: JCL BATCH PROCESSING
# ═════════════════════════════════════════════════════════════════════════════

echo ""
echo "${BOLD}═══ Phase 5: JCL Batch Processing ═══${RESET}"

# Use the dedicated JCL test script (it handles its own server lifecycle)
JCL_SCRIPT="$SCRIPT_DIR/test-carddemo-jcl.sh"
if [ -f "$JCL_SCRIPT" ] && command -v zowe &>/dev/null; then
    log_info "Running JCL batch test suite (separate server instance)..."
    # Stop our server so the JCL script can start its own
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
    SERVER_PID=""
    sleep 1

    JCL_OUTPUT=$("$JCL_SCRIPT" 2>&1) || true

    # Parse results from the JCL script output (use printf to preserve escapes)
    JCL_PASSED=$(printf '%s\n' "$JCL_OUTPUT" | grep -c '\[PASS\]' || true)
    JCL_FAILED=$(printf '%s\n' "$JCL_OUTPUT" | grep -c '\[FAIL\]' || true)
    JCL_SKIPPED=$(printf '%s\n' "$JCL_OUTPUT" | grep -c '\[SKIP\]' || true)
    : "${JCL_PASSED:=0}" "${JCL_FAILED:=0}" "${JCL_SKIPPED:=0}"

    # Display the JCL results
    printf '%s\n' "$JCL_OUTPUT" | grep -E '\[(PASS|FAIL|SKIP)\]' || true

    # Update our counters — add JCL counts directly to counter file
    read -r _p _f _s _t < "$COUNTER_FILE"
    _p=$((_p + JCL_PASSED))
    _f=$((_f + JCL_FAILED))
    _s=$((_s + JCL_SKIPPED))
    _t=$((_p + _f + _s))
    echo "$_p $_f $_s $_t" > "$COUNTER_FILE"

    log_info "JCL batch: ${JCL_PASSED} passed, ${JCL_FAILED} failed, ${JCL_SKIPPED} skipped"
else
    if ! command -v zowe &>/dev/null; then
        log_skip "Zowe CLI not found — skipping JCL batch tests"
    else
        log_skip "JCL test script not found — skipping"
    fi
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
echo "CardDemo: $CARDDEMO_DIR"

exit "$FAIL_COUNT"
