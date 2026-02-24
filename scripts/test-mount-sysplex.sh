#!/usr/bin/env bash
#
# test-mount-sysplex.sh — Integration tests for Mount Bridge and Multisystem features
#
# Tests:
#   Phase 1: Server startup with mount flags
#   Phase 2: Mounted PDS dataset operations (list members, read, write)
#   Phase 3: Mounted sequential dataset operations
#   Phase 4: Mounted USS filesystem operations
#   Phase 5: Mount/unmount via REST API + file_filter
#   Phase 6: Multisystem topology
#   Phase 7: JCL job with mounted dataset + IDCAMS LISTCAT
#   Phase 8: Config file mounts with sysplex
#   Phase 9: System-targeted jobs (ROUTE XEQ, private DASD)
#   Phase 10: Shared DASD + JES2 spool isolation
#
# Usage:
#   bash scripts/test-mount-sysplex.sh

set -euo pipefail

OMFRAME_DIR="${OMFRAME_DIR:-$(cd "$(dirname "$0")/.." && pwd)}"
ZOSMF_HOST="localhost"
ZOSMF_PORT="${ZOSMF_PORT:-10444}"
ZOSMF_USER="IBMUSER"
ZOSMF_PASS="SYS1"
ZOSMF_BASE_URL="http://${ZOSMF_HOST}:${ZOSMF_PORT}"
SERVER_PID=""

# Counters
TOTAL=0
PASSED=0
FAILED=0

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

log_pass() { printf "${GREEN}[PASS]${NC} %s\n" "$1"; TOTAL=$((TOTAL+1)); PASSED=$((PASSED+1)); }
log_fail() { printf "${RED}[FAIL]${NC} %s\n" "$1"; TOTAL=$((TOTAL+1)); FAILED=$((FAILED+1)); }
log_header() { printf "\n${BOLD}═══ %s ═══${NC}\n" "$1"; }
log_info() { printf "${BLUE}[INFO]${NC} %s\n" "$1"; }

# JSON field extraction (portable)
json_field() { python3 -c "import sys,json; d=json.load(sys.stdin); print(d$1)" 2>/dev/null; }
json_field_count() { python3 -c "import sys,json; d=json.load(sys.stdin); print(len(d$1))" 2>/dev/null; }

api_get()    { curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" "$@"; }
api_put()    { curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" -X PUT "$@"; }
api_post()   { curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" -X POST "$@"; }
api_delete() { curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" -X DELETE "$@"; }

cleanup() {
    if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
        log_info "Stopping z/OSMF server (PID $SERVER_PID)..."
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
    # Clean up temp test directories
    rm -rf "$MOUNT_PDS_DIR" "$MOUNT_SEQ_FILE" "$MOUNT_USS_DIR" "$MOUNT_CONFIG" 2>/dev/null || true
}
trap cleanup EXIT

# ─── Setup test data ────────────────────────────────────────────────────────

log_header "Setup"

# Create temp directories for mount sources
MOUNT_PDS_DIR=$(mktemp -d -t mount-pds-XXXX)
MOUNT_USS_DIR=$(mktemp -d -t mount-uss-XXXX)
MOUNT_SEQ_FILE=$(mktemp -t mount-seq-XXXX)
MOUNT_CONFIG=$(mktemp -t mount-config-XXXX.toml)

# Create COBOL source files in PDS mount directory
printf '       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYROLL.\n       PROCEDURE DIVISION.\n           DISPLAY "PAYROLL PROCESSING".\n           STOP RUN.\n' \
    > "$MOUNT_PDS_DIR/payroll.cbl"
printf '       IDENTIFICATION DIVISION.\n       PROGRAM-ID. REPORTS.\n       PROCEDURE DIVISION.\n           DISPLAY "REPORTS PROCESSING".\n           STOP RUN.\n' \
    > "$MOUNT_PDS_DIR/reports.cbl"
printf '       IDENTIFICATION DIVISION.\n       PROGRAM-ID. UTILITY.\n       PROCEDURE DIVISION.\n           DISPLAY "UTILITY PROCESSING".\n           STOP RUN.\n' \
    > "$MOUNT_PDS_DIR/utility.cbl"
# Non-COBOL file (should be filtered out when filter is active)
echo "This is a readme" > "$MOUNT_PDS_DIR/readme.txt"

# Create sequential dataset content
echo "SEQUENTIAL DATASET CONTENT LINE 1" > "$MOUNT_SEQ_FILE"
echo "SEQUENTIAL DATASET CONTENT LINE 2" >> "$MOUNT_SEQ_FILE"

# Create USS mount directory with files
mkdir -p "$MOUNT_USS_DIR/src"
echo "int main() { return 0; }" > "$MOUNT_USS_DIR/src/main.c"
echo "# Project README" > "$MOUNT_USS_DIR/README.md"

log_info "PDS mount source: $MOUNT_PDS_DIR ($(ls "$MOUNT_PDS_DIR" | wc -l | tr -d ' ') files)"
log_info "SEQ mount source: $MOUNT_SEQ_FILE"
log_info "USS mount source: $MOUNT_USS_DIR"

# ─── Build server ───────────────────────────────────────────────────────────

SERVER_BIN="$OMFRAME_DIR/target/debug/zosmf-server"
if [[ ! -x "$SERVER_BIN" ]]; then
    log_info "Building z/OSMF server..."
    cargo build --package open-mainframe-zosmf --quiet 2>/dev/null
fi

# Kill any stale server on our port
lsof -ti :"$ZOSMF_PORT" 2>/dev/null | xargs kill 2>/dev/null || true
sleep 0.5

# ─── Phase 1: Server startup with mount flags ──────────────────────────────

log_header "Phase 1: Server Startup with Mounts"

ZOSMF_PORT=$ZOSMF_PORT "$SERVER_BIN" \
    --mount-dataset "${MOUNT_PDS_DIR}:MOUNT.COBOL.SRC:pds" \
    --mount-dataset "${MOUNT_SEQ_FILE}:MOUNT.SEQ.DATA:seq" \
    --mount-uss "${MOUNT_USS_DIR}:/u/ibmuser/mounted" \
    &
SERVER_PID=$!
sleep 1

# Wait for server ready
READY=false
for i in $(seq 1 20); do
    if curl -sf -o /dev/null "$ZOSMF_BASE_URL/zosmf/info" 2>/dev/null; then
        READY=true
        break
    fi
    sleep 0.5
done

if $READY; then
    log_pass "Server started with mount flags (PID $SERVER_PID)"
else
    log_fail "Server failed to start"
    exit 1
fi

# Verify mounts appear in filesystem list
MFS_COUNT=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/mfs" | json_field_count "['items']")
if [[ "$MFS_COUNT" -ge 5 ]]; then
    log_pass "Mount table has $MFS_COUNT entries (2 system + 3 external)"
else
    log_fail "Mount table has $MFS_COUNT entries (expected >= 5)"
fi

# ─── Phase 2: Mounted PDS dataset operations ───────────────────────────────

log_header "Phase 2: Mounted PDS Dataset"

# 2.1: List mounted PDS members
MEMBERS=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/ds/MOUNT.COBOL.SRC/member" \
    -H "X-IBM-Attributes: member")
MEM_COUNT=$(printf '%s' "$MEMBERS" | json_field_count "['items']")
if [[ "$MEM_COUNT" -ge 3 ]]; then
    log_pass "List PDS members: $MEM_COUNT members found"
else
    log_fail "List PDS members: $MEM_COUNT (expected >= 3)"
fi

# 2.2: Read a specific member
CONTENT=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/ds/MOUNT.COBOL.SRC(PAYROLL)" \
    -H "Content-Type: text/plain")
if printf '%s' "$CONTENT" | grep -q "PAYROLL"; then
    log_pass "Read PDS member PAYROLL — content matches"
else
    log_fail "Read PDS member PAYROLL — content mismatch"
fi

# 2.3: Read another member
CONTENT=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/ds/MOUNT.COBOL.SRC(REPORTS)" \
    -H "Content-Type: text/plain")
if printf '%s' "$CONTENT" | grep -q "REPORTS"; then
    log_pass "Read PDS member REPORTS — content matches"
else
    log_fail "Read PDS member REPORTS — content mismatch"
fi

# 2.4: Write a new member to mounted PDS
api_put "$ZOSMF_BASE_URL/zosmf/restfiles/ds/MOUNT.COBOL.SRC(NEWMEM)" \
    -H "Content-Type: text/plain" \
    -d "       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEWMEM." > /dev/null
if [[ -f "$MOUNT_PDS_DIR/NEWMEM" ]]; then
    log_pass "Write new PDS member — file created on host"
else
    log_fail "Write new PDS member — file NOT created on host"
fi

# 2.5: Verify dataset appears in listing
DS_LIST=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/ds?dslevel=MOUNT.*")
if printf '%s' "$DS_LIST" | grep -qi "MOUNT.COBOL.SRC"; then
    log_pass "Mounted dataset appears in dataset listing"
else
    log_fail "Mounted dataset NOT in dataset listing"
fi

# ─── Phase 3: Mounted sequential dataset ───────────────────────────────────

log_header "Phase 3: Mounted Sequential Dataset"

# 3.1: Read sequential dataset
SEQ_CONTENT=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/ds/MOUNT.SEQ.DATA" \
    -H "Content-Type: text/plain")
if printf '%s' "$SEQ_CONTENT" | grep -q "SEQUENTIAL DATASET CONTENT LINE 1"; then
    log_pass "Read sequential dataset — content matches"
else
    log_fail "Read sequential dataset — content mismatch"
fi

# 3.2: Sequential dataset appears in listing
if printf '%s' "$DS_LIST" | grep -qi "MOUNT.SEQ.DATA"; then
    log_pass "Sequential mounted dataset in listing"
else
    log_fail "Sequential mounted dataset NOT in listing"
fi

# ─── Phase 4: Mounted USS filesystem ───────────────────────────────────────

log_header "Phase 4: Mounted USS Filesystem"

# 4.1: List mounted USS directory
USS_LIST=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/fs/u/ibmuser/mounted")
if printf '%s' "$USS_LIST" | grep -q "README.md"; then
    log_pass "List mounted USS directory — README.md found"
else
    log_fail "List mounted USS directory — README.md not found"
fi

# 4.2: Read a file from mounted USS
USS_CONTENT=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/fs/u/ibmuser/mounted/src/main.c")
if printf '%s' "$USS_CONTENT" | grep -q "int main"; then
    log_pass "Read mounted USS file — content matches"
else
    log_fail "Read mounted USS file — content mismatch"
fi

# 4.3: Write a new file to mounted USS
api_put "$ZOSMF_BASE_URL/zosmf/restfiles/fs/u/ibmuser/mounted/newfile.txt" \
    -H "Content-Type: text/plain" \
    -d "Hello from OpenMainframe!" > /dev/null
if [[ -f "$MOUNT_USS_DIR/newfile.txt" ]]; then
    WRITTEN=$(cat "$MOUNT_USS_DIR/newfile.txt")
    if printf '%s' "$WRITTEN" | grep -q "Hello from OpenMainframe"; then
        log_pass "Write to mounted USS — file created and content matches"
    else
        log_fail "Write to mounted USS — file created but content wrong"
    fi
else
    log_fail "Write to mounted USS — file NOT created on host"
fi

# 4.4: List subdirectory
USS_SUBDIR=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/fs/u/ibmuser/mounted/src")
if printf '%s' "$USS_SUBDIR" | grep -q "main.c"; then
    log_pass "List mounted USS subdirectory — main.c found"
else
    log_fail "List mounted USS subdirectory — main.c not found"
fi

# ─── Phase 5: Mount/unmount via REST API ────────────────────────────────────

log_header "Phase 5: REST API Mount/Unmount"

# 5.1: Create a new USS mount via REST API
API_MOUNT_DIR=$(mktemp -d -t api-mount-XXXX)
echo "API mounted content" > "$API_MOUNT_DIR/apifile.txt"

HTTP_CODE=$(curl -sf -o /dev/null -w "%{http_code}" \
    -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restfiles/mfs/API.MOUNT" \
    -H "Content-Type: application/json" \
    -d "{\"action\":\"mount\",\"mount-type\":\"uss\",\"host-path\":\"${API_MOUNT_DIR}\",\"mount-point\":\"/u/ibmuser/apimount\"}")
if [[ "$HTTP_CODE" == "204" ]]; then
    log_pass "REST mount created (HTTP $HTTP_CODE)"
else
    log_fail "REST mount creation failed (HTTP $HTTP_CODE)"
fi

# 5.2: Verify the API mount works
API_LIST=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/fs/u/ibmuser/apimount" 2>/dev/null || echo "")
if printf '%s' "$API_LIST" | grep -q "apifile.txt"; then
    log_pass "API-mounted USS directory accessible — apifile.txt found"
else
    log_fail "API-mounted USS directory not accessible"
fi

# 5.3: Unmount via REST API (use mount ID from the MFS listing)
# Find the mount ID for our API mount
MOUNT_ID=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/mfs" | \
    python3 -c "import sys,json; items=json.load(sys.stdin)['items']; print(next((i['name'] for i in items if '/u/ibmuser/apimount' in i.get('mountPoint','')), 'NOTFOUND'))")
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
    -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restfiles/mfs/${MOUNT_ID}" \
    -H "Content-Type: application/json" \
    -d '{"action":"unmount"}')
if [[ "$HTTP_CODE" == "204" ]]; then
    log_pass "REST unmount succeeded (HTTP $HTTP_CODE)"
else
    log_fail "REST unmount failed (HTTP $HTTP_CODE, mount=$MOUNT_ID)"
fi

rm -rf "$API_MOUNT_DIR"

# 5.4: Mount PDS with file_filter via REST API
FILTER_DIR=$(mktemp -d -t filter-mount-XXXX)
echo "       IDENTIFICATION DIVISION." > "$FILTER_DIR/ALPHA.cbl"
echo "       IDENTIFICATION DIVISION." > "$FILTER_DIR/BETA.cbl"
echo "This is a readme" > "$FILTER_DIR/README.txt"
echo "data file" > "$FILTER_DIR/DATA.dat"

HTTP_CODE=$(curl -sf -o /dev/null -w "%{http_code}" \
    -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restfiles/mfs/FILTER.TEST.PDS" \
    -H "Content-Type: application/json" \
    -d "{\"action\":\"mount\",\"mount-type\":\"dataset-pds\",\"host-path\":\"${FILTER_DIR}\",\"mount-point\":\"FILTER.TEST.PDS\",\"file-filter\":\"*.cbl\"}")
if [[ "$HTTP_CODE" == "204" ]]; then
    log_pass "Mount with file_filter created (HTTP $HTTP_CODE)"
else
    log_fail "Mount with file_filter failed (HTTP $HTTP_CODE)"
fi

# 5.5: List members with filter — only .cbl files should appear
FILTER_MEMBERS=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/ds/FILTER.TEST.PDS/member" \
    -H "X-IBM-Attributes: member")
FILTER_COUNT=$(printf '%s' "$FILTER_MEMBERS" | json_field_count "['items']")
if [[ "$FILTER_COUNT" -eq 2 ]]; then
    log_pass "File filter: only $FILTER_COUNT members (ALPHA, BETA — .txt/.dat excluded)"
else
    log_fail "File filter: $FILTER_COUNT members (expected 2, only .cbl files)"
fi

# 5.6: Verify filtered-out files are not readable as members
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
    -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    "$ZOSMF_BASE_URL/zosmf/restfiles/ds/FILTER.TEST.PDS(README)" \
    -H "Content-Type: text/plain")
if [[ "$HTTP_CODE" == "404" || "$HTTP_CODE" == "500" ]]; then
    log_pass "Filtered-out member README not accessible (HTTP $HTTP_CODE)"
else
    log_fail "Filtered-out member README should not be accessible (HTTP $HTTP_CODE)"
fi

# Clean up filter mount
curl -s -o /dev/null -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restfiles/mfs/FILTER.TEST.PDS" \
    -H "Content-Type: application/json" \
    -d '{"action":"unmount"}'
rm -rf "$FILTER_DIR"

# ─── Phase 6: Multisystem Topology ─────────────────────────────────────────

log_header "Phase 6: Multisystem Topology"

# 6.1: Default topology returns at least 1 system
TOPO=$(api_get "$ZOSMF_BASE_URL/zosmf/resttopology/systems")
TOPO_COUNT=$(printf '%s' "$TOPO" | json_field "['numRows']")
if [[ "$TOPO_COUNT" -ge 1 ]]; then
    log_pass "Topology lists $TOPO_COUNT system(s)"
else
    log_fail "Topology lists $TOPO_COUNT systems (expected >= 1)"
fi

# 6.2: Default system is SYS1
SYS1_NAME=$(printf '%s' "$TOPO" | json_field "['items'][0]['sysname']")
if [[ "$SYS1_NAME" == "SYS1" ]]; then
    log_pass "Default system is SYS1"
else
    log_fail "Default system is '$SYS1_NAME' (expected SYS1)"
fi

# 6.3: Get specific system by name
SYS_DETAIL=$(api_get "$ZOSMF_BASE_URL/zosmf/resttopology/systems/SYS1")
SYS_STATUS=$(printf '%s' "$SYS_DETAIL" | json_field "['status']")
if [[ "$SYS_STATUS" == "active" ]]; then
    log_pass "System SYS1 status is active"
else
    log_fail "System SYS1 status is '$SYS_STATUS' (expected active)"
fi

# 6.4: Unknown system returns 404
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
    -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    "$ZOSMF_BASE_URL/zosmf/resttopology/systems/UNKNOWN")
if [[ "$HTTP_CODE" == "404" ]]; then
    log_pass "Unknown system returns HTTP 404"
else
    log_fail "Unknown system returns HTTP $HTTP_CODE (expected 404)"
fi

# 6.5: Sysplex name in topology response
SYSPLEX_NAME=$(printf '%s' "$SYS_DETAIL" | json_field "['sysplex']")
if [[ "$SYSPLEX_NAME" == "LOCAL" ]]; then
    log_pass "Sysplex name is LOCAL (default)"
else
    log_fail "Sysplex name is '$SYSPLEX_NAME' (expected LOCAL)"
fi

# ─── Phase 7: JCL Job with Mounted Dataset ────────────────────────────────

log_header "Phase 7: JCL Job with Mounted Dataset"

# 7.1: Submit JCL that reads from mounted PDS via IEBGENER
JCL_BODY=$(cat <<'ENDJCL'
//MOUNTJOB JOB (ACCT),'MOUNT TEST',CLASS=A
//STEP1   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MOUNT.COBOL.SRC(PAYROLL),DISP=SHR
//SYSUT2   DD DSN=IBMUSER.COPY.OUTPUT,DISP=(NEW,CATLG)
ENDJCL
)

SUBMIT_RESP=$(curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restjobs/jobs" \
    -H "Content-Type: text/plain" \
    -d "$JCL_BODY" 2>/dev/null || echo '{}')
JOBID=$(printf '%s' "$SUBMIT_RESP" | json_field "['jobid']" 2>/dev/null || echo "")
RETCODE=$(printf '%s' "$SUBMIT_RESP" | json_field "['retcode']" 2>/dev/null || echo "")
if [[ -n "$JOBID" && "$JOBID" != "None" ]]; then
    log_pass "JCL job submitted (jobid=$JOBID)"
else
    log_fail "JCL job submission failed"
fi

# 7.2: Check job completed with CC 0000 (read from mounted dataset)
if printf '%s' "$RETCODE" | grep -q "CC 0000"; then
    log_pass "JCL job completed with $RETCODE (read from mount)"
else
    log_fail "JCL job retcode='$RETCODE' (expected CC 0000)"
fi

# 7.3: IDCAMS LISTCAT includes mounted datasets
AMS_RESP=$(curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restfiles/ams" \
    -H "Content-Type: application/json" \
    -d '{"input":["LISTCAT LEVEL(MOUNT)"]}' 2>/dev/null || echo '{}')
AMS_OUTPUT=$(printf '%s' "$AMS_RESP" | json_field "['output']" 2>/dev/null || echo "")
if printf '%s' "$AMS_OUTPUT" | grep -qi "MOUNT.COBOL.SRC"; then
    log_pass "IDCAMS LISTCAT includes mounted dataset MOUNT.COBOL.SRC"
else
    log_fail "IDCAMS LISTCAT missing mounted dataset (output: $AMS_OUTPUT)"
fi

# ─── Phase 8: Config File Mounts ──────────────────────────────────────────

log_header "Phase 8: Config File Mounts"

# Stop the current server
kill "$SERVER_PID" 2>/dev/null || true
wait "$SERVER_PID" 2>/dev/null || true
sleep 0.5

# Create a config file with mounts, sysplex definition, and per-system dataset dirs
CFG_MOUNT_DIR=$(mktemp -d -t cfg-mount-XXXX)
echo "Config-mounted COBOL" > "$CFG_MOUNT_DIR/program.cbl"

# Per-system dataset directories for isolation testing
SYSA_DS_DIR=$(mktemp -d -t sysa-datasets-XXXX)
SYSB_DS_DIR=$(mktemp -d -t sysb-datasets-XXXX)

cat > "$MOUNT_CONFIG" <<EOF
[server]
host = "127.0.0.1"
port = $ZOSMF_PORT

[uss]
root_directory = "$(mktemp -d -t cfg-uss-XXXX)"

[sysplex]
name = "TESTPLEX"

[[sysplex.systems]]
sysname = "SYSA"
sysclone = "SA"
dataset_dir = "$SYSA_DS_DIR"

[[sysplex.systems]]
sysname = "SYSB"
sysclone = "SB"
dataset_dir = "$SYSB_DS_DIR"

[[mounts]]
type = "dataset-pds"
host_path = "$CFG_MOUNT_DIR"
virtual_path = "CONFIG.COBOL.SRC"
read_only = false

[[mounts]]
type = "uss"
host_path = "$CFG_MOUNT_DIR"
virtual_path = "/u/config/data"
EOF

ZOSMF_PORT=$ZOSMF_PORT "$SERVER_BIN" --config "$MOUNT_CONFIG" &
SERVER_PID=$!
sleep 1

READY=false
for i in $(seq 1 20); do
    if curl -sf -o /dev/null "$ZOSMF_BASE_URL/zosmf/info" 2>/dev/null; then
        READY=true
        break
    fi
    sleep 0.5
done

if $READY; then
    log_pass "Server started with config file"
else
    log_fail "Server failed to start with config file"
    TOTAL=$((TOTAL+1)); FAILED=$((FAILED+1))
    # Skip remaining Phase 8 tests
    printf "\n${BOLD}═══ Test Summary ═══${NC}\n\n"
    printf "  ${BOLD}Total:${NC}   %d\n" "$TOTAL"
    printf "  ${GREEN}Passed:${NC}  %d\n" "$PASSED"
    printf "  ${RED}Failed:${NC}  %d\n" "$FAILED"
    exit "$FAILED"
fi

# 8.1: Config-mounted PDS is accessible
CFG_MEMBERS=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/ds/CONFIG.COBOL.SRC/member" \
    -H "X-IBM-Attributes: member" 2>/dev/null || echo '{"items":[]}')
CFG_MEM_COUNT=$(printf '%s' "$CFG_MEMBERS" | json_field_count "['items']")
if [[ "$CFG_MEM_COUNT" -ge 1 ]]; then
    log_pass "Config-mounted PDS has $CFG_MEM_COUNT member(s)"
else
    log_fail "Config-mounted PDS has $CFG_MEM_COUNT members (expected >= 1)"
fi

# 8.2: Config-mounted USS is accessible
CFG_USS=$(api_get "$ZOSMF_BASE_URL/zosmf/restfiles/fs/u/config/data" 2>/dev/null || echo "")
if printf '%s' "$CFG_USS" | grep -q "program.cbl"; then
    log_pass "Config-mounted USS directory accessible"
else
    log_fail "Config-mounted USS directory not accessible"
fi

# 8.3: Sysplex has 2 systems from config
CFG_TOPO=$(api_get "$ZOSMF_BASE_URL/zosmf/resttopology/systems")
CFG_TOPO_COUNT=$(printf '%s' "$CFG_TOPO" | json_field "['numRows']")
if [[ "$CFG_TOPO_COUNT" -eq 2 ]]; then
    log_pass "Config sysplex has 2 systems"
else
    log_fail "Config sysplex has $CFG_TOPO_COUNT systems (expected 2)"
fi

# 8.4: First system is SYSA
CFG_SYS1=$(printf '%s' "$CFG_TOPO" | json_field "['items'][0]['sysname']")
if [[ "$CFG_SYS1" == "SYSA" ]]; then
    log_pass "First system in config sysplex is SYSA"
else
    log_fail "First system is '$CFG_SYS1' (expected SYSA)"
fi

# 8.5: Second system is SYSB
CFG_SYS2=$(printf '%s' "$CFG_TOPO" | json_field "['items'][1]['sysname']")
if [[ "$CFG_SYS2" == "SYSB" ]]; then
    log_pass "Second system in config sysplex is SYSB"
else
    log_fail "Second system is '$CFG_SYS2' (expected SYSB)"
fi

# 8.6: Sysplex name is TESTPLEX
CFG_PLEX=$(printf '%s' "$CFG_TOPO" | json_field "['items'][0]['sysplex']")
if [[ "$CFG_PLEX" == "TESTPLEX" ]]; then
    log_pass "Sysplex name is TESTPLEX from config"
else
    log_fail "Sysplex name is '$CFG_PLEX' (expected TESTPLEX)"
fi

# ─── Phase 9: System-Targeted Jobs ─────────────────────────────────────────

log_header "Phase 9: System-Targeted Jobs"

# 9.1: Submit job targeted at SYSA via X-IBM-Target-System header
JCL_SYSA=$(cat <<'ENDJCL'
//SYSAJOB  JOB (ACCT),'SYSA TEST',CLASS=A
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
DATA FOR SYSA
/*
//SYSUT2   DD DSN=SYSA.OUTPUT.DATA,DISP=(NEW,CATLG)
ENDJCL
)

SYSA_RESP=$(curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restjobs/jobs" \
    -H "Content-Type: text/plain" \
    -H "X-IBM-Target-System: SYSA" \
    -d "$JCL_SYSA" 2>/dev/null || echo '{}')
SYSA_RC=$(printf '%s' "$SYSA_RESP" | json_field "['retcode']" 2>/dev/null || echo "")
SYSA_SYS=$(printf '%s' "$SYSA_RESP" | json_field "['exec-system']" 2>/dev/null || echo "None")
if printf '%s' "$SYSA_RC" | grep -q "CC 0000"; then
    log_pass "Job targeted at SYSA completed with $SYSA_RC"
else
    log_fail "Job targeted at SYSA: retcode='$SYSA_RC' (expected CC 0000)"
fi

# 9.2: Verify exec-system shows SYSA
if [[ "$SYSA_SYS" == "SYSA" ]]; then
    log_pass "Job exec-system is SYSA"
else
    log_fail "Job exec-system is '$SYSA_SYS' (expected SYSA)"
fi

# 9.3: Verify SYSA job created output in SYSA's dataset directory
if [[ -d "$SYSA_DS_DIR/SYSA/OUTPUT" ]]; then
    log_pass "Job output created in SYSA dataset directory"
else
    log_fail "Job output NOT in SYSA dataset directory ($SYSA_DS_DIR)"
fi

# 9.4: Submit job targeted at SYSB via /*ROUTE XEQ
JCL_SYSB=$(cat <<'ENDJCL'
/*ROUTE XEQ SYSB
//SYSBJOB  JOB (ACCT),'SYSB TEST',CLASS=A
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
DATA FOR SYSB
/*
//SYSUT2   DD DSN=SYSB.OUTPUT.DATA,DISP=(NEW,CATLG)
ENDJCL
)

SYSB_RESP=$(curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restjobs/jobs" \
    -H "Content-Type: text/plain" \
    -d "$JCL_SYSB" 2>/dev/null || echo '{}')
SYSB_RC=$(printf '%s' "$SYSB_RESP" | json_field "['retcode']" 2>/dev/null || echo "")
SYSB_SYS=$(printf '%s' "$SYSB_RESP" | json_field "['exec-system']" 2>/dev/null || echo "None")
if printf '%s' "$SYSB_RC" | grep -q "CC 0000"; then
    log_pass "Job targeted at SYSB (via ROUTE XEQ) completed with $SYSB_RC"
else
    log_fail "Job targeted at SYSB: retcode='$SYSB_RC' (expected CC 0000)"
fi

# 9.5: Verify exec-system shows SYSB
if [[ "$SYSB_SYS" == "SYSB" ]]; then
    log_pass "Job exec-system is SYSB"
else
    log_fail "Job exec-system is '$SYSB_SYS' (expected SYSB)"
fi

# 9.6: Private DASD — SYSA's output not in SYSB's directory
if [[ ! -d "$SYSB_DS_DIR/SYSA" ]]; then
    log_pass "Private DASD: SYSA output NOT visible in SYSB directory"
else
    log_fail "Private DASD: SYSA directory found in SYSB's dataset_dir"
fi

# 9.7: Invalid target system returns 404
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
    -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restjobs/jobs" \
    -H "Content-Type: text/plain" \
    -H "X-IBM-Target-System: NOSYSTEM" \
    -d "//BADJOB JOB\n//STEP1 EXEC PGM=IEFBR14")
if [[ "$HTTP_CODE" == "404" ]]; then
    log_pass "Invalid target system returns HTTP 404"
else
    log_fail "Invalid target system returns HTTP $HTTP_CODE (expected 404)"
fi

rm -rf "$CFG_MOUNT_DIR"

# ─── Phase 10: Shared DASD + JES2 Spool Isolation ──────────────────────────

log_header "Phase 10: Shared DASD + Spool Isolation"

# Stop current server, start one with shared DASD (both systems use same directory)
kill "$SERVER_PID" 2>/dev/null || true
wait "$SERVER_PID" 2>/dev/null || true
sleep 0.5

SHARED_DS_DIR=$(mktemp -d -t shared-datasets-XXXX)
SHARED_CONFIG=$(mktemp -t shared-config-XXXX.toml)

cat > "$SHARED_CONFIG" <<EOF
[server]
host = "127.0.0.1"
port = $ZOSMF_PORT

[uss]
root_directory = "$(mktemp -d -t shared-uss-XXXX)"

[sysplex]
name = "SHAREDPLEX"

[[sysplex.systems]]
sysname = "PROD1"
sysclone = "P1"
dataset_dir = "$SHARED_DS_DIR"

[[sysplex.systems]]
sysname = "PROD2"
sysclone = "P2"
dataset_dir = "$SHARED_DS_DIR"
EOF

ZOSMF_PORT=$ZOSMF_PORT "$SERVER_BIN" --config "$SHARED_CONFIG" &
SERVER_PID=$!
sleep 1

READY=false
for i in $(seq 1 20); do
    if curl -sf -o /dev/null "$ZOSMF_BASE_URL/zosmf/info" 2>/dev/null; then
        READY=true
        break
    fi
    sleep 0.5
done

if $READY; then
    log_pass "Server started with shared DASD config"
else
    log_fail "Server failed to start with shared DASD config"
    TOTAL=$((TOTAL+1)); FAILED=$((FAILED+1))
    printf "\n${BOLD}═══ Test Summary ═══${NC}\n\n"
    printf "  ${BOLD}Total:${NC}   %d\n" "$TOTAL"
    printf "  ${GREEN}Passed:${NC}  %d\n" "$PASSED"
    printf "  ${RED}Failed:${NC}  %d\n" "$FAILED"
    exit "$FAILED"
fi

# 10.1: Create dataset on PROD1
JCL_P1=$(cat <<'ENDJCL'
//PROD1JOB JOB (ACCT),'PROD1 DS',CLASS=A
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
SHARED DATA FROM PROD1
/*
//SYSUT2   DD DSN=SHARED.TEST.DATA,DISP=(NEW,CATLG)
ENDJCL
)

P1_RESP=$(curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restjobs/jobs" \
    -H "Content-Type: text/plain" \
    -H "X-IBM-Target-System: PROD1" \
    -d "$JCL_P1" 2>/dev/null || echo '{}')
P1_RC=$(printf '%s' "$P1_RESP" | json_field "['retcode']" 2>/dev/null || echo "")
if printf '%s' "$P1_RC" | grep -q "CC 0000"; then
    log_pass "PROD1 created dataset SHARED.TEST.DATA"
else
    log_fail "PROD1 job retcode='$P1_RC'"
fi

# 10.2: Shared DASD — dataset created on PROD1 is visible from PROD2
# Since both use the same dataset_dir, PROD2 should see the file
JCL_P2=$(cat <<'ENDJCL'
//PROD2JOB JOB (ACCT),'PROD2 READ',CLASS=A
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=SHARED.TEST.DATA,DISP=SHR
//SYSUT2   DD DSN=SHARED.COPY.DATA,DISP=(NEW,CATLG)
ENDJCL
)

P2_RESP=$(curl -sf -u "${ZOSMF_USER}:${ZOSMF_PASS}" \
    -X PUT "$ZOSMF_BASE_URL/zosmf/restjobs/jobs" \
    -H "Content-Type: text/plain" \
    -H "X-IBM-Target-System: PROD2" \
    -d "$JCL_P2" 2>/dev/null || echo '{}')
P2_RC=$(printf '%s' "$P2_RESP" | json_field "['retcode']" 2>/dev/null || echo "")
if printf '%s' "$P2_RC" | grep -q "CC 0000"; then
    log_pass "Shared DASD: PROD2 read dataset created by PROD1 (CC 0000)"
else
    log_fail "Shared DASD: PROD2 job retcode='$P2_RC' (expected CC 0000)"
fi

# 10.3: JES2 spool isolation — filter jobs by exec-member
PROD1_JOBS=$(api_get "$ZOSMF_BASE_URL/zosmf/restjobs/jobs?owner=*&exec-member=PROD1")
PROD1_COUNT=$(printf '%s' "$PROD1_JOBS" | json_field_count "" 2>/dev/null || echo "0")
if [[ "$PROD1_COUNT" -eq 1 ]]; then
    log_pass "Spool filter: PROD1 has $PROD1_COUNT job(s)"
else
    log_fail "Spool filter: PROD1 has $PROD1_COUNT jobs (expected 1)"
fi

# 10.4: PROD2 also has exactly 1 job
PROD2_JOBS=$(api_get "$ZOSMF_BASE_URL/zosmf/restjobs/jobs?owner=*&exec-member=PROD2")
PROD2_COUNT=$(printf '%s' "$PROD2_JOBS" | json_field_count "" 2>/dev/null || echo "0")
if [[ "$PROD2_COUNT" -eq 1 ]]; then
    log_pass "Spool filter: PROD2 has $PROD2_COUNT job(s)"
else
    log_fail "Spool filter: PROD2 has $PROD2_COUNT jobs (expected 1)"
fi

# 10.5: Total jobs (no system filter) should be 2
ALL_JOBS=$(api_get "$ZOSMF_BASE_URL/zosmf/restjobs/jobs?owner=*")
ALL_COUNT=$(printf '%s' "$ALL_JOBS" | json_field_count "" 2>/dev/null || echo "0")
if [[ "$ALL_COUNT" -eq 2 ]]; then
    log_pass "Total jobs without system filter: $ALL_COUNT"
else
    log_fail "Total jobs: $ALL_COUNT (expected 2)"
fi

rm -rf "$SHARED_DS_DIR" "$SHARED_CONFIG" "$SYSA_DS_DIR" "$SYSB_DS_DIR"

# ─── Summary ────────────────────────────────────────────────────────────────

printf "\n${BOLD}═══ Test Summary ═══${NC}\n\n"
printf "  ${BOLD}Total:${NC}   %d\n" "$TOTAL"
printf "  ${GREEN}Passed:${NC}  %d\n" "$PASSED"
printf "  ${RED}Failed:${NC}  %d\n" "$FAILED"

if [[ "$FAILED" -eq 0 ]]; then
    printf "\n  ${GREEN}${BOLD}All tests passed!${NC}\n"
else
    printf "\n  ${RED}${BOLD}%d test(s) failed${NC}\n" "$FAILED"
fi

exit "$FAILED"
