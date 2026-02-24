#!/usr/bin/env bash
#
# test-carddemo-jcl.sh — Submit all CardDemo JCL jobs to OpenMainframe z/OSMF server
#
# Usage:
#   ./scripts/test-carddemo-jcl.sh [--carddemo-dir DIR] [--port PORT] [--skip-server]
#
# Prerequisites:
#   - OpenMainframe built: cargo build --package open-mainframe-zosmf
#   - Zowe CLI installed (optional, falls back to curl)
#   - CardDemo repository at ~/Desktop/code/carddemo (or specify --carddemo-dir)
#
# What it does:
#   1. Starts the OpenMainframe z/OSMF server
#   2. Configures Zowe CLI profile (or uses curl)
#   3. Submits JCL jobs in dependency order
#   4. Reports pass/fail for each job
#   5. Prints summary

set -euo pipefail

# ─── Configuration ───────────────────────────────────────────────────────────

CARDDEMO_DIR="${CARDDEMO_DIR:-/Users/ThangLT4/Desktop/code/carddemo}"
OMFRAME_DIR="${OMFRAME_DIR:-/Users/ThangLT4/Desktop/code/OpenMainframe}"
ZOSMF_HOST="localhost"
ZOSMF_PORT="${ZOSMF_PORT:-10443}"
ZOSMF_USER="IBMUSER"
ZOSMF_PASS="SYS1"
ZOSMF_BASE_URL="http://${ZOSMF_HOST}:${ZOSMF_PORT}"
PROFILE_NAME="openmainframe"
SKIP_SERVER=false
USE_ZOWE=false
SERVER_PID=""
POLL_INTERVAL=2
POLL_TIMEOUT=60

# Counters
TOTAL=0
PASSED=0
FAILED=0
SKIPPED=0

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# ─── Argument parsing ────────────────────────────────────────────────────────

while [[ $# -gt 0 ]]; do
    case "$1" in
        --carddemo-dir) CARDDEMO_DIR="$2"; shift 2 ;;
        --port) ZOSMF_PORT="$2"; ZOSMF_BASE_URL="http://${ZOSMF_HOST}:${ZOSMF_PORT}"; shift 2 ;;
        --skip-server) SKIP_SERVER=true; shift ;;
        --help|-h)
            echo "Usage: $0 [--carddemo-dir DIR] [--port PORT] [--skip-server]"
            echo ""
            echo "Options:"
            echo "  --carddemo-dir DIR  Path to carddemo repository (default: ~/Desktop/code/carddemo)"
            echo "  --port PORT         z/OSMF server port (default: 10443)"
            echo "  --skip-server       Don't start/stop the server (use existing)"
            exit 0
            ;;
        *) echo "Unknown argument: $1"; exit 1 ;;
    esac
done

# ─── Helper functions ────────────────────────────────────────────────────────

log_info()  { echo -e "${BLUE}[INFO]${NC} $*"; }
log_ok()    { echo -e "${GREEN}[PASS]${NC} $*"; }
log_fail()  { echo -e "${RED}[FAIL]${NC} $*"; }
log_skip()  { echo -e "${YELLOW}[SKIP]${NC} $*"; }
log_header(){ echo -e "\n${BOLD}═══ $* ═══${NC}"; }

cleanup() {
    if [[ -n "$SERVER_PID" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
        log_info "Stopping z/OSMF server (PID $SERVER_PID)..."
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

# Authenticate and get token (curl fallback)
get_auth_token() {
    local creds
    creds=$(echo -n "${ZOSMF_USER}:${ZOSMF_PASS}" | base64)
    local resp
    resp=$(curl -s -w "\n%{http_code}" -X POST \
        "${ZOSMF_BASE_URL}/zosmf/services/authenticate" \
        -H "Authorization: Basic ${creds}" \
        -H "Content-Type: application/json" 2>/dev/null) || true

    local http_code
    http_code=$(echo "$resp" | tail -1)
    if [[ "$http_code" == "200" ]]; then
        # Extract JWT from Set-Cookie header
        AUTH_TOKEN=$(curl -s -D - -o /dev/null -X POST \
            "${ZOSMF_BASE_URL}/zosmf/services/authenticate" \
            -H "Authorization: Basic ${creds}" \
            -H "Content-Type: application/json" 2>/dev/null \
            | grep -i 'set-cookie:' | sed 's/.*jwtToken=\([^;]*\).*/\1/' | tr -d '\r')
        if [[ -n "$AUTH_TOKEN" ]]; then
            return 0
        fi
    fi
    return 1
}

# Submit JCL via curl and return jobid
submit_jcl_curl() {
    local jcl_file="$1"
    local jcl_content
    jcl_content=$(cat "$jcl_file")

    local resp
    resp=$(curl -s -X PUT \
        "${ZOSMF_BASE_URL}/zosmf/restjobs/jobs" \
        -H "Cookie: jwtToken=${AUTH_TOKEN}" \
        -H "Content-Type: text/plain" \
        -d "$jcl_content" 2>/dev/null)

    # Extract jobid and jobname from response
    local jobid jobname
    jobid=$(echo "$resp" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('jobid',''))" 2>/dev/null || echo "")
    jobname=$(echo "$resp" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('jobname',''))" 2>/dev/null || echo "")

    if [[ -n "$jobid" ]]; then
        echo "${jobname}:${jobid}"
        return 0
    else
        echo "ERROR:$(echo "$resp" | head -1)"
        return 1
    fi
}

# Submit JCL via Zowe CLI and return jobid
submit_jcl_zowe() {
    local jcl_file="$1"
    local resp
    resp=$(zowe jobs submit local-file "$jcl_file" --zosmf-profile "$PROFILE_NAME" --response-format-json 2>/dev/null)

    local jobid jobname
    jobid=$(echo "$resp" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('data',{}).get('jobid',''))" 2>/dev/null || echo "")
    jobname=$(echo "$resp" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('data',{}).get('jobname',''))" 2>/dev/null || echo "")

    if [[ -n "$jobid" ]]; then
        echo "${jobname}:${jobid}"
        return 0
    else
        echo "ERROR:$(echo "$resp" | head -1)"
        return 1
    fi
}

# Poll job status until completion or timeout
poll_job_status() {
    local jobname="$1"
    local jobid="$2"
    local elapsed=0

    while [[ $elapsed -lt $POLL_TIMEOUT ]]; do
        local resp status retcode
        if $USE_ZOWE; then
            resp=$(zowe jobs view job-status-by-jobid "$jobid" --zosmf-profile "$PROFILE_NAME" --response-format-json 2>/dev/null || echo '{}')
            status=$(echo "$resp" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('data',{}).get('status','UNKNOWN'))" 2>/dev/null || echo "UNKNOWN")
            retcode=$(echo "$resp" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('data',{}).get('retcode',''))" 2>/dev/null || echo "")
        else
            resp=$(curl -s "${ZOSMF_BASE_URL}/zosmf/restjobs/jobs/${jobname}/${jobid}" \
                -H "Cookie: jwtToken=${AUTH_TOKEN}" 2>/dev/null || echo '{}')
            status=$(echo "$resp" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('status','UNKNOWN'))" 2>/dev/null || echo "UNKNOWN")
            retcode=$(echo "$resp" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('retcode',''))" 2>/dev/null || echo "")
        fi

        if [[ "$status" == "OUTPUT" ]]; then
            echo "$retcode"
            return 0
        elif [[ "$status" == "INPUT" || "$status" == "ACTIVE" ]]; then
            sleep "$POLL_INTERVAL"
            elapsed=$((elapsed + POLL_INTERVAL))
        else
            echo "STATUS:$status"
            return 1
        fi
    done

    echo "TIMEOUT"
    return 1
}

# Submit a JCL file, poll for completion, report result
# Check if retcode matches any pipe-delimited expected value (e.g. "CC 0000|CC 0012")
matches_expected() {
    local retcode="$1"
    local expect="$2"
    local IFS='|'
    for candidate in $expect; do
        if [[ "$retcode" == "$candidate" ]]; then
            return 0
        fi
    done
    return 1
}

run_jcl() {
    local label="$1"
    local jcl_file="$2"
    local expect_rc="${3:-CC 0000}"  # Expected return code (pipe-delimited alternatives)

    TOTAL=$((TOTAL + 1))

    if [[ ! -f "$jcl_file" ]]; then
        log_skip "$label — file not found: $jcl_file"
        SKIPPED=$((SKIPPED + 1))
        return 0
    fi

    # Submit
    local submit_result
    if $USE_ZOWE; then
        submit_result=$(submit_jcl_zowe "$jcl_file") || true
    else
        submit_result=$(submit_jcl_curl "$jcl_file") || true
    fi

    if [[ "$submit_result" == ERROR:* ]]; then
        log_fail "$label — submit failed: ${submit_result#ERROR:}"
        FAILED=$((FAILED + 1))
        return 0
    fi

    local jobname jobid
    jobname="${submit_result%%:*}"
    jobid="${submit_result##*:}"

    # Poll for completion
    local retcode
    retcode=$(poll_job_status "$jobname" "$jobid") || true

    if [[ "$retcode" == "TIMEOUT" ]]; then
        log_fail "$label ($jobid) — timed out after ${POLL_TIMEOUT}s"
        FAILED=$((FAILED + 1))
    elif [[ "$retcode" == STATUS:* ]]; then
        log_fail "$label ($jobid) — unexpected status: ${retcode#STATUS:}"
        FAILED=$((FAILED + 1))
    elif matches_expected "$retcode" "$expect_rc" || [[ "$retcode" == "CC 0000" ]]; then
        log_ok "$label ($jobid) — $retcode"
        PASSED=$((PASSED + 1))
    elif [[ "$retcode" == "CC 0004" ]]; then
        # RC=4 is a warning, usually acceptable
        log_ok "$label ($jobid) — $retcode (warning)"
        PASSED=$((PASSED + 1))
    elif [[ "$retcode" == "CC 0008" ]] && matches_expected "CC 0008" "$expect_rc"; then
        log_ok "$label ($jobid) — $retcode (expected)"
        PASSED=$((PASSED + 1))
    else
        log_fail "$label ($jobid) — $retcode (expected $expect_rc)"
        FAILED=$((FAILED + 1))
    fi
}

# ─── Preflight checks ───────────────────────────────────────────────────────

log_header "Preflight Checks"

# Check carddemo directory
if [[ ! -d "$CARDDEMO_DIR/app/jcl" ]]; then
    echo "ERROR: CardDemo JCL directory not found at $CARDDEMO_DIR/app/jcl"
    echo "Specify --carddemo-dir or set CARDDEMO_DIR"
    exit 1
fi
log_info "CardDemo directory: $CARDDEMO_DIR"

JCL_DIR="$CARDDEMO_DIR/app/jcl"
SAMPLE_JCL_DIR="$CARDDEMO_DIR/samples/jcl"

# Check for Zowe CLI
if command -v zowe &>/dev/null; then
    USE_ZOWE=true
    log_info "Zowe CLI found — using Zowe for job submission"
else
    USE_ZOWE=false
    log_info "Zowe CLI not found — using curl REST API fallback"
fi

# Check for server binary
SERVER_BIN="$OMFRAME_DIR/target/debug/zosmf-server"
if [[ ! -f "$SERVER_BIN" ]] && ! $SKIP_SERVER; then
    log_info "Building z/OSMF server..."
    (cd "$OMFRAME_DIR" && cargo build --package open-mainframe-zosmf 2>&1 | tail -3)
fi

# ─── Start server ────────────────────────────────────────────────────────────

if ! $SKIP_SERVER; then
    log_header "Starting z/OSMF Server"

    # Kill any existing server on this port
    lsof -ti ":${ZOSMF_PORT}" 2>/dev/null | xargs kill 2>/dev/null || true
    sleep 1

    # Clean up stale dataset files from previous runs so DEFINE operations succeed.
    # Clean the entire AWS tree, not just AWS/M2/CARDDEMO, because some JCL jobs
    # (e.g. DEFCUST) define datasets under other HLQs (AWS.CUSTDATA, AWS.CCDA).
    DATASET_DIR="${TMPDIR:-/tmp}/openmainframe-datasets"
    if [[ -d "$DATASET_DIR/AWS" ]]; then
        log_info "Cleaning stale VSAM/GDG files from previous run..."
        find "$DATASET_DIR/AWS" \
            \( -name "*.vsam" -o -name "*.aix" -o -name "*.aix.built" \
               -o -name "*.path" -o -name "*.nonvsam" -o -name "*.alias" \
               -o -name ".gdg" \) \
            -exec rm -rf {} + 2>/dev/null || true
    fi

    ZOSMF_PORT="$ZOSMF_PORT" "$SERVER_BIN" 2>/tmp/zosmf-debug.log &
    SERVER_PID=$!
    log_info "Server started (PID $SERVER_PID) on port $ZOSMF_PORT"

    # Wait for server to be ready
    log_info "Waiting for server to be ready..."
    for i in $(seq 1 30); do
        if curl -s "${ZOSMF_BASE_URL}/zosmf/info" >/dev/null 2>&1; then
            log_info "Server is ready!"
            break
        fi
        if [[ $i -eq 30 ]]; then
            echo "ERROR: Server did not start within 30 seconds"
            exit 1
        fi
        sleep 1
    done
else
    log_info "Using existing server at $ZOSMF_BASE_URL"
fi

# ─── Authentication ──────────────────────────────────────────────────────────

log_header "Authentication"

AUTH_TOKEN=""

if $USE_ZOWE; then
    # Create Zowe profile
    log_info "Creating Zowe z/OSMF profile '$PROFILE_NAME'..."
    zowe profiles create zosmf-profile "$PROFILE_NAME" \
        --host "$ZOSMF_HOST" \
        --port "$ZOSMF_PORT" \
        --user "$ZOSMF_USER" \
        --password "$ZOSMF_PASS" \
        --reject-unauthorized false \
        --overwrite 2>/dev/null || \
    zowe config set "profiles.${PROFILE_NAME}.properties.host" "$ZOSMF_HOST" 2>/dev/null || true

    # Also get a curl token for any direct API calls
    get_auth_token && log_info "JWT token obtained" || log_info "Token auth skipped (Zowe handles it)"
else
    if get_auth_token; then
        log_info "Authenticated as $ZOSMF_USER (token: ${AUTH_TOKEN:0:20}...)"
    else
        echo "ERROR: Authentication failed"
        exit 1
    fi
fi

# ─── Data Setup ─────────────────────────────────────────────────────────────
# Copy CardDemo proc and data files to the server's dataset directory
# so that JCLLIB ORDER and DSN references can be resolved.

DATASET_DIR="${DATASET_DIR:-${TMPDIR:-/tmp}/openmainframe-datasets}"
CARDDEMO_DS="$DATASET_DIR/AWS/M2/CARDDEMO"

log_info "Setting up CardDemo datasets in $CARDDEMO_DS"

# Copy procedure files (REPROC.prc → REPROC member)
PROC_DEST="$CARDDEMO_DS/PROC"
mkdir -p "$PROC_DEST"
for prc in "$CARDDEMO_DIR/app/proc/"*.prc; do
    if [[ -f "$prc" ]]; then
        base=$(basename "$prc" .prc)
        cp "$prc" "$PROC_DEST/$base" 2>/dev/null || true
    fi
done

# Copy EBCDIC flat files (AWS.M2.CARDDEMO.*.PS) converting dot-separated
# dataset names to directory paths (e.g. ACCTDATA/PS)
# Sort by name length (shortest first) so shorter-path files are copied before
# longer-path files that may need a parent to be a directory.
if [[ -d "$CARDDEMO_DIR/app/data/EBCDIC" ]]; then
    for f in $(ls -1 "$CARDDEMO_DIR/app/data/EBCDIC/" | awk '{ print length, $0 }' | sort -n | cut -d' ' -f2-); do
        src="$CARDDEMO_DIR/app/data/EBCDIC/$f"
        if [[ -f "$src" ]]; then
            dsname="$f"
            # Convert dots to path separators: AWS.M2.CARDDEMO.ACCTDATA.PS → AWS/M2/CARDDEMO/ACCTDATA/PS
            dspath=$(echo "$dsname" | tr '.' '/')
            dest="$DATASET_DIR/$dspath"
            # If a parent component exists as a file but we need it to be a
            # directory (e.g. DALYTRAN/PS is a file but DALYTRAN/PS/INIT needs
            # PS to be a directory), move the file into the new directory.
            parent="$(dirname "$dest")"
            while [[ "$parent" != "$DATASET_DIR" && "$parent" != "/" ]]; do
                if [[ -f "$parent" ]]; then
                    tmpfile="${parent}.__tmp__"
                    mv "$parent" "$tmpfile"
                    mkdir -p "$parent"
                    mv "$tmpfile" "$parent/data"
                    break
                fi
                parent="$(dirname "$parent")"
            done
            mkdir -p "$(dirname "$dest")" 2>/dev/null || true
            cp "$src" "$dest" 2>/dev/null || true
        fi
    done
fi

# Copy control files (REPROCT.ctl → AWS/M2/CARDDEMO/CNTL/REPROCT)
# These are PDS members referenced as DSN=AWS.M2.CARDDEMO.CNTL(REPROCT)
CNTL_DEST="$CARDDEMO_DS/CNTL"
mkdir -p "$CNTL_DEST"
for ctl in "$CARDDEMO_DIR/app/ctl/"*.ctl; do
    if [[ -f "$ctl" ]]; then
        base=$(basename "$ctl" .ctl)
        cp "$ctl" "$CNTL_DEST/$base" 2>/dev/null || true
    fi
done

# ─── Phase 1: GDG Base Definitions ──────────────────────────────────────────

log_header "Phase 1: GDG Base Definitions"

run_jcl "Define GDG Bases"                  "$JCL_DIR/DEFGDGB.jcl"      "CC 0000"
run_jcl "Define Daily Rejections GDG"       "$JCL_DIR/DALYREJS.jcl"     "CC 0000"
run_jcl "Define Report File GDG"            "$JCL_DIR/REPTFILE.jcl"     "CC 0000|CC 0012"

# ─── Phase 2: VSAM File Definitions ─────────────────────────────────────────

log_header "Phase 2: VSAM File Definitions"

run_jcl "Define Account VSAM"              "$JCL_DIR/ACCTFILE.jcl"     "CC 0000"
run_jcl "Define Card Data VSAM"            "$JCL_DIR/CARDFILE.jcl"     "CC 0000"
run_jcl "Define Customer VSAM"             "$JCL_DIR/CUSTFILE.jcl"     "CC 0000"
run_jcl "Define Cross-Reference VSAM"      "$JCL_DIR/XREFFILE.jcl"     "CC 0000"
run_jcl "Define Transaction Master VSAM"   "$JCL_DIR/TRANFILE.jcl"     "CC 0000"
run_jcl "Define Trans Category Balance"    "$JCL_DIR/TCATBALF.jcl"     "CC 0000"
run_jcl "Define Transaction Category"      "$JCL_DIR/TRANCATG.jcl"     "CC 0000"
run_jcl "Define Transaction Type"          "$JCL_DIR/TRANTYPE.jcl"     "CC 0000"
run_jcl "Define Disclosure Group"          "$JCL_DIR/DISCGRP.jcl"      "CC 0000"
run_jcl "Define User Security File"        "$JCL_DIR/DUSRSECJ.jcl"     "CC 0000"
run_jcl "Define ESDS/RRDS Files"           "$JCL_DIR/ESDSRRDS.jcl"     "CC 0000"

# ─── Phase 3: Initial Data Load ─────────────────────────────────────────────

log_header "Phase 3: Initial Data Load (GDG First Generations)"

run_jcl "Create GDGs + Load Data"          "$JCL_DIR/DEFGDGD.jcl"      "CC 0000"
run_jcl "Define Customer File"             "$JCL_DIR/DEFCUST.jcl"      "CC 0000|CC 0008"
run_jcl "Alternate Index on Transactions"  "$JCL_DIR/TRANIDX.jcl"      "CC 0000"

# ─── Phase 4: Data Read/Verification Jobs ───────────────────────────────────

log_header "Phase 4: Data Read/Verification Jobs"

run_jcl "Read Account Data"                "$JCL_DIR/READACCT.jcl"     "CC 0000"
run_jcl "Read Card Data"                   "$JCL_DIR/READCARD.jcl"     "CC 0000"
run_jcl "Read Customer Data"               "$JCL_DIR/READCUST.jcl"     "CC 0000"
run_jcl "Read Cross-Reference Data"        "$JCL_DIR/READXREF.jcl"     "CC 0000"

# ─── Phase 5: Batch Processing Jobs ─────────────────────────────────────────

log_header "Phase 5: Batch Processing Jobs"

run_jcl "Post Transactions"                "$JCL_DIR/POSTTRAN.jcl"     "CC 0000"
run_jcl "Interest Calculator"              "$JCL_DIR/INTCALC.jcl"      "CC 0000"
run_jcl "Combine Transactions"             "$JCL_DIR/COMBTRAN.jcl"     "CC 0000"

# ─── Phase 6: Reporting Jobs ────────────────────────────────────────────────

log_header "Phase 6: Reporting Jobs"

run_jcl "Create Statements"                "$JCL_DIR/CREASTMT.JCL"     "CC 0000"
run_jcl "Transaction Report"               "$JCL_DIR/TRANREPT.jcl"     "CC 0000"
run_jcl "Print Category Balance"           "$JCL_DIR/PRTCATBL.jcl"     "CC 0000"

# ─── Phase 7: Backup/Maintenance Jobs ───────────────────────────────────────

log_header "Phase 7: Backup/Maintenance Jobs"

run_jcl "Backup Transaction Master"        "$JCL_DIR/TRANBKP.jcl"      "CC 0000"

# ─── Phase 8: Export/Import Jobs ─────────────────────────────────────────────

log_header "Phase 8: Export/Import Jobs"

run_jcl "Export Customer Data"             "$JCL_DIR/CBEXPORT.jcl"     "CC 0000"
run_jcl "Import Customer Data"             "$JCL_DIR/CBIMPORT.jcl"     "CC 0000"

# ─── Phase 9: Utility/Sample Jobs ───────────────────────────────────────────

log_header "Phase 9: Utility/Sample Jobs"

run_jcl "List Catalog"                     "$SAMPLE_JCL_DIR/LISTCAT.jcl"   "CC 0000"
run_jcl "SORT Test"                        "$SAMPLE_JCL_DIR/SORTTEST.jcl"  "CC 0000"
run_jcl "REPRO Test"                       "$SAMPLE_JCL_DIR/REPRTEST.jcl"  "CC 0000"
run_jcl "Wait Step"                        "$JCL_DIR/WAITSTEP.jcl"         "CC 0000"

# ─── Phase 10: Skipped Jobs (require external subsystems) ───────────────────

log_header "Phase 10: Skipped Jobs (External Subsystem Dependencies)"

SKIP_JOBS=(
    "CBADMCDJ.jcl:CICS CSD Utility (DFHCSDUP)"
    "OPENFIL.jcl:CICS File Control (SDSF)"
    "CLOSEFIL.jcl:CICS File Control (SDSF)"
    "TXT2PDF1.JCL:TSO TXT2PDF Utility"
    "FTPJCL.JCL:FTP Transfer (requires network)"
    "INTRDRJ1.JCL:Internal Reader (chains INTRDRJ2)"
    "INTRDRJ2.JCL:Internal Reader (submitted by INTRDRJ1)"
    "RACFCMDS.jcl:RACF Commands (external groups)"
)

for entry in "${SKIP_JOBS[@]}"; do
    name="${entry%%:*}"
    reason="${entry##*:}"
    log_skip "$name — $reason"
    TOTAL=$((TOTAL + 1))
    SKIPPED=$((SKIPPED + 1))
done

# Skipped IMS/DB2/MQ jobs
IMS_DB2_JOBS=(
    "CBPAUP0J.jcl:IMS BMP Program"
    "DBPAUTP0.jcl:IMS Database Unload"
    "LOADPADB.JCL:IMS Database Load"
    "UNLDGSAM.JCL:IMS GSAM Unload"
    "UNLDPADB.JCL:IMS Database Unload"
    "CREADB21.jcl:DB2 Database Create"
    "MNTTRDB2.jcl:DB2 Table Maintenance"
    "TRANEXTR.jcl:DB2 Transaction Extract"
)

for entry in "${IMS_DB2_JOBS[@]}"; do
    name="${entry%%:*}"
    reason="${entry##*:}"
    log_skip "$name — requires $reason"
    TOTAL=$((TOTAL + 1))
    SKIPPED=$((SKIPPED + 1))
done

# Skipped compilation jobs
COMPILE_JOBS=(
    "BATCMP.jcl:Batch COBOL Compile"
    "BMSCMP.jcl:BMS Map Compile"
    "CICCMP.jcl:CICS Program Compile"
    "CICDBCMP.jcl:CICS DB2 Program Compile"
    "IMSMQCMP.jcl:IMS MQ Program Compile"
)

for entry in "${COMPILE_JOBS[@]}"; do
    name="${entry%%:*}"
    reason="${entry##*:}"
    log_skip "$name — requires $reason procedure"
    TOTAL=$((TOTAL + 1))
    SKIPPED=$((SKIPPED + 1))
done

# ─── Summary ────────────────────────────────────────────────────────────────

log_header "Test Summary"

echo ""
echo -e "  ${BOLD}Total:${NC}   $TOTAL"
echo -e "  ${GREEN}Passed:${NC}  $PASSED"
echo -e "  ${RED}Failed:${NC}  $FAILED"
echo -e "  ${YELLOW}Skipped:${NC} $SKIPPED"
echo ""

if [[ $FAILED -eq 0 ]]; then
    echo -e "  ${GREEN}${BOLD}All submitted jobs passed!${NC}"
    EXIT_CODE=0
else
    echo -e "  ${RED}${BOLD}$FAILED job(s) failed.${NC}"
    EXIT_CODE=1
fi

echo ""
echo "Server: $ZOSMF_BASE_URL"
echo "CardDemo: $CARDDEMO_DIR"
echo "Method: $(if $USE_ZOWE; then echo "Zowe CLI"; else echo "curl REST API"; fi)"
echo ""

exit $EXIT_CODE
