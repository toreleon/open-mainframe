# Ralph Loop Prompt: z/OSMF API Full Conformance

## Mission

You are fixing the **`open-mainframe-zosmf`** crate to achieve full wire-compatibility with the real IBM z/OSMF REST API. All core endpoints exist but have gaps — missing fields, missing query parameters, missing sub-endpoints, and behavioral differences vs the IBM specification.

**4 tiers, 45 stories, zero gaps left behind.**

The crate is at: `crates/open-mainframe-zosmf/`

---

## Iteration Protocol

**Before doing ANY work, determine where you are:**

1. Check the progress tracker file: `_bmad-output/conformance-progress.md`
   - If it does not exist, create it (see Progress Tracker Format below)
   - If it exists, read it to find the current tier and story

2. Find the **first story with status `pending` or `in_progress`** and work on it

3. **Before modifying any file, READ it first.** Understand the existing code structure, types, and handler patterns before making changes.

4. After completing a story:
   - Run `PATH="/home/tore/.rustup/toolchains/1.82-x86_64-unknown-linux-gnu/bin:$PATH" cargo check -p open-mainframe-zosmf` — must pass
   - Run `PATH="/home/tore/.rustup/toolchains/1.82-x86_64-unknown-linux-gnu/bin:$PATH" cargo test -p open-mainframe-zosmf` — must pass
   - Update the progress tracker: mark story `complete`, set next story to `in_progress`
   - Commit with message: `fix(zosmf): CONF-{tier}.{story} — {title}`

5. If ALL 45 stories are `complete` in the progress tracker, AND cargo check + test pass:
   - Output the completion promise

6. If you encounter a build error or test failure:
   - Do NOT skip to the next story
   - Fix the error in the current story before proceeding

---

## Reference: Key Source Files

| File | Contents |
|------|----------|
| `src/handlers/info.rs` | GET /zosmf/info handler |
| `src/handlers/authenticate.rs` | POST/DELETE /zosmf/services/authenticate |
| `src/handlers/datasets.rs` | All /zosmf/restfiles/ds/* handlers |
| `src/handlers/jobs.rs` | All /zosmf/restjobs/jobs/* handlers |
| `src/handlers/tso.rs` | All /zosmf/tsoApp/* handlers |
| `src/handlers/console.rs` | PUT /zosmf/restconsoles/consoles/* handler |
| `src/handlers/files.rs` | All /zosmf/restfiles/fs/* handlers |
| `src/types/info.rs` | ZosmfInfo, ZosmfPlugin structs |
| `src/types/datasets.rs` | Dataset list/create/member types |
| `src/types/jobs.rs` | JobResponse, SpoolFile, JobFeedback types |
| `src/types/tso.rs` | TSO session/command/response types |
| `src/types/console.rs` | ConsoleRequest, ConsoleResponse types |
| `src/types/error.rs` | ZosmfErrorResponse, ZosmfErrorBody types |
| `src/types/auth.rs` | AuthContext extractor |
| `src/middleware/csrf.rs` | X-CSRF-ZOSMF-HEADER middleware |
| `src/state.rs` | AppState shared state |
| `src/main.rs` | Server startup and route registration |

All paths relative to `crates/open-mainframe-zosmf/`.

---

## TIER 1: Missing Endpoints (13 stories)

### CONF-1.1: Console — GET solicited messages endpoint
Add `GET /zosmf/restconsoles/consoles/{name}/solmsgs/{key}` endpoint.
- Returns the response text associated with a previously issued command's `cmd-response-key`
- Store command responses in AppState keyed by `cmd-response-key`
- Response: JSON with `cmd-response` field containing the stored message
- Status: 200 OK, or 404 if key not found
- Register route in `handlers/console.rs` `routes()` function

### CONF-1.2: Console — GET unsolicited message detection
Add `GET /zosmf/restconsoles/consoles/{name}/detections/{key}` endpoint.
- Returns detection status for unsolicited messages
- Response: JSON with `status` field ("detected" or "not-detected") and `message` field
- Status: 200 OK
- Can return a stub/minimal response since we don't have real MVS unsolicited messages

### CONF-1.3: TSO — Ping session endpoint
Add `PUT /zosmf/tsoApp/tso/ping/{servlet_key}` endpoint (note: PUT method per IBM spec).
- Verifies TSO session is alive
- Response: JSON with `servletKey` and `ver` fields
- Status: 200 OK if session exists, 404 if not found
- Register route in `handlers/tso.rs` `routes()` function

### CONF-1.4: Dataset — Rename operation
Add rename support to `PUT /zosmf/restfiles/ds/{name}`.
- When request body contains `{"request":"rename","from-dataset":{"dsn":"OLD.NAME"}}`, perform rename
- Check for `request` field in JSON body to distinguish from content write
- Use `X-IBM-Option` header or Content-Type to differentiate JSON action from raw content write
- Status: 200 OK on success

### CONF-1.5: Dataset — Copy operation
Add copy support to `PUT /zosmf/restfiles/ds/{name}`.
- When body contains `{"request":"copy","from-dataset":{"dsn":"SOURCE.NAME","member":"SRCMEM"}}`, perform copy
- Support copying between datasets and between PDS members
- Status: 200 OK on success

### CONF-1.6: Dataset — Migrate operation
Add migrate support to `PUT /zosmf/restfiles/ds/{name}`.
- When body contains `{"request":"hmigrate"}`, mark dataset as migrated in catalog
- Status: 200 OK on success
- Can set a `migr` flag on the dataset in the catalog

### CONF-1.7: Dataset — Recall operation
Add recall support to `PUT /zosmf/restfiles/ds/{name}`.
- When body contains `{"request":"hrecall"}`, recall a migrated dataset
- Status: 200 OK on success
- Clear the `migr` flag on the dataset in the catalog

### CONF-1.8: Job — Submit from dataset
Extend `PUT /zosmf/restjobs/jobs` to accept JSON body `{"file":"//'DATASET.NAME'"}` or `{"file":"//'DATASET.NAME(MEMBER)'"}`.
- Detect JSON vs raw JCL by Content-Type header or by attempting JSON parse
- Read JCL content from the referenced dataset, then submit it
- Same response format as raw JCL submission (full JobResponse)

### CONF-1.9: USS — chmod operation
Add chmod support to `PUT /zosmf/restfiles/fs/{path}`.
- When JSON body contains `{"request":"chmod","mode":"RWXRW-RW-"}`, change file permissions
- Parse the mode string to Unix permission bits
- Status: 200 OK on success

### CONF-1.10: USS — chown operation
Add chown support to `PUT /zosmf/restfiles/fs/{path}`.
- When body contains `{"request":"chown","owner":"USER","group":"GROUP"}`, change ownership
- Status: 200 OK on success
- On non-Unix platforms, return success without actual change

### CONF-1.11: USS — chtag operation
Add chtag support to `PUT /zosmf/restfiles/fs/{path}`.
- When body contains `{"request":"chtag","action":"set","type":"text","codeset":"ISO8859-1"}`, tag file
- Store tags as extended attributes or in-memory metadata
- Status: 200 OK on success

### CONF-1.12: USS — Copy file/directory
Add copy support to `PUT /zosmf/restfiles/fs/{path}`.
- When body contains `{"request":"copy","from":"/source/path"}`, copy file or directory
- Support recursive directory copy
- Status: 200 OK on success

### CONF-1.13: USS — Move/Rename file/directory
Add move support to `PUT /zosmf/restfiles/fs/{path}`.
- When body contains `{"request":"move","from":"/source/path"}`, move/rename
- Status: 200 OK on success

---

## TIER 2: Missing Fields on Existing Endpoints (13 stories)

### CONF-2.1: Dataset list — Add missing item fields
Add these fields to `DatasetListItem` in `types/datasets.rs`:
- `dev` (String, optional) — device type
- `edate` (String, optional) — expiration date
- `extx` (u32, optional) — extents used
- `migr` (String, optional) — migration status ("YES"/"NO")
- `mvol` (String, optional) — multi-volume flag
- `ovf` (String, optional) — overflow status
- `sizex` (u32, optional) — allocated size in tracks
- `spacu` (String, optional) — space unit (TRACKS/CYLINDERS/BLOCKS)
- `used` (u32, optional) — percentage used
- `vols` (String, optional) — volume list
- `dsntp` (String, optional) — dataset name type (PDS, PDSE, LIBRARY, etc.)
All fields should use `#[serde(skip_serializing_if = "Option::is_none")]`.
Populate from catalog metadata where available, default to None.

### CONF-2.2: Dataset list — Remove extra totalRows field
Real z/OSMF does NOT include `totalRows` in the dataset list response body. It uses `X-IBM-Response-Rows` response header instead.
- Remove `total_rows` from `DatasetListResponse`
- Add `X-IBM-Response-Rows` response header to the dataset list handler
- Do the same for `MemberListResponse` and USS `UssListResponse` if they have `totalRows`

### CONF-2.3: Dataset create — Add missing allocation fields
Add these fields to `DatasetCreateParams` in `types/datasets.rs`:
- `volser` (String, optional) — volume serial
- `unit` (String, optional) — device type
- `avgblk` (u32, optional) — average block length
- `storclass` (String, optional) — SMS storage class
- `mgntclass` (String, optional) — SMS management class
- `dataclass` (String, optional) — SMS data class
- `dsntype` (String, optional) — "PDS", "PDSE", "LIBRARY", "BASIC", etc.
All fields should use `#[serde(skip_serializing_if = "Option::is_none")]` for deserialization acceptance. Pass relevant ones to the dataset catalog when creating.

### CONF-2.4: Job response — Add missing exec/step fields
Add these fields to `JobResponse` in `types/jobs.rs`:
- `exec_started` (String, optional) — serialized as `exec-started`, ISO timestamp
- `exec_ended` (String, optional) — serialized as `exec-ended`, ISO timestamp
- `exec_member` (String, optional) — serialized as `exec-member`, system member name
- `exec_submitted` (String, optional) — serialized as `exec-submitted`, ISO timestamp
- `exec_system` (String, optional) — serialized as `exec-system`, system name
- `reason_not_running` (String, optional) — serialized as `reason-not-running`
All with `#[serde(skip_serializing_if = "Option::is_none")]`.
Populate `exec_submitted` from JES2 job submission time. Others can default to None or use reasonable defaults.

### CONF-2.5: Job response — Add step-data support
Add `step_data` field to `JobResponse`:
- Type: `Option<Vec<JobStepData>>` serialized as `step-data`
- `JobStepData` struct with fields: `smfid`, `completion`, `step-name`, `proc-step-name`, `program-name`, `active`
- Only populated when `exec-data=Y` query param is set on job list/status
- Skip serializing when None

### CONF-2.6: Job feedback — Add missing fields
Add these fields to `JobFeedback` in `types/jobs.rs`:
- `original_jobid` (String, optional) — serialized as `original-jobid`
- `owner` (String, optional)
- `member` (String, optional) — system member
- `sysname` (String, optional) — system name
- `job_correlator` (String, optional) — serialized as `job-correlator`
- `internal_code` (String, optional) — serialized as `internal-code`
Populate from the job being purged/actioned.

### CONF-2.7: TSO start response — Add missing fields
Add these fields to `TsoStartResponse` in `types/tso.rs`:
- `ver` (String) — version, value "0100"
- `timeout` (bool) — whether session timed out, default false
- `reused` (bool) — whether session was reused, default false

### CONF-2.8: TSO v1 response — Add missing fields
Add these fields to the V1 stateless TSO response:
- `tso_prompt_received` (String) — serialized as `tsoPromptReceived`, value "Y" or "N"
- `keyword_detected` (String) — serialized as `keywordDetected`, value "Y" or "N"
Set `tsoPromptReceived` to "N" and `keywordDetected` to "N" by default (or "Y" if prompt detected).

### CONF-2.9: USS entry — Add tag field
Add `tag` field to `UssEntry` in `handlers/files.rs`:
- Type: `Option<String>` — file codeset tag (e.g., "t ISO8859-1", "b", "untagged")
- Default to `None` or `"untagged"` for all files
- Use `#[serde(skip_serializing_if = "Option::is_none")]`

### CONF-2.10: Error response — Add details and stack fields
Add to `ZosmfErrorBody` in `types/error.rs`:
- `details` (Vec<String>, optional) — additional error details, skip_serializing_if empty
- `stack` (String, optional) — stack trace info, skip_serializing_if None

### CONF-2.11: Console request — Add missing optional fields
Add these optional fields to `ConsoleRequest` in `types/console.rs`:
- `sol_key_reg` — serialized as `solKeyReg`
- `unsol_key` — serialized as `unsol-key`
- `unsol_key_reg` — serialized as `unsolKeyReg`
- `detect_time` — serialized as `detect-time`
- `unsol_detect_sync` — serialized as `unsol-detect-sync`
- `unsol_detect_timeout` — serialized as `unsol-detect-timeout`
- `routcode` — route code
- `auth` — authorization level
All as `Option<String>` with `skip_serializing_if`.

### CONF-2.12: Console response — Add cmd-response-uri field
Add `cmd_response_uri` field to `ConsoleResponse`:
- Type: `Option<String>` — serialized as `cmd-response-uri`
- Set to same value as `cmd-response-url` (IBM provides both)

### CONF-2.13: Spool file — Verify all fields present
Audit `SpoolFile` struct against IBM spec. Ensure these are all present:
- `id`, `ddname`, `stepname`, `procstep`, `class`, `byte-count`, `record-count`, `recfm`, `lrecl`, `job-correlator`, `subsystem`, `records-url`, `jobid`, `jobname`
If any are missing, add them. This is a verification story.

---

## TIER 3: Missing Query Parameters & Headers (12 stories)

### CONF-3.1: Dataset list — Add volser and start query params
Add `volser` and `start` query parameters to the dataset list endpoint:
- `volser`: filter datasets on a specific volume
- `start`: return datasets starting after this name (for pagination)
- Add to the query struct in `handlers/datasets.rs`

### CONF-3.2: Member list — Add pattern and start query params
Add `pattern` and `start` query parameters to the member list endpoint:
- `pattern`: filter members matching pattern (supports `*` wildcard)
- `start`: return members starting after this name
- Add to the handler in `handlers/datasets.rs`

### CONF-3.3: Job list — Add max-jobs and exec-data query params
Add `max_jobs` and `exec_data` query parameters to the job list endpoint:
- `max-jobs`: limit number of jobs returned (0 = unlimited)
- `exec-data`: when "Y", include `step-data` in each job response
- Add to `JobListQuery` in `handlers/jobs.rs`

### CONF-3.4: Dataset read — Add X-IBM-Data-Type header support
Support `X-IBM-Data-Type` request header on dataset read:
- Values: `text` (default), `binary`, `record`
- `text`: return as UTF-8 text (current behavior)
- `binary`: return raw bytes with `application/octet-stream` content type
- `record`: return records with RDW (record descriptor words)
- Extract header in the read handler

### CONF-3.5: Dataset write — Add X-IBM-Data-Type header support
Support `X-IBM-Data-Type` request header on dataset write:
- Same values as read: `text`, `binary`, `record`
- `text`: write as text (current behavior)
- `binary`: write raw bytes
- `record`: parse RDW-prefixed records

### CONF-3.6: USS read — Add X-IBM-Data-Type header support
Support `X-IBM-Data-Type` on USS file read:
- `text` (default): return as UTF-8
- `binary`: return raw bytes with `application/octet-stream`

### CONF-3.7: USS write — Add X-IBM-Data-Type header support
Support `X-IBM-Data-Type` on USS file write, same values.

### CONF-3.8: USS delete — Add X-IBM-Option recursive header
Support `X-IBM-Option` request header on USS delete:
- When `X-IBM-Option: recursive`, delete directory recursively (current default behavior)
- When header is absent and target is a non-empty directory, return 400 error
- Only empty directories should be deletable without the recursive flag

### CONF-3.9: USS create — Parse JSON request body
Update USS create (POST) to read JSON request body:
- Expected body: `{"type":"mkdir","mode":"RWXRW-RW-"}` or `{"type":"file","mode":"..."}`
- Parse `type` field to determine mkdir vs file creation
- Parse `mode` field to set Unix permissions
- If no body, default to current behavior (mkdir)

### CONF-3.10: Dataset list — Fix X-IBM-Attributes logic
Fix the `X-IBM-Attributes` header handling:
- `dsname` or absent: return only `dsname` field per item
- `base`: return full attributes (dsorg, recfm, lrecl, blksz, vol, cdate, rdate, catnm, etc.)
- `vol`: return dsname + volume info
- Currently the logic is inverted — fix this

### CONF-3.11: Dataset/USS — Add ETag response headers
Add `ETag` response header to dataset read and USS file read:
- Compute ETag from content hash (e.g., hex-encoded CRC32 or short SHA256)
- Return as `ETag: "hash"` response header
- Support `If-None-Match` request header: if ETag matches, return 304 Not Modified

### CONF-3.12: Dataset/USS — Add If-Match write support
Add `If-Match` request header support to dataset write and USS file write:
- If `If-Match` header is present, verify it matches current content's ETag
- If mismatch, return 412 Precondition Failed
- This enables optimistic concurrency control

---

## TIER 4: Behavioral Fixes (7 stories)

### CONF-4.1: USS write — Distinguish create vs update status code
Fix USS file write to return correct status code:
- `201 Created` when file did not previously exist
- `204 No Content` when overwriting an existing file
- Check `full_path.exists()` before writing

### CONF-4.2: Dataset write — Distinguish create vs update for members
When writing a PDS member:
- `201 Created` if the member is new
- `204 No Content` if overwriting existing member
- Check member existence before writing

### CONF-4.3: Login response — Match IBM behavior
The real z/OSMF authenticate endpoint returns the JWT only via `Set-Cookie` header, with an empty or minimal JSON body.
- Keep the `Set-Cookie: jwtToken=...` header (already correct)
- Change response body to empty or `{}` to match IBM behavior
- OR keep `{"token":"..."}` as an extension but ensure it doesn't break Zowe CLI

### CONF-4.4: Dataset list — Add X-IBM-Response-Rows header
After removing `totalRows` from the JSON body (CONF-2.2), add it as a response header:
- `X-IBM-Response-Rows: <count>` on dataset list responses
- Same for member list responses

### CONF-4.5: X-IBM-Max-Items — Return 206 Partial Content
When `X-IBM-Max-Items` header truncates results:
- Return HTTP 206 Partial Content instead of 200 OK
- Include `X-IBM-Response-Rows` header with total count
- Only truncate when `X-IBM-Max-Items > 0` and results exceed limit

### CONF-4.6: Dataset PUT — Differentiate action vs content write
The `PUT /zosmf/restfiles/ds/{name}` endpoint now handles both content writes AND actions (rename/copy/migrate/recall from TIER 1).
- If `Content-Type: application/json` AND body contains `"request"` field, route to action handler
- If `Content-Type: text/plain` or `application/octet-stream` or no Content-Type, route to content write
- This is needed for CONF-1.4 through CONF-1.7 to coexist with existing write handler

### CONF-4.7: USS PUT — Differentiate action vs content write
Same as CONF-4.6 but for USS files:
- If `Content-Type: application/json` AND body contains `"request"` field, route to action handler (chmod/chown/chtag/copy/move)
- Otherwise, route to content write
- This is needed for CONF-1.9 through CONF-1.13 to coexist with existing write handler

---

## Progress Tracker Format

Create `_bmad-output/conformance-progress.md` with this format:

```markdown
---
currentTier: CONF-1
currentStory: CONF-1.1
storiesComplete: 0
storiesTotal: 45
lastVerified: ""
---

# z/OSMF Conformance Progress

## TIER 1: Missing Endpoints
- [ ] CONF-1.1: Console — GET solicited messages endpoint
- [ ] CONF-1.2: Console — GET unsolicited message detection
- [ ] CONF-1.3: TSO — Ping session endpoint
- [ ] CONF-1.4: Dataset — Rename operation
- [ ] CONF-1.5: Dataset — Copy operation
- [ ] CONF-1.6: Dataset — Migrate operation
- [ ] CONF-1.7: Dataset — Recall operation
- [ ] CONF-1.8: Job — Submit from dataset
- [ ] CONF-1.9: USS — chmod operation
- [ ] CONF-1.10: USS — chown operation
- [ ] CONF-1.11: USS — chtag operation
- [ ] CONF-1.12: USS — Copy file/directory
- [ ] CONF-1.13: USS — Move/Rename file/directory

## TIER 2: Missing Fields
- [ ] CONF-2.1: Dataset list — Add missing item fields
- [ ] CONF-2.2: Dataset list — Remove extra totalRows field
- [ ] CONF-2.3: Dataset create — Add missing allocation fields
- [ ] CONF-2.4: Job response — Add missing exec/step fields
- [ ] CONF-2.5: Job response — Add step-data support
- [ ] CONF-2.6: Job feedback — Add missing fields
- [ ] CONF-2.7: TSO start response — Add missing fields
- [ ] CONF-2.8: TSO v1 response — Add missing fields
- [ ] CONF-2.9: USS entry — Add tag field
- [ ] CONF-2.10: Error response — Add details and stack fields
- [ ] CONF-2.11: Console request — Add missing optional fields
- [ ] CONF-2.12: Console response — Add cmd-response-uri field
- [ ] CONF-2.13: Spool file — Verify all fields present

## TIER 3: Missing Query Parameters & Headers
- [ ] CONF-3.1: Dataset list — Add volser and start query params
- [ ] CONF-3.2: Member list — Add pattern and start query params
- [ ] CONF-3.3: Job list — Add max-jobs and exec-data query params
- [ ] CONF-3.4: Dataset read — Add X-IBM-Data-Type header support
- [ ] CONF-3.5: Dataset write — Add X-IBM-Data-Type header support
- [ ] CONF-3.6: USS read — Add X-IBM-Data-Type header support
- [ ] CONF-3.7: USS write — Add X-IBM-Data-Type header support
- [ ] CONF-3.8: USS delete — Add X-IBM-Option recursive header
- [ ] CONF-3.9: USS create — Parse JSON request body
- [ ] CONF-3.10: Dataset list — Fix X-IBM-Attributes logic
- [ ] CONF-3.11: Dataset/USS — Add ETag response headers
- [ ] CONF-3.12: Dataset/USS — Add If-Match write support

## TIER 4: Behavioral Fixes
- [ ] CONF-4.1: USS write — Distinguish create vs update status code
- [ ] CONF-4.2: Dataset write — Distinguish create vs update for members
- [ ] CONF-4.3: Login response — Match IBM behavior
- [ ] CONF-4.4: Dataset list — Add X-IBM-Response-Rows header
- [ ] CONF-4.5: X-IBM-Max-Items — Return 206 Partial Content
- [ ] CONF-4.6: Dataset PUT — Differentiate action vs content write
- [ ] CONF-4.7: USS PUT — Differentiate action vs content write
```

---

## Ordering Constraints

- **CONF-4.6 must be done before** CONF-1.4, CONF-1.5, CONF-1.6, CONF-1.7 (dataset action routing)
- **CONF-4.7 must be done before** CONF-1.9, CONF-1.10, CONF-1.11, CONF-1.12, CONF-1.13 (USS action routing)
- **CONF-2.2 must be done before** CONF-4.4 (remove totalRows before adding header)
- All other stories are independent and can be done in listed order

**Recommended order:** Do TIER 4 stories 4.6 and 4.7 FIRST (they are prerequisites), then TIER 1, then TIER 2, then TIER 3, then remaining TIER 4.

Adjusted execution order:
1. CONF-4.6, CONF-4.7 (routing prerequisites)
2. CONF-1.1 through CONF-1.13 (new endpoints)
3. CONF-2.1 through CONF-2.13 (field additions)
4. CONF-3.1 through CONF-3.12 (query params & headers)
5. CONF-2.2, then CONF-4.4 (totalRows removal chain)
6. CONF-4.1 through CONF-4.5 (remaining behavioral fixes)

---

## Rules

1. **READ before WRITE** — Always read a file before modifying it
2. **One story at a time** — Complete and verify each story before starting the next
3. **Don't break existing tests** — All existing tests must continue to pass
4. **Add tests for new functionality** — Each new endpoint or behavior change should have at least one unit test
5. **Use existing patterns** — Follow the handler/type/route patterns already established in the codebase
6. **Skip serializing None** — All new optional fields must use `#[serde(skip_serializing_if = "Option::is_none")]`
7. **Correct serde renames** — All hyphenated IBM field names must use `#[serde(rename = "field-name")]`
8. **No new dependencies unless necessary** — Prefer using existing crate dependencies
9. **Commit after each story** — `fix(zosmf): CONF-X.Y — title`
