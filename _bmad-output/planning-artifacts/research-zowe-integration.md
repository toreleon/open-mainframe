---
stepsCompleted: [1, 2, 3, 4, 5, 6]
inputDocuments: [RALPH-ZOWE-RESEARCH.md, Cargo.toml, README.md]
workflowType: 'research'
lastStep: 6
research_type: 'technical'
research_topic: 'Zowe Integration for OpenMainframe'
research_goals: 'Determine how OpenMainframe can expose z/OSMF-compatible REST APIs and services so that Zowe CLI, Zowe Explorer, and the Zowe API Mediation Layer can connect to it as if it were a real z/OS instance'
user_name: 'Tore'
date: '2026-02-20'
web_research_enabled: true
source_verification: true
---

# Zowe Integration for OpenMainframe: Comprehensive Technical Research

## Step 1 — Research Scope Confirmation

### Research Objective

This research investigates how **OpenMainframe** (an open-source Rust-based mainframe emulator with 24 crates covering COBOL, JCL, REXX, HLASM, PL/I, ISPF, TSO, RACF, JES2, CICS, IMS, DB2, MQ, VSAM, SMF, and WLM) can expose its subsystems through **Zowe-compatible interfaces** so that the full Zowe ecosystem can connect to it as a z/OS-equivalent target.

### Scope Areas

#### 1. Architecture Analysis

Zowe is a collection of components forming a framework that makes z/OS functionality accessible through modern interfaces. The architecture spans server-side components (running on z/OS or Linux/Docker) and client-side tools (CLI, VS Code extensions, SDKs).

**Key architectural components** ([Zowe Architecture Docs](https://docs.zowe.org/stable/getting-started/zowe-architecture/)):
- **API Mediation Layer (API ML)** — Gateway + Discovery Service + API Catalog. Acts as a Level 7 reverse proxy routing requests to backend z/OS services. Built on Java/Spring Cloud.
- **Zowe Application Framework (ZLUX)** — Web-based desktop for z/OS applications, served by ZSS (z/OS native HTTPS server).
- **Zowe CLI** — Node.js-based command-line tool for remote z/OS interaction.
- **Zowe Explorer** — VS Code and IntelliJ extensions for browsing datasets, USS files, and jobs.
- **Zowe System Services (ZSS)** — z/OS native extensible HTTPS server for REST and WebSocket APIs around z/OS system calls.
- **Client SDKs** — Node.js, Python, Kotlin, Swift, and Go ([Go SDK 2025](https://openmainframeproject.org/blog/summer-mentorship-2025-ojus-chugh/)) SDKs.

**Current version:** Zowe V3 LTS ([LTS V3 announcement](https://www.linuxfoundation.org/press/open-mainframe-project-announces-zowes-lts-v3-release)), documentation v3.4.x.

#### 2. Integration Patterns — What Zowe Expects from z/OS

The **primary interface** Zowe expects from a z/OS target is **z/OSMF REST APIs over HTTPS** ([IBM z/OSMF docs](https://docs.zowe.org/v3.2.x/user-guide/cli-install-configure-zosmf/)):

| Protocol/Interface | Zowe Component That Uses It | Description |
|---|---|---|
| **z/OSMF REST APIs** | CLI, Explorer, API ML | Primary interface — datasets, jobs, TSO, console, USS files |
| **TN3270E** | Zowe Desktop (TN3270 app) | Terminal emulation for 3270 applications |
| **SSH/SFTP** | CLI (optional), USS access | Alternative file transfer and command execution |
| **FTP/FTPS** | CLI (zos-ftp plugin) | Legacy file transfer protocol |
| **API ML Gateway** | All components | Reverse proxy — routes `https://gateway/api/v1/zosmf/...` to z/OSMF |

The z/OSMF REST API is the **non-negotiable core** — without it, Zowe CLI and Zowe Explorer cannot function. The API ML sits in front of z/OSMF as a reverse proxy, adding SSO and service discovery ([API ML Overview](https://docs.zowe.org/stable/user-guide/api-mediation/api-mediation-overview/)).

#### 3. Technology Stack

**Zowe server-side:**
- API ML: Java, Spring Cloud (Netflix Eureka for discovery, Zuul/Spring Cloud Gateway for routing)
- ZLUX/App Framework: Node.js
- ZSS: C (z/OS native)

**Zowe client-side:**
- CLI: Node.js (Imperative CLI Framework)
- Explorer: TypeScript (VS Code/IntelliJ extensions)
- SDKs: Node.js, Python, Kotlin, Swift, Go

**What OpenMainframe must expose:**
- z/OSMF-compatible REST API server (HTTPS, JSON)
- SAF/RACF-compatible authentication (username/password at minimum, JWT preferred)
- TN3270E server (already partially exists in `open-mainframe-tui`)

#### 4. Compatibility Requirements — Zowe Conformance Program

The Zowe Conformance Program ([Conformance Docs](https://docs.zowe.org/stable/extend/zowe-conformance-program/)) defines criteria for extensions to be certified as Zowe-conformant. Key areas:

- **API ML Conformance (Minimum):** Service provides a default service ID prefixed by provider name, service ID configurable externally, API ID follows Java package naming, supports basic authentication, supports static service definition.
- **API ML Conformance (Preferred):** Accepts Zowe JWT tokens in cookies, supports dynamic registration with Eureka discovery service.
- **CLI Plug-in Conformance:** Built on Imperative CLI Framework, installable/uninstallable via `zowe plugins`, no command group name conflicts.
- **App Framework Conformance:** Unique plugin ID (Java package naming), no modification of Zowe runtime directories.

**For OpenMainframe:** The most relevant conformance path is making the REST API layer compatible as a **z/OSMF backend target** rather than as a Zowe extension. This means implementing the z/OSMF REST API spec faithfully so Zowe treats OpenMainframe as a z/OS system.

#### 5. OpenMainframe Gap Assessment — Crate Mapping

| Zowe-Required Interface | OpenMainframe Crate | Current Status | Gap Level |
|---|---|---|---|
| z/OSMF REST API server | **None** | No REST API layer exists | **CRITICAL** — new crate needed |
| Dataset operations (list, read, write, create, delete) | `open-mainframe-dataset` | VSAM/QSAM/PDS/PDSE/catalog implemented | **MEDIUM** — need REST wrappers |
| Job operations (submit, status, output, cancel) | `open-mainframe-jes2` | JES2 job queue/spool/initiators implemented | **MEDIUM** — need REST wrappers |
| TSO/Console commands | `open-mainframe-tso` | TSO command processor implemented | **MEDIUM** — need REST wrappers + session mgmt |
| USS file system | **None** | No USS emulation | **HIGH** — new crate or major feature needed |
| RACF/SAF authentication | `open-mainframe-racf` | SAF router, user profiles, auth implemented | **LOW** — need HTTP auth integration |
| TN3270E terminal | `open-mainframe-tui` | TN3270E protocol implemented (588 lines) | **LOW** — may need server mode |
| HTTPS/TLS transport | `open-mainframe-deploy` | Async HTTP infrastructure exists | **MEDIUM** — need TLS, z/OSMF routing |
| JWT/SSO tokens | **None** | No JWT support | **HIGH** — need token generation/validation |
| z/OSMF info/status endpoints | **None** | No z/OSMF system info | **MEDIUM** — new implementation needed |

### Confirmed Research Scope

This research will investigate and document:

1. **z/OSMF REST API specification** — Exact endpoints, request/response JSON schemas, authentication flows
2. **Architecture options** — How to build a z/OSMF-compatible REST layer in Rust
3. **Authentication bridge** — How to connect RACF/SAF to Zowe's JWT/SSO expectations
4. **Gap-by-gap implementation plan** — For each Zowe feature, what OpenMainframe needs
5. **Conformance path** — Steps to achieve Zowe Conformance certification
6. **Epic structure** — Proposed implementation epics following the project's pattern

### Sources — Step 1

- [Zowe Architecture](https://docs.zowe.org/stable/getting-started/zowe-architecture/)
- [Zowe Overview](https://docs.zowe.org/stable/getting-started/overview/)
- [API Mediation Layer Overview](https://docs.zowe.org/stable/user-guide/api-mediation/api-mediation-overview/)
- [Zowe Conformance Program](https://docs.zowe.org/stable/extend/zowe-conformance-program/)
- [Zowe CLI Usage](https://docs.zowe.org/stable/user-guide/cli-using-usingcli/)
- [Connecting to z/OS via Zowe CLI](https://ibm.github.io/zopeneditor-about/Docs/connect_to_zos_with_zowe_cli_e2e.html)
- [Zowe V3 LTS Release](https://www.linuxfoundation.org/press/open-mainframe-project-announces-zowes-lts-v3-release)
- [Zowe Go SDK Mentorship](https://openmainframeproject.org/blog/summer-mentorship-2025-ojus-chugh/)
- [API ML GitHub](https://github.com/zowe/api-layer)
- [Zowe CLI GitHub](https://github.com/zowe/zowe-cli)
- [Conformance Program (OMP)](https://www.openmainframeproject.org/projects/zowe/conformance)
- [Authentication for API ML](https://docs.zowe.org/stable/extend/extend-apiml/authentication-for-apiml-services/)

---

## Step 2 — Technology Stack Analysis

### 2.1 Zowe Server-Side Components

Zowe server components run in two deployment modes ([Zowe Architecture](https://docs.zowe.org/stable/getting-started/zowe-architecture/)):

**On z/OS (USS):**
- **API Mediation Layer** — Java-based (Spring Cloud). Runs under USS via BPXBATSL started task ZWESLSTC. Comprises Gateway (Netflix Zuul/Spring Cloud Gateway), Discovery Service (Netflix Eureka), and API Catalog.
- **Zowe Application Framework (ZLUX)** — Node.js-based app server (Express.js) providing the web desktop.
- **ZSS (Zowe System Services)** — C-based z/OS native HTTPS server on default port 7557. Provides low-level z/OS APIs (datasets, USS files, security) via REST and WebSocket. Works with ZIS (Cross Memory Server) for authorized system calls.
- **ZIS (Cross Memory Server)** — Authorized cross-memory server (runs as separate STC ZWESISTC). Not network-accessible — only serves ZSS via cross-memory calls.

**Off-platform (Linux/Docker):**
- API ML Gateway, Discovery Service, and API Catalog can run in Docker containers on Linux/z/Linux.
- ZSS and ZIS must remain on z/OS as they require z/OS system calls.

**Implication for OpenMainframe:** Since OpenMainframe runs on Linux (not z/OS), it must implement the z/OSMF REST API surface directly — it cannot use ZSS or ZIS. The target is to appear as a z/OSMF-compatible endpoint that Zowe clients connect to.

### 2.2 z/OSMF REST API — Complete Service Surface

z/OSMF exposes 20 REST service categories ([IBM z/OSMF REST Services](https://www.ibm.com/docs/en/zos/2.4.0?topic=guide-using-zosmf-rest-services)). The ones **required by Zowe** are:

| Service Category | Base Path | Priority for OpenMainframe |
|---|---|---|
| **z/OS Data Set and File REST Interface** | `/zosmf/restfiles/ds/*` (datasets), `/zosmf/restfiles/fs/*` (USS) | **CRITICAL** — Zowe Explorer primary |
| **z/OS Jobs REST Interface** | `/zosmf/restjobs/jobs/*` | **CRITICAL** — Zowe Explorer + CLI |
| **TSO/E Address Space Services** | `/zosmf/tsoApp/*` | **HIGH** — CLI `zowe tso` commands |
| **z/OS Console Services** | `/zosmf/restconsoles/consoles/*` | **HIGH** — CLI `zowe console` commands |
| **z/OSMF Authentication Services** | `/zosmf/services/authenticate` | **CRITICAL** — Login/token exchange |
| **z/OSMF Information Retrieval** | `/zosmf/info` | **CRITICAL** — Version/capability discovery |
| **z/OSMF Workflow Services** | `/zosmf/workflow/rest/*` | **LOW** — Optional for initial release |
| **WLM Resource Pooling** | `/zosmf/wlm/*` | **LOW** — Advanced feature |
| **Software Management** | `/zosmf/softwaremanagement/*` | **LOW** — Not needed initially |

### 2.3 z/OS Jobs REST API — Detailed Endpoint Specification

The Jobs REST API ([IBM z/OS Jobs REST Interface](https://www.ibm.com/docs/en/zos/2.4.0?topic=services-zos-jobs-rest-interface)) is one of the two critical API surfaces:

| Operation | Method | Path | Request Body | Response |
|---|---|---|---|---|
| **List jobs** | GET | `/zosmf/restjobs/jobs?owner=&prefix=&jobid=` | — | JSON array of job objects |
| **Get job status** | GET | `/zosmf/restjobs/jobs/{jobname}/{jobid}` | — | JSON job object |
| **Submit job** | PUT | `/zosmf/restjobs/jobs` | `application/octet-stream` (JCL) | 201 + JSON job object |
| **List spool files** | GET | `/zosmf/restjobs/jobs/{jobname}/{jobid}/files` | — | JSON array of spool DD entries |
| **Read spool content** | GET | `/zosmf/restjobs/jobs/{jobname}/{jobid}/files/{id}/records` | — | `text/plain` spool content |
| **Hold job** | PUT | `/zosmf/restjobs/jobs/{jobname}/{jobid}` | `{"request":"hold","version":"2.0"}` | JSON feedback |
| **Release job** | PUT | `/zosmf/restjobs/jobs/{jobname}/{jobid}` | `{"request":"release","version":"2.0"}` | JSON feedback |
| **Cancel job** | PUT | `/zosmf/restjobs/jobs/{jobname}/{jobid}` | `{"request":"cancel","version":"2.0"}` | JSON feedback |
| **Purge job** | DELETE | `/zosmf/restjobs/jobs/{jobname}/{jobid}` | — | 200 + JSON feedback |
| **Change class** | PUT | `/zosmf/restjobs/jobs/{jobname}/{jobid}` | `{"request":"change","class":"A"}` | JSON feedback |

**Job JSON object fields:** `jobid`, `jobname`, `owner`, `status` (INPUT/ACTIVE/OUTPUT), `type` (JOB/STC/TSU), `class`, `retcode`, `url`, `files-url`, `phase`, `phase-name`, `subsystem`

### 2.4 z/OS Data Set and File REST API — Key Endpoints

| Operation | Method | Path | Notes |
|---|---|---|---|
| **List datasets** | GET | `/zosmf/restfiles/ds?dslevel={pattern}` | Supports wildcards |
| **List PDS members** | GET | `/zosmf/restfiles/ds/{dsname}/member` | Member list |
| **Read sequential/member** | GET | `/zosmf/restfiles/ds/{dsname}` or `ds/{dsname}({member})` | Returns text content |
| **Write sequential/member** | PUT | `/zosmf/restfiles/ds/{dsname}` or `ds/{dsname}({member})` | Creates/overwrites |
| **Create dataset** | POST | `/zosmf/restfiles/ds/{dsname}` | JSON allocation params |
| **Delete dataset** | DELETE | `/zosmf/restfiles/ds/{dsname}` | — |
| **List USS files** | GET | `/zosmf/restfiles/fs?path={path}` | Directory listing |
| **Read USS file** | GET | `/zosmf/restfiles/fs/{path}` | File content |
| **Write USS file** | PUT | `/zosmf/restfiles/fs/{path}` | Creates/overwrites |
| **Delete USS file** | DELETE | `/zosmf/restfiles/fs/{path}` | — |

### 2.5 Authentication and Security

Zowe supports multiple authentication flows ([JWT Config](https://docs.zowe.org/stable/user-guide/api-mediation/configuration-jwt/), [PassTickets](https://docs.zowe.org/stable/user-guide/api-mediation/configuration-extender-passtickets/), [Client Certs](https://docs.zowe.org/stable/user-guide/api-mediation/configuration-client-certificates/)):

#### Authentication Flow Options

| Method | How It Works | OpenMainframe Support |
|---|---|---|
| **Basic Auth** (username/password) | HTTP `Authorization: Basic` header → z/OSMF validates against RACF | **Can implement** — RACF crate has `auth.rs` |
| **z/OSMF LTPA Token** | Login returns LTPA2 cookie; reused for subsequent requests | **Must implement** — Cookie-based session token |
| **Zowe JWT Token** | API ML Gateway issues JWT after auth; passed as `Authorization: Bearer` or `apimlAuthenticationToken` cookie | **Should implement** — Standard JWT library |
| **PassTickets** | Single-use encrypted tokens generated via RACF PTKTDATA class; used by API ML to authenticate to z/OSMF on behalf of user | **Complex** — Requires RACF PTKTDATA implementation |
| **X.509 Client Certificates** | Client cert mapped to RACF user via SAF/RACMAP | **Defer** — TLS client cert handling is complex |
| **OIDC** | External IdP authenticates user; API ML maps to mainframe identity | **Defer** — Requires distributed identity mapping |

#### Minimum Authentication Requirements for Zowe CLI Compatibility

1. **Basic auth** → POST `/zosmf/services/authenticate` with `Authorization: Basic <base64>` → return LTPA2 token cookie
2. **Token reuse** → Accept LTPA2 cookie on subsequent requests and map to authenticated session
3. **JWT** (preferred) → Accept `Authorization: Bearer <jwt>` and validate against RACF user

#### RACF Integration Points

The existing `open-mainframe-racf` crate provides ([`crates/open-mainframe-racf/src/`]):
- `auth.rs` — Username/password authentication
- `saf.rs` — SAF router/RACROUTE interface
- `certificate.rs` — RACDCERT certificate management
- `profile.rs` — User/group profile management
- `resource.rs` — General resource access control
- `setropts.rs` — System options (class activation, etc.)

**What's needed:** HTTP middleware that calls `racf::auth::authenticate(userid, password)` and returns a session token (LTPA2 or JWT). The SAF router should be called for resource authorization checks on each API request.

### 2.6 Transport Requirements

| Requirement | Details | OpenMainframe Approach |
|---|---|---|
| **HTTPS** | Zowe requires TLS 1.2+ for all connections | Rust `rustls` or `native-tls` crate |
| **Port** | z/OSMF default port 443 or custom (typically 10443) | Configurable in `open-mainframe-deploy` |
| **Certificates** | Server certificate trusted by Zowe clients; optionally Zowe keyring integration | Self-signed for dev; CA-signed for production |
| **CORS** | Zowe Desktop may need CORS headers for cross-origin requests | Standard CORS middleware |
| **HTTP/1.1** | z/OSMF uses HTTP/1.1; some responses are chunked | Standard in Rust HTTP frameworks |

### 2.7 Zowe API ML Service Registration

For OpenMainframe to appear in the Zowe API Catalog and be routed through the Gateway, it must register with the Discovery Service ([Direct Eureka Call](https://docs.zowe.org/stable/extend/extend-apiml/onboard-direct-eureka-call/)):

**Static definition (Minimum conformance):**
```yaml
# Static API ML definition file
services:
  - serviceId: openmainframe
    title: OpenMainframe z/OSMF Compatible API
    description: Open-source z/OS emulator REST API
    catalogUiTileId: openmainframe
    instanceBaseUrls:
      - https://openmainframe-host:10443
    routes:
      - gatewayUrl: api/v1
        serviceRelativeUrl: /zosmf
    authentication:
      scheme: zosmf
    apiInfo:
      - apiId: org.openmainframe.zosmf
        gatewayUrl: api/v1
        version: 1.0.0
```

**Dynamic registration (Preferred conformance):**
- POST to `https://{eureka}:{port}/eureka/apps/{serviceId}` with XML metadata
- Send heartbeat every 30 seconds via PUT to `eureka/apps/{serviceId}/{instanceId}`
- Requires HTTPS mutual TLS with Eureka server

### Sources — Step 2

- [IBM z/OSMF REST Services](https://www.ibm.com/docs/en/zos/2.4.0?topic=guide-using-zosmf-rest-services)
- [z/OS Jobs REST Interface](https://www.ibm.com/docs/en/zos/2.4.0?topic=services-zos-jobs-rest-interface)
- [z/OSMF One Stop Hub](https://ibm.github.io/zOSMF/)
- [IBM z/OSMF REST Client Samples](https://github.com/IBM/IBM-Z-zOS/blob/main/zOSMF/ZosmfRESTClient/README.md)
- [Zowe JWT Configuration](https://docs.zowe.org/stable/user-guide/api-mediation/configuration-jwt/)
- [Zowe PassTickets Configuration](https://docs.zowe.org/stable/user-guide/api-mediation/configuration-extender-passtickets/)
- [Zowe Client Certificates](https://docs.zowe.org/stable/user-guide/api-mediation/configuration-client-certificates/)
- [Direct Eureka Registration](https://docs.zowe.org/stable/extend/extend-apiml/onboard-direct-eureka-call/)
- [API ML Discovery Service Config](https://docs.zowe.org/stable/user-guide/api-mediation/discovery-service-configuration/)
- [Zowe API ML GitHub](https://github.com/zowe/api-layer)
- [SSO to z/OS REST APIs with Zowe (Medium)](https://medium.com/zowe/single-sign-on-to-z-os-rest-apis-with-zowe-6e35fd022a95)
- [Zowe USS Requirements](https://docs.zowe.org/v3.0.x/user-guide/configure-uss/)

---

## Step 3 — Integration Patterns Analysis

### 3.1 Complete Zowe-to-z/OS Interface Mapping

This section maps every Zowe feature to the exact z/OSMF REST endpoint it uses, then maps that to the corresponding OpenMainframe crate and identifies specific gaps.

### 3.2 Dataset Operations — `/zosmf/restfiles/ds`

**What Zowe Does:** Zowe Explorer and Zowe CLI (`zowe zos-files`) allow users to list, read, write, create, and delete datasets and PDS members through z/OSMF REST APIs ([IBM Dataset REST Interface](https://www.ibm.com/support/knowledgecenter/SSLTBW_2.4.0/com.ibm.zos.v2r4.izua700/IZUHPINFO_API_RESTFILES.htm)).

| z/OSMF Endpoint | Method | Operation | OpenMainframe Crate API | Gap |
|---|---|---|---|---|
| `/zosmf/restfiles/ds?dslevel={pattern}` | GET | List datasets matching pattern | `Catalog::lookup()` + `CatalogEntry` | **LOW** — Catalog search exists; need REST handler + JSON serialization of `DatasetAttributes` |
| `/zosmf/restfiles/ds/{dsname}/member` | GET | List PDS/PDSE members | `Pds::list_members()` returns `Vec<PdsMember>` | **LOW** — PDS member listing exists; need JSON serialization with `IspfStats` |
| `/zosmf/restfiles/ds/{dsname}` | GET | Read sequential dataset | `QsamReader::read_all_records()` | **LOW** — Sequential read exists; need content-type negotiation (text vs binary) |
| `/zosmf/restfiles/ds/{dsname}({member})` | GET | Read PDS member | `Pds::read_member()` via `bpam_read_member()` | **LOW** — Member read exists; need REST wrapper |
| `/zosmf/restfiles/ds/{dsname}` | PUT | Write sequential dataset | `QsamWriter::write_records()` | **LOW** — Sequential write exists; need HTTP body parsing |
| `/zosmf/restfiles/ds/{dsname}({member})` | PUT | Write PDS member | `Pds` write operations | **LOW** — Write exists; need REST wrapper |
| `/zosmf/restfiles/ds/{dsname}` | POST | Create dataset | `Idcams` + allocation attributes from `DatasetAttributes` | **MEDIUM** — Allocation exists via IDCAMS; need JSON-to-`DatasetAttributes` mapping (recfm, lrecl, blksize, dsorg, primary, secondary, etc.) |
| `/zosmf/restfiles/ds/{dsname}` | DELETE | Delete dataset | `Idcams` delete or catalog removal | **MEDIUM** — Delete exists; need REST wrapper with error handling |
| `/zosmf/restfiles/ds/{dsname}({member})` | DELETE | Delete PDS member | `Pds` member deletion | **MEDIUM** — Need to verify member delete API exists |

**Required HTTP Headers:**
- `X-IBM-Max-Items` — Pagination control for list operations
- `X-IBM-Attributes` — Controls which dataset attributes are returned (`base`, `vol`, `dsname`)
- `X-CSRF-ZOSMF-HEADER` — CSRF protection (any non-empty value)
- `Authorization: Basic <base64>` or Bearer token

**JSON Response Format for Dataset List:**
```json
{
  "items": [
    {
      "dsname": "HLQ.DATA.SET",
      "blksz": "800",
      "catnm": "CATALOG.MASTER",
      "cdate": "2024/01/15",
      "dev": "3390",
      "dsorg": "PS",
      "extx": "1",
      "lrecl": "80",
      "migr": "NO",
      "mvol": "N",
      "ovf": "",
      "rdate": "2024/06/15",
      "recfm": "FB",
      "sizex": "15",
      "spacu": "TRACKS",
      "used": "33",
      "vol": "VOL001"
    }
  ],
  "returnedRows": 1,
  "totalRows": 1,
  "JSONversion": 1
}
```

**OpenMainframe Gap:** The `open-mainframe-dataset` crate has comprehensive dataset I/O (`QsamReader`, `QsamWriter`, `BsamReader`, `BsamWriter`, `Pds`, `Vsam`, `Catalog`). The gap is purely the REST/JSON layer — no dataset functionality is missing; it just needs HTTP wrappers and JSON serialization of existing types like `DatasetAttributes`, `CatalogEntry`, `PdsMember`, and `IspfStats`.

### 3.3 Job Operations — `/zosmf/restjobs/jobs`

**What Zowe Does:** Zowe Explorer and CLI (`zowe zos-jobs`) allow users to submit JCL, monitor job status, view spool output, and manage jobs ([IBM Jobs REST Interface](https://www.ibm.com/docs/en/zos/2.4.0?topic=services-zos-jobs-rest-interface)).

| z/OSMF Endpoint | Method | Operation | OpenMainframe Crate API | Gap |
|---|---|---|---|---|
| `/zosmf/restjobs/jobs?owner=&prefix=&jobid=` | GET | List jobs by filter | `Jes2` queue query + `Job` structs | **LOW** — Job queue exists; need filter logic + JSON serialization |
| `/zosmf/restjobs/jobs/{jobname}/{jobid}` | GET | Get job status | `Job` state lookup via `JobId` | **LOW** — Job status exists; need REST handler |
| `/zosmf/restjobs/jobs` | PUT | Submit JCL | `InternalReader` + JCL parsing pipeline | **MEDIUM** — Internal reader exists; need HTTP→JCL submission bridge |
| `/zosmf/restjobs/jobs/{name}/{id}/files` | GET | List spool files | `SpoolManager` + `OutputDescriptor` | **LOW** — Spool listing exists; need JSON format |
| `/zosmf/restjobs/jobs/{name}/{id}/files/{n}/records` | GET | Read spool content | `SpoolManager` spool data retrieval | **LOW** — Spool read exists; need text streaming |
| `/zosmf/restjobs/jobs/{name}/{id}` | PUT | Hold job | `Jes2::execute_command()` with hold | **MEDIUM** — JES2 commands exist; need REST→command mapping |
| `/zosmf/restjobs/jobs/{name}/{id}` | PUT | Release job | `Jes2::execute_command()` with release | **MEDIUM** — Same as hold |
| `/zosmf/restjobs/jobs/{name}/{id}` | PUT | Cancel job | `Jes2::execute_command()` with cancel | **MEDIUM** — Same as hold |
| `/zosmf/restjobs/jobs/{name}/{id}` | DELETE | Purge job | `Jes2::execute_command()` with purge | **MEDIUM** — Same as hold |

**JSON Response Format for Job Status:**
```json
{
  "jobid": "JOB00123",
  "jobname": "MYJOB",
  "owner": "IBMUSER",
  "status": "OUTPUT",
  "type": "JOB",
  "class": "A",
  "retcode": "CC 0000",
  "url": "https://host/zosmf/restjobs/jobs/MYJOB/JOB00123",
  "files-url": "https://host/zosmf/restjobs/jobs/MYJOB/JOB00123/files",
  "subsystem": "JES2",
  "phase": 20,
  "phase-name": "Job is on the hard copy queue"
}
```

**OpenMainframe Gap:** The `open-mainframe-jes2` crate has `Jes2` (queue management), `Job`/`JobId`/`JobState`/`JobClass` types, `SpoolManager`, `InternalReader`, and `Jes2Command`. The SDSF panel functions (`sdsf_display_active`, `sdsf_browse_output`, `sdsf_input_queue`) already provide the query/view logic. The gap is REST/JSON wrappers and mapping `JobState` enum to z/OSMF status strings.

### 3.4 TSO Address Space — `/zosmf/tsoApp`

**What Zowe Does:** Zowe CLI (`zowe zos-tso`) issues TSO commands via z/OSMF's TSO REST API. Two approaches exist ([IBM TSO REST API](https://www.ibm.com/docs/en/zos/2.5.0?topic=services-issue-tsoe-command-zosmf-rest-api)):

| z/OSMF Endpoint | Method | Operation | OpenMainframe Crate API | Gap |
|---|---|---|---|---|
| `/zosmf/tsoApp/tso` (stateless) | POST | Issue TSO command (single request) | `TsoSession::execute()` | **MEDIUM** — TSO execute exists; need stateless REST wrapper that creates/destroys session per request |
| `/zosmf/tsoApp/tso/{servletKey}` | POST | Start TSO address space | `TsoSession::new()` with `TsoProfile` | **MEDIUM** — Session creation exists; need servlet key management |
| `/zosmf/tsoApp/tso/{servletKey}` | PUT | Send command to address space | `TsoSession::execute()` via `TsoIo` | **MEDIUM** — Command execution exists; need async response handling |
| `/zosmf/tsoApp/tso/{servletKey}` | GET | Receive response from address space | `TsoIo` / `MemoryIo` output | **MEDIUM** — I/O exists; need polling/response buffering |
| `/zosmf/tsoApp/tso/{servletKey}` | DELETE | Stop TSO address space | Session cleanup | **LOW** — Cleanup is straightforward |

**OpenMainframe Gap:** The `open-mainframe-tso` crate has `TsoSession`, `TsoProfile`, command parsing/execution, `TsoIo`/`MemoryIo` for I/O, and REXX integration via `RexxTsoHost`. The main gap is servlet key (session ID) management for the multi-step TSO API and the stateless single-call shortcut endpoint.

### 3.5 Console Commands — `/zosmf/restconsoles`

**What Zowe Does:** Zowe CLI (`zowe zos-console issue command`) issues MVS operator commands via z/OSMF console REST API ([IBM Console REST](https://ibm.github.io/ibm_zos_zosmf/modules/zmf_console_command.html)).

| z/OSMF Endpoint | Method | Operation | OpenMainframe Crate API | Gap |
|---|---|---|---|---|
| `/zosmf/restconsoles/consoles/{name}` | PUT | Issue MVS console command | `TsoSession::execute()` (console commands) | **MEDIUM** — TSO can execute commands; need console-specific wrapper with EMCS console semantics |
| `/zosmf/restconsoles/consoles/{name}` | GET | Get solicited response | Response buffering | **HIGH** — Need async response collection with keyword matching (`sol-key`) |

**Request JSON:**
```json
{
  "cmd": "D A,L",
  "sol-key": "IEE114I",
  "async": "N"
}
```

**OpenMainframe Gap:** Console command issuing maps to TSO/operator commands. The gap is the EMCS console simulation — creating a named console, routing command output to it, and matching solicited responses by keyword.

### 3.6 USS File Operations — `/zosmf/restfiles/fs`

**What Zowe Does:** Zowe Explorer shows USS (Unix System Services) files in a tree view. Zowe CLI (`zowe zos-files`) can list, read, write, and delete USS files.

| z/OSMF Endpoint | Method | Operation | OpenMainframe Crate API | Gap |
|---|---|---|---|---|
| `/zosmf/restfiles/fs?path={path}` | GET | List directory | **None** | **CRITICAL** — No USS emulation exists |
| `/zosmf/restfiles/fs/{path}` | GET | Read file | **None** | **CRITICAL** — No USS file I/O |
| `/zosmf/restfiles/fs/{path}` | PUT | Write file | **None** | **CRITICAL** — No USS file I/O |
| `/zosmf/restfiles/fs/{path}` | DELETE | Delete file/dir | **None** | **CRITICAL** — No USS file ops |
| `/zosmf/restfiles/fs/{path}` | POST | Create dir/change perms | **None** | **CRITICAL** — No USS file ops |

**OpenMainframe Gap:** This is the largest functional gap. OpenMainframe has no USS (Unix System Services) emulation. Options:
1. **Map USS paths to host filesystem** — Simplest approach; map `/u/userid/` to a directory on the host OS. Low fidelity but functional.
2. **Implement USS emulation** — Create `open-mainframe-uss` crate with POSIX-like file operations, permissions, and uid/gid mapping to RACF.
3. **Hybrid** — Host filesystem for data, RACF for permission checks.

**Recommendation:** Option 1 (host filesystem mapping) for initial release; Option 2 for future enhancement.

### 3.7 Authentication — `/zosmf/services/authenticate`

| z/OSMF Endpoint | Method | Operation | OpenMainframe Crate API | Gap |
|---|---|---|---|---|
| `/zosmf/services/authenticate` | POST | Login (Basic Auth → LTPA2/JWT) | `RacfDatabase` + `AuthService::authenticate()` | **MEDIUM** — Auth exists; need token generation |
| `/zosmf/services/authenticate` | DELETE | Logout (invalidate token) | Token store invalidation | **MEDIUM** — Need token management |

**OpenMainframe Gap:** The `open-mainframe-racf` crate has `AuthService` with `authenticate()` returning `AuthResult`, and `PasswordPolicy` for password rules. What's needed: a token store (in-memory or persistent) that maps LTPA2/JWT tokens to authenticated RACF sessions.

### 3.8 System Information — `/zosmf/info`

| z/OSMF Endpoint | Method | Operation | OpenMainframe Crate API | Gap |
|---|---|---|---|---|
| `/zosmf/info` | GET | Return z/OSMF version & capabilities | **None** | **MEDIUM** — Need static response with OpenMainframe version info |

**Required Response:**
```json
{
  "api_version": "1",
  "plugins": [
    {"pluginVersion": "HSMA230", "pluginDefaultName": "z/OS Jobs", "pluginStatus": "ACTIVE"},
    {"pluginVersion": "HSMA230", "pluginDefaultName": "z/OS Data Sets and Unix Files", "pluginStatus": "ACTIVE"},
    {"pluginVersion": "HSMA230", "pluginDefaultName": "TSO/E Address Spaces", "pluginStatus": "ACTIVE"},
    {"pluginVersion": "HSMA230", "pluginDefaultName": "z/OS Consoles", "pluginStatus": "ACTIVE"}
  ],
  "zos_version": "02.05.00",
  "zosmf_full_version": "27.0",
  "zosmf_hostname": "openmainframe-host",
  "zosmf_port": "10443",
  "zosmf_saf_realm": "SAFRealm",
  "zosmf_version": "27"
}
```

**OpenMainframe Gap:** Straightforward — return a static/configured JSON response advertising which capabilities are enabled.

### 3.9 TN3270E Terminal — Direct Protocol

| Interface | Protocol | OpenMainframe Crate | Gap |
|---|---|---|---|
| 3270 terminal access | TN3270E over TCP | `open-mainframe-tui` (`tn3270` module) | **MEDIUM** — TUI crate implements TN3270E protocol with terminal models, structured fields, DBCS, but operates as a client-side renderer. Need a **server mode** that accepts TN3270E connections and serves ISPF/TSO screens. |

**OpenMainframe Gap:** The `open-mainframe-tui` crate has `Session`, `TerminalModel`, `tn3270` protocol handling, structured fields, and event processing. It currently functions as a TUI client. For Zowe Desktop's TN3270 app to connect, OpenMainframe needs a TCP listener that accepts TN3270E connections and maps them to `TsoSession`/`IspfSession` backends.

### 3.10 Consolidated Gap Summary

| Priority | Component | Gap Description | Effort |
|---|---|---|---|
| **P0 — CRITICAL** | z/OSMF REST Server | New `open-mainframe-zosmf` crate: HTTP server, routing, JSON serialization | **XL** |
| **P0 — CRITICAL** | Authentication | JWT/LTPA2 token generation, RACF integration, session management | **L** |
| **P0 — CRITICAL** | `/zosmf/info` | System info endpoint | **S** |
| **P1 — HIGH** | Dataset REST handlers | JSON wrappers around `open-mainframe-dataset` APIs | **L** |
| **P1 — HIGH** | Jobs REST handlers | JSON wrappers around `open-mainframe-jes2` APIs | **L** |
| **P1 — HIGH** | TSO REST handlers | Session management + JSON wrappers around `open-mainframe-tso` | **M** |
| **P2 — MEDIUM** | Console REST handlers | EMCS console simulation, command routing | **M** |
| **P2 — MEDIUM** | USS File Operations | Host filesystem mapping or new `open-mainframe-uss` crate | **L–XL** |
| **P2 — MEDIUM** | TN3270E Server | Server-mode listener for `open-mainframe-tui` | **L** |
| **P3 — LOW** | API ML Registration | Eureka client for dynamic service discovery | **M** |
| **P3 — LOW** | CSRF Protection | `X-CSRF-ZOSMF-HEADER` validation middleware | **S** |
| **P3 — LOW** | Pagination | `X-IBM-Max-Items` / `X-IBM-Attributes` header support | **S** |

### Sources — Step 3

- [IBM Dataset and File REST Interface](https://www.ibm.com/support/knowledgecenter/SSLTBW_2.4.0/com.ibm.zos.v2r4.izua700/IZUHPINFO_API_RESTFILES.htm)
- [IBM Jobs REST Interface](https://www.ibm.com/docs/en/zos/2.4.0?topic=services-zos-jobs-rest-interface)
- [IBM TSO REST API](https://www.ibm.com/docs/en/zos/2.5.0?topic=services-issue-tsoe-command-zosmf-rest-api)
- [IBM Console Command Module](https://ibm.github.io/ibm_zos_zosmf/modules/zmf_console_command.html)
- [Zowe Explorer GitHub](https://github.com/zowe/zowe-explorer-vscode)
- [Zowe Explorer API README](https://github.com/zowe/vscode-extension-for-zowe/blob/main/packages/zowe-explorer-api/README.md)
- [Common z/OSMF Restfiles Issues](https://community.ibm.com/community/user/blogs/joe-malinowski1/2023/12/13/common-zosmf-restfiles-and-datasets-api-issues)
- [z/OSMF One Stop Hub](https://ibm.github.io/zOSMF/)

---

## Step 4 — Architectural Patterns

### 4.1 Three Architecture Options

Based on the research in Steps 1–3, three architectural approaches are viable for making OpenMainframe Zowe-compatible.

### Option A: z/OSMF-Compatible REST Server (Recommended)

**Concept:** Build a new `open-mainframe-zosmf` crate that implements the z/OSMF REST API surface in Rust using the Axum framework. This crate acts as an HTTP server that translates z/OSMF REST requests into calls to existing OpenMainframe crates (`dataset`, `jes2`, `tso`, `racf`, etc.).

```
Zowe CLI / Explorer / API ML
        |
        | HTTPS (z/OSMF REST API)
        v
+---------------------------+
| open-mainframe-zosmf      |  <-- NEW CRATE (Axum HTTP server)
|  /zosmf/restfiles/ds/*    |----> open-mainframe-dataset
|  /zosmf/restjobs/jobs/*   |----> open-mainframe-jes2
|  /zosmf/tsoApp/*          |----> open-mainframe-tso
|  /zosmf/restconsoles/*    |----> open-mainframe-tso
|  /zosmf/restfiles/fs/*    |----> open-mainframe-uss (NEW) or host FS
|  /zosmf/services/auth     |----> open-mainframe-racf
|  /zosmf/info              |----> static config
+---------------------------+
```

**Analysis:**

| Criterion | Assessment |
|---|---|
| **Implementation Complexity** | **MEDIUM-HIGH** — Requires building ~40 REST endpoints across 6 service categories. Axum provides excellent ergonomics for this in Rust. Most work is JSON serialization and adapter logic. |
| **Compatibility Coverage** | **EXCELLENT** — Full Zowe CLI, Explorer, and API ML compatibility. Zowe clients talk to OpenMainframe exactly as they would to a real z/OSMF instance. |
| **Authentication Integration** | **GOOD** — Direct integration: HTTP Basic Auth → `racf::AuthService::authenticate()` → JWT/LTPA2 token response. No intermediary needed. |
| **Performance** | **EXCELLENT** — Rust + Axum is among the fastest web frameworks. Direct function calls to crate APIs with zero serialization overhead internally. |
| **Maintenance Burden** | **MEDIUM** — Must track z/OSMF REST API spec changes. IBM's API is stable but evolves with new z/OS releases. |
| **Zowe Conformance** | **FULL** — Can achieve both Minimum and Preferred API ML conformance via static definition or Eureka registration. |

**Pros:**
- Maximum compatibility — Zowe treats OpenMainframe as a real z/OS
- Single server process — no sidecar or bridge services needed
- Rust type safety ensures API contract compliance
- Existing crate APIs map cleanly to z/OSMF operations
- Can be tested with Zowe CLI directly

**Cons:**
- Must implement z/OSMF API spec faithfully (40+ endpoints)
- Some z/OSMF response fields may be z/OS-specific and hard to populate authentically
- No existing open-source z/OSMF implementation to reference ([no mock servers found](https://ibm.github.io/zOSMF/))

### Option B: Zowe API ML Plugin

**Concept:** Instead of implementing z/OSMF, create a custom Zowe-conformant service that registers with the API ML and provides OpenMainframe's functionality through a custom API. Zowe CLI/Explorer would use a custom plugin to talk to this API.

```
Zowe CLI + Custom Plugin     Zowe Explorer + Custom Extension
        |                              |
        | Custom REST API              | Custom REST API
        v                              v
+-------------------------------------------+
| Zowe API ML Gateway                       |
|  /api/v1/openmainframe/datasets/*         |
|  /api/v1/openmainframe/jobs/*             |
+-------------------------------------------+
        |
        | Eureka registration
        v
+-------------------------------------------+
| open-mainframe-api (Custom REST server)   |
+-------------------------------------------+
```

**Analysis:**

| Criterion | Assessment |
|---|---|
| **Implementation Complexity** | **MEDIUM** — Freedom to design own API; less constrained by z/OSMF spec. But must build Zowe CLI plugin + Explorer extension too. |
| **Compatibility Coverage** | **POOR** — Standard Zowe CLI commands (`zowe zos-files`, `zowe zos-jobs`) won't work. Requires custom plugins. Zowe Explorer's built-in views won't work without custom extension. |
| **Authentication Integration** | **GOOD** — Can use any auth method; API ML handles SSO. |
| **Performance** | **GOOD** — Same Rust performance, but extra hop through API ML Gateway. |
| **Maintenance Burden** | **HIGH** — Must maintain custom CLI plugin, custom Explorer extension, AND server. Three components instead of one. |
| **Zowe Conformance** | **PARTIAL** — Conformant as an API ML service, but not as a z/OSMF replacement. |

**Pros:**
- API can be optimized for OpenMainframe's data model
- Not constrained by z/OSMF response format quirks
- Could expose features z/OSMF doesn't support

**Cons:**
- Breaks standard Zowe CLI/Explorer workflows
- Requires building and maintaining CLI plugin + VS Code extension
- Users must install additional plugins
- Does not achieve the goal of "connect as if it were a real z/OS"

### Option C: Hybrid Approach

**Concept:** Implement z/OSMF REST API compatibility for core operations (Option A) plus custom Zowe plugins for extended features that z/OSMF doesn't natively support (e.g., CICS, IMS, MQ management).

```
Zowe CLI (standard)     Zowe CLI + OpenMainframe Plugin
        |                        |
        | z/OSMF REST            | Custom REST
        v                        v
+---------------------------+---------------------------+
| open-mainframe-zosmf      | open-mainframe-api        |
| (z/OSMF compat endpoints) | (extended endpoints)      |
+---------------------------+---------------------------+
        |                        |
        +--- Both served by same Axum server ---+
```

**Analysis:**

| Criterion | Assessment |
|---|---|
| **Implementation Complexity** | **HIGH** — Superset of Option A; additional custom API surface. |
| **Compatibility Coverage** | **EXCELLENT+** — Standard Zowe works for core ops. Extra plugin provides CICS/IMS/MQ/REXX features. |
| **Authentication Integration** | **GOOD** — Same as Option A for core; custom endpoints share auth. |
| **Performance** | **EXCELLENT** — Single server, shared auth, Rust speed. |
| **Maintenance Burden** | **HIGH** — z/OSMF API tracking + custom API + custom plugins. |
| **Zowe Conformance** | **FULL+** — z/OSMF conformance + custom conformant extensions. |

### 4.2 Architectural Recommendation

**Option A (z/OSMF-Compatible REST Server) is the recommended approach** for the following reasons:

1. **Goal alignment** — The research question asks "how to connect via Zowe as if it were a real z/OS." Option A achieves this directly.
2. **Minimum viable product** — One new crate (`open-mainframe-zosmf`) with no external dependencies beyond Axum.
3. **Standard tooling** — Users can use `zowe zos-files ls ds`, `zowe zos-jobs submit`, etc. without any custom plugins.
4. **Future extensibility** — Option C can be pursued later by adding custom endpoints to the same server.
5. **Testing** — Can be validated by simply pointing Zowe CLI at `https://openmainframe:10443`.

**Option C should be the long-term target** — start with Option A, then add custom endpoints for CICS, IMS, MQ, and other subsystems that z/OSMF doesn't cover.

### 4.3 Proposed Architecture — `open-mainframe-zosmf` Crate

```
crates/open-mainframe-zosmf/
├── Cargo.toml
└── src/
    ├── lib.rs              # Crate root, server builder
    ├── server.rs           # Axum server setup, TLS, port binding
    ├── auth.rs             # Authentication middleware (Basic, JWT, LTPA2)
    ├── csrf.rs             # X-CSRF-ZOSMF-HEADER validation
    ├── info.rs             # GET /zosmf/info handler
    ├── datasets.rs         # /zosmf/restfiles/ds/* handlers
    ├── files.rs            # /zosmf/restfiles/fs/* handlers (USS)
    ├── jobs.rs             # /zosmf/restjobs/jobs/* handlers
    ├── tso.rs              # /zosmf/tsoApp/* handlers
    ├── console.rs          # /zosmf/restconsoles/* handlers
    ├── types.rs            # Shared JSON request/response types
    ├── error.rs            # Error types and HTTP error responses
    └── eureka.rs           # Optional: API ML dynamic registration
```

**Dependencies:**
- `axum` — HTTP framework
- `axum-server` — TLS support via `rustls`
- `tower-http` — CORS, tracing, compression middleware
- `serde` / `serde_json` — JSON serialization
- `jsonwebtoken` — JWT token generation/validation
- `tokio` — Async runtime
- Internal: `open-mainframe-dataset`, `open-mainframe-jes2`, `open-mainframe-tso`, `open-mainframe-racf`, `open-mainframe-deploy`

### Sources — Step 4

- [Axum Documentation](https://docs.rs/axum/latest/axum/)
- [Axum GitHub](https://github.com/tokio-rs/axum)
- [Axum TLS Discussion](https://github.com/tokio-rs/axum/discussions/1032)
- [axum-server Crate](https://docs.rs/axum-server)
- [Zowe API ML Onboarding Overview](https://docs.zowe.org/v2.7.x/extend/extend-apiml/onboard-overview/)
- [Static Onboarding (No Code Changes)](https://docs.zowe.org/v3.2.x/extend/extend-apiml/onboard-static-definition/)
- [Zowe Conformance Test Evaluation Guide](https://github.com/openmainframeproject/foundation/blob/main/zowe_conformance/test_evaluation_guide.md)
- [IBM z/OSMF REST Client Samples](https://github.com/IBM/IBM-Z-zOS/blob/main/zOSMF/ZosmfRESTClient/README.md)

---

## Step 5 — Implementation Research

### 5.1 Rust HTTP/REST Framework Selection

**Recommendation: Axum** ([docs.rs/axum](https://docs.rs/axum/latest/axum/), [GitHub](https://github.com/tokio-rs/axum))

| Framework | JSON Support | TLS | Middleware | Ecosystem | Recommendation |
|---|---|---|---|---|---|
| **Axum** | Built-in `axum::Json` + `serde` | Via `axum-server` + `rustls` | Tower ecosystem (tracing, CORS, compression, auth) | Tokio team, largest community | **RECOMMENDED** |
| Actix-web | Built-in | Built-in | Custom middleware system | Large, but separate ecosystem | Good alternative |
| Rocket | Built-in | Built-in | Fairings | Smaller community | Less flexible |
| Warp | Built-in | Built-in via `hyper` | Filter-based | Smaller community | More complex API |

**Why Axum:**
- Backed by the Tokio team — same async runtime OpenMainframe already uses
- `#![forbid(unsafe_code)]` — 100% safe Rust ([Axum docs](https://docs.rs/axum/latest/axum/))
- Macro-free routing API — easier to maintain
- Tower middleware compatibility — reuse tracing/metrics from `open-mainframe-deploy`
- Current version 0.8.x stable, 0.9 in development
- Comparable performance to raw `hyper` ([Shuttle Guide](https://www.shuttle.dev/blog/2023/12/06/using-axum-rust))

**Example z/OSMF endpoint in Axum:**
```rust
use axum::{extract::Path, Json, Router};
use serde::Serialize;

#[derive(Serialize)]
struct ZosmfInfo {
    api_version: String,
    zosmf_version: String,
    zosmf_hostname: String,
    zosmf_port: String,
    zosmf_saf_realm: String,
    plugins: Vec<ZosmfPlugin>,
}

async fn get_info() -> Json<ZosmfInfo> {
    Json(ZosmfInfo {
        api_version: "1".into(),
        zosmf_version: "27".into(),
        // ...
    })
}

fn zosmf_router() -> Router {
    Router::new()
        .route("/zosmf/info", axum::routing::get(get_info))
        .route("/zosmf/restfiles/ds", axum::routing::get(list_datasets))
        .route("/zosmf/restjobs/jobs", axum::routing::get(list_jobs).put(submit_job))
        // ...
}
```

### 5.2 JSON Schema Compliance

z/OSMF responses follow specific JSON field naming conventions (snake_case with hyphens for some fields like `files-url`). Serde's `#[serde(rename)]` handles this:

```rust
#[derive(Serialize)]
struct JobResponse {
    jobid: String,
    jobname: String,
    owner: String,
    status: String,  // "INPUT" | "ACTIVE" | "OUTPUT"
    #[serde(rename = "type")]
    job_type: String, // "JOB" | "STC" | "TSU"
    class: String,
    retcode: Option<String>,
    url: String,
    #[serde(rename = "files-url")]
    files_url: String,
    subsystem: String,
    phase: u32,
    #[serde(rename = "phase-name")]
    phase_name: String,
}
```

**Key consideration:** z/OSMF returns some fields as strings even for numeric values (e.g., `"port": "443"`, `"blksz": "800"`). The OpenMainframe implementation must match this behavior exactly for Zowe client compatibility.

### 5.3 TLS/Certificate Handling

**Approach:** Use `axum-server` with `rustls` for TLS termination.

```rust
use axum_server::tls_rustls::RustlsConfig;

let config = RustlsConfig::from_pem_file("cert.pem", "key.pem").await?;
axum_server::bind_rustls(addr, config)
    .serve(app.into_make_service())
    .await?;
```

**Certificate requirements:**
- Self-signed certificates for development (Zowe CLI accepts `--reject-unauthorized false`)
- CA-signed certificates for production
- PKCS#12 or PEM format support
- Optional: Client certificate authentication for future Zowe mTLS support

**Crate dependencies:** `rustls`, `axum-server` (with `tls-rustls` feature), `rcgen` (for self-signed cert generation in dev mode).

### 5.4 Service Discovery Registration

For Zowe API ML integration, OpenMainframe can register via two methods:

**Method 1: Static Definition (Minimum Conformance)**

Place a YAML file in the API ML's static definitions directory:
```yaml
services:
  - serviceId: openmainframe
    title: OpenMainframe z/OSMF API
    description: Open-source z/OS emulator REST API
    catalogUiTileId: openmainframe
    instanceBaseUrls:
      - https://openmainframe-host:10443
    routes:
      - gatewayUrl: api/v1
        serviceRelativeUrl: /zosmf
    authentication:
      scheme: zosmf
    apiInfo:
      - apiId: org.openmainframe.zosmf
        gatewayUrl: api/v1
        version: 1.0.0
catalogUiTiles:
  openmainframe:
    title: OpenMainframe
    description: Open-source mainframe emulator with z/OSMF-compatible API
```

**Method 2: Dynamic Eureka Registration (Preferred Conformance)**

Implement a Eureka client in the `eureka.rs` module:
```rust
// POST https://{eureka}:{port}/eureka/apps/OPENMAINFRAME
// with XML registration body
// Heartbeat: PUT every 30 seconds
// Deregister: DELETE on shutdown
```

**Recommendation:** Start with static definition (no code needed, just a YAML file). Add dynamic registration in a later epic.

### 5.5 Zowe CLI Profile Configuration

Users connecting Zowe CLI to OpenMainframe would use:

**Direct connection (no API ML):**
```json
{
  "profiles": {
    "openmainframe": {
      "properties": { "host": "openmainframe-host" },
      "profiles": {
        "zosmf": {
          "type": "zosmf",
          "properties": { "port": 10443 }
        }
      }
    },
    "base": {
      "type": "base",
      "properties": { "rejectUnauthorized": false },
      "secure": ["user", "password"]
    }
  }
}
```

**Via API ML:**
```json
{
  "profiles": {
    "zosmf": {
      "type": "zosmf",
      "properties": { "basePath": "openmainframe/api/v1" }
    },
    "base": {
      "type": "base",
      "properties": {
        "host": "apiml-gateway",
        "port": 7554,
        "tokenType": "apimlAuthenticationToken"
      },
      "secure": ["tokenValue"]
    }
  }
}
```

Or via legacy command:
```bash
zowe profiles create zosmf-profile openmainframe \
  --host openmainframe-host --port 10443 \
  --user IBMUSER --password secret \
  --reject-unauthorized false
```

([Zowe Profile Configuration](https://docs.zowe.org/stable/user-guide/cli-using-creating-profiles/))

### 5.6 Testing Strategy

**Tier 1 — Unit Tests (Rust):**
- Test each z/OSMF endpoint handler in isolation
- Mock `dataset`, `jes2`, `tso`, `racf` crate responses
- Validate JSON response schemas match z/OSMF spec
- Test auth middleware (valid/invalid credentials, token expiry)

**Tier 2 — Integration Tests (Zowe CLI):**
```bash
# Test dataset operations
zowe zos-files list ds "HLQ.*" --zosmf-profile openmainframe
zowe zos-files download ds "HLQ.TEST.DATA" -f ./test.txt --zosmf-profile openmainframe
zowe zos-files upload ftds ./test.txt "HLQ.TEST.DATA" --zosmf-profile openmainframe

# Test job operations
zowe zos-jobs submit lf ./test.jcl --zosmf-profile openmainframe
zowe zos-jobs list jobs --owner IBMUSER --zosmf-profile openmainframe
zowe zos-jobs view sfbi JOB00123 1 --zosmf-profile openmainframe

# Test TSO
zowe zos-tso issue cmd "TIME" --zosmf-profile openmainframe
```

**Tier 3 — Zowe Explorer (VS Code):**
- Configure Zowe Explorer to point at OpenMainframe
- Verify dataset tree view loads
- Verify job tree view loads
- Verify USS tree view loads (if USS implemented)
- Verify edit/save roundtrip for PDS members

**Tier 4 — Zowe Conformance (Optional):**
- Use the [Zowe Conformance Test Evaluation Guide](https://github.com/openmainframeproject/foundation/blob/main/zowe_conformance/test_evaluation_guide.md) for self-certification
- Submit for formal conformance if targeting Zowe marketplace listing

### 5.7 Existing Reference Implementations

No open-source z/OSMF mock server was found in the research. The closest references are:
- **IBM z/OSMF REST Client Samples** ([GitHub](https://github.com/IBM/IBM-Z-zOS/blob/main/zOSMF/ZosmfRESTClient/README.md)) — Java/JavaScript client samples showing request/response formats
- **Zowe CLI source code** ([GitHub](https://github.com/zowe/zowe-cli)) — The CLI's z/OSMF SDK shows exact expected request/response formats
- **Zowe Explorer ZosmfApi** ([GitHub](https://github.com/zowe/zowe-explorer-vscode/blob/main/packages/zowe-explorer-api/src/profiles/ZoweExplorerZosmfApi.ts)) — TypeScript implementation showing which z/OSMF endpoints Explorer calls
- **General API mock tools** (WireMock, Prism, Mockoon) — Could be used to prototype z/OSMF responses before building the Rust server

**Recommendation:** Use the Zowe CLI source code as the definitive reference for expected request/response formats, as it is the primary consumer of the z/OSMF API.

### Sources — Step 5

- [Axum Documentation](https://docs.rs/axum/latest/axum/)
- [Axum GitHub](https://github.com/tokio-rs/axum)
- [axum-server Crate](https://docs.rs/axum-server)
- [Axum REST API Sample (sheroz)](https://github.com/sheroz/axum-rest-api-sample)
- [Ultimate Guide to Axum (Shuttle)](https://www.shuttle.dev/blog/2023/12/06/using-axum-rust)
- [Axum REST API (Twilio)](https://www.twilio.com/en-us/blog/developers/community/build-high-performance-rest-apis-rust-axum)
- [Create API in Rust with Axum 2025 (CodevoWeb)](https://codevoweb.com/create-a-simple-api-in-rust-using-the-axum-framework/)
- [Zowe CLI Profiles](https://docs.zowe.org/stable/user-guide/cli-using-creating-profiles/)
- [Zowe Team Configuration](https://medium.com/zowe/zowe-cli-team-config-101-be57345ed668)
- [Zowe Conformance Test Guide](https://github.com/openmainframeproject/foundation/blob/main/zowe_conformance/test_evaluation_guide.md)
- [IBM z/OSMF REST Client Samples](https://github.com/IBM/IBM-Z-zOS/blob/main/zOSMF/ZosmfRESTClient/README.md)
- [Zowe CLI GitHub](https://github.com/zowe/zowe-cli)
- [Zowe Explorer ZosmfApi](https://github.com/zowe/zowe-explorer-vscode)

---

## Step 6 — Research Synthesis and Completion

### Executive Summary

This research investigated how **OpenMainframe**, an open-source Rust-based mainframe emulator with 24 crates, can expose its subsystems through **Zowe-compatible interfaces** to enable modern development tools (Zowe CLI, Zowe Explorer, VS Code) to interact with it as if it were a real z/OS instance.

**Key Finding:** The primary interface Zowe expects from a z/OS system is the **z/OSMF REST API** — a set of HTTPS/JSON endpoints for datasets, jobs, TSO, console, USS files, and authentication. OpenMainframe already implements the backend subsystems (datasets via VSAM/QSAM/PDS, jobs via JES2, commands via TSO, security via RACF) but lacks the REST API layer that exposes them.

**Primary Recommendation:** Build a new **`open-mainframe-zosmf`** crate using the **Axum** web framework that implements the z/OSMF REST API specification. This single crate bridges existing OpenMainframe subsystem crates to Zowe's expected interface, requiring no changes to Zowe itself and enabling standard `zowe zos-files`, `zowe zos-jobs`, and `zowe zos-tso` commands to work out of the box.

### Table of Contents

1. [Step 1 — Research Scope Confirmation](#step-1--research-scope-confirmation)
2. [Step 2 — Technology Stack Analysis](#step-2--technology-stack-analysis)
3. [Step 3 — Integration Patterns Analysis](#step-3--integration-patterns-analysis)
4. [Step 4 — Architectural Patterns](#step-4--architectural-patterns)
5. [Step 5 — Implementation Research](#step-5--implementation-research)
6. [Step 6 — Research Synthesis and Completion](#step-6--research-synthesis-and-completion)

### Recommended Architecture

**Option A: z/OSMF-Compatible REST Server** — A new `open-mainframe-zosmf` crate that:
- Serves z/OSMF REST API endpoints via Axum over HTTPS
- Translates HTTP requests into calls to existing crate APIs
- Handles authentication via RACF integration (Basic Auth + JWT)
- Returns z/OSMF-compatible JSON responses
- Optionally registers with Zowe API ML via Eureka

### Proposed New Crates

| Crate | Purpose | Priority |
|---|---|---|
| **`open-mainframe-zosmf`** | z/OSMF REST API server (Axum-based) with authentication, dataset, job, TSO, console, and USS endpoints | **P0 — Required** |
| **`open-mainframe-uss`** | USS (Unix System Services) file operations mapping to host filesystem with RACF permission checks | **P2 — Recommended** |

### Implementation Roadmap — Proposed Epics

Following the project's existing epic naming convention and phased delivery model:

#### Phase F: Zowe Integration (10 Epics)

| Epic | Title | Size | Dependencies | Description |
|---|---|---|---|---|
| **ZOW-100** | z/OSMF REST Server Foundation | **XL** | deploy | New `open-mainframe-zosmf` crate: Axum HTTP server, TLS via rustls, CORS, route scaffolding, `/zosmf/info` endpoint, `X-CSRF-ZOSMF-HEADER` middleware |
| **ZOW-101** | z/OSMF Authentication & Session Management | **L** | ZOW-100, racf | Basic Auth login via `racf::AuthService`, JWT token generation/validation, LTPA2 cookie support, session store, `/zosmf/services/authenticate` endpoint |
| **ZOW-102** | z/OSMF Dataset REST API | **L** | ZOW-101, dataset | `/zosmf/restfiles/ds/*` endpoints: list datasets, list PDS members, read/write sequential and member data, create/delete datasets. JSON serialization of `DatasetAttributes`, `CatalogEntry`, `PdsMember` |
| **ZOW-103** | z/OSMF Jobs REST API | **L** | ZOW-101, jes2 | `/zosmf/restjobs/jobs/*` endpoints: list jobs, get status, submit JCL, list spool files, read spool content, hold/release/cancel/purge. JSON serialization of `Job`, `JobState`, `SpoolManager` output |
| **ZOW-104** | z/OSMF TSO REST API | **M** | ZOW-101, tso | `/zosmf/tsoApp/*` endpoints: stateless command issue, address space lifecycle (start/send/receive/stop), servlet key session management. Integration with `TsoSession` and `TsoIo` |
| **ZOW-105** | z/OSMF Console REST API | **M** | ZOW-101, tso | `/zosmf/restconsoles/*` endpoints: issue MVS command, receive solicited response, EMCS console simulation with keyword matching |
| **ZOW-106** | USS File System — Host Mapping | **L** | ZOW-101 | `/zosmf/restfiles/fs/*` endpoints: list directories, read/write/delete files, create directories. Map USS paths to configurable host filesystem root with RACF permission checks |
| **ZOW-107** | TN3270E Server Mode | **L** | tui, tso, ispf | TCP listener accepting TN3270E connections, session multiplexing, mapping to `TsoSession`/ISPF backends. Enables Zowe Desktop TN3270 app connectivity |
| **ZOW-108** | Zowe API ML Integration | **M** | ZOW-100, ZOW-101 | Eureka dynamic registration client, heartbeat/renewal, deregistration on shutdown, static definition YAML generation, API catalog metadata |
| **ZOW-109** | Zowe Conformance & End-to-End Testing | **L** | ZOW-100–108 | Zowe CLI integration test suite, Zowe Explorer validation, conformance self-certification checklist, documentation for connecting Zowe to OpenMainframe |

#### Epic Dependency Graph

```
ZOW-100 (Server Foundation)
  ├── ZOW-101 (Authentication)
  │     ├── ZOW-102 (Datasets)
  │     ├── ZOW-103 (Jobs)
  │     ├── ZOW-104 (TSO)
  │     ├── ZOW-105 (Console)
  │     ├── ZOW-106 (USS Files)
  │     └── ZOW-108 (API ML)
  └── ZOW-107 (TN3270E Server)

ZOW-109 (Conformance Testing) ← depends on all above
```

### Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| **z/OSMF API spec ambiguity** — IBM's documentation may not cover all edge cases | HIGH | MEDIUM | Use Zowe CLI source code as reference implementation; test against real z/OSMF if available |
| **JSON response format mismatches** — Zowe clients may expect undocumented fields | MEDIUM | HIGH | Capture real z/OSMF responses for comparison; implement incrementally and test with Zowe CLI after each endpoint |
| **Authentication complexity** — JWT/LTPA2/PassTicket flows are complex | MEDIUM | MEDIUM | Start with Basic Auth + simple JWT; add PassTickets and client certs in later iterations |
| **USS file permission model** — POSIX permissions vs RACF authorization | LOW | MEDIUM | Start with host filesystem mapping (Option 1); add RACF integration incrementally |
| **Zowe version compatibility** — Zowe V3 may change expected API behavior | LOW | HIGH | Pin to Zowe V3 LTS; test with specific CLI/Explorer versions |
| **Performance under load** — Many concurrent Zowe sessions | LOW | MEDIUM | Axum + Tokio handle high concurrency natively; benchmark early |

### Complete Source Citations

**Zowe Documentation:**
- [Zowe Architecture](https://docs.zowe.org/stable/getting-started/zowe-architecture/)
- [Zowe Overview](https://docs.zowe.org/stable/getting-started/overview/)
- [API Mediation Layer Overview](https://docs.zowe.org/stable/user-guide/api-mediation/api-mediation-overview/)
- [Zowe Conformance Program](https://docs.zowe.org/stable/extend/zowe-conformance-program/)
- [Zowe CLI Usage](https://docs.zowe.org/stable/user-guide/cli-using-usingcli/)
- [Zowe CLI Profiles](https://docs.zowe.org/stable/user-guide/cli-using-creating-profiles/)
- [Zowe JWT Configuration](https://docs.zowe.org/stable/user-guide/api-mediation/configuration-jwt/)
- [Zowe PassTickets Configuration](https://docs.zowe.org/stable/user-guide/api-mediation/configuration-extender-passtickets/)
- [Zowe Client Certificates](https://docs.zowe.org/stable/user-guide/api-mediation/configuration-client-certificates/)
- [Direct Eureka Registration](https://docs.zowe.org/stable/extend/extend-apiml/onboard-direct-eureka-call/)
- [Static Onboarding](https://docs.zowe.org/v3.2.x/extend/extend-apiml/onboard-static-definition/)
- [API ML Discovery Service Config](https://docs.zowe.org/stable/user-guide/api-mediation/discovery-service-configuration/)
- [Zowe USS Requirements](https://docs.zowe.org/v3.0.x/user-guide/configure-uss/)
- [Authentication for API ML](https://docs.zowe.org/stable/extend/extend-apiml/authentication-for-apiml-services/)

**IBM z/OSMF:**
- [z/OSMF One Stop Hub](https://ibm.github.io/zOSMF/)
- [z/OSMF REST Services](https://www.ibm.com/docs/en/zos/2.4.0?topic=guide-using-zosmf-rest-services)
- [z/OS Jobs REST Interface](https://www.ibm.com/docs/en/zos/2.4.0?topic=services-zos-jobs-rest-interface)
- [z/OS Dataset and File REST Interface](https://www.ibm.com/support/knowledgecenter/SSLTBW_2.4.0/com.ibm.zos.v2r4.izua700/IZUHPINFO_API_RESTFILES.htm)
- [TSO REST API](https://www.ibm.com/docs/en/zos/2.5.0?topic=services-issue-tsoe-command-zosmf-rest-api)
- [IBM z/OSMF REST Client Samples](https://github.com/IBM/IBM-Z-zOS/blob/main/zOSMF/ZosmfRESTClient/README.md)
- [IBM Console Command Module](https://ibm.github.io/ibm_zos_zosmf/modules/zmf_console_command.html)

**Zowe Source Code:**
- [Zowe API ML GitHub](https://github.com/zowe/api-layer)
- [Zowe CLI GitHub](https://github.com/zowe/zowe-cli)
- [Zowe Explorer GitHub](https://github.com/zowe/zowe-explorer-vscode)
- [Zowe Explorer API](https://github.com/zowe/vscode-extension-for-zowe/blob/main/packages/zowe-explorer-api/README.md)
- [Zowe Conformance Test Guide](https://github.com/openmainframeproject/foundation/blob/main/zowe_conformance/test_evaluation_guide.md)

**Rust Ecosystem:**
- [Axum Documentation](https://docs.rs/axum/latest/axum/)
- [Axum GitHub](https://github.com/tokio-rs/axum)
- [axum-server Crate](https://docs.rs/axum-server)

**Other:**
- [Zowe V3 LTS Release](https://www.linuxfoundation.org/press/open-mainframe-project-announces-zowes-lts-v3-release)
- [Zowe Go SDK Mentorship](https://openmainframeproject.org/blog/summer-mentorship-2025-ojus-chugh/)
- [Connecting to z/OS via Zowe CLI](https://ibm.github.io/zopeneditor-about/Docs/connect_to_zos_with_zowe_cli_e2e.html)
- [SSO to z/OS REST APIs with Zowe](https://medium.com/zowe/single-sign-on-to-z-os-rest-apis-with-zowe-6e35fd022a95)
- [Zowe Conformance Program (OMP)](https://www.openmainframeproject.org/projects/zowe/conformance)

---

*Research completed 2026-02-20 by BMAD Technical Research workflow.*
*All 6 steps completed. Document ready for architecture and epic planning phases.*
