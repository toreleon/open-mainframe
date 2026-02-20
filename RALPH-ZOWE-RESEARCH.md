# Ralph Loop Prompt: BMAD Technical Research — Zowe Integration for OpenMainframe

## Mission

You are conducting **BMAD Technical Research (TR)** on how to make **OpenMainframe** (an open-source mainframe COBOL compiler and JCL interpreter written in Rust) connect and interoperate with **Zowe** — the open-source framework that provides modern interfaces to z/OS.

Your goal is to produce a comprehensive technical research document at `_bmad-output/planning-artifacts/research-zowe-integration.md` that covers everything needed to design and implement Zowe-compatible APIs, CLI interfaces, and service endpoints in OpenMainframe.

---

## Iteration Protocol

**Before doing ANY work, check your progress:**

1. Read `_bmad-output/planning-artifacts/research-zowe-integration.md` — if it exists, check the `stepsCompleted` array in the YAML frontmatter
2. Resume from the next incomplete step
3. If ALL 6 steps are completed and the document is finalized, output `<promise>ZOWE RESEARCH COMPLETE</promise>` and stop

---

## Research Context

### What is OpenMainframe (this project)?

- Rust workspace with 24 crates emulating z/OS subsystems
- Implements: COBOL, JCL, REXX, HLASM, PL/I, ISPF, TSO, RACF, JES2, CICS, IMS, DB2, MQ, VSAM, SMF, WLM
- Has TN3270E terminal protocol support (`open-mainframe-tui` crate)
- Has async HTTP deployment infrastructure (`open-mainframe-deploy` crate)
- Has RACF security with SAF router (`open-mainframe-racf` crate)
- Has JES2 job queue/spool system (`open-mainframe-jes2` crate)
- Has dataset catalog and VSAM support (`open-mainframe-dataset` crate)
- See `Cargo.toml` workspace members and `README.md` for full scope

### What is Zowe?

Zowe is an open-source project under the Open Mainframe Project (Linux Foundation) that provides:
- **API Mediation Layer (API ML)** — API gateway, discovery service, authentication for z/OS services
- **Zowe CLI** — Command-line interface for interacting with z/OS from modern workstations
- **Zowe Explorer** — VS Code extension for browsing datasets, USS files, jobs
- **Zowe Desktop (zlux)** — Web-based virtual desktop for z/OS applications
- **Zowe SDKs** — Node.js, Python, Swift SDKs for building z/OS integrations
- **Conformance Program** — Certification for Zowe-compatible extensions

### Research Question

**How should OpenMainframe expose its subsystems so that Zowe CLI, Zowe Explorer, and the Zowe API Mediation Layer can connect to it as if it were a real z/OS instance?**

---

## BMAD Technical Research Steps

Follow the BMAD 6-step technical research workflow. Each step must include **web search verification** of current facts. Write findings directly into the output document after each step.

### Step 1 — Research Scope Confirmation (Init)

**Role:** Technical Research Planner

Confirm and document the research scope:

- **Architecture Analysis:** Zowe's architecture, how it connects to z/OS, what protocols/APIs it expects
- **Integration Patterns:** z/OSMF REST APIs, SSH, FTP, TN3270 — what Zowe uses under the hood
- **Technology Stack:** Zowe's technology stack (Node.js, Java, Spring Cloud) and what OpenMainframe must expose
- **Compatibility Requirements:** What the Zowe Conformance Program requires
- **OpenMainframe Gap Assessment:** Map existing crates to Zowe's expected z/OS interfaces

**Web searches to perform:**
- `Zowe architecture z/OS integration 2025 2026`
- `Zowe API Mediation Layer z/OSMF REST API`
- `Zowe conformance program requirements`
- `Zowe CLI z/OS connection protocols`

Create the output document with YAML frontmatter. Mark step 1 complete.

### Step 2 — Technology Stack Analysis

**Role:** Technology Stack Analyst

Research and document:

- **Zowe server-side components** — What runs on z/OS vs. off-platform
- **z/OSMF REST APIs** — The primary REST API surface Zowe expects (datasets, jobs, TSO, console, files)
- **Zowe API ML protocols** — Service registration, discovery, authentication (Zowe SSO, SAF, PassTickets)
- **Data formats** — JSON schemas Zowe CLI/Explorer expect for datasets, jobs, spool, members
- **Authentication** — How Zowe authenticates to z/OS (SAF, RACF, AT-TLS, JWT, certificates)
- **Transport** — HTTPS, TLS requirements, port conventions

**Web searches to perform:**
- `z/OSMF REST API specification endpoints datasets jobs`
- `Zowe API ML service registration discovery Spring Cloud`
- `Zowe authentication SAF RACF JWT PassTickets`
- `Zowe server component architecture z/OS USS`

Write findings into the document. Mark step 2 complete.

### Step 3 — Integration Patterns Analysis

**Role:** Integration Analyst

Research and document how Zowe integrates with each z/OS subsystem OpenMainframe implements:

| Zowe Feature | z/OS Interface Used | OpenMainframe Crate | Gap |
|---|---|---|---|
| Dataset browsing | z/OSMF REST `/zosmf/restfiles/ds` | `open-mainframe-dataset` | ? |
| Job submission | z/OSMF REST `/zosmf/restjobs/jobs` | `open-mainframe-jes2` | ? |
| Job output/spool | z/OSMF REST `/zosmf/restjobs/jobs/{id}/files` | `open-mainframe-jes2` | ? |
| TSO commands | z/OSMF REST `/zosmf/tsoApp` | `open-mainframe-tso` | ? |
| USS file access | z/OSMF REST `/zosmf/restfiles/fs` | N/A (potential new crate) | ? |
| Console commands | z/OSMF REST `/zosmf/restconsoles` | `open-mainframe-tso` | ? |
| TN3270 terminal | TN3270E protocol | `open-mainframe-tui` | ? |
| RACF security | SAF/RACF authentication | `open-mainframe-racf` | ? |

**Web searches to perform:**
- `z/OSMF REST API dataset operations specification`
- `z/OSMF REST API job operations JES2 spool`
- `z/OSMF REST API TSO console USS files`
- `Zowe Explorer VS Code z/OS integration API`

Fill in the gap analysis table. Document each integration point in detail. Mark step 3 complete.

### Step 4 — Architectural Patterns

**Role:** Systems Architect

Based on research so far, document architectural patterns for implementing Zowe compatibility:

- **Option A: z/OSMF-Compatible REST Server** — Build a z/OSMF-compatible REST API layer in Rust (new crate: `open-mainframe-zosmf`) that exposes existing subsystems
- **Option B: Zowe API ML Plugin** — Create a Zowe-conformant API ML plugin that bridges to OpenMainframe's native APIs
- **Option C: Hybrid Approach** — z/OSMF REST compatibility for core operations + custom Zowe plugins for extended features

For each option, analyze:
- Implementation complexity for Rust
- Compatibility coverage (which Zowe features would work)
- Authentication integration (RACF SAF ↔ Zowe SSO)
- Performance characteristics
- Maintenance burden

**Web searches to perform:**
- `z/OSMF API implementation open source`
- `Zowe conformant API service development guide`
- `Zowe plugin development REST API`
- `open mainframe project Zowe integration examples`

Document the architectural comparison. Mark step 4 complete.

### Step 5 — Implementation Research

**Role:** Implementation Engineer

Research practical implementation details:

- **Rust HTTP/REST frameworks** suitable for z/OSMF API compatibility (axum, actix-web, etc.)
- **JSON schema compliance** for z/OSMF response formats
- **TLS/certificate handling** in Rust for Zowe HTTPS requirements
- **Service discovery registration** — how OpenMainframe would register with Zowe API ML
- **Zowe CLI profiles** — what connection configuration users need (`zowe profiles create zosmf-profile`)
- **Testing strategy** — how to verify Zowe compatibility (Zowe conformance test suite)
- **Existing open-source z/OSMF implementations** or mocks to reference

**Web searches to perform:**
- `Rust REST API framework z/OSMF compatible`
- `Zowe CLI zosmf profile configuration`
- `Zowe conformance test suite validation`
- `z/OSMF mock server open source`

Document implementation recommendations. Mark step 5 complete.

### Step 6 — Research Synthesis and Completion

**Role:** Technical Research Strategist

Finalize the document with:

1. **Executive Summary** — Key findings and primary recommendation
2. **Table of Contents** — Complete structured TOC
3. **Recommended Architecture** — Best approach for OpenMainframe + Zowe integration
4. **Implementation Roadmap** — Proposed epics/phases (following project's existing epic pattern)
5. **Risk Assessment** — Technical risks and mitigations
6. **Source Citations** — All URLs and references used
7. **Proposed New Crates** — Any new Rust crates needed (e.g., `open-mainframe-zosmf`, `open-mainframe-uss`)
8. **Proposed Epic Structure** — Draft epics in the project's format (ZOW-100, ZOW-101, etc.)

Ensure the document is comprehensive, well-cited, and actionable. Mark step 6 complete.

After completing step 6, output: `<promise>ZOWE RESEARCH COMPLETE</promise>`

---

## Output Document Location

```
_bmad-output/planning-artifacts/research-zowe-integration.md
```

## Quality Standards

- Every factual claim must cite a web source URL
- Use markdown tables for comparison matrices
- Follow the format established by existing documents in `_bmad-output/planning-artifacts/`
- Proposed epics must follow the project's naming convention (see `RALPH-IMPLEMENT.md`)
- Gap analysis must reference specific existing crate source files
- Be thorough — this document will drive actual implementation

## Completion Promise

When ALL 6 steps are complete and the final document is polished:

```
<promise>ZOWE RESEARCH COMPLETE</promise>
```
