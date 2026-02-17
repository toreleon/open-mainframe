---
stepsCompleted: [1, 2, 3, 4, 5]
inputDocuments:
  - research-domain-modernization.md
  - research-technical-copilotkit-langgraph.md
  - research-technical-openmainframe-tools.md
date: 2026-02-17
author: Tore
---

# Product Brief: OpenMainframe Agent

## Executive Summary

OpenMainframe Agent is an AI-powered copilot application that helps enterprises modernize and migrate their mainframe workloads through interactive, conversational guidance. Built on CopilotKit + LangGraph, it wraps the OpenMainframe compiler/runtime as agent tools — enabling users to assess codebases, compile COBOL, execute JCL, explore datasets, and plan migrations through a modern chat-based interface. Unlike cloud-vendor-locked solutions (AWS Transform, Google Dual Run), OpenMainframe Agent is open-source, self-hosted, and vendor-neutral — the first end-to-end open-source mainframe modernization agent.

---

## Core Vision

### Problem Statement

Enterprises face a USD 8.2B+ mainframe modernization challenge with no open-source, end-to-end solution. Existing tools are either cloud-vendor-locked (AWS Transform, Google MAT), prohibitively expensive (Micro Focus/Rocket, LzLabs), or provide only partial capabilities (GnuCOBOL, Zowe). The emerging agentic AI paradigm (AWS Transform launched May 2025 with multi-agent architecture) proves the approach works, but leaves enterprises dependent on cloud vendors and opaque pricing.

Meanwhile, OpenMainframe already provides the core execution engine — COBOL compiler, JCL interpreter, VSAM dataset management, CICS transaction processing, DB2 SQL, IMS hierarchical database — all in open-source Rust. What's missing is the intelligent layer that guides users through modernization using these capabilities.

### Problem Impact

- **No open-source modernization agent exists** — enterprises must pay USD 1M+ for AWS Transform or similar
- **Assessment is manual and slow** — teams spend months inventorying codebases by hand
- **Migration planning requires rare expertise** — 25% of mainframe workforce retired by end of 2025
- **Trial-and-error compilation** — developers iterate without intelligent guidance
- **Knowledge loss** — undocumented COBOL business logic dies with retiring engineers

### Why Existing Solutions Fall Short

| Solution | Limitation |
|----------|-----------|
| AWS Transform | AWS lock-in, opaque pricing, no self-hosting |
| Google Dual Run | Requires maintaining two environments, GCP-centric |
| Micro Focus/Rocket | USD 2.275B acquisition = premium pricing, proprietary |
| LzLabs | Binary rehosting only, doesn't modernize code |
| GnuCOBOL | Compiler only, no assessment/migration/agent capabilities |
| Zowe | Works WITH mainframes, not as replacement |
| IBM Watsonx for Z | IBM lock-in, perpetuates vendor dependency |

### Proposed Solution

OpenMainframe Agent wraps the existing OpenMainframe platform with a CopilotKit-powered conversational interface and LangGraph-based agent workflows. Users interact through a modern web UI to:

1. **Assess** — Scan codebases for complexity, compatibility, and migration risk
2. **Compile & Validate** — Compile COBOL and check JCL with intelligent error guidance
3. **Execute & Test** — Run JCL jobs and interpret COBOL with result analysis
4. **Explain** — Get natural language explanations of legacy code and business rules
5. **Plan Migration** — Generate dependency-aware migration roadmaps with effort estimates
6. **Manage Data** — Browse and manage VSAM, QSAM, PDS datasets interactively

### Key Differentiators

- **Open source** — Full transparency, community-driven, no vendor lock-in
- **Self-hosted** — Runs on-premises for regulated industries (banking, government, healthcare)
- **Vendor neutral** — Not tied to any cloud provider
- **Interactive agent** — Conversational UI, not batch-oriented CLI
- **Full-stack platform** — Assessment + execution + migration in one tool
- **Built on proven engine** — OpenMainframe's 15-crate Rust compiler/runtime
- **Human-in-the-loop** — Approval workflows for destructive operations
- **Real-time UI** — Generative UI showing assessment dashboards, code diffs, migration progress

---

## Target Users

### Primary Users

#### 1. Migration Architect — "Priya Sharma"

**Context:** Priya leads mainframe modernization for a mid-size insurance company with 2M+ lines of COBOL across 400+ programs, 150 JCL jobs, and CICS online transactions. Her team of 3 needs to build a business case for migration and execute a phased plan.

**Current Pain Points:**
- Spent 4 months manually inventorying programs — still found surprises
- Cannot determine migration order due to undocumented dependencies
- Leadership demands effort estimates she can't confidently provide
- Every vendor demo shows their cloud, not her on-premises requirements
- No tool lets her "try before she buys" with actual production COBOL

**Goals:**
- Automated codebase assessment with confidence scores
- Dependency-aware migration sequencing
- Effort estimates grounded in actual code analysis
- Self-hosted tool she can run in her secure environment
- Interactive guidance through the migration process

**Success Looks Like:**
"I uploaded our COBOL codebase, got an assessment in hours instead of months, and the agent generated a 12-wave migration plan. I showed the dashboard to leadership and got approval the same week."

---

#### 2. Mainframe Developer — "Marcus Chen"

**Context:** Marcus is a senior COBOL developer with 15 years of experience. He maintains critical batch processing and CICS online systems. His team is shrinking as colleagues retire, and new hires struggle with mainframe concepts.

**Current Pain Points:**
- Waits hours for compile/test cycles on shared LPAR
- Cannot explain legacy code to new team members efficiently
- No modern IDE experience — stuck with ISPF or expensive proprietary tools
- Debugging requires submitting JCL and reading spool output
- Knowledge in his head, not documented anywhere

**Goals:**
- Compile and run COBOL locally with instant feedback
- AI-assisted code explanation for onboarding new developers
- Interactive debugging without mainframe access
- Modern web-based development experience

**Success Looks Like:**
"I paste a COBOL program into the chat, ask 'what does CALC-PREMIUM do?', and get a clear English explanation with the business rules extracted. New hires are productive in days instead of months."

---

### Secondary Users

#### IT Leadership / Decision Makers
- Need TCO comparisons and ROI projections
- Want risk dashboards and compliance evidence
- Require vendor-neutral evaluation tools

#### Integration Developers
- Need to understand mainframe data formats (EBCDIC, COMP-3, VSAM)
- Want API access to mainframe capabilities
- Benefit from dataset exploration tools

---

### User Journey

#### Migration Architect Journey

1. **Discovery:** Finds OpenMainframe Agent through Open Mainframe Project community
2. **Evaluation:** Installs locally, uploads a sample COBOL directory
3. **Assessment:** Runs codebase assessment — gets complexity scores, feature inventory, compatibility report
4. **Planning:** Agent generates migration roadmap with dependency-aware wave sequencing
5. **Aha Moment:** First assessment report replaces 4 months of manual inventory work
6. **Adoption:** Uses agent to plan and track migration, iteratively moving programs to OpenMainframe
7. **Long-term:** Full production workloads running on OpenMainframe with ongoing agent monitoring

#### Developer Journey

1. **Discovery:** Team lead introduces OpenMainframe Agent as development companion
2. **First Use:** Pastes a COBOL program, asks for explanation
3. **Development:** Uses compile/check tools for rapid iteration
4. **Debugging:** Runs programs through interpreter with variable inspection
5. **Aha Moment:** Gets instant compilation feedback instead of waiting for mainframe LPAR
6. **Adoption:** Daily driver for COBOL development, testing, and code understanding

---

## Success Metrics

### User Success Metrics

| Metric | Target | Measurement |
|--------|--------|-------------|
| Assessment time | Hours (not months) for full codebase scan | Time from upload to report |
| Migration plan generation | < 1 day for initial roadmap | Time from assessment to plan |
| Compilation feedback | < 30 seconds per program | CLI tool execution time |
| Code explanation quality | 90%+ accuracy on business rules | Manual review of sample explanations |
| Developer onboarding | 50% faster for new COBOL developers | Time to first productive contribution |

### Business Objectives

#### Phase 1: Community & Validation (Months 1-6)
- Open-source release on GitHub
- 500+ GitHub stars, 50+ forks
- 10+ enterprises evaluating with their own COBOL codebases
- CardDemo end-to-end demonstration working
- 3+ community contributors

#### Phase 2: Enterprise Adoption (Months 6-12)
- 5+ enterprises using for active migration projects
- Integration with CI/CD pipelines
- Commercial support offering launched
- Conference presentations at SHARE, Open Mainframe Project events

#### Phase 3: Market Position (Months 12-24)
- Recognized as the open-source alternative to AWS Transform
- 20+ enterprise deployments
- Partnership with system integrators
- Managed cloud offering for enterprises wanting hosted solution

### North Star Metric

> **Programs assessed and migrated** — Total count of mainframe programs successfully analyzed, planned, and executing on OpenMainframe through the agent. Captures user adoption, technical success, and business value.

---

## MVP Scope

### Core Features (MVP)

#### 1. Assessment Agent
- Scan COBOL/JCL directories for inventory
- Generate complexity scores per program
- Produce compatibility reports (OpenMainframe feature support %)
- Output in JSON + visual dashboard

#### 2. Compilation Agent
- Compile COBOL via `open-mainframe compile`
- Syntax check via `open-mainframe check`
- Parse and explain compiler errors with fix suggestions
- Show compilation results in generative UI

#### 3. Execution Agent
- Run JCL jobs via `open-mainframe run`
- Interpret COBOL via `open-mainframe interpret`
- Capture and display output, return codes, errors
- Step-by-step job progress visualization

#### 4. Code Explanation Agent
- Explain COBOL programs in natural language
- Extract business rules from procedure division
- Annotate data division with field purposes
- Explain JCL job flows

#### 5. Interactive Chat UI
- CopilotKit sidebar with chat interface
- Code viewer with syntax highlighting
- File tree navigation for uploaded codebases
- Assessment dashboard with charts

#### 6. Human-in-the-Loop
- Approval required for job execution
- Confirmation for dataset modifications
- Review checkpoints in migration planning

---

### Out of Scope for MVP

| Feature | Rationale | Target Release |
|---------|-----------|----------------|
| Migration Planning Agent | Requires call graph (Epic 1002) | v1.1 |
| Interactive Debugger | Requires debug API for interpreter | v1.1 |
| CICS/DB2 Feature Inventory | Requires Epics 1003/1004 | v1.1 |
| Code Transformation (COBOL→Java) | Major feature, needs separate PRD | v2.0 |
| Multi-user/Multi-tenant | Enterprise feature | v2.0 |
| REST API for CI/CD Integration | Developer platform feature | v1.2 |
| VS Code Extension | IDE integration | v1.2 |
| Progress Tracking Dashboard | Requires Epic 1006 | v1.1 |

---

### MVP Success Criteria

**Technical Validation:**

| Criterion | Target |
|-----------|--------|
| CardDemo assessment | Complete assessment report generated in < 5 minutes |
| COBOL compilation | Successful compile of 90%+ of CardDemo programs |
| JCL execution | Run CardDemo batch jobs end-to-end |
| Code explanation | Accurate explanation of 10+ CardDemo programs |
| Chat responsiveness | < 5 second response time for most queries |

**User Validation:**

| Criterion | Target |
|-----------|--------|
| Setup time | Developer productive within 30 minutes of install |
| Assessment value | User reports assessment saved significant manual effort |
| Explanation accuracy | 90%+ of explanations validated by COBOL expert |
| Return usage | Users come back for 3+ sessions |

---

## Technology Stack

| Layer | Technology | Rationale |
|-------|-----------|-----------|
| **Frontend** | Next.js 14+ (App Router) | Native CopilotKit support, SSR |
| **UI Framework** | CopilotKit (`@copilotkit/react-core`, `react-ui`) | Sidebar layout, generative UI, shared state |
| **Agent Framework** | LangGraph (Python) | Stateful multi-step workflows, tool calling |
| **Agent Protocol** | AG-UI via `ag_ui_langgraph` | Standard streaming protocol |
| **Runtime** | Self-hosted CopilotRuntime | Full control, agent-lock mode |
| **LLM** | Claude (Anthropic) or GPT-4o | Strong code understanding + tool calling |
| **Backend Engine** | OpenMainframe CLI (Rust) | compile, interpret, run, check, assess |
| **CLI Integration** | Python `subprocess` wrapper | MVP approach; HTTP API in Phase 2 |
| **Persistence** | PostgreSQL | Conversation history, migration projects |
| **Dev Persistence** | MemorySaver (LangGraph) | In-memory for development |

---

## Future Vision

### Near-Term (v1.x)

**v1.1 — Migration Planning**
- Call graph analysis (Epic 1002)
- Dependency-aware migration wave generation
- Effort estimation per wave
- Migration progress tracking

**v1.2 — Developer Platform**
- REST API for CI/CD integration
- VS Code extension
- Multi-project support
- Assessment history and comparison

### Long-Term (v2.0+)

**Code Transformation Engine**
- COBOL → modern language (Java/Python) with OpenMainframe AST
- Incremental transformation (one module at a time)
- Business rule preservation verification
- Generated code quality analysis

**Enterprise Platform**
- Multi-tenant deployment
- Role-based access control
- Audit logging and compliance
- Integration with Jira, ServiceNow, Azure DevOps
- Kubernetes deployment with auto-scaling

**Advanced Agent Capabilities**
- Multi-agent collaboration (assessment agent → planning agent → transformation agent)
- Automated regression testing
- Production parity validation (similar to Google Dual Run)
- Natural language-to-JCL generation
- CICS screen modernization to web UI

**Ultimate Goal:**
> An open-source, AI-powered platform that makes mainframe modernization accessible to every enterprise — from Fortune 500 companies running 10M+ lines of COBOL to small organizations with a few hundred programs. No vendor lock-in, no hidden costs, full transparency.
