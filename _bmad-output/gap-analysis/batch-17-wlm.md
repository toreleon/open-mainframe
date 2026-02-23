# Gap Analysis: WLM (Workload Manager)

## Official Specification Summary

z/OS Workload Manager (WLM) is the core goal-based resource-allocation subsystem that controls how CPU, storage, and I/O bandwidth are distributed across all work running in a z/OS system or sysplex. WLM replaced the earlier manual performance-tuning model (IPS/ICS/OPT) with a declarative goal-driven approach: installations define *what* service levels they want (response times, throughput), and WLM dynamically adjusts *how* the system delivers those goals.

WLM is classified as **Core** — every z/OS installation runs WLM; it underpins JES2, CICS, DB2, IMS, MQ, USS, TSO, and all other subsystems:
- **Goal-based scheduling**: Service classes define performance goals (response time, velocity, discretionary); WLM adjusts dispatching priorities, I/O priority, storage allocation, and processor weights to meet goals
- **Classification rules**: Incoming work is mapped to service classes via a hierarchical rule tree keyed by subsystem type, transaction name, user ID, job class, etc.
- **Resource groups**: CPU and storage capping — hard or soft limits per service class or group of service classes
- **WLM-managed initiators**: WLM starts/stops JES2 batch initiators dynamically based on service goals (replaced classic JES2-managed initiators)
- **Enclaves**: Independent dispatchable units (IDUs) representing a unit of work that spans address spaces (e.g., a DB2 stored procedure, a CICS transaction calling DB2)
- **IWM services API**: System-level macros (IWMCLSFY, IWMCONN, IWMSSEL, IWMECREA, etc.) for subsystems to interact with WLM
- **Service definition**: The complete WLM policy stored in a couple dataset for sysplex-wide sharing
- **Performance Index (PI)**: Ratio of actual-to-goal performance; PI < 1.0 means goal is met, PI > 1.0 means goal is missed
- **Health API**: Modern REST/JSON interface for querying WLM state, plus traditional IWMQRYS and RMF integration

Key documentation:
- **z/OS MVS Planning: Workload Management** (SA23-1390) — concepts, planning, service definition design
- **z/OS MVS Programming: Workload Management Services** (SA23-1391) — IWM callable services API
- **z/OS MVS Setting Up a Sysplex** — couple dataset, sysplex-wide WLM policy
- **z/OS RMF User's Guide** — WLM monitoring via RMF reports (Workload Activity, Channel Path Activity, etc.)

## Key Features & Capabilities

### 1. Service Definition Structure

| Component | Description |
|-----------|-------------|
| Service Policy | Named active policy (only one active at a time); contains all rules, classes, groups |
| Service Class | Named bucket for work with an assigned goal and importance |
| Service Class Period | Multi-period classes; each period has its own goal and duration (in service units) |
| Report Class | Optional grouping for RMF reporting without affecting scheduling |
| Workload | Logical grouping of service classes for organizational purposes |
| Resource Group | CPU/storage capping entity |
| Classification Rule | Hierarchical rule mapping work to service class and report class |
| Scheduling Environment | Named set of resource requirements (e.g., "needs DB2") checked before starting batch jobs |
| Application Environment | WLM-managed server address space definition (e.g., DB2 stored-procedure JVM) |

### 2. Goal Types

| Goal Type | Parameters | Description |
|-----------|------------|-------------|
| Response Time — Average | Target time (seconds), percentile (optional) | Average response time for transactions in period |
| Response Time — Percentile | Target time, percentile (e.g., 90%) | Nth-percentile response time target |
| Execution Velocity | 1–99 | How fast work should run when ready (100 = never delayed, 1 = always delayed) |
| Discretionary | (none) | Best-effort; system gives resources only when higher-importance work doesn't need them |

### 3. Importance Levels

| Level | Meaning |
|-------|---------|
| 1 | Highest — system never delays this work (e.g., production CICS) |
| 2 | High — important production batch and online work |
| 3 | Medium — normal production work |
| 4 | Low — development, test, ad-hoc queries |
| 5 | Lowest — discretionary work only |

WLM uses importance to break ties when multiple service classes compete for the same resources. A service class with importance 1 missing its goal will get resources before an importance 3 class that is also missing.

### 4. Classification Rules

Classification maps incoming work to a service class and optional report class. Rules form a hierarchical tree:

| Level | Qualifier | Description |
|-------|-----------|-------------|
| 1 | Subsystem Type | JES, STC, TSO, OMVS, ASCH, CICS, IMS, DB2, DDF, CB, MQ, IWEB, LDAP, TCP |
| 2+ | Subsystem-specific qualifiers | Varies by subsystem type (see below) |

#### Classification Qualifiers by Subsystem Type

| Subsystem Type | Available Qualifiers |
|----------------|---------------------|
| JES (batch) | Job class, SCHENV, submitter user ID, accounting info, job name, service class override |
| STC (started tasks) | Procedure name, started-task user ID |
| TSO | User ID, performance group, accounting info |
| OMVS (USS) | User ID, process name |
| CICS | Transaction ID, transaction class, user ID, LU name |
| IMS | Transaction code, user ID, IMS subsystem |
| DB2 | Plan name, auth ID, connection type, collection ID, package name, correlation ID |
| DDF (DB2 distributed) | Similar to DB2 plus network ID, requester location |
| MQ | Queue manager name, channel name |
| ASCH (APPC scheduler) | Transaction name, network ID, LU name |
| CB (WebSphere) | Application name, server name, cluster name |
| IWEB | Application name, server name |
| LDAP | Server name, client IP |
| TCP | Service name, port number |

### 5. Resource Groups

| Feature | Description |
|---------|-------------|
| CPU Cap — MSU (million service units) | Absolute CPU capacity limit |
| CPU Cap — Percentage | Percentage of total system capacity |
| CPU Cap — CP Count | Number of logical CPs |
| Memory Limit | Address space storage limit (above/below bar) |
| Minimum Capacity | Guaranteed minimum resources |
| Maximum Capacity | Hard cap on resources |
| Type 1 | Single-system resource group |
| Type 2 | Sysplex-wide resource group |
| Type 3 | Single-system group defined at sysplex level |
| Type 4 | Tenant resource group (z/OS 2.4+) for cloud-like isolation |

### 6. WLM-Managed Initiators

| Feature | Description |
|---------|-------------|
| Dynamic initiator management | WLM starts/stops batch initiators based on service goals |
| CP/zIIP capacity analysis | WLM considers available CP and zIIP capacity |
| Job-class affinity | Initiators started for specific job classes |
| Queued job detection | WLM monitors JES2 job queues for pending work |
| SCHENV matching | WLM checks scheduling environment requirements before starting initiator |
| JES2/JES3 integration | WLM communicates with JES for initiator lifecycle |

### 7. Enclaves

| Feature | Description |
|---------|-------------|
| IWMECREA | Create an enclave (independent dispatchable unit) |
| IWMEDELE | Delete an enclave |
| IWMEJOIN | Join a thread to an enclave (contribute resources) |
| IWMELEAV | Leave an enclave |
| IWMEREG | Register as an enclave participant |
| IWMEDREG | Deregister from an enclave |
| IWMEQTME | Query enclave CPU time |
| IWMEGCOR | Get enclave correlator (for cross-system tracking) |
| Cross-address-space | Enclave can span multiple address spaces (e.g., CICS → DB2) |
| Independent classification | Enclaves classified separately from their host address space |
| Enclave SRB (Service Request Block) | SRB dispatched under enclave's service class, not host's |

### 8. IWM Services API

| Service | Function |
|---------|----------|
| IWMCLSFY | Classify work into a service class |
| IWMCONN | Connect a subsystem instance to WLM |
| IWMDISC | Disconnect a subsystem instance |
| IWMSSEL | Select a server address space for work routing |
| IWMRPT | Report transaction completion (for response time measurement) |
| IWMQRYS | Query WLM service definition |
| IWMWREG | Register a work manager |
| IWMWDREG | Deregister a work manager |
| IWMSRSRG | Register a server address space |
| IWMSRSUP | Update server status |
| IWMSRSDG | Deregister a server address space |
| IWMSCDEL | Signal delay (report that work is waiting for resource) |
| IWMSCHDT | Schedule work to server via application environment |

### 9. Service Coefficients & Service Units

| Component | Default Weight | Description |
|-----------|---------------|-------------|
| CPU service | 1.0 | CPU time consumed (weighted by engine speed) |
| MSO (Memory) | 0.0 | Central storage frames used |
| IOC (I/O) | 0.0 | I/O operations (EXCP count) |
| SRB service | 1.0 | SRB (system request block) CPU time |

Service Units = (CPU × CPU_coeff) + (MSO × MSO_coeff) + (IOC × IOC_coeff) + (SRB × SRB_coeff)

Multi-period service classes use accumulated service units to determine when to transition from one period to the next.

### 10. Performance Index & Monitoring

| Feature | Description |
|---------|-------------|
| Performance Index (PI) | PI = actual / goal; PI < 1.0 = goal met; PI > 1.0 = goal missed |
| RMF Workload Activity Report | Monitors PI, response times, velocity, resource consumption per service class |
| WLM Health API | REST/JSON interface for querying active policy, service classes, PI values |
| VARY WLM command | Activate/deactivate service policies |
| D WLM command | Display WLM status, active policy, service classes |
| SETOMVS WLM= | Change WLM settings for USS |

### 11. Couple Dataset & Sysplex Integration

| Feature | Description |
|---------|-------------|
| WLM couple dataset | Stores the service definition (policies, rules, classes, groups) |
| Sysplex-wide policy | Single active policy shared by all systems in the sysplex |
| VARY WLM,POLICY= | Activate a named policy across the sysplex |
| WLM ISPF Administrative Application | ISPF panels for editing service definitions |
| Batch utility IWMARIN0 | Batch extraction and installation of service definitions |

## Current OpenMainframe Status

### Codebase Search Results

A comprehensive search of the OpenMainframe codebase found **no WLM-specific implementation**. The following adjacent/partial features exist:

#### 1. JCL SCHENV Parameter — Parsed but Unused
**File:** `crates/open-mainframe-jcl/src/ast/mod.rs:170-171`
```rust
pub schenv: Option<String>,
```
**File:** `crates/open-mainframe-jcl/src/parser/mod.rs:507-509`
```rust
"SCHENV" => {
    if let Token::Ident(v) = &tokens[idx] {
        params.schenv = Some(v.clone());
    }
}
```
The `SCHENV` (scheduling environment) JOB parameter is parsed from JCL but has no WLM engine behind it to evaluate scheduling environment resources.

#### 2. JCL Job Class — Parsed but Not WLM-Classified
**File:** `crates/open-mainframe-jcl/src/ast/mod.rs:153`
```rust
pub class: Option<char>,
```
Job class (A-Z, 0-9) is parsed as a JCL JOB parameter but is not used for WLM classification. In real z/OS, WLM classification rules can use job class as a qualifier for JES subsystem work.

#### 3. Batch Metrics Infrastructure
**File:** `crates/open-mainframe-deploy/src/batch_metrics.rs`
- `JobMetrics` and `StepMetrics` track execution duration, return codes, I/O counts
- `BatchMetricsCollector` aggregates metrics across job runs
- This provides monitoring data that a WLM implementation could consume for service-level measurement

#### 4. Prometheus/OpenTelemetry Integration
**File:** `crates/open-mainframe-deploy/src/metrics.rs`
- Prometheus metrics for COBOL, CICS, IMS, DB2
- Could serve as the foundation for WLM performance-index reporting

#### 5. JES2 Gap Analysis Cross-Reference
**File:** `_bmad-output/gap-analysis/batch-11-jes2.md`
- Lines 193, 416, 426: WLM-managed initiators and scheduling environments documented as JES2 gaps
- WLM integration is a prerequisite for completing JES2 initiator management

## Gap Details

### Service Definition & Policy

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Service policy (named, activate/deactivate) | VARY WLM,POLICY= activates one policy sysplex-wide | None | **Missing** |
| Service class definition | Named class with goal + importance + periods | None | **Missing** |
| Multi-period service class | Up to 8 periods with duration triggers (service units) | None | **Missing** |
| Report class | Named class for RMF reporting | None | **Missing** |
| Workload grouping | Logical grouping of service classes | None | **Missing** |
| Service coefficients | CPU/MSO/IOC/SRB weights | None | **Missing** |
| Service unit calculation | Weighted sum of resource consumption | None | **Missing** |
| Service definition ISPF panels | WLM admin application | None | **Missing** |
| Batch utility IWMARIN0 | Extract/install definitions | None | **Missing** |

### Goal Types & Performance Index

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Response time — average | Goal in seconds, measured per completed transaction | None | **Missing** |
| Response time — percentile | Nth-percentile target (e.g., 90% within 1 second) | None | **Missing** |
| Execution velocity (1–99) | Ratio of using vs delay time | None | **Missing** |
| Discretionary | Best-effort, no goal | None | **Missing** |
| Performance Index (PI) | PI = actual/goal, computed per service class period | None | **Missing** |
| Goal adjustment algorithm | WLM dynamically adjusts dispatching priority to meet goals | None | **Missing** |
| Importance levels (1–5) | Tie-breaking across service classes | None | **Missing** |

### Classification Rules

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Hierarchical rule tree | Multi-level qualifier matching | None | **Missing** |
| Subsystem type — JES (batch) | Classify by job class, SCHENV, user ID, job name | SCHENV/class parsed in JCL | **Partial** |
| Subsystem type — STC | Classify by procedure name, user ID | None | **Missing** |
| Subsystem type — TSO | Classify by user ID, performance group | None | **Missing** |
| Subsystem type — OMVS | Classify by user ID, process name | None | **Missing** |
| Subsystem type — CICS | Classify by transaction ID, class, user ID, LU name | None | **Missing** |
| Subsystem type — IMS | Classify by transaction code, user ID | None | **Missing** |
| Subsystem type — DB2 | Classify by plan, auth ID, connection type, collection, package | None | **Missing** |
| Subsystem type — DDF | DB2 distributed qualifiers + network ID | None | **Missing** |
| Subsystem type — MQ | Queue manager name, channel name | None | **Missing** |
| Subsystem type — ASCH | APPC scheduler transaction name, LU | None | **Missing** |
| Subsystem type — CB (WebSphere) | Application, server, cluster name | None | **Missing** |
| Default service class | Catch-all for unclassified work | None | **Missing** |
| Service class override | JCL or subsystem-requested override | None | **Missing** |

### Resource Groups

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| CPU cap — MSU | Absolute million-service-unit limit | None | **Missing** |
| CPU cap — percentage | Percentage of total capacity | None | **Missing** |
| CPU cap — CP count | Logical processor count limit | None | **Missing** |
| Memory limit | Address space storage limits | None | **Missing** |
| Minimum capacity guarantee | Guaranteed minimum resources | None | **Missing** |
| Type 1 (single-system) | Local resource group | None | **Missing** |
| Type 2 (sysplex-wide) | Shared resource group across systems | None | **Missing** |
| Type 4 (tenant) | Cloud-like isolation (z/OS 2.4+) | None | **Missing** |

### WLM-Managed Initiators

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Dynamic initiator start/stop | WLM starts initiators when queued work needs them | None | **Missing** |
| CP/zIIP capacity analysis | WLM considers engine types and available capacity | None | **Missing** |
| Job-class affinity | Initiators started for specific job classes | None | **Missing** |
| SCHENV matching | Check scheduling environment before starting | SCHENV parsed only | **Partial** |
| JES2 integration | WLM communicates with JES for initiator lifecycle | None | **Missing** |

### Enclaves

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Create/delete enclave | IWMECREA / IWMEDELE | None | **Missing** |
| Join/leave enclave | IWMEJOIN / IWMELEAV | None | **Missing** |
| Register/deregister participant | IWMEREG / IWMEDREG | None | **Missing** |
| Query CPU time | IWMEQTME | None | **Missing** |
| Get correlator | IWMEGCOR for cross-system tracking | None | **Missing** |
| Cross-address-space span | Enclave spans CICS, DB2, IMS in one unit | None | **Missing** |
| Enclave SRB dispatching | SRB dispatched under enclave's class | None | **Missing** |
| Independent enclave classification | Enclave classified separately from host | None | **Missing** |

### IWM Services API

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| IWMCLSFY | Classify work into service class | None | **Missing** |
| IWMCONN / IWMDISC | Connect/disconnect subsystem | None | **Missing** |
| IWMSSEL | Select server for work routing | None | **Missing** |
| IWMRPT | Report transaction completion | None | **Missing** |
| IWMQRYS | Query service definition | None | **Missing** |
| IWMWREG / IWMWDREG | Work manager register/deregister | None | **Missing** |
| IWMSRSRG / IWMSRSDG | Server registration/deregistration | None | **Missing** |
| IWMSRSUP | Update server status | None | **Missing** |
| IWMSCDEL | Signal delay (waiting for resource) | None | **Missing** |
| IWMSCHDT | Schedule work to application environment | None | **Missing** |

### Scheduling Environments & Application Environments

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| Scheduling environment definition | Named set of resource requirements | SCHENV name parsed in JCL | **Partial** |
| Resource state matching | Check that required resources are available | None | **Missing** |
| Application environment | WLM-managed server definitions | None | **Missing** |
| Server start/stop management | WLM starts application-env servers on demand | None | **Missing** |

### Monitoring & Operator Commands

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| D WLM (display) | Show active policy, service classes, status | None | **Missing** |
| VARY WLM,POLICY= | Activate a policy | None | **Missing** |
| VARY WLM,APPLENV= | Manage application environments | None | **Missing** |
| RMF Workload Activity Report | Detailed per-service-class monitoring | Prometheus metrics (adjacent) | **Partial** |
| WLM Health API (REST/JSON) | Modern query interface for WLM state | None | **Missing** |
| SMF type 72 records | WLM workload activity data | None | **Missing** |
| SMF type 99 records | WLM policy changes | None | **Missing** |

### Couple Dataset & Sysplex

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| WLM couple dataset | Stores service definition | None | **Missing** |
| Sysplex-wide policy activation | Single active policy for all systems | None | **Missing** |
| Cross-system resource groups | Sysplex-wide caps | None | **Missing** |

## Proposed Epic Structure

### WLM-100: Service Definition Data Model
**Scope:** Define Rust data structures for the WLM service definition: policies, service classes (with periods, goals, importance), report classes, workloads, resource groups, classification rules, scheduling environments, application environments.
**Complexity:** L
**Rationale:** Core data model that everything else depends on. Many interrelated structs but no runtime logic yet.

### WLM-101: Classification Rule Engine
**Scope:** Implement the hierarchical classification rule matcher. Given work attributes (subsystem type + qualifiers), traverse the rule tree and return the matching service class and report class. Support all 14 subsystem types and their qualifier hierarchies.
**Complexity:** L
**Rationale:** Rule tree evaluation with multiple qualifier types. Must handle default rules, wildcards, and qualifier precedence.

### WLM-102: Goal Evaluation & Performance Index
**Scope:** Implement goal types (response time average/percentile, execution velocity, discretionary), PI calculation, multi-period transitions triggered by accumulated service units, and importance-based tie-breaking.
**Complexity:** L
**Rationale:** Mathematical model with sliding-window statistics (percentile calculation), service-unit accounting, and period state machines.

### WLM-103: Resource Groups & Capping
**Scope:** Implement resource group definitions (CPU-MSU, CPU-percentage, CP-count, memory limit, min/max capacity), enforcement of caps at process/container level, and type 1/2/3/4 group semantics.
**Complexity:** M
**Rationale:** Maps to Linux cgroups or Kubernetes resource limits. The mapping layer is the main complexity.

### WLM-104: WLM-Managed Initiator Scheduling
**Scope:** Implement dynamic batch initiator management: monitor JES job queues, evaluate service class goals for queued batch work, start/stop initiators to meet goals, honor SCHENV requirements and job-class affinity.
**Complexity:** L
**Rationale:** Requires integration with JES2 job queue, SCHENV evaluation, and capacity analysis. Core WLM value proposition for batch.

### WLM-105: Enclave Framework
**Scope:** Implement enclave lifecycle (create/delete/join/leave), enclave registration, CPU-time tracking, correlator propagation, cross-address-space classification. Map to OpenTelemetry spans or tokio tasks for the cloud-native runtime.
**Complexity:** L
**Rationale:** Novel concurrency model. Must integrate with async runtime while preserving WLM enclave semantics.

### WLM-106: IWM Services API
**Scope:** Implement the core IWM callable services: IWMCLSFY, IWMCONN/IWMDISC, IWMSSEL, IWMRPT, IWMQRYS, IWMWREG/IWMWDREG, IWMSRSRG/IWMSRSDG/IWMSRSUP, IWMSCDEL, IWMSCHDT. Expose as Rust traits with a runtime implementation.
**Complexity:** L
**Rationale:** Large API surface (12+ services) but mostly delegation to classification engine, enclave framework, and server management.

### WLM-107: Scheduling & Application Environments
**Scope:** Implement scheduling environment definitions (named resource sets), resource-state matching for batch scheduling, application environment definitions for WLM-managed servers, and server lifecycle management (start on demand, stop when idle).
**Complexity:** M
**Rationale:** Scheduling environments are relatively simple state checks. Application environments require integration with container/process management.

### WLM-108: Service Definition Persistence & Policy Activation
**Scope:** Implement service definition serialization/deserialization (equivalent to couple dataset), policy activation/deactivation (VARY WLM,POLICY=), and the IWMARIN0-equivalent batch utility for export/import of service definitions.
**Complexity:** M
**Rationale:** Serialization is straightforward. Policy activation must atomically switch all classification and goal evaluation to the new policy.

### WLM-109: Operator Commands & Monitoring
**Scope:** Implement D WLM and VARY WLM operator commands, integrate with existing Prometheus metrics to expose PI values, service class performance, and resource group utilization. Implement SMF type 72/99 record generation for WLM activity.
**Complexity:** M
**Rationale:** Builds on existing Prometheus and metrics infrastructure. Display commands follow established operator-command patterns.

### WLM-110: WLM Health API
**Scope:** Implement a REST/JSON interface for querying WLM state: active policy, service class list with current PI, resource group utilization, enclave counts, initiator status. Modeled after z/OS 2.4+ WLM Health API.
**Complexity:** S
**Rationale:** Thin HTTP layer over data already computed by WLM-102 and WLM-109. Can reuse existing deploy crate's HTTP server.

### WLM-111: Subsystem Integration Hooks
**Scope:** Add WLM classification calls (IWMCLSFY) and transaction-completion reporting (IWMRPT) to existing CICS, DB2, IMS, MQ, JES2 subsystem implementations. Wire up enclave creation for cross-subsystem work (e.g., CICS→DB2 requests classified under one enclave).
**Complexity:** L
**Rationale:** Touches many existing crates (CICS, DB2, IMS, MQ, JES). Each integration point is small but the total scope is large.

## Dependencies

| Epic | Depends On |
|------|-----------|
| WLM-100 | None (foundational data model) |
| WLM-101 | WLM-100 (needs service class definitions) |
| WLM-102 | WLM-100, WLM-101 (needs classified work and service classes with goals) |
| WLM-103 | WLM-100 (resource group definitions) |
| WLM-104 | WLM-101, WLM-102, Batch 11 JES2 (initiator management) |
| WLM-105 | WLM-101 (enclave classification) |
| WLM-106 | WLM-101, WLM-105 (IWM services depend on classification and enclaves) |
| WLM-107 | WLM-100, WLM-101 (scheduling environment evaluation needs classification) |
| WLM-108 | WLM-100 (serialization of service definition) |
| WLM-109 | WLM-102, `open-mainframe-deploy` (Prometheus metrics), Batch 14 SMF |
| WLM-110 | WLM-102, WLM-109 (REST layer over computed metrics) |
| WLM-111 | WLM-106, `open-mainframe-cics`, `open-mainframe-db2`, `open-mainframe-ims`, `open-mainframe-jcl` (JES2) |

### Cross-Batch Dependencies

| Batch | Relationship |
|-------|-------------|
| Batch 11 — JES2 | WLM-managed initiators require JES2 job queue integration |
| Batch 14 — SMF | SMF type 72/99 records for WLM monitoring |
| Batch 8 — RACF | Service definition access control |
| Batch 9 — TSO/ISPF | WLM ISPF admin application |
| Batch 12 — LE | Service unit accounting for LE-managed applications |
| Batch 13 — IMS TM | IMS transaction classification |

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| WLM-100 | L | Large data model: policies, service classes (multi-period), goals, rules, groups |
| WLM-101 | L | Hierarchical rule engine with 14 subsystem types and varied qualifiers |
| WLM-102 | L | Statistical goal evaluation, PI calculation, period state machines |
| WLM-103 | M | Resource capping maps to cgroups/K8s limits; mapping layer is main work |
| WLM-104 | L | Dynamic initiator management with JES2 integration |
| WLM-105 | L | Novel enclave concurrency model spanning address spaces |
| WLM-106 | L | 12+ callable services forming the subsystem integration API |
| WLM-107 | M | Scheduling environment matching and application environment lifecycle |
| WLM-108 | M | Serialization/deserialization and atomic policy activation |
| WLM-109 | M | Operator commands and Prometheus metrics integration |
| WLM-110 | S | Thin REST/JSON layer over existing computed data |
| WLM-111 | L | Integration hooks across 5+ existing subsystem crates |

**Overall Complexity: XL** — 12 proposed epics (1×S, 4×M, 7×L). WLM is the central nervous system of z/OS workload management; its classification rules touch every subsystem, and goal-based scheduling requires real-time resource monitoring and adjustment.

## Feature Count Summary

- **Total features analyzed:** 75+
- **Present:** 0
- **Partial:** 4 (SCHENV parsing, job class parsing, Prometheus metrics adjacency, batch metrics adjacency)
- **Missing:** 71+

## Reference Documentation

- [z/OS MVS Planning: Workload Management (SA23-1390)](https://www.ibm.com/docs/en/zos/2.5.0?topic=mvs-zos-planning-workload-management)
- [z/OS MVS Programming: Workload Management Services (SA23-1391)](https://www.ibm.com/docs/en/zos/2.5.0?topic=mvs-zos-programming-workload-management-services)
- [z/OS MVS Initialization and Tuning Reference — WLM](https://www.ibm.com/docs/en/zos/2.5.0?topic=reference-wlm)
- [z/OS RMF User's Guide — Workload Activity Report](https://www.ibm.com/docs/en/zos/2.5.0?topic=rmf-zos-users-guide)
- [z/OS MVS Setting Up a Sysplex — WLM Couple Dataset](https://www.ibm.com/docs/en/zos/2.5.0?topic=sysplex-zos-mvs-setting-up)
- [IBM z/OS WLM overview (IBM Documentation)](https://www.ibm.com/docs/en/zos/2.5.0?topic=management-workload-overview)
- [Workload Management on z/OS — IBM Redbook SG24-6472](https://www.redbooks.ibm.com/abstracts/sg246472.html)
- [z/OS WLM Health API (z/OS 2.4+)](https://www.ibm.com/docs/en/zos/2.5.0?topic=services-health-api)

## Implementation Status

**Reviewed:** 2026-02-23
**Crate:** `open-mainframe-wlm` (12 source files, 176 passing tests)
**Compiler:** `cargo check` passes; `cargo test` passes (after minor fix to `health.rs` test import)

### Fix Applied During Review

- **health.rs** test module: Added missing `use crate::operator::WlmMode;` import so that tests referencing `WlmMode::Goal` and `WlmMode::Compat` compile correctly.

### Service Definition & Policy (WLM-100)

| Feature | Status | Location |
|---------|--------|----------|
| Service policy (named, activate/deactivate) | YES | `service.rs:109-186` — `ServicePolicy` with `new`, `activate`, `define_class`, `modify_class`, `remove_class`, `list_classes` |
| Service class definition | YES | `service.rs:59-102` — `ServiceClass` with name, description, periods, cpu/storage critical flags |
| Multi-period service class (up to 8 periods) | YES | `service.rs:90-96` — `add_period()` with goal, importance, duration; `goals.rs:204-238` — `WorkUnitTracker` for period transitions |
| Report class | YES | `policy.rs:57-73` — `ReportClass` struct with name and description |
| Workload grouping | YES | `policy.rs:17-49` — `Workload` with `add_class`, `remove_class` |
| Service coefficients (CPU/MSO/IOC/SRB) | YES | `goals.rs:13-43` — `ServiceUnits` with cpu, mso, ioc, srb fields and `total()` |
| Service unit calculation | YES | `goals.rs:32-42` — `total()` and `add()` methods |
| Resource group definition | YES | `policy.rs:89-144` — `ResourceGroup` with CPU cap, memory limit, minimum, cap type |
| Service definition composition | YES | `policy.rs:152-266` — `ServiceDefinition` wrapping policy + workloads + report classes + resource groups |
| Service definition validation | YES | `policy.rs:215-253` — `validate()` checks workload/RG references and duplicate assignments |
| JSON serialization/deserialization | YES | `policy.rs:256-265` — `to_json()` / `from_json()` |
| Service definition ISPF panels | GAP | Not implemented (ISPF admin application) |
| Batch utility IWMARIN0 | YES | `persistence.rs:142-163` — `export_all()` / `import_all()` equivalent |

### Goal Types & Performance Index (WLM-102)

| Feature | Status | Location |
|---------|--------|----------|
| Response time — average | YES | `service.rs:17-22` — `GoalType::ResponseTime { target_seconds, percentile }` |
| Response time — percentile | YES | `service.rs:17-22` — percentile field in ResponseTime variant |
| Execution velocity (1-99) | YES | `service.rs:24` — `GoalType::Velocity(u8)` |
| Discretionary | YES | `service.rs:26` — `GoalType::Discretionary` |
| Performance Index (PI) | YES | `goals.rs:52-108` — `PerformanceIndex` with `response_time()`, `velocity()`, `discretionary()`, `from_goal()` |
| Goal adjustment algorithm | GAP | Dynamic dispatching priority adjustment not implemented |
| Importance levels (1-5) | YES | `service.rs:30-37` — `Importance(u8)` with Ord |
| Sliding window aggregation | YES | `goals.rs:127-198` — `SlidingWindow` with configurable duration, `add_sample`, `average`, `max_pi`, `expire` |
| Importance-based prioritization | YES | `goals.rs:258-277` — `rank_by_urgency()` sorts by unmet goal, importance, PI |
| Period transition tracking | YES | `goals.rs:204-238` — `WorkUnitTracker` with cumulative SU thresholds |

### Classification Rules (WLM-101)

| Feature | Status | Location |
|---------|--------|----------|
| Hierarchical rule tree | YES | `classify.rs:136-171` — `ClassificationRule` with priority ordering, AND-logic qualifiers |
| Subsystem type — JES (batch) | YES | `classify.rs:214,234` — JES/JES2/JES3 parsing; qualifiers: JobClass, UserId, TransactionName |
| Subsystem type — STC | YES | `classify.rs:222,240` — STC with ProcedureName, TransactionName |
| Subsystem type — TSO | YES | `classify.rs:216,235` — TSO with UserId, PerformanceGroup, Accounting |
| Subsystem type — OMVS | YES | `classify.rs:224,242` — OMVS with UserId, TransactionName |
| Subsystem type — CICS | YES | `classify.rs:218,237` — CICS with TransactionName, LuName, SubsystemName |
| Subsystem type — IMS | YES | `classify.rs:219,237` — IMS with TransactionName, LuName, SubsystemName |
| Subsystem type — DB2 | YES | `classify.rs:220,238` — DB2 with TransactionName, SubsystemName, UserId |
| Subsystem type — DDF | YES | `classify.rs:226,244` — DDF with TransactionName, UserId |
| Subsystem type — MQ | YES | `classify.rs:221,239` — MQ with SubsystemName, TransactionName |
| Subsystem type — ASCH | YES | `classify.rs:223,241` — ASCH with TransactionName, UserId |
| Subsystem type — CB (WebSphere) | YES | `classify.rs:225,243` — CB with SubsystemName, TransactionName |
| Subsystem type — IWEB | YES | `classify.rs:228,246` — IWEB with SubsystemName, TransactionName |
| Default service class | YES | `classify.rs:264-271` — `Classifier::new(default_class)` with global default |
| Subsystem-type default | YES | `classify.rs:288-296` — `set_subsystem_default()` / `subsystem_default()` |
| Service class override | YES | `classify.rs:307-311` — checks `service_class_override` first |
| Wildcard matching | YES | `classify.rs:124-131` — `*` for match-all, prefix `*` for starts-with |
| Qualifier types (9 types) | YES | `classify.rs:76-95` — SubsystemType, SubsystemName, TransactionName, UserId, JobClass, Accounting, ProcedureName, LuName, PerformanceGroup |
| LDAP classification | GAP | LDAP subsystem type not in enum (server name, client IP) |
| TCP classification | GAP | TCP subsystem type not in enum (service name, port number) |

### Resource Groups (WLM-103)

| Feature | Status | Location |
|---------|--------|----------|
| CPU cap — percentage | YES | `policy.rs:96-97` — `cpu_limit: Option<f64>`, `set_cpu_limit()` |
| CPU cap type (hard/soft) | YES | `policy.rs:78-84` — `CpuCapType::Hard` / `CpuCapType::Soft` |
| Memory limit | YES | `policy.rs:100` — `memory_limit_mb: Option<u64>`, `set_memory_limit()` |
| Minimum capacity guarantee | YES | `policy.rs:102` — `cpu_minimum: Option<f64>`, `set_cpu_minimum()` |
| CPU cap — MSU | GAP | MSU (million service units) cap type not modeled separately |
| CPU cap — CP count | GAP | Logical processor count limit not modeled |
| Type 1-4 resource groups | GAP | Resource group types (single-system, sysplex-wide, tenant) not modeled |
| Capping enforcement engine | YES | `capping.rs:86-245` — `CappingEngine` with throttle evaluation |
| Throttle decisions | YES | `capping.rs:30-38` — `ThrottleAction::Allow`, `Throttle`, `AllowTemporary` |
| Linux cgroup enforcement | YES | `capping.rs:198-216` — `CgroupCpuLimit`, `CgroupMemoryLimit` actions |
| Kubernetes resource quotas | YES | `capping.rs:218-232` — `K8sResourceQuota` actions |
| Utilization tracking | YES | `capping.rs:14-27` — `GroupUtilization` with CPU%, memory, HWM, work units |

### WLM-Managed Initiators (WLM-104)

| Feature | Status | Location |
|---------|--------|----------|
| Dynamic initiator start/stop | YES | `scheduling.rs:482-544` — `InitiatorScheduler::evaluate()` with start/stop decisions |
| Job queue analysis | YES | `scheduling.rs:469-479` — `analyze_queue()` with distribution and goals-at-risk |
| Job-class affinity | YES | `scheduling.rs:78-80` — `ManagedInitiator::supports_class()` |
| SCHENV matching | YES | `scheduling.rs:449-453,516-519` — `is_schenv_available()` check before start |
| Idle timeout management | YES | `scheduling.rs:439-441,490-498` — configurable idle timeout with stop decisions |
| Initiator lifecycle | YES | `scheduling.rs:547-616` — `start_initiator`, `activate_initiator`, `idle_initiator`, `stop_initiator` |
| CP/zIIP capacity analysis | GAP | Engine type distinction not modeled |
| JES2 integration | GAP | No direct JES2 job queue integration (interface only) |

### Enclaves (WLM-105)

| Feature | Status | Location |
|---------|--------|----------|
| Create/delete enclave | YES | `enclave.rs:115-151,207-212` — `EnclaveManager::create()`, `complete()` |
| Join/leave enclave | YES | `enclave.rs:154-184` — `join()` by correlator |
| CPU time tracking | YES | `enclave.rs:187-203` — `record_cpu()` per participant |
| Get correlator | YES | `enclave.rs:219-223` — `get_by_correlator()` |
| Cross-address-space span | YES | `enclave.rs:176-183` — multiple participants from different subsystems |
| Independent enclave classification | YES | `enclave.rs:138` — enclave has its own `service_class` |
| Total service unit tracking | YES | `enclave.rs:70-76` — `total_service_units()` aggregates across participants |
| Active enclave counts | YES | `enclave.rs:226-242` — `active_count()`, `active_by_subsystem()` |
| Register/deregister participant | GAP | No explicit IWMEREG/IWMEDREG (join/leave covers core functionality) |
| Query CPU time (IWMEQTME) | YES | `enclave.rs:65-67` — `total_cpu_seconds()` |
| Enclave SRB dispatching | GAP | SRB dispatching model not implemented |

### IWM Services API (WLM-106)

| Feature | Status | Location |
|---------|--------|----------|
| IWMCLSFY | YES | `iwm.rs:104-116` — classify + report class lookup |
| IWMCONN / IWMDISC | YES | `iwm.rs:121-153` — connect/disconnect subsystems with tokens |
| IWMSSEL | YES | `iwm.rs:158-165` — select least-loaded available server |
| IWMRPT | YES | `iwm.rs:170-178` — report transaction and compute PI sample |
| IWMQRYS | YES | `iwm.rs:190-213` — query all service class info with PI |
| IWMSRSRG / IWMSRSDG | YES | `iwm.rs:218-243` — register/deregister servers |
| IWMWREG / IWMWDREG | GAP | Work manager register/deregister not modeled |
| IWMSRSUP | GAP | Server status update not separately modeled |
| IWMSCDEL | GAP | Signal delay (waiting for resource) not modeled |
| IWMSCHDT | GAP | Schedule work to application environment not modeled |

### Scheduling & Application Environments (WLM-107)

| Feature | Status | Location |
|---------|--------|----------|
| Scheduling environment definition | YES | `scheduling.rs:154-216` — `SchedulingEnvironment` with resource list |
| Resource state matching | YES | `scheduling.rs:191-198` — `is_available()` checks all resources |
| Resource state tracking | YES | `scheduling.rs:201-206` — `set_resource_state()` |
| Application environment | YES | `scheduling.rs:248-323` — `ApplicationEnvironment` with start procedure, start limit |
| Server start/stop management | YES | `scheduling.rs:296-323` — `start_server()`, `server_ready()`, `stop_server()` |
| Start-on-demand evaluation | YES | `scheduling.rs:368-390` — `AppEnvironmentManager::evaluate_start_on_demand()` |
| Resource definition | YES | `scheduling.rs:127-145` — `ResourceDefinition` with Available/Unavailable/Unknown states |

### Service Definition Persistence & Policy Activation (WLM-108)

| Feature | Status | Location |
|---------|--------|----------|
| Policy store (couple dataset equivalent) | YES | `persistence.rs:19-170` — `PolicyStore` with store/load/list/remove |
| VARY WLM,POLICY= activation | YES | `persistence.rs:103-132` — `activate()` with atomic swap |
| Validation before activation | YES | `persistence.rs:110-115` — validates, keeps current on failure |
| Export/import (IWMARIN0 equivalent) | YES | `persistence.rs:142-163` — `export_all()` / `import_all()` |
| Deactivation (COMPAT mode) | YES | `persistence.rs:135-139` — `deactivate()` |
| Cannot remove active policy | YES | `persistence.rs:62-66` — protection check |

### Operator Commands & Monitoring (WLM-109)

| Feature | Status | Location |
|---------|--------|----------|
| D WLM (display) | YES | `operator.rs:44-114` — `DisplayWlmResponse` with formatted console output |
| VARY WLM,POLICY= | YES | `operator.rs:140-172` — `VaryWlmCommand::parse()` with VARY/V short forms |
| VARY WLM,COMPAT | YES | `operator.rs:145,166-168` — `VaryWlmCommand::Compat` |
| VARY WLM,APPLENV= | GAP | Application environment VARY command not parsed |
| Prometheus metrics | YES | `operator.rs:204-275` — PI, work units, CPU%, memory, throttled, initiator/enclave counts |
| SMF type 72 records | YES | `operator.rs:336-349` — `generate_type72()` workload activity |
| SMF type 99 records | YES | `operator.rs:352-367` — `generate_type99()` policy changes |
| RMF Workload Activity Report | YES | `operator.rs` — Prometheus metrics serve as RMF equivalent |

### WLM Health API (WLM-110)

| Feature | Status | Location |
|---------|--------|----------|
| GET /api/wlm/policy | YES | `health.rs:19-33,106-115` — `PolicyResponse` + builder |
| GET /api/wlm/classes | YES | `health.rs:36-55,118-132` — `ClassesResponse` with PI, goal, importance |
| GET /api/wlm/resource-groups | YES | `health.rs:58-77,135-149` — `ResourceGroupsResponse` |
| GET /api/wlm/initiators | YES | `health.rs:80-92,152-166` — `InitiatorResponse` with state counts |
| GET /api/wlm/enclaves | YES | `health.rs:95-101,169-177` — `EnclaveResponse` with by-subsystem breakdown |
| JSON serialization | YES | `health.rs:182-184` — generic `to_json()` |

### Couple Dataset & Sysplex

| Feature | Status | Location |
|---------|--------|----------|
| WLM couple dataset | YES | `persistence.rs` — `PolicyStore` serves as couple dataset equivalent |
| Sysplex-wide policy activation | GAP | No multi-system sysplex coordination |
| Cross-system resource groups | GAP | Resource groups are single-system only |

### Subsystem Integration Hooks (WLM-111)

| Feature | Status | Location |
|---------|--------|----------|
| CICS classification hook | GAP | No integration with open-mainframe-cics crate |
| DB2 classification hook | GAP | No integration with open-mainframe-db2 crate |
| IMS classification hook | GAP | No integration with open-mainframe-ims crate |
| MQ classification hook | GAP | No integration with open-mainframe-mq crate |
| JES2 initiator management hook | GAP | No integration with JES2 crate |

### Summary

| Category | Implemented | Gap | Total |
|----------|------------|-----|-------|
| Service Definition & Policy (WLM-100) | 12 | 1 | 13 |
| Goal Types & Performance Index (WLM-102) | 9 | 1 | 10 |
| Classification Rules (WLM-101) | 16 | 2 | 18 |
| Resource Groups (WLM-103) | 9 | 3 | 12 |
| WLM-Managed Initiators (WLM-104) | 6 | 2 | 8 |
| Enclaves (WLM-105) | 8 | 2 | 10 |
| IWM Services API (WLM-106) | 6 | 4 | 10 |
| Scheduling & App Environments (WLM-107) | 7 | 0 | 7 |
| Persistence & Activation (WLM-108) | 6 | 0 | 6 |
| Operator Commands & Monitoring (WLM-109) | 6 | 1 | 7 |
| Health API (WLM-110) | 6 | 0 | 6 |
| Couple Dataset & Sysplex | 1 | 2 | 3 |
| Subsystem Integration (WLM-111) | 0 | 5 | 5 |
| **TOTAL** | **92** | **23** | **115** |

**Implementation coverage: ~80%** of analyzed features are implemented in the `open-mainframe-wlm` crate with 176 passing tests.

**Remaining gaps** are primarily:
1. **Subsystem integration hooks** (WLM-111) — wiring WLM classification into CICS, DB2, IMS, MQ, JES2 crates
2. **Sysplex coordination** — multi-system policy sharing and cross-system resource groups
3. **Advanced IWM services** — IWMWREG, IWMSRSUP, IWMSCDEL, IWMSCHDT
4. **LDAP/TCP subsystem types** — not included in classifier enum
5. **MSU/CP-count caps and resource group types 1-4** — not modeled
6. **Dynamic dispatching priority adjustment** — the goal adjustment algorithm
7. **ISPF admin application** — UI for editing service definitions
