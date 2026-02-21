---
version: 'v5.0'
planningGroup: 'PG-12'
technology: 'Workload Manager (WLM)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-wlm-v5.0.md'
  - 'architecture-wlm-v5.0.md'
  - 'gap-analysis/batch-17-wlm.md'
totalEpics: 11
totalStories: 62
frCoverage: '11/11 (100%)'
nfrCoverage: '4/4 (100%)'
---

# Epics & Stories: Workload Manager (WLM)

## Epic Overview

| Epic | Title | Size | Stories | Phase |
|------|-------|------|---------|-------|
| WLM-100 | Service Definition Data Model | L | 7 | B |
| WLM-101 | Classification Rule Engine | L | 6 | B |
| WLM-102 | Goal Evaluation & Performance Index | L | 6 | B |
| WLM-103 | Resource Groups & Capping | M | 5 | B |
| WLM-104 | WLM-Managed Initiator Scheduling | L | 6 | B |
| WLM-105 | Enclave Framework | L | 5 | C |
| WLM-106 | IWM Services API | L | 7 | C |
| WLM-107 | Scheduling & Application Environments | M | 5 | C |
| WLM-108 | Service Definition Persistence | M | 4 | B |
| WLM-109 | Operator Commands & Monitoring | M | 5 | C |
| WLM-110 | WLM Health API | S | 6 | C |

---

## WLM-100: Service Definition Data Model

**User Value:** WLM service policies, classes, and resource groups are defined as structured data matching z/OS conventions.

### WLM-100.1: Service Class Definition

**As a** WLM administrator, **I want** service class definitions with goals and importance.

**Acceptance Criteria:**
- Given service class ONLINE with goal=ResponseTime(200ms,95th percentile), importance=1, when defined, then it is stored in the policy
- Given service class BATCH with goal=Velocity(50), importance=3, when defined, then velocity goal is tracked

### WLM-100.2: Service Goal Types

**As a** WLM administrator, **I want** three goal types: response time, velocity, discretionary.

**Acceptance Criteria:**
- Given ResponseTime goal with average vs percentile variants, when defined, then the correct measurement method is used
- Given Velocity(1-99) goal, when defined, then the target velocity percentage is stored
- Given Discretionary goal, when defined, then the class gets resources only when available

### WLM-100.3: Multi-Period Service Classes

**As a** WLM administrator, **I want** service classes with multiple periods for graduated goals.

**Acceptance Criteria:**
- Given service class with Period 1 (0-500 service units, ResponseTime 200ms) and Period 2 (500+, Velocity 50), when work accumulates 500+ SU, then it transitions to Period 2

### WLM-100.4: Workload and Report Class

**As a** WLM administrator, **I want** workload groupings and report classes for RMF reporting.

**Acceptance Criteria:**
- Given workload PRODUCTION containing service classes ONLINE and BATCH, when queried, then both classes are members
- Given report class RPTONL, when RMF-type reporting runs, then work in RPTONL is grouped for statistics

### WLM-100.5: Resource Group Definition

**As a** WLM administrator, **I want** resource groups to cap CPU and memory.

**Acceptance Criteria:**
- Given resource group DEVGROUP with CPU cap=20%, when defined, then all work in DEVGROUP is limited to 20% of available CPU
- Given memory limit in resource group, when a service class is assigned, then memory usage is tracked and capped

### WLM-100.6: Service Policy Composition

**As a** WLM administrator, **I want** a named policy containing all definitions.

**Acceptance Criteria:**
- Given policy PROD with 5 service classes, 3 resource groups, and classification rules, when created, then all components are accessible by name
- Given policy validation, when a referenced service class doesn't exist, then an error is reported

### WLM-100.7: Data Model Tests

**Acceptance Criteria:**
- Given policy with all component types, when serialized and deserialized, then all data is preserved
- Given `cargo test` WLM data model tests, then all pass

---

## WLM-101: Classification Rule Engine

**User Value:** Work entering the system is automatically classified into the correct service class based on hierarchical rules.

### WLM-101.1: Rule Tree Structure

**As a** WLM administrator, **I want** hierarchical classification rules by subsystem type.

**Acceptance Criteria:**
- Given rule tree with JES subsystem type, qualifier JOBCLASS=A → service class ONLINE, when a batch job with CLASS=A is submitted, then it maps to ONLINE
- Given no matching rule, when work is classified, then the default service class is assigned

### WLM-101.2: Subsystem Type Handlers

**As a** developer, **I want** handlers for all 14 subsystem types.

**Acceptance Criteria:**
- Given JES type with qualifiers (job class, SCHENV, userid), when evaluated, then the correct qualifier is checked
- Given CICS type with qualifiers (transaction ID, connection ID), when evaluated, then CICS-specific attributes are matched
- Given DB2 type with qualifiers (plan name, connection type), when evaluated, then DB2 attributes are used

### WLM-101.3: Qualifier Matching

**As a** WLM administrator, **I want** qualifier patterns with wildcards.

**Acceptance Criteria:**
- Given qualifier `JOBNAME=PAY*`, when job PAYROLL is classified, then it matches
- Given multiple qualifiers (AND logic), when all match, then the rule fires
- Given qualifier precedence, when multiple rules match, then the most specific rule wins

### WLM-101.4: Default Rules and Inheritance

**As a** WLM administrator, **I want** default rules for unmatched work.

**Acceptance Criteria:**
- Given work with no matching specific rule, when classified, then the subsystem default is used
- Given no subsystem default, when classified, then the global default service class is used

### WLM-101.5: Classification Performance

**As a** developer, **I want** classification to complete in < 1ms.

**Acceptance Criteria:**
- Given 100 rules across 5 subsystem types, when 1000 work units are classified, then average classification time is < 1ms

### WLM-101.6: Classification Tests

**Acceptance Criteria:**
- Given 10 classification scenarios across JES/CICS/DB2/TSO, when classified, then each maps to the expected service class
- Given `cargo test` classification tests, then all pass

---

## WLM-102: Goal Evaluation & Performance Index

**User Value:** Performance Index (PI) measures how well the system meets service goals, enabling goal-based resource allocation.

### WLM-102.1: Response Time PI Calculation

**As a** developer, **I want** PI = actual_response_time / goal_response_time.

**Acceptance Criteria:**
- Given goal=200ms and actual average=240ms, when PI is calculated, then PI=1.20 (goal not met)
- Given goal=200ms at 95th percentile and 95th percentile actual=180ms, then PI=0.90 (goal met)

### WLM-102.2: Velocity PI Calculation

**As a** developer, **I want** PI = goal_velocity / actual_velocity.

**Acceptance Criteria:**
- Given velocity goal=50 and actual=40, when PI is calculated, then PI=1.25 (running too slow)
- Given velocity goal=50 and actual=60, then PI=0.83 (exceeding goal)

### WLM-102.3: Service Unit Accounting

**As a** developer, **I want** service units (CPU + MSO + IOC + SRB) tracked per work unit.

**Acceptance Criteria:**
- Given CPU=100 SU, MSO=50 SU, IOC=30 SU, SRB=20 SU, when totaled, then 200 composite service units
- Given multi-period class, when 500 SU accumulated, then period transition check is performed

### WLM-102.4: Sliding Window Aggregation

**As a** developer, **I want** PI calculated over a 5-minute sliding window.

**Acceptance Criteria:**
- Given PI samples over 5 minutes, when calculated, then the rolling average smooths out spikes
- Given a PI sample older than 5 minutes, when the window slides, then it is dropped

### WLM-102.5: Importance-Based Prioritization

**As a** developer, **I want** importance levels (1-5) to prioritize classes with PI > 1.0.

**Acceptance Criteria:**
- Given importance-1 class with PI=1.5 and importance-3 class with PI=1.5, when resources are scarce, then importance-1 gets priority

### WLM-102.6: Goal Evaluation Tests

**Acceptance Criteria:**
- Given synthetic workloads with known response times, when PI is calculated, then values match expected
- Given `cargo test` goal evaluation tests, then all pass

---

## WLM-103: Resource Groups & Capping

**User Value:** Resource groups enforce CPU and memory limits, preventing runaway workloads from consuming all resources.

### WLM-103.1: CPU Capping

**As a** WLM administrator, **I want** CPU caps on resource groups.

**Acceptance Criteria:**
- Given resource group with CPU=20%, when enforced, then total CPU used by grouped work does not exceed 20%

### WLM-103.2: Memory Limits

**As a** WLM administrator, **I want** memory limits on resource groups.

**Acceptance Criteria:**
- Given memory limit=2GB, when tracked, then groups approaching the limit are flagged

### WLM-103.3: Group Types (Soft/Hard)

**As a** WLM administrator, **I want** hard and soft caps.

**Acceptance Criteria:**
- Given hard cap, when limit is reached, then work is throttled
- Given soft cap, when limit is reached but resources available, then work may exceed temporarily

### WLM-103.4: Resource Group Enforcement

**As a** developer, **I want** enforcement via OS-level controls.

**Acceptance Criteria:**
- Given Linux runtime, when CPU cap is enforced, then cgroups are configured
- Given Kubernetes runtime, when limits are set, then resource quotas are applied

### WLM-103.5: Resource Group Tests

**Acceptance Criteria:**
- Given `cargo test` resource group tests, then all pass

---

## WLM-104: WLM-Managed Initiator Scheduling

**User Value:** Batch initiators are dynamically started and stopped based on service class goals, optimizing batch throughput.

### WLM-104.1: Job Queue Monitoring

**As a** developer, **I want** WLM to monitor the JES2 batch queue.

**Acceptance Criteria:**
- Given 50 jobs queued, when WLM evaluates, then the count and service class distribution are known

### WLM-104.2: Initiator Start Decision

**As a** developer, **I want** WLM to start initiators when goals are at risk.

**Acceptance Criteria:**
- Given importance-1 jobs queued with PI > 1.0, when WLM evaluates, then a new initiator is started

### WLM-104.3: Initiator Stop Decision

**As a** developer, **I want** WLM to stop idle initiators to free resources.

**Acceptance Criteria:**
- Given initiator idle for 5 minutes with no eligible work, when WLM evaluates, then the initiator is stopped

### WLM-104.4: SCHENV Evaluation

**As a** developer, **I want** scheduling environment requirements checked before starting initiators.

**Acceptance Criteria:**
- Given job requires SCHENV=DB2PROD, when WLM evaluates, then it checks if DB2PROD resources are available before starting an initiator

### WLM-104.5: Job-Class Affinity

**As a** developer, **I want** initiators matched to job class requirements.

**Acceptance Criteria:**
- Given job CLASS=A requiring specific initiator type, when started, then the initiator matches the class

### WLM-104.6: Managed Initiator Tests

**Acceptance Criteria:**
- Given `cargo test` managed initiator tests, then all pass

---

## WLM-105: Enclave Framework

**User Value:** Cross-address-space work (e.g., CICS → DB2 transactions) is tracked as a single dispatchable unit.

### WLM-105.1: Enclave Lifecycle

**Acceptance Criteria:**
- Given enclave created by CICS, when DB2 joins, then CPU time is tracked across both

### WLM-105.2: CPU Tracking Across Address Spaces

**Acceptance Criteria:**
- Given enclave spanning CICS and DB2, when CPU is consumed, then total is attributed to the enclave's service class

### WLM-105.3: Correlator Propagation

**Acceptance Criteria:**
- Given enclave correlator, when passed from CICS to DB2, then the same enclave is joined

### WLM-105.4: Enclave Classification

**Acceptance Criteria:**
- Given enclave, when classified, then the creating subsystem's classification rules apply

### WLM-105.5: Enclave Tests

**Acceptance Criteria:**
- Given `cargo test` enclave tests, then all pass

---

## WLM-106: IWM Services API

**User Value:** Subsystems integrate with WLM via standard callable services for classification, reporting, and server management.

### WLM-106.1: IWMCLSFY — Classify Work

**Acceptance Criteria:**
- Given work attributes (subsystem type, qualifiers), when IWMCLSFY is called, then service class and report class are returned

### WLM-106.2: IWMCONN/IWMDISC — Connect/Disconnect

**Acceptance Criteria:**
- Given subsystem connecting to WLM, when IWMCONN is called, then the subsystem is registered for classification

### WLM-106.3: IWMSSEL — Select Server

**Acceptance Criteria:**
- Given application environment with 3 servers, when IWMSSEL is called, then the least-loaded server is selected

### WLM-106.4: IWMRPT — Report Transaction

**Acceptance Criteria:**
- Given transaction completing with 150ms response time, when IWMRPT is called, then the sample is recorded for PI calculation

### WLM-106.5: IWMQRYS — Query Service Definition

**Acceptance Criteria:**
- Given active policy, when IWMQRYS is called, then service class definitions are returned

### WLM-106.6: Server Register/Deregister

**Acceptance Criteria:**
- Given IWMSRSRG with server info, when called, then the server is registered for IWMSSEL selection

### WLM-106.7: IWM Services Tests

**Acceptance Criteria:**
- Given `cargo test` IWM services tests, then all pass

---

## WLM-107: Scheduling & Application Environments

**User Value:** Batch jobs can specify scheduling requirements, and application environments manage server lifetimes.

### WLM-107.1: Scheduling Environment Definition

**Acceptance Criteria:**
- Given SCHENV DB2PROD with resource=DB2 state=AVAILABLE, when defined, then batch jobs requiring DB2PROD are only dispatched when DB2 is available

### WLM-107.2: Application Environment Definition

**Acceptance Criteria:**
- Given application environment WAS with start_limit=5, when 3 servers are running, then 2 more can be started

### WLM-107.3: Server Start-on-Demand

**Acceptance Criteria:**
- Given application environment with no running servers, when work arrives, then WLM starts a server

### WLM-107.4: Resource State Tracking

**Acceptance Criteria:**
- Given SCHENV resource goes UNAVAILABLE, when evaluated, then queued jobs wait

### WLM-107.5: Environment Tests

**Acceptance Criteria:**
- Given `cargo test` environment tests, then all pass

---

## WLM-108: Service Definition Persistence

**User Value:** WLM policies persist across restarts and can be activated/deactivated.

### WLM-108.1: Policy Serialization

**Acceptance Criteria:**
- Given a complete policy, when serialized, then all classes, rules, and groups are stored

### WLM-108.2: Policy Deserialization

**Acceptance Criteria:**
- Given a serialized policy, when loaded, then all components are restored

### WLM-108.3: Policy Activation (VARY WLM,POLICY=)

**Acceptance Criteria:**
- Given `VARY WLM,POLICY=NEWPOLICY`, when issued, then the new policy replaces the current one atomically

### WLM-108.4: Persistence Tests

**Acceptance Criteria:**
- Given `cargo test` persistence tests, then all pass

---

## WLM-109: Operator Commands & Monitoring

**User Value:** Operators can view and control WLM via standard commands, and metrics are available via Prometheus.

### WLM-109.1: DISPLAY WLM Command

**Acceptance Criteria:**
- Given `D WLM`, when issued, then active policy name, service classes with current PI, and resource group utilization are displayed

### WLM-109.2: VARY WLM Command

**Acceptance Criteria:**
- Given `VARY WLM,POLICY=PROD`, when issued, then policy PROD is activated

### WLM-109.3: Prometheus Metrics

**Acceptance Criteria:**
- Given Prometheus endpoint, when scraped, then `wlm_performance_index{class="ONLINE"}` shows current PI

### WLM-109.4: SMF Type 72/99 Records

**Acceptance Criteria:**
- Given SMF recording enabled, when WLM interval expires, then SMF type 72 workload activity records are generated

### WLM-109.5: Monitoring Tests

**Acceptance Criteria:**
- Given `cargo test` monitoring tests, then all pass

---

## WLM-110: WLM Health API

**User Value:** REST/JSON API for monitoring WLM state programmatically.

### WLM-110.1: Active Policy Query

**Acceptance Criteria:**
- Given GET /api/wlm/policy, when called, then JSON with active policy name and summary is returned

### WLM-110.2: Service Class Status

**Acceptance Criteria:**
- Given GET /api/wlm/classes, when called, then JSON array with service class name, goal, PI, work count is returned

### WLM-110.3: Resource Group Status

**Acceptance Criteria:**
- Given GET /api/wlm/resource-groups, when called, then JSON with group name, cap, current usage is returned

### WLM-110.4: Initiator Status

**Acceptance Criteria:**
- Given GET /api/wlm/initiators, when called, then JSON with active/idle initiator counts is returned

### WLM-110.5: Enclave Summary

**Acceptance Criteria:**
- Given GET /api/wlm/enclaves, when called, then JSON with active enclave count by subsystem is returned

### WLM-110.6: Health API Tests

**Acceptance Criteria:**
- Given `cargo test` health API tests, then all pass

---

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-WLM-001 | WLM-100.1, WLM-100.2 |
| FR-WLM-002 | WLM-101.1, WLM-101.2, WLM-101.3 |
| FR-WLM-003 | WLM-102.1, WLM-102.2 |
| FR-WLM-004 | WLM-103.1, WLM-103.2 |
| FR-WLM-005 | WLM-104.1, WLM-104.2, WLM-104.3 |
| FR-WLM-006 | WLM-105.1, WLM-105.2 |
| FR-WLM-007 | WLM-106.1 through WLM-106.6 |
| FR-WLM-008 | WLM-107.1 |
| FR-WLM-009 | WLM-108.3 |
| FR-WLM-010 | WLM-109.1 |
| FR-WLM-011 | WLM-110.1 through WLM-110.5 |

**Coverage: 11/11 FRs (100%), 4/4 NFRs (100%)**
