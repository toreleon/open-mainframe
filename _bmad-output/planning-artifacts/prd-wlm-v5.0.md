---
version: 'v5.0'
planningGroup: 'PG-12'
technology: 'Workload Manager (WLM)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'gap-analysis/batch-17-wlm.md'
epicIds: ['WLM-100', 'WLM-101', 'WLM-102', 'WLM-103', 'WLM-104', 'WLM-105', 'WLM-106', 'WLM-107', 'WLM-108', 'WLM-109', 'WLM-110']
sysEpicIds: []
---

# PRD: Workload Manager (WLM)

## 1. Problem Statement

z/OS WLM is the core resource-allocation subsystem that controls CPU, storage, and I/O distribution across all work. It uses goal-based scheduling with service classes, classification rules, resource groups, and performance index (PI) monitoring. Every z/OS installation runs WLM. OpenMainframe parses JCL SCHENV and job class but has no WLM engine — no service definitions, no classification, no PI calculation, no managed initiators. Without WLM, batch job scheduling is simplistic and subsystem resource management is absent.

## 2. Functional Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| FR-WLM-001 | Service policy shall define service classes with goals (response time, velocity, discretionary) | Batch 17 |
| FR-WLM-002 | Classification rules shall route work to service classes based on subsystem type and qualifiers | Batch 17 |
| FR-WLM-003 | Performance Index shall be calculated as actual/goal ratio | Batch 17 |
| FR-WLM-004 | Resource groups shall cap CPU and memory usage | Batch 17 |
| FR-WLM-005 | WLM-managed initiators shall start/stop based on batch queue goals | Batch 17 |
| FR-WLM-006 | Enclaves shall track work across address space boundaries | Batch 17 |
| FR-WLM-007 | IWM callable services (IWMCLSFY, IWMCONN, IWMSSEL, IWMRPT) | Batch 17 |
| FR-WLM-008 | Scheduling environments shall define resource requirements for batch | Batch 17 |
| FR-WLM-009 | Service policy persistence and activation (VARY WLM,POLICY=) | Batch 17 |
| FR-WLM-010 | DISPLAY WLM operator command | Batch 17 |
| FR-WLM-011 | REST/JSON health API for WLM state | Batch 17 |

## 3. Non-Functional Requirements

| ID | Requirement | Threshold |
|----|-------------|-----------|
| NFR-WLM-001 | Classification rule evaluation | < 1ms per work unit |
| NFR-WLM-002 | PI calculation window | 5-minute sliding window |
| NFR-WLM-003 | No unsafe code | `#![forbid(unsafe_code)]` |
| NFR-WLM-004 | Error diagnostics | thiserror + miette |

## 4. Dependencies

| Dependency | Direction | Description |
|------------|-----------|-------------|
| open-mainframe-jcl | WLM → JCL | JES2 batch queue, SCHENV |
| open-mainframe-cics | WLM → CICS | CICS transaction classification |
| open-mainframe-db2 | WLM → DB2 | DB2 stored procedure environments |
| open-mainframe-deploy | WLM → Deploy | Prometheus metrics |
