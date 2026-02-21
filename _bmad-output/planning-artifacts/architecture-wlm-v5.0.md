---
version: 'v5.0'
planningGroup: 'PG-12'
technology: 'Workload Manager (WLM)'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-wlm-v5.0.md'
---

# Architecture: Workload Manager (WLM)

## 1. Crate Strategy

**New crate:** `open-mainframe-wlm`

Rationale: WLM is a large subsystem with its own data model, classification engine, goal evaluation, and API surface (IWM services). It integrates with many other crates (JES2, CICS, DB2, IMS, MQ) but has a distinct lifecycle. A separate crate keeps the dependency graph clean.

## 2. Module Layout

```
crates/open-mainframe-wlm/src/
├── lib.rs
├── policy/
│   ├── mod.rs          # ServicePolicy, activation/deactivation
│   ├── service_class.rs # ServiceClass, goals, importance, periods
│   ├── report_class.rs  # ReportClass
│   ├── workload.rs      # Workload grouping
│   ├── resource_group.rs # ResourceGroup, CPU/memory caps
│   └── persistence.rs   # Policy serialization/deserialization
├── classify/
│   ├── mod.rs           # ClassificationEngine
│   ├── rules.rs         # Rule tree, subsystem types, qualifiers
│   └── subsystems.rs    # 14 subsystem type handlers
├── goals/
│   ├── mod.rs           # GoalEvaluator
│   ├── performance_index.rs # PI calculation
│   └── service_units.rs # CPU/MSO/IOC/SRB accounting
├── scheduling/
│   ├── mod.rs           # WLM-managed initiator scheduler
│   ├── environment.rs   # SchedulingEnvironment, ApplicationEnvironment
│   └── initiator.rs     # Initiator lifecycle management
├── enclave/
│   ├── mod.rs           # Enclave lifecycle (create/join/leave/delete)
│   └── tracking.rs      # Cross-address-space work tracking
├── services/
│   ├── mod.rs           # IWM callable services trait
│   ├── classify.rs      # IWMCLSFY
│   ├── connect.rs       # IWMCONN/IWMDISC
│   ├── server.rs        # IWMSSEL, IWMSRSRG/IWMSRSDG/IWMSRSUP
│   ├── report.rs        # IWMRPT
│   └── query.rs         # IWMQRYS
├── commands.rs          # D WLM, VARY WLM operator commands
└── health.rs            # REST/JSON health API, Prometheus metrics
```

## 3. Key Types

```rust
/// Service policy (top-level WLM configuration)
pub struct ServicePolicy {
    pub name: String,
    pub service_classes: Vec<ServiceClass>,
    pub report_classes: Vec<ReportClass>,
    pub workloads: Vec<Workload>,
    pub resource_groups: Vec<ResourceGroup>,
    pub classification_rules: ClassificationRuleTree,
}

/// Service class with goals
pub struct ServiceClass {
    pub name: String,
    pub description: String,
    pub goal: ServiceGoal,
    pub importance: u8,  // 1-5 (1=highest)
    pub periods: Vec<ServicePeriod>,
    pub workload: String,
}

pub enum ServiceGoal {
    ResponseTime { target_ms: u64, percentile: Option<u8> },
    Velocity { target: u8 },  // 1-99
    Discretionary,
}

/// Classification rule tree
pub struct ClassificationRuleTree {
    pub rules: Vec<ClassificationRule>,
}

pub struct ClassificationRule {
    pub subsystem_type: SubsystemType,
    pub qualifiers: Vec<Qualifier>,
    pub service_class: String,
    pub report_class: Option<String>,
}

pub enum SubsystemType {
    JES, STC, TSO, OMVS, CICS, IMS, DB2, DDF, MQ, ASCH, CB, IWEB, LDAP, TCP,
}
```

## 4. Design Decisions

### DD-5.0-WLM-01: Classification as Rule Tree
**Decision:** Classification rules are stored as a hierarchical tree (subsystem type → qualifiers). Evaluation traverses from specific to general, returning first match.

### DD-5.0-WLM-02: PI as Sliding Window
**Decision:** Performance Index is calculated over a 5-minute sliding window using accumulated service unit samples. This avoids instantaneous spikes affecting scheduling.

### DD-5.0-WLM-03: Managed Initiators via JES2 API
**Decision:** WLM starts/stops batch initiators by calling JES2's initiator management API. WLM does not directly manage job execution; it signals JES2 to adjust capacity.

### DD-5.0-WLM-04: Resource Groups via OS Abstractions
**Decision:** CPU capping maps to cgroups (Linux) or Kubernetes resource limits (K8s). Memory capping uses similar OS-level controls. The WLM crate provides the policy; enforcement is delegated to the runtime environment.
