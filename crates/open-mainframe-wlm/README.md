# open-mainframe-wlm

z/OS Workload Manager (WLM) — a sophisticated Rust implementation of the mainframe's goal-oriented resource management subsystem for the OpenMainframe project.

## Overview

WLM is responsible for dynamically managing the distribution of system resources across various workloads to meet business goals. This crate reimplements the core WLM algorithms, including service class goal monitoring, work classification, enclave management, and resource capping.

## Architecture

```
    Work Incoming                         WLM Management Hub
    ┌──────────────┐                      ┌────────────────────┐
    │  Work Request│    Classification    │    Classifier      │
    │  Job, Trans  │ ──────────────────>  │    (Rules)         │
    └──────────────┘    WorkAttribute     │  Service Classes   │
           │                               └────────────────────┘
           ▼                                        │
    ┌──────────────┐    Monitoring        ┌────────────────────┐
    │  Enclaves /  │ ──────────────────>  │   Policy Engine    │
    │  Address Spc │    PerformanceIdx    │   Goals vs Actual  │
    └──────────────┘                      │  Velocity, Resp    │
                                          └────────────────────┘
           │                                        │
           ▼                                        ▼
    ┌──────────────┐    Action            ┌────────────────────┐
    │  Resource    │ <──────────────────  │   Capping Engine   │
    │  Adjustment  │    ThrottleAction    │   CPU / Mem Cap    │
    └──────────────┘                      └────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `classify` | Work classification: Mapping work attributes to service classes based on rules |
| `service` | Service definitions: Service classes, goals, importance, and policies |
| `goals` | Goal management: Monitoring execution velocity and response time goals |
| `enclave` | Enclave management: Tracking work units that span address spaces |
| `capping` | Resource capping: Enforcement of CPU and group utilization limits |
| `scheduling`| Scheduling environments: Resource-aware job dispatching and server management |
| `iwm` | IWM (Installation Workload Management) services: IWMCLSY, IWMENC, etc. |
| `operator` | WLM operator commands: VARY WLM and display performance status |
| `persistence`| Policy store: Activation and storage of service definitions |

## Key Types and Components

### Classification
- `Classifier`: The main engine for matching incoming work to service classes.
- `WorkAttribute`: Attributes used for classification (USERID, TRANSACTION, etc.).
- `ClassificationRule`: Defined criteria for workload routing.

### Goal Performance
- `ServiceGoal`: Definition of a performance target (e.g., Velocity 50%).
- `PerformanceIndex (PI)`: A calculated ratio indicating how well a goal is being met (PI > 1.0 means goal is missed).
- `WorkUnitTracker`: Tracks the resource consumption and completion status of individual work units.

### Enforcement
- `CappingEngine`: Dynamically adjusts resource access based on utilization goals.
- `ThrottleAction`: Specific actions taken to enforce caps (e.g., Suspend, Delay).

## Feature Coverage

| Feature | Category | Status |
|---------|----------|--------|
| Classification  | Routing  | Implemented (Rules, Attributes) |
| Goal Types      | Policy   | Implemented (Velocity, Response, Disc) |
| Enclaves        | Process  | Implemented (Create, Join, Delete) |
| Scheduling Envs | System   | Implemented (Resources, States) |
| Performance PI  | Monitor  | Implemented (Dynamic calculation) |
| Service Policy  | Mgmt     | Implemented (Activate, Display) |
| CPU Capping     | Enforcement| Implemented |

## Usage Examples

### Classifying Incoming Work

```rust
use open_mainframe_wlm::{Classifier, WorkRequest, SubsystemType};

// classifier setup placeholder
let request = WorkRequest::new(SubsystemType::Cics)
    .with_attribute("TRANSACTION", "PAYR");

let result = classifier.classify(&request).unwrap();
println!("Work routed to service class: {}", result.service_class);
```

### Creating a Service Policy

```rust
use open_mainframe_wlm::{ServicePolicy, ServiceClass, ServiceGoal, GoalType};

let mut policy = ServicePolicy::new("STANDARD");
let mut class = ServiceClass::new("ONLINE", 1); // Importance 1
class.set_goal(ServiceGoal::new(GoalType::Velocity, 60));

policy.add_class(class);
```

## Testing

The WLM crate includes 200+ tests:
- **Classification**: Validates rule precedence and attribute matching logic.
- **Goals**: Tests the PI calculation algorithm across various performance scenarios.
- **Enclaves**: Verifies enclave state transitions and attribute inheritance.
- **Capping**: Simulates high-utilization scenarios to test capping enforcement.

```sh
cargo test -p open-mainframe-wlm
```
