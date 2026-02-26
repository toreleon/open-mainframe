# open-mainframe-zosmf

z/OSMF REST API Server — a high-performance Rust implementation of the standard z/OS management interface, enabling Zowe CLI, Zowe Explorer, and Zowe API ML to connect to the OpenMainframe environment.

## Overview

z/OSMF (z/OS Management Facility) is the modern REST-based management interface for IBM mainframes. This crate reimplements the core z/OSMF REST APIs, providing endpoints for dataset management, job control, TSO command execution, and console communication. It serves as the gateway for all Zowe-based integrations.

## Architecture

```
    Zowe CLI / Explorer                   z/OSMF REST Gateway
    ┌────────────────────┐                ┌────────────────────────┐
    │  REST Requests     │    HTTPS       │    Axum Router         │
    │  (JSON / Binary)   │ ─────────────> │    (Endpoints)         │
    └────────────────────┘    TLS 1.2+    └────────────────────────┘
                                                       │
    ┌────────────────────┐                ┌────────────────────────┐
    │  Auth & Security   │ <── RACF ────  │    JWT & Basic Auth    │
    │  JWT Tokens        │                │    Middleware          │
    └────────────────────┘                └────────────────────────┘
                                                       │
                                                       ▼
    ┌────────────────────┐                ┌────────────────────────┐
    │  Subsystem API     │ <──────────────────  State Manager      │
    │  JES2, TSO, MVS    │    Internal    │    (AppState)          │
    └────────────────────┘    Crates      └────────────────────────┘
```

### Module Structure

| Module | Description |
|--------|-------------|
| `handlers` | API endpoint implementations: Datasets, Jobs, TSO, Console, and USS |
| `types` | Request/Response DTOs: Binary-compatible with standard z/OSMF JSON |
| `middleware`| Authentication and logging: Basic Auth, JWT, and tracing layers |
| `state` | Global application state: Context for internal subsystem handles |
| `jwt` | Token management: Signing and verification of JWTs for stateless auth |
| `eureka` | API Mediation Layer integration: Dynamic registration with Eureka |
| `mounts` | Filesystem mapping: Mapping USS paths to REST URIs |
| `tn3270` | Terminal gateway: Implementation of the TSO terminal REST API |

## Key Types and Components

### API Server
- `AppState`: The central context object shared across all request handlers.
- `ZosmfConfig`: Configuration for ports, TLS certificates, and subsystem endpoints.
- `build_router`: Orchestrates the assembly of all REST API routes into an Axum router.

### Authentication
- `ZosmfAuth`: Middleware providing unified Basic and JWT authentication.
- `Claims`: Internal structure for JWT payload data (USERID, Group, etc.).

### REST Handlers
- `JobHandler`: Manages JCL submission, status monitoring, and spool retrieval.
- `DatasetHandler`: Provides CRUD operations for MVS datasets and PDS members.

## Feature Coverage

| Feature | API Category | Status |
|---------|--------------|--------|
| Datasets        | REST         | Implemented (List, Read, Write, Delete) |
| Jobs            | REST         | Implemented (Submit, Status, Output) |
| TSO Commands    | REST         | Implemented (Stateless execution) |
| Console         | REST         | Implemented (Operator commands) |
| USS Files       | REST         | Implemented |
| API ML (Eureka) | Integration  | Implemented (Heartbeat, Registration) |
| HTTPS / TLS     | Security     | Implemented |

## Usage Examples

### Starting the z/OSMF Server

```rust
use open_mainframe_zosmf::{build_router, ZosmfConfig};

#[tokio::main]
async fn main() {
    let config = ZosmfConfig::from_env();
    let router = build_router(config);
    
    let listener = tokio::net::TcpListener::bind("0.0.0.0:443").await.unwrap();
    axum::serve(listener, router).await.unwrap();
}
```

### Submitting JCL via REST (Internal Handler logic)

```rust
// Internally used by handlers to route to open-mainframe-jcl
let result = state.jcl_executor.execute(&parsed_job).unwrap();
```

## Testing

The z/OSMF crate includes 200+ tests:
- **API**: Integration tests for every REST endpoint using `axum::serve`.
- **Auth**: Validation of JWT token expiration and Basic Auth credential mapping.
- **DTOs**: Ensures JSON compatibility with Zowe CLI and other clients.
- **Subsystem**: Mocked subsystem tests to verify correct routing of requests.

```sh
cargo test -p open-mainframe-zosmf
```
