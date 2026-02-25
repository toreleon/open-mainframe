# open-mainframe-deploy

Deployment, health checks, and observability infrastructure for the OpenMainframe z/OS clone. This crate provides everything needed to run the mainframe runtime in a cloud-native Kubernetes environment: configuration management, multi-stage Docker builds, Kubernetes manifest generation with health probes, secrets resolution, Prometheus metrics for COBOL/CICS/IMS/database/batch workloads, RAII-based instrumentation, Grafana dashboards, Prometheus alert rules, distributed trace context propagation, and structured logging with optional OpenTelemetry export.

## Overview

The `open-mainframe-deploy` crate bridges mainframe workloads and cloud-native infrastructure. While the other crates in the OpenMainframe project emulate z/OS subsystems (COBOL, CICS, IMS, JCL, datasets), this crate provides the operational layer needed to deploy, monitor, and observe those workloads in production.

The crate covers three major areas: **deployment** (Dockerfile generation, Kubernetes manifests, secrets management, configuration), **observability** (Prometheus metrics, Grafana dashboards, alerting rules, OpenTelemetry tracing, structured logging), and **runtime integration** (health checks, RAII instrumentation guards, batch job metrics). All features are designed to work together — Kubernetes manifests reference the health endpoints, Prometheus scrape annotations point to the metrics port, and Grafana dashboards query the same metric names registered by the instrumentation layer.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      Public API (lib.rs)                       │
├──────────────────┬─────────────────────┬────────────────────────┤
│   Deployment     │   Observability     │   Runtime Integration  │
├──────────────────┼─────────────────────┼────────────────────────┤
│ config           │ metrics             │ health                 │
│ container        │ batch_metrics       │ instrumentation        │
│ k8s_manifest     │ dashboards          │ server                 │
│ secrets          │ trace_context       │                        │
│                  │ tracing_setup       │                        │
├──────────────────┴─────────────────────┴────────────────────────┤
│  prometheus  │  tracing  │  opentelemetry  │  tokio  │  serde   │
└──────────────┴───────────┴─────────────────┴─────────┴──────────┘
```

### Module Structure

| Module | Lines | Description |
|--------|------:|-------------|
| `config` | 273 | YAML/env-var configuration loading with priority chain |
| `container` | 258 | Multi-stage Dockerfile and `.dockerignore` generation |
| `k8s_manifest` | 368 | Kubernetes Deployment, Service, ConfigMap YAML generation |
| `secrets` | 289 | Three-tier credential resolution (K8s mount > env > config) |
| `server` | 360 | HTTP health/readiness endpoints and Prometheus metrics server |
| `health` | 268 | Liveness and readiness probes with atomic component flags |
| `metrics` | 540 | Prometheus metrics for COBOL, CICS, IMS, database, HTTP |
| `batch_metrics` | 442 | JCL batch job and step execution metrics |
| `instrumentation` | 425 | RAII guards for automatic COBOL/CICS/IMS metric recording |
| `dashboards` | 342 | Grafana dashboard JSON and Prometheus alert rule generation |
| `trace_context` | 282 | Distributed trace context with CICS EIBTASKN-derived IDs |
| `tracing_setup` | 244 | Structured logging and OpenTelemetry OTLP initialization |
| `lib.rs` | 68 | Crate root and re-exports |
| **Total** | **~4,160** | |

## Key Types and Traits

### Configuration

- **`Config`** — Top-level configuration with `ServerConfig`, `DatabaseConfig`, `CobolConfig`, `CicsConfig`, `ImsConfig`, and `ObservabilityConfig`. Loads from YAML files with environment variable overrides.
- **`ServerConfig`** — Host, port (default 8080), and metrics port (default 9090).
- **`DatabaseConfig`** — PostgreSQL URL, pool size, idle timeout, max lifetime.
- **`ObservabilityConfig`** — Log level, format, OpenTelemetry endpoint, metrics prefix, sample rate.

### Container and Kubernetes

- **`DockerConfig`** — Dockerfile generation parameters: Rust version, runtime base image, binary name, ports, features, healthcheck toggle, OCI labels.
- **`ManifestOverrides`** — Kubernetes deployment parameters: replicas, CPU/memory requests and limits, image, namespace, app name.
- **`GeneratedManifests`** — Contains generated `deployment`, `service`, and `configmap` YAML strings.

### Secrets

- **`SecretsResolver`** — Resolves database credentials through a three-tier priority chain: Kubernetes Secret volume mount → environment variables → config fallback.
- **`DatabaseCredentials`** — URL with optional username and password.
- **`CredentialSource`** — Enum tracking which tier resolved the credential (`SecretMount`, `Environment`, `Config`).

### Health Checks

- **`HealthChecker`** — Thread-safe liveness and readiness checks using `Arc<AtomicBool>` flags for database, CICS, and IMS components. Clones share the same flags.
- **`HealthStatus`** — Liveness response with status, version, and uptime.
- **`ReadinessStatus`** — Readiness response with per-component status.

### Metrics

- **`MetricsRegistry`** — Central Prometheus registry containing all metric groups: `Metrics` (HTTP), `CobolMetrics`, `CicsMetrics`, `ImsMetrics`, `DatabaseMetrics`.
- **`Metrics`** — HTTP request counters, duration histograms, active connections gauge, error counters.
- **`CobolMetrics`** — Program execution/compilation counters and duration histograms with per-program labels.
- **`CicsMetrics`** — Transaction counters and duration histograms with per-transid labels, queue depth gauges.
- **`ImsMetrics`** — DL/I call counters and duration histograms with per-database labels, segment retrieval counters.
- **`DatabaseMetrics`** — Query duration histograms, connection pool gauges, error counters.
- **`BatchMetricsCollector`** — Aggregates batch job execution metrics: success/failure counts, per-job averages, error tracking, Prometheus text export.
- **`JobExecutionTracker`** — RAII tracker for a single job execution, recording per-step metrics.

### Instrumentation

- **`InstrumentedRuntime`** — Trait for attaching metrics to runtime components.
- **`CobolInstrumentation`** — Records COBOL program execution and compilation metrics. Returns `ProgramExecGuard` (RAII) from `begin_execution()`.
- **`CicsInstrumentation`** — Records CICS transaction metrics. Returns `TransactionGuard` (RAII) from `begin_transaction()`.
- **`ImsInstrumentation`** — Records DL/I call metrics, segment counts, and PSB lifecycle.
- **`ProgramExecGuard`** / **`TransactionGuard`** — RAII guards that automatically record duration and decrement active gauges on drop, even during panics.

### Tracing

- **`TransactionTrace`** — Builds a hierarchical span tree for a CICS transaction. Creates W3C-compatible trace IDs from EIBTASKN values.
- **`TraceSpan`** — Individual span with parent tracking, operation name, kind, duration, and arbitrary attributes.
- **`SpanKind`** — Span types: `CicsLink`, `CicsXctl`, `SqlQuery`, `Transaction`, `Program`.
- **`TraceId`** / **`SpanId`** — W3C trace context compatible identifiers (32 and 16 hex chars respectively).
- **`TracingConfig`** — Logging and tracing configuration: log level, format (JSON/text/compact), OpenTelemetry endpoint, service name, sample rate.

### Dashboards and Alerts

- **`DashboardConfig`** — Grafana dashboard generation parameters: datasource, prefix, title.
- **`AlertConfig`** — Prometheus alert rule parameters: latency threshold, pool utilization threshold, readiness timeout.

## Implementation Details

### Configuration Loading

Configuration follows a priority chain:
1. YAML file at path from `OPEN_MAINFRAME_CONFIG_PATH` environment variable
2. Default path `/etc/open-mainframe/config.yaml`
3. Environment variable overrides applied on top: `OPEN_MAINFRAME_SERVER_HOST`, `OPEN_MAINFRAME_SERVER_PORT`, `OPEN_MAINFRAME_METRICS_PORT`, `OPEN_MAINFRAME_DB_URL`, `OPEN_MAINFRAME_LOG_LEVEL`, `OPEN_MAINFRAME_LOG_FORMAT`, `OTEL_EXPORTER_OTLP_ENDPOINT`

### Dockerfile Generation

The `generate_dockerfile` function produces a multi-stage build:
- **Builder stage**: `rust:{version}` image, compiles with `--release`, strips the binary
- **Runtime stage**: Minimal `debian:bookworm-slim` with only `ca-certificates`, `libssl3`, and `curl`
- Non-root user `omf` (UID 1000), secrets mount at `/etc/open-mainframe/secrets`
- Optional `HEALTHCHECK` using `curl` against the health endpoint

### Kubernetes Manifests

Generated Deployment manifests include:
- Prometheus scrape annotations (`prometheus.io/scrape: "true"`, port, path)
- Liveness probe at `/health` (initialDelay: 10s, period: 15s)
- Readiness probe at `/ready` (initialDelay: 5s, period: 10s)
- Resource requests and limits
- Environment variables from ConfigMap
- Secret volume mount for database credentials at `/etc/open-mainframe/secrets`

### Secrets Resolution

The `SecretsResolver` implements a three-tier priority chain:
1. **Kubernetes Secret mount**: Reads `url`, `username`, `password` files from the mount path (default `/etc/open-mainframe/secrets`)
2. **Environment variables**: `OPEN_MAINFRAME_DB_URL`, `OPEN_MAINFRAME_DB_USERNAME`, `OPEN_MAINFRAME_DB_PASSWORD`
3. **Config fallback**: Uses the `database.url` from the configuration file

### HTTP Server

The server module runs two TCP listeners on separate ports using Tokio:
- **Application server** (default 8080): `/health` (liveness, always 200) and `/ready` (readiness, 200 or 503 based on component status)
- **Metrics server** (default 9090): `/metrics` returns Prometheus text exposition format

Both servers support graceful shutdown via a `tokio::sync::watch` channel.

### RAII Instrumentation Guards

`ProgramExecGuard` and `TransactionGuard` implement the RAII pattern:
- Created via `begin_execution()` / `begin_transaction()`, which increments the active gauge
- Explicitly finished via `finish(success)`, which records duration and decrements the gauge
- `Drop` implementation ensures the gauge is decremented even if `finish()` is never called (e.g., on panic)
- A `finished` flag prevents double-decrement

### Trace Context

Transaction traces create W3C-compatible identifiers:
- **TraceId**: EIBTASKN (u32) zero-padded to 32 hex characters
- **SpanId**: FNV-1a hash of program name XOR'd with sequence number, truncated to 16 hex characters
- Spans form a parent-child tree tracked via `active_span` index. `end_span` restores the parent as active.

### Batch Job Metrics

The `BatchMetricsCollector` tracks:
- Job completion counts (success/failure) and success rate
- Per-job running average duration
- Per-job error counts (steps with RC > 4)
- Last return code per job name
- Prometheus text format export with `# HELP` and `# TYPE` headers

## Feature Coverage

### Deployment

| Feature | Status |
|---------|--------|
| Multi-stage Dockerfile generation | Implemented |
| Non-root container user | Implemented |
| Docker HEALTHCHECK | Implemented |
| `.dockerignore` generation | Implemented |
| Custom Rust features in build | Implemented |
| OCI labels | Implemented |
| Kubernetes Deployment YAML | Implemented |
| Kubernetes Service YAML | Implemented |
| Kubernetes ConfigMap YAML | Implemented |
| Liveness/readiness probes in K8s | Implemented |
| Prometheus scrape annotations | Implemented |
| Resource requests/limits | Implemented |
| Secret volume mounts | Implemented |

### Configuration and Secrets

| Feature | Status |
|---------|--------|
| YAML configuration loading | Implemented |
| Environment variable overrides | Implemented |
| Config priority chain | Implemented |
| Kubernetes Secret mount resolution | Implemented |
| Environment-based credential resolution | Implemented |
| Config fallback credentials | Implemented |
| Three-tier priority chain | Implemented |

### Metrics and Observability

| Feature | Status |
|---------|--------|
| Prometheus metric registry | Implemented |
| HTTP request metrics (counter, histogram) | Implemented |
| COBOL execution/compilation metrics | Implemented |
| CICS transaction metrics | Implemented |
| CICS queue depth gauges | Implemented |
| IMS DL/I call metrics | Implemented |
| IMS segment retrieval counters | Implemented |
| Database query duration histograms | Implemented |
| Connection pool gauges | Implemented |
| Batch job execution metrics | Implemented |
| Per-step metrics with I/O counts | Implemented |
| Prometheus text format export | Implemented |
| Grafana dashboard JSON generation | Implemented |
| Prometheus alert rules YAML generation | Implemented |
| 5 pre-built alert rules | Implemented |

### Instrumentation

| Feature | Status |
|---------|--------|
| RAII program execution guards | Implemented |
| RAII transaction guards | Implemented |
| Drop-safe gauge management | Implemented |
| COBOL instrumentation | Implemented |
| CICS instrumentation | Implemented |
| IMS instrumentation (PSB/DLI) | Implemented |
| InstrumentedRuntime trait | Implemented |

### Tracing

| Feature | Status |
|---------|--------|
| W3C-compatible trace IDs from EIBTASKN | Implemented |
| Hierarchical span tree | Implemented |
| CICS LINK/XCTL/SQL span kinds | Implemented |
| Span attributes (key-value) | Implemented |
| Structured JSON logging | Implemented |
| Text and compact log formats | Implemented |
| OpenTelemetry OTLP export (feature-gated) | Implemented |
| Trace-ID-ratio-based sampling | Implemented |
| Environment-based tracing config | Implemented |

### Health Checks

| Feature | Status |
|---------|--------|
| Liveness endpoint (/health) | Implemented |
| Readiness endpoint (/ready) | Implemented |
| Per-component readiness flags | Implemented |
| Thread-safe atomic flags | Implemented |
| Uptime tracking | Implemented |
| Graceful server shutdown | Implemented |

## Usage Examples

### Configuration and Server Startup

```rust
use open_mainframe_deploy::{Config, HealthChecker, MetricsRegistry, start_servers};
use std::sync::Arc;

let config = Config::load()?;
let health = HealthChecker::new();
let registry = Arc::new(MetricsRegistry::new(&config.observability.metrics_prefix)?);

let handle = start_servers(&config.server, health.clone(), registry).await?;
health.set_database_ready(true);

// ... run application ...
handle.shutdown();
handle.wait().await;
```

### COBOL Instrumentation with RAII Guards

```rust
use open_mainframe_deploy::{CobolInstrumentation, MetricsRegistry};
use std::sync::Arc;

let registry = Arc::new(MetricsRegistry::new("open_mainframe")?);
let cobol = CobolInstrumentation::new(registry);

// Guard auto-records duration and decrements active count on drop
let guard = cobol.begin_execution("PAYROLL");
// ... execute COBOL program ...
guard.finish(true); // Records success + duration
```

### Batch Job Tracking

```rust
use open_mainframe_deploy::{JobExecutionTracker, BatchMetricsCollector};
use std::time::Duration;

let mut collector = BatchMetricsCollector::new();
let mut tracker = JobExecutionTracker::start("NIGHTJOB", 12345);
tracker.record_step("STEP1", "IEFBR14", 0, Duration::from_secs(1));
tracker.record_step_with_io("STEP2", "SORT", 0, Duration::from_secs(30), 1000000, 999000);

let metrics = tracker.finish();
collector.record_job(&metrics);
println!("Success rate: {:.1}%", collector.success_rate());
println!("{}", collector.to_prometheus("omf"));
```

### Kubernetes Deployment

```rust
use open_mainframe_deploy::{Config, ManifestOverrides, generate_manifests};

let config = Config::load()?;
let overrides = ManifestOverrides {
    replicas: 3,
    memory_limit: "512Mi".into(),
    image: "registry.example.com/open-mainframe:v1.0".into(),
    ..Default::default()
};
let manifests = generate_manifests(&config, &overrides);
std::fs::write("deployment.yaml", &manifests.deployment)?;
std::fs::write("service.yaml", &manifests.service)?;
std::fs::write("configmap.yaml", &manifests.configmap)?;
```

### Dockerfile Generation

```rust
use open_mainframe_deploy::{DockerConfig, generate_dockerfile, generate_dockerignore};

let config = DockerConfig {
    features: vec!["postgres".into()],
    ..Default::default()
};
std::fs::write("Dockerfile", generate_dockerfile(&config))?;
std::fs::write(".dockerignore", generate_dockerignore())?;
```

### Distributed Tracing

```rust
use open_mainframe_deploy::{TransactionTrace, SpanKind};

let mut trace = TransactionTrace::new(42, "PAY1");
let tx_span = trace.start_transaction_span("PAYROLL");
let sql_span = trace.start_sql_span("SELECT * FROM EMPLOYEE");
trace.end_span(sql_span);
let link_span = trace.start_link_span("PAYCALC");
trace.end_span(link_span);
trace.end_span(tx_span);

for span in trace.completed_spans() {
    println!("{}: {}us", span.operation, span.duration_us.unwrap());
}
```

## Dependencies

| Dependency | Purpose |
|------------|---------|
| `prometheus` | Prometheus metric types and registry |
| `tracing` | Structured logging and span instrumentation |
| `tracing-subscriber` | Log formatting (JSON, text, compact) with env-filter |
| `tracing-opentelemetry` | OpenTelemetry tracing bridge |
| `opentelemetry` | OpenTelemetry API |
| `opentelemetry_sdk` | OpenTelemetry SDK with Tokio runtime |
| `opentelemetry-otlp` | OTLP gRPC exporter via tonic |
| `serde` / `serde_json` / `serde_yaml` | Configuration and metrics serialization |
| `tokio` | Async runtime for HTTP servers |
| `thiserror` | Error type derive macros |

### Feature Flags

| Feature | Description |
|---------|-------------|
| `otel` | Enables OpenTelemetry OTLP trace export |

## Testing

Run the full test suite:

```bash
cargo test -p open-mainframe-deploy
```

The crate includes approximately 82 unit tests:

- **Config** (3 tests): Default values, YAML loading
- **Container** (12 tests): Dockerfile structure, non-root user, healthcheck, features, labels
- **Server** (4 tests): Health/readiness/metrics endpoints, graceful shutdown (async)
- **Health** (6 tests): Liveness, readiness, component status, clone-shares-state
- **Metrics** (6 tests): Registry creation, COBOL/CICS/IMS/database metric recording
- **Batch Metrics** (9 tests): Job tracking, error counting, success rate, Prometheus export
- **Instrumentation** (9 tests): RAII guards, drop safety, concurrent executions
- **Dashboards** (8 tests): Grafana JSON validity, alert rule generation, custom thresholds
- **K8s Manifest** (10 tests): Deployment, service, configmap, probes, secrets, overrides
- **Secrets** (8 tests): Mount resolution, env fallback, config fallback, file trimming
- **Trace Context** (4 tests): TraceId format, span hierarchy, parent chains
- **Tracing Setup** (3 tests): Default config, log format parsing

## Limitations and Future Work

- **No live database health checks**: The `HealthChecker` uses manually-set atomic flags; it does not perform actual database connectivity checks.
- **Raw TCP HTTP server**: The server uses raw TCP parsing rather than an HTTP framework. It handles only GET requests and does not support HTTP/1.1 keep-alive, chunked encoding, or TLS.
- **No Helm chart generation**: Kubernetes manifests are generated as raw YAML; Helm chart templating is not supported.
- **No horizontal pod autoscaler**: HPA manifests are not generated.
- **No network policy generation**: Kubernetes NetworkPolicy manifests are not included.
- **Grafana dashboard is static**: The dashboard JSON is generated as a static template; it does not support dynamic panel configuration or variable templating beyond the prefix.
- **No metric aggregation**: Batch metrics maintain running averages in-memory; there is no persistent storage or time-series aggregation.
- **OpenTelemetry feature is compile-time**: The `otel` feature flag controls compilation; there is no runtime toggle for OTLP export.
- **Single-database credential model**: Secrets resolution supports only one set of database credentials. Multiple database connections would require manual resolution.
- **No container registry push**: Dockerfile generation does not include build or push automation.
