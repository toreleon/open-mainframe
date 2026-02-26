# OpenMainframe

A z/OSMF-compatible REST API server written in Rust. Provides a drop-in replacement for IBM z/OSMF that works with standard Zowe CLI and Zowe Explorer tooling, backed by Rust implementations of core z/OS subsystems (JES2, RACF, TSO, ISPF, datasets, JCL, COBOL, REXX, and more).

## Quick Start

Requires Rust 1.82 or later.

```bash
git clone https://github.com/toreleon/OpenMainframe.git
cd OpenMainframe
cargo run --release --package open-mainframe-zosmf --bin zosmf-server
```

The server starts on `http://127.0.0.1:10443` with a default user `IBMUSER` / `SYS1`.

Override the port with the `ZOSMF_PORT` environment variable:

```bash
ZOSMF_PORT=8443 cargo run --release --package open-mainframe-zosmf --bin zosmf-server
```

## Features

- **Zowe CLI / Zowe Explorer compatible** — connect standard z/OS tooling to a local server
- **Dataset operations** — create, list, read, write, delete sequential datasets and PDS members
- **Job management** — submit JCL, list jobs, view status, read spool files, cancel, purge
- **TSO commands** — stateful sessions and stateless v1 API (Zowe CLI v3+)
- **MVS console commands** — issue operator commands with solicitation key support
- **USS file operations** — list, read, write, delete with Unix permissions and metadata
- **JWT authentication** — RACF user validation with HTTP Basic Auth and token cookies
- **TN3270E terminal server** — 3270 screen emulation
- **Zowe API ML registration** — Eureka service discovery

### Connecting Zowe CLI

```bash
# Install Zowe CLI (if not already installed)
npm install -g @zowe/cli

# Create a Zowe profile pointing to the OpenMainframe server
zowe profiles create zosmf openmf --host 127.0.0.1 --port 10443 --user IBMUSER --password SYS1 --reject-unauthorized false

# Or with Zowe v3 team config (zowe.config.json):
zowe config init
# Then edit the host/port/user/password to point to your server
```

### Verify Connection

```bash
# Check server status
zowe zosmf check status

# List datasets
zowe files list ds "IBMUSER.*"

# Create and write a dataset
zowe files create ds "IBMUSER.MY.DATA" --dsorg PS --recfm FB --lrecl 80
zowe files upload ftds "local-file.txt" "IBMUSER.MY.DATA"
zowe files view ds "IBMUSER.MY.DATA"

# Submit JCL and view output
zowe jobs submit lf my-job.jcl
zowe jobs list jobs
zowe jobs view sfbi JOB00001 1

# Issue TSO command
zowe tso issue command "TIME"

# Issue console command
zowe console issue command "D A,L"

# List USS files
zowe files list uss "/"
```

### REST API Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/zosmf/info` | Server information |
| `POST` | `/zosmf/services/authenticate` | Authenticate (returns JWT) |
| `GET` | `/zosmf/restfiles/ds?dslevel=HLQ.*` | List datasets |
| `PUT` | `/zosmf/restfiles/ds/{dsname}` | Create dataset |
| `GET` | `/zosmf/restfiles/ds/{dsname}` | Read dataset content |
| `PUT` | `/zosmf/restfiles/ds/{dsname}` | Write dataset content |
| `DELETE` | `/zosmf/restfiles/ds/{dsname}` | Delete dataset |
| `GET` | `/zosmf/restfiles/ds/{dsname}/member` | List PDS members |
| `PUT` | `/zosmf/restjobs/jobs` | Submit JCL |
| `GET` | `/zosmf/restjobs/jobs?owner=*&prefix=*` | List jobs |
| `GET` | `/zosmf/restjobs/jobs/{name}/{id}` | Job status |
| `GET` | `/zosmf/restjobs/jobs/{name}/{id}/files` | List spool files |
| `GET` | `/zosmf/restjobs/jobs/{name}/{id}/files/{n}/records` | Read spool content |
| `PUT` | `/zosmf/restjobs/jobs/{name}/{id}` | Hold/release/cancel job |
| `DELETE` | `/zosmf/restjobs/jobs/{name}/{id}` | Purge job |
| `POST` | `/zosmf/tsoApp/tso` | Start TSO session |
| `PUT` | `/zosmf/tsoApp/tso/{key}` | Send TSO command |
| `PUT` | `/zosmf/tsoApp/v1/tso` | Stateless TSO command (v1 API) |
| `PUT` | `/zosmf/restconsoles/consoles/{name}` | Issue console command |
| `GET` | `/zosmf/restfiles/fs?path=/` | List USS directory |
| `GET` | `/zosmf/restfiles/fs/{path}` | Read USS file |
| `PUT` | `/zosmf/restfiles/fs/{path}` | Write USS file |
| `DELETE` | `/zosmf/restfiles/fs/{path}` | Delete USS file/directory |

All endpoints require HTTP Basic Auth (`Authorization: Basic ...`) or a JWT token (`Cookie: jwtToken=...`).

### Server Configuration

Create a `zosmf.toml` file to customize the server:

```toml
[server]
host = "0.0.0.0"
port = 10443

[auth]
token_ttl_seconds = 28800  # 8 hours
token_secret = "change-me-in-production"

[uss]
root_directory = "/opt/openmainframe/uss"

[cors]
allowed_origins = ["*"]

[zosmf_info]
hostname = "my-mainframe"
saf_realm = "SAFRealm"
```

### Adding Users

Users are managed through the RACF subsystem. The default server creates `IBMUSER` with password `SYS1`. Additional users can be added programmatically through the RACF API or by extending `src/main.rs`.

## Subsystem Libraries

The z/OSMF server is backed by standalone Rust implementations of z/OS subsystems. Each library is documented with detailed architecture diagrams, implementation deep-dives, and z/OS gap analysis:

| Crate | Subsystem | Documentation |
|-------|-----------|---------------|
| `open-mainframe-zosmf` | z/OSMF REST API server | [README](crates/open-mainframe-zosmf/README.md) |
| `open-mainframe-jes2` | JES2 job entry subsystem | [README](crates/open-mainframe-jes2/README.md) |
| `open-mainframe-racf` | RACF security (users, groups, profiles, SAF) | [README](crates/open-mainframe-racf/README.md) |
| `open-mainframe-tso` | TSO command processor | [README](crates/open-mainframe-tso/README.md) |
| `open-mainframe-ispf` | ISPF panels, tables, editor, file tailoring | [README](crates/open-mainframe-ispf/README.md) |
| `open-mainframe-rexx` | REXX interpreter | [README](crates/open-mainframe-rexx/README.md) |
| `open-mainframe-dataset` | Dataset I/O (VSAM, QSAM, PDS, GDG, catalog) | [README](crates/open-mainframe-dataset/README.md) |
| `open-mainframe-jcl` | JCL parser, executor, utilities | [README](crates/open-mainframe-jcl/README.md) |
| `open-mainframe-cobol` | COBOL lexer, parser, semantic analysis, codegen | [README](crates/open-mainframe-cobol/README.md) |
| `open-mainframe-hlasm` | HLASM assembler | [README](crates/open-mainframe-hlasm/README.md) |
| `open-mainframe-mq` | IBM MQ queue manager | [README](crates/open-mainframe-mq/README.md) |
| `open-mainframe-encoding` | EBCDIC encoding (21 code pages) | [README](crates/open-mainframe-encoding/README.md) |
| `open-mainframe-sort` | DFSORT (SORT/MERGE/COPY, ICETOOL) | [README](crates/open-mainframe-sort/README.md) |
| `open-mainframe-runtime` | Language Environment runtime | [README](crates/open-mainframe-runtime/README.md) |
| `open-mainframe-cics` | CICS transaction processor | [README](crates/open-mainframe-cics/README.md) |
| `open-mainframe-ims` | IMS/DB (DL/I) | [README](crates/open-mainframe-ims/README.md) |
| `open-mainframe-db2` | DB2 (EXEC SQL, PostgreSQL backend) | [README](crates/open-mainframe-db2/README.md) |
| `open-mainframe-smf` | SMF records | [README](crates/open-mainframe-smf/README.md) |
| `open-mainframe-wlm` | Workload Manager | [README](crates/open-mainframe-wlm/README.md) |
| `open-mainframe-pli` | PL/I parser and interpreter | [README](crates/open-mainframe-pli/README.md) |
| `open-mainframe-tui` | TN3270E terminal server | [README](crates/open-mainframe-tui/README.md) |
| `open-mainframe-mvs` | MVS system services (SVCs) | [README](crates/open-mainframe-mvs/README.md) |
| `open-mainframe-uss` | UNIX system services | [README](crates/open-mainframe-uss/README.md) |
| `open-mainframe-utilities` | Standard z/OS utility programs | [README](crates/open-mainframe-utilities/README.md) |
| `open-mainframe-deploy` | Container deployment (K8s, Docker) | [README](crates/open-mainframe-deploy/README.md) |
| `open-mainframe-assess` | Code complexity analysis | [README](crates/open-mainframe-assess/README.md) |
| `open-mainframe-lang-core` | Shared AST, spans, diagnostics | [README](crates/open-mainframe-lang-core/README.md) |

## Engineering Standards

OpenMainframe is developed with a focus on architectural fidelity and rigorous validation:

- **Correctness First**: Subsystem emulations are verified against real z/OS behavior using a suite of 5,000+ automated tests.
- **Traceability**: All major components (e.g., JCL executor, MVS SVCs) include documentation of implementation vs. mainframe gaps.
- **Modern Performance**: Written in safe, concurrent Rust, providing a high-throughput alternative to traditional emulators.
- **Cloud Native**: Designed for containerized environments with built-in health checks and Prometheus metrics.

## License

Apache-2.0

## Contributing

Contributions welcome! Please ensure:
- Code passes `cargo fmt --check`
- Code passes `cargo clippy -- -D warnings`
- All tests pass with `cargo test`
