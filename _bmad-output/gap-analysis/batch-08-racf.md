# Gap Analysis: RACF (Resource Access Control Facility)

## Official Specification Summary

RACF (Resource Access Control Facility) is IBM's primary External Security Manager (ESM) for z/OS, introduced in 1976. Renamed to z/OS Security Server (RACF), it provides authentication, authorization, and auditing for all z/OS resources. RACF is one of three mainframe ESMs (alongside Broadcom ACF2 and Top Secret), but is the IBM-supplied standard.

RACF is classified as **Core** (required infrastructure) on mainframes:
- Built into every z/OS system — controls access to ALL resources
- Authenticates users via passwords, password phrases, digital certificates, Kerberos, PassTickets, or MFA
- Authorizes access to datasets, programs, transactions, system commands, and general resources
- Provides 100+ IBM-supplied resource classes for different resource types
- SAF (System Authorization Facility) provides the standard interface between z/OS and ESMs
- RACROUTE macro is the programmatic interface (SVC) used by all z/OS subsystems
- Audit trail via SMF Type 80/81 records
- Digital certificate management via RACDCERT for TLS/AT-TLS
- Mandatory Access Control via Security Labels (SECLABEL)

Key documentation:
- **z/OS Security Server RACF Command Language Reference (SA23-2292)** — command syntax
- **z/OS Security Server RACF Security Administrator's Guide (SA23-2289)** — administration
- **z/OS Security Server RACF System Programmer's Guide (SA23-2287)** — configuration
- **z/OS Security Server RACROUTE Macro Reference (SA23-2294)** — programmatic interface

## Key Features & Capabilities

### 1. RACF Commands (~20 core commands)

#### User Management
| Command | Purpose |
|---------|---------|
| ADDUSER | Create user profile |
| ALTUSER | Modify user profile attributes |
| LISTUSER | Display user profile |
| DELUSER | Delete user profile |
| PASSWORD | Change password/phrase |

User attributes: SPECIAL, OPERATIONS, AUDITOR, REVOKE, CLAUTH, DEFAULT-GROUP, OWNER, PASSDATE, PASSINTERVAL, PASSWORD, PHRASE, DFLTGRP, RESTRICTED, OIDCARD, ADSP

#### Group Management
| Command | Purpose |
|---------|---------|
| ADDGROUP | Create group profile |
| ALTGROUP | Modify group profile |
| LISTGRP | Display group profile |
| DELGROUP | Delete group profile |
| CONNECT | Connect user to group |
| REMOVE | Remove user from group |

Group attributes: OWNER, SUPGROUP (superior group), TERMUACC, UNIVERSAL, MODEL, DATA
CONNECT attributes: AUTH(USE/CREATE/CONNECT/JOIN), GROUP-SPECIAL, GROUP-OPERATIONS, GROUP-AUDITOR

#### Dataset Profile Management
| Command | Purpose |
|---------|---------|
| ADDSD | Add dataset profile (discrete or generic) |
| ALTDSD | Alter dataset profile |
| LISTDSD | List dataset profile with access list |
| DELDSD | Delete dataset profile |

Profile types: Discrete (exact name) vs Generic (patterns with `*`, `**`, `%`)
HLQ model: High-level qualifier determines default profile ownership

#### General Resource Management
| Command | Purpose |
|---------|---------|
| RDEFINE | Define general resource profile in a class |
| RALTER | Alter general resource profile |
| RLIST | List general resource profile |
| RDELETE | Delete general resource profile |

#### Access Control
| Command | Purpose |
|---------|---------|
| PERMIT | Grant/revoke access to a resource |
| SEARCH | Search RACF database for profiles |

PERMIT syntax: `PERMIT resource CLASS(class) ID(user/group) ACCESS(level)`

#### System Administration
| Command | Purpose |
|---------|---------|
| SETROPTS | Set system-wide RACF options |
| RVARY | Activate/deactivate RACF, switch databases |
| RACDCERT | Manage digital certificates and keyrings |

### 2. Access Levels (5 levels)

| Level | Value | Description |
|-------|-------|-------------|
| NONE | 0 | No access |
| READ | 2 | Read-only access |
| UPDATE | 4 | Read and write access |
| CONTROL | 8 | Full control (e.g., VSAM control interval access) |
| ALTER | 128 | Full authority including delete and administration |

### 3. RACF Resource Classes (~100+ IBM-supplied)

#### Core Classes
| Class | Purpose |
|-------|---------|
| DATASET | Dataset access control |
| FACILITY | General-purpose system functions (IRR.*, BPX.*, STGADMIN.*) |
| PROGRAM | Program/load module execution control |
| OPERCMDS | z/OS and JES operator commands |
| STARTED | Started procedure identities |
| SURROGAT | Surrogate job submission authority |

#### JES Classes
| Class | Purpose |
|-------|---------|
| JESSPOOL | JES spool dataset access |
| JESJOBS | Job submission/cancellation by job name |
| JESINPUT | JES input device control |
| WRITER | JES writer access |

#### CICS Classes
| Class | Purpose |
|-------|---------|
| CICSTRN / GCICSTRN | CICS transaction access |
| TCICSTRN | CICS transaction attachment facility |

#### DB2 Classes
| Class | Purpose |
|-------|---------|
| DSNR | DB2 subsystem access |

#### MQ Classes
| Class | Purpose |
|-------|---------|
| MQCONN | MQ connection authority |
| MQADMIN | MQ administration authority |
| MQQUEUE | MQ queue access |
| MQPROC | MQ process access |

#### Cryptography (ICSF)
| Class | Purpose |
|-------|---------|
| CSFKEYS / GCSFKEYS | Cryptographic key access |
| CSFSERV | Cryptographic service access |

#### Networking / Security
| Class | Purpose |
|-------|---------|
| SERVAUTH | Server authentication |
| APPL | Application access control |
| TERMINAL | Terminal access control |
| CONSOLE | Console access control |
| NODES | NJE node control |
| SECLABEL | Security label usage |
| SECLEVEL | Security classification levels |

#### z/OS UNIX (USS)
| Class | Purpose |
|-------|---------|
| UNIXPRIV | UNIX privilege granularity |
| FSSEC | zFS/TFS file system security |

#### Certificate / Authentication
| Class | Purpose |
|-------|---------|
| DIGTCERT | Digital certificate management |
| DIGTRING | Digital certificate keyring management |
| RDATALIB | Granular keyring access control |
| PTKTDATA / PTKTVAL | PassTicket management |

#### Other Important Classes
| Class | Purpose |
|-------|---------|
| CDT | Class Descriptor Table (custom class definitions) |
| GLOBAL | Global access checking table |
| RACFVARS | RACF variables (profile name substitution) |
| LOGSTRM | Log stream access |
| DASDVOL | DASD volume access |
| TAPEVOL | Tape volume access |
| TEMPDSN | Temporary dataset control |
| TSOAUTH | TSO authority |
| TSOPROC | TSO logon procedure |
| SDSF | SDSF command access |

### 4. SETROPTS (System Options, ~30+ options)

| Option | Purpose |
|--------|---------|
| CLASSACT(class) | Activate a resource class |
| GENCMD(class) | Enable generic command processing for class |
| GENERIC(class) | Enable generic profile checking for class |
| GENLIST(class) | Enable in-storage generic profile lists |
| GLOBAL(class) | Enable global access checking for class |
| GRPLIST | Enable group list checking |
| ERASE(class) | Erase DASD dataset content on delete |
| PROTECTALL | Protect all datasets (fail-closed) |
| AUDIT(class) | Enable auditing for class |
| SAUDIT(class) | Enable special audit for class |
| LIST | List RACF options |
| STATISTICS(class) | Enable statistics for class |
| RACLIST(class) | RACLIST processing (in-storage profiles) |
| REFRESH | Refresh in-storage profiles |
| PASSWORD | Password rules (sub-options below) |
| LOGOPTIONS | Logging options |
| MLS / MLACTIVE / MLQUIET | Mandatory Access Control settings |
| MLSTABLE / MLITSTABLE | Security label inheritance |
| CATDSNS | Catalog datasets tracking |
| INITSTATS | Initialization statistics |
| OPERAUDIT | Operator command auditing |
| SECLABELAUDIT | Security label auditing |
| SESSIONINTERVAL | Session timeout |
| KERBLVL | Kerberos level |
| JES(BATCHALLRACF) | JES batch security settings |

#### Password Sub-options
| Option | Purpose |
|--------|---------|
| MINCHANGE(n) | Minimum days between password changes |
| WARNING(n) | Days before expiration to warn user |
| REVOKE(n) | Revoke after n failed attempts |
| HISTORY(n) | Remember n previous passwords |
| INTERVAL(n) | Maximum password age in days |
| RULES(n) | Password syntax rules (length, char requirements) |
| ENQUEUE | Enqueue to serialize password changes |

### 5. RACROUTE Macro (Programmatic Interface)

| REQUEST Type | Purpose |
|--------------|---------|
| AUTH | Check access authorization to a resource |
| FASTAUTH | Fast authorization check (in-storage profiles) |
| VERIFY | Authenticate user (password/certificate validation) |
| VERIFYX | Extended verify (more options than VERIFY) |
| DEFINE | Define/modify a security profile |
| EXTRACT | Extract data from RACF profiles |
| LIST | Build in-storage profile list |
| AUDIT | Create audit record |
| STAT | Check RACF status and class activation |
| TOKENBLD | Build UTOKEN (user security token) |
| TOKENXTR | Extract information from UTOKEN |

#### SAF Return Codes
| RC | Meaning |
|----|---------|
| 0 | Requested function processed successfully |
| 4 | Requested function not processed (ESM not active or class not active) |
| 8 | Requested function processed but failed (access denied) |

### 6. RACDCERT (Digital Certificate Management, ~26 functions)

| Function | Purpose |
|----------|---------|
| ADD | Add external certificate to RACF database |
| GENCERT | Generate certificate (self-signed or CA-signed) |
| GENREQ | Generate Certificate Signing Request (CSR) |
| MAP | Map certificate to RACF user ID |
| LIST | List certificates |
| DELETE | Delete certificate |
| ADDRING | Create keyring |
| LISTRING | List keyring contents |
| DELRING | Delete keyring |
| CONNECT | Connect certificate to keyring |
| REMOVE | Remove certificate from keyring |
| LISTCHAIN | List certificate chain |
| CHECKCERT | Check certificate validity |
| REKEY | Rekey certificate |
| ROLLOVER | Roll over certificate |
| EXPORT | Export certificate |
| IMPORT | Import certificate |

Owner types: ID(userid) — user certificate, CERTAUTH — CA certificate, SITE — site certificate

FACILITY class permissions for RACDCERT:
- IRR.DIGTCERT.GENCERT
- IRR.DIGTCERT.GENREQ
- IRR.DIGTCERT.CONNECT
- IRR.DIGTCERT.LIST
- IRR.DIGTCERT.LISTRING
- IRR.DIGTCERT.ADD
- IRR.DIGTCERT.DELETE

### 7. Security Labels (Mandatory Access Control)

| Feature | Description |
|---------|-------------|
| SECLABEL class | Define security labels (combinations of level + categories) |
| Security Levels | Hierarchical: e.g., TOP SECRET > SECRET > CONFIDENTIAL > UNCLASSIFIED |
| Security Categories | Non-hierarchical: e.g., PAYROLL, PERSONNEL, FINANCIAL |
| MLACTIVE | Enforce MAC (deny access if label doesn't dominate) |
| MLQUIET | MAC without user notification of denial |
| MLS | MAC with notification |
| User SECLABEL | Assigned via ALTUSER SECLABEL(label) |
| Resource SECLABEL | Assigned via RALTER SECLABEL(label) |

### 8. Conditional Access (WHEN Clause)

```
PERMIT resource CLASS(class) ID(user) ACCESS(READ) WHEN(CONSOLE(name))
PERMIT resource CLASS(class) ID(user) ACCESS(READ) WHEN(TERMINAL(name))
PERMIT resource CLASS(class) ID(user) ACCESS(READ) WHEN(PROGRAM(name))
PERMIT resource CLASS(class) ID(user) ACCESS(READ) WHEN(JESINPUT)
PERMIT resource CLASS(class) ID(user) ACCESS(READ) WHEN(SERVAUTH(name))
PERMIT resource CLASS(class) ID(user) ACCESS(READ) WHEN(SYSID(name))
PERMIT resource CLASS(class) ID(user) ACCESS(READ) WHEN(APPCPORT(name))
```

### 9. SMF Audit Records

| SMF Type | Purpose |
|----------|---------|
| Type 80 | RACF processing events — authorization checks, profile changes |
| Type 81 | RACF initialization events |
| Type 83 | RACF database changes |

Type 80 subtypes cover: ADDUSER, ALTUSER, CONNECT, PERMIT, ADDSD, RDEFINE, SETROPTS, authorization successes/failures, certificate operations, password changes.

### 10. RACF Database and Utilities

| Component | Purpose |
|-----------|---------|
| Primary RACF Database | Active database — typically on a shared DASD |
| Backup RACF Database | Automatic backup (recommended) |
| RVARY | Switch active/backup databases |
| IRRUT100 | RACF database search utility |
| IRRUT200 | RACF database verify/repair utility |
| IRRUT400 | RACF database split/merge utility |
| IRRDBU00 | RACF database unload to flat file (for reporting) |
| ICHAUTAB | Authorization table (in-storage) |
| ICHRFR01 | RACF router module |
| IRR@PRIM / IRR@BACK | Primary/backup database DDnames |

### 11. RACF Exits and Customization

| Exit | Purpose |
|------|---------|
| ICHRTX00 | RACROUTE REQUEST=AUTH pre-processing exit |
| ICHRCX01 | SAF callable services exit |
| ICHRCX02 | RACROUTE REQUEST=VERIFY pre-processing exit |
| ICHPWX01 | Password quality exit |
| ICHPWX11 | New password phrase exit |
| IRREVX01 | Event notification exit |
| IRRPX001 | PassTicket exit |
| CDT | Class Descriptor Table — define custom resource classes |

## Current OpenMainframe Status

**No RACF implementation exists.** The codebase contains:
- Zero RACF commands, profiles, or resource classes
- Zero RACROUTE/SAF interface code
- Zero RACDCERT certificate management
- Placeholder CICS NOTAUTH (error code 70) response code — not functional authorization
- Dataset-level locking in `open-mainframe-dataset/src/locking.rs` — concurrency control only, not access control
- Secrets management in `open-mainframe-deploy/src/secrets.rs` — Kubernetes credentials, not RACF

Planning documents indicate RACF is deferred to v2.0+. The architecture notes "Transaction authorization checks (future: RACF integration)" in the CICS security section.

## Gap Details

| Feature | Official z/OS | OpenMainframe | Gap |
|---------|--------------|---------------|-----|
| User profiles (ADDUSER/ALTUSER/LISTUSER/DELUSER) | Full | None | **Missing** |
| Group profiles (ADDGROUP/ALTGROUP/CONNECT/REMOVE) | Full | None | **Missing** |
| User attributes (SPECIAL/OPERATIONS/AUDITOR/REVOKE) | Full | None | **Missing** |
| Dataset profiles (ADDSD/ALTDSD/LISTDSD/DELDSD) | Full | None | **Missing** |
| Generic vs discrete profiles (wildcard patterns) | Full | None | **Missing** |
| General resource profiles (RDEFINE/RALTER/RLIST/RDELETE) | Full | None | **Missing** |
| 100+ resource classes (DATASET, FACILITY, PROGRAM, etc.) | Full | None | **Missing** |
| PERMIT command (5 access levels) | Full | None | **Missing** |
| Conditional access (WHEN clause) | Full | None | **Missing** |
| SETROPTS system options (~30+ options) | Full | None | **Missing** |
| Password rules (MINCHANGE/REVOKE/HISTORY/RULES) | Full | None | **Missing** |
| RACROUTE macro interface (AUTH/VERIFY/FASTAUTH/etc.) | Full | None | **Missing** |
| SAF router interface | Full | None | **Missing** |
| RACDCERT certificate management (~26 functions) | Full | None | **Missing** |
| Keyrings (ADDRING/CONNECT/LISTRING) | Full | None | **Missing** |
| AT-TLS integration | Full | None | **Missing** |
| Security Labels / MAC (SECLABEL) | Full | None | **Missing** |
| SMF audit records (Type 80/81/83) | Full | None (no SMF) | **Missing** |
| RACF database (primary/backup, utilities) | Full | None | **Missing** |
| RACF exits (ICHRTX00, ICHRCX01, ICHPWX01, etc.) | Full | None | **Missing** |
| SEARCH command (database search) | Full | None | **Missing** |
| PassTickets (PTKTDATA/PTKTVAL) | Full | None | **Missing** |
| Kerberos authentication | Full | None | **Missing** |
| Multi-Factor Authentication (MFADEF class) | Full | None | **Missing** |
| Custom class definition (CDT) | Full | None | **Missing** |

## Proposed Epic Structure

### Epic S100: RACF Core Data Model
- **S100.1**: User profiles — user ID, attributes (SPECIAL/OPERATIONS/AUDITOR/REVOKE), password, default group, owner
- **S100.2**: Group profiles — group name, superior group (SUPGROUP), TERMUACC, UNIVERSAL
- **S100.3**: CONNECT — user-group membership with AUTH levels (USE/CREATE/CONNECT/JOIN), group authorities
- **S100.4**: RACF database — profile storage, primary/backup, serialization
- **S100.5**: Profile search — SEARCH command, wildcard matching
- **Complexity**: M (Medium)

### Epic S101: Dataset Access Control
- **S101.1**: Dataset profiles — ADDSD/ALTDSD/LISTDSD/DELDSD
- **S101.2**: Generic profiles — wildcard patterns (`*`, `**`, `%`)
- **S101.3**: HLQ model — high-level qualifier default ownership
- **S101.4**: Access levels — NONE/READ/UPDATE/CONTROL/ALTER
- **S101.5**: Access list — PERMIT entries per profile
- **S101.6**: Integration with dataset crate — enforce access during OPEN/READ/WRITE
- **Complexity**: M (Medium)

### Epic S102: General Resource Framework
- **S102.1**: Resource class infrastructure — Class Descriptor Table (CDT)
- **S102.2**: RDEFINE/RALTER/RLIST/RDELETE commands
- **S102.3**: PERMIT for general resources — CLASS(class) ACCESS(level)
- **S102.4**: Conditional access — WHEN clause (PROGRAM, TERMINAL, CONSOLE, JESINPUT, etc.)
- **S102.5**: IBM-supplied classes — FACILITY, PROGRAM, OPERCMDS, STARTED, SURROGAT
- **S102.6**: CICS classes — CICSTRN, TCICSTRN (integrate with CICS crate)
- **S102.7**: JES classes — JESSPOOL, JESJOBS, JESINPUT, WRITER
- **S102.8**: Custom class definition via CDT
- **Complexity**: L (Large — 100+ classes, each with unique semantics)

### Epic S103: SETROPTS System Administration
- **S103.1**: Class activation — CLASSACT, GENCMD, GENERIC, GENLIST, GLOBAL, RACLIST
- **S103.2**: Password policy — MINCHANGE, WARNING, REVOKE, HISTORY, INTERVAL, RULES
- **S103.3**: Audit options — AUDIT, SAUDIT, LOGOPTIONS, OPERAUDIT
- **S103.4**: System protection — PROTECTALL, ERASE, GRPLIST
- **S103.5**: MAC settings — MLS, MLACTIVE, MLQUIET, MLSTABLE
- **S103.6**: REFRESH/RVARY — profile refresh and database switching
- **Complexity**: M (Medium)

### Epic S104: SAF / RACROUTE Interface
- **S104.1**: SAF router — dispatch authorization requests to RACF
- **S104.2**: RACROUTE REQUEST=AUTH — resource authorization check
- **S104.3**: RACROUTE REQUEST=FASTAUTH — in-storage fast authorization
- **S104.4**: RACROUTE REQUEST=VERIFY — user authentication
- **S104.5**: RACROUTE REQUEST=EXTRACT — profile data extraction
- **S104.6**: RACROUTE REQUEST=DEFINE — profile creation via API
- **S104.7**: RACROUTE REQUEST=LIST — in-storage profile building
- **S104.8**: RACROUTE REQUEST=STAT — RACF status query
- **S104.9**: SAF return codes (0/4/8) and RACF return/reason codes
- **Complexity**: L (Large — critical interface used by ALL subsystems)

### Epic S105: Authentication
- **S105.1**: Password authentication — password validation, change, expiration
- **S105.2**: Password phrase support — extended passphrases
- **S105.3**: PassTicket authentication — PTKTDATA/PTKTVAL classes
- **S105.4**: Certificate-based authentication — RACDCERT MAP
- **S105.5**: Kerberos (basic support)
- **S105.6**: Multi-Factor Authentication (MFADEF class)
- **Complexity**: M (Medium)

### Epic S106: RACDCERT Certificate Management
- **S106.1**: GENCERT — generate self-signed and CA-signed certificates
- **S106.2**: GENREQ — generate CSR
- **S106.3**: ADD/DELETE — add/remove external certificates
- **S106.4**: Keyring management — ADDRING/DELRING/LISTRING
- **S106.5**: CONNECT/REMOVE — associate certificates with keyrings
- **S106.6**: MAP — certificate-to-userid mapping
- **S106.7**: LIST/LISTCHAIN — certificate listing
- **S106.8**: FACILITY class profiles for RACDCERT (IRR.DIGTCERT.*)
- **Complexity**: L (Large — full PKI operations)

### Epic S107: Security Labels (MAC)
- **S107.1**: Security levels — hierarchical classification
- **S107.2**: Security categories — non-hierarchical compartments
- **S107.3**: SECLABEL class — define label combinations
- **S107.4**: Label enforcement — user label dominance checking
- **S107.5**: MLACTIVE/MLQUIET/MLS enforcement modes
- **Complexity**: S (Small — straightforward dominance model)

### Epic S108: Audit & SMF Integration
- **S108.1**: SMF Type 80 records — RACF event recording
- **S108.2**: SMF Type 81 records — RACF initialization events
- **S108.3**: SMF Type 83 records — database change tracking
- **S108.4**: Audit settings — per-profile, per-class, and global auditing
- **S108.5**: IRRDBU00 — database unload for reporting
- **Complexity**: M (Medium — depends on SMF implementation, Batch 14)

### Epic S109: RACF Exits & Customization
- **S109.1**: ICHRTX00 — pre-authorization exit
- **S109.2**: ICHRCX01/02 — SAF callable services exits
- **S109.3**: ICHPWX01/11 — password quality exits
- **S109.4**: IRREVX01 — event notification exit
- **S109.5**: Custom CDT entries — installation-defined resource classes
- **Complexity**: M (Medium)

### Epic S110: RACF Database Utilities
- **S110.1**: IRRUT100 — database search utility
- **S110.2**: IRRUT200 — database verify/repair
- **S110.3**: IRRUT400 — database split/merge
- **S110.4**: IRRDBU00 — database unload to flat file
- **S110.5**: Backup/recovery — primary/backup database management
- **Complexity**: M (Medium)

## Dependencies

| Dependency | Crate / Component | Reason |
|------------|-------------------|--------|
| open-mainframe-dataset | Dataset access | Enforce DATASET class profiles during file operations |
| open-mainframe-cics | CICS security | Enforce CICSTRN/TCICSTRN classes for transactions |
| open-mainframe-db2 | DB2 security | Enforce DSNR class for DB2 connections |
| open-mainframe-jcl | Batch security | Enforce JESSPOOL/JESJOBS/SURROGAT for job execution |
| open-mainframe-tui | Terminal security | Enforce TERMINAL class for 3270 sessions |
| SMF (not implemented, Batch 14) | Audit records | SMF Type 80/81/83 recording |
| TSO (not implemented, Batch 9) | Command interface | RACF commands issued from TSO |
| JES2 (not implemented, Batch 11) | Spool security | JESSPOOL/JESJOBS enforcement |

**Critical dependency**: RACF is foundational infrastructure. Nearly every other z/OS subsystem depends on RACF for authorization. The SAF/RACROUTE interface (S104) is the most critical epic — all subsystems call RACROUTE to check authorization. However, RACF can be implemented incrementally: start with SAF interface + user/dataset profiles, then add resource classes as subsystems need them.

## Complexity Estimate

| Epic | Complexity | Rationale |
|------|-----------|-----------|
| S100 (Core Data Model) | M | User/group profiles, RACF database |
| S101 (Dataset Access) | M | Generic profiles, pattern matching, access lists |
| S102 (General Resources) | L | 100+ resource classes with unique semantics |
| S103 (SETROPTS) | M | System-wide option management |
| S104 (SAF/RACROUTE) | L | Core interface — used by ALL subsystems |
| S105 (Authentication) | M | Multiple auth methods |
| S106 (RACDCERT/Certs) | L | Full PKI certificate lifecycle |
| S107 (Security Labels) | S | Hierarchical dominance model |
| S108 (Audit/SMF) | M | SMF record generation |
| S109 (Exits) | M | Customization hooks |
| S110 (Database Utilities) | M | Admin tools |

**Total estimated effort**: 11 epics, overall XL (Extra Large) complexity. RACF is the most pervasive z/OS subsystem — every other component depends on it for authorization. The 100+ resource classes each have unique enforcement semantics tied to specific subsystems.

**Recommendation**: Implement RACF in stages:
1. **Phase A (Foundation)**: S100 (core model) + S101 (dataset access) + S104 (SAF/RACROUTE) — enables basic access control
2. **Phase B (Integration)**: S102 (resource classes) — add classes as subsystems need them
3. **Phase C (Advanced)**: S105 (auth), S106 (certs), S107 (MAC), S108 (audit)
4. **Phase D (Admin)**: S103 (SETROPTS), S109 (exits), S110 (utilities)

## Reference Documentation

- [IBM z/OS 3.1 RACF Command Language Reference (SA23-2292, PDF)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/icha400_v3r1.pdf)
- [IBM z/OS 3.1 RACF Security Administrator's Guide (SA23-2289, PDF)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/icha700_v3r1.pdf)
- [IBM z/OS 3.1 RACF System Programmer's Guide (SA23-2287, PDF)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/icha200_v3r1.pdf)
- [IBM z/OS 3.1 RACROUTE Macro Reference (SA23-2294, PDF)](https://www.ibm.com/docs/en/SSLTBW_3.1.0/pdf/ichc600_v3r1.pdf)
- [IBM z/OS RACF Resource Classes Reference](https://www.ibm.com/docs/en/zos/2.4.0?topic=configuration-racf-resource-classes)
- [RACDCERT — Manage RACF Digital Certificates](https://www.ibm.com/docs/en/zos/2.4.0?topic=syntax-racdcert-manage-racf-digital-certificates)
- [IBM RACF Product Page](https://www.ibm.com/products/resource-access-control-facility)
- [RACF — Wikipedia](https://en.wikipedia.org/wiki/Resource_Access_Control_Facility)
- [Broadcom — IBM-Supplied Resource Classes Reference](https://techdocs.broadcom.com/us/en/ca-mainframe-software/security/ca-acf2-for-z-os/16-0/administrating/ibm-supplied-resource-classes.html)
- [IBM RACF Security Target (Common Criteria)](https://www.commoncriteriaportal.org/files/epfiles/st_racfv2r3_v5.5_public.pdf)
- [RACDCERT Survival Guide (Nigel Pentland, PDF)](https://nigelpentland.co.uk/pdfs/racdcert.pdf)
- [IBM/IBM-Z-zOS RACF Downloads — GitHub](https://github.com/IBM/IBM-Z-zOS/blob/main/zOS-RACF/Downloads/readme.md)
