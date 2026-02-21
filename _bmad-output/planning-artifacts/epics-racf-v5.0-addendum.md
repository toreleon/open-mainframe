---
version: 'v5.0-addendum'
planningGroup: 'PG-2'
technology: 'RACF Extensions'
date: '2026-02-21'
status: 'complete'
parentDocument: 'epics-racf-v4.0.md'
inputDocuments:
  - 'epics-racf-v4.0.md'
  - 'zos-complete-inventory.md (AREA-6)'
  - 'gap-analysis/batch-08-racf.md'
newEpics: 2
newStories: 14
---

# RACF Extensions Addendum (v5.0)

This addendum extends the existing RACF v4.0 planning with two new epics (SYS-109 and SYS-110) identified during the z/OS complete inventory research. The v4.0 epics (S100-S106) provide foundation coverage; this addendum adds deeper resource class support and enhanced PassTicket capabilities.

## Relationship to Existing Epics

| Existing v4.0 | New SYS Epic | Relationship |
|---------------|-------------|-------------|
| S102 (General Resource Framework) | SYS-109 | SYS-109 extends S102 with CDT framework and specific class implementations |
| S105.2 (PassTicket Authentication) | SYS-110 | SYS-110 extends S105.2 with full PassTicket management and digital certificate enhancements |

**Dependency:** SYS-109 and SYS-110 both depend on S100-S104 (core RACF) being complete.

---

## SYS-109: RACF General Resource Class Framework

**Description:** Implement the Class Descriptor Table (CDT) infrastructure and specific IBM-defined resource classes critical to z/OS subsystem integration. While S102 provides the RDEFINE/RALTER command framework, SYS-109 adds the class-specific behavior, grouping classes, and conditional access checking that makes RACF classes work correctly for each subsystem.

**User Value:** z/OS subsystems (CICS, DB2, JES2, MQ, USS, console commands) can perform authorization checks using their specific RACF resource classes, enabling realistic multi-user access control.

**Size:** L | **Stories:** 7

### SYS-109.1: Class Descriptor Table (CDT) Infrastructure

**As a** security administrator, **I want** a Class Descriptor Table that defines the properties of each resource class, **so that** RACF knows how to process profiles for each class.

**Acceptance Criteria:**
- Given a CDT entry for FACILITY class, when the class is referenced in RDEFINE, then CDT properties (POSIT number, default UACC, RACLIST eligibility, max name length, FIRST/OTHER/MEMBER naming rules) are applied
- Given `RDEFINE CDT MYCLASS POSIT(200) MAXLNTH(246)`, when executed, then a custom class is defined in the CDT
- Given CDT entries for IBM-supplied classes, when the system initializes, then all standard classes are available with correct properties
- Given `SETROPTS CLASSACT(classname) RACLIST(classname)`, when executed, then only classes defined in the CDT can be activated

**FR Coverage:** Inventory Gap #1 (General Resource Class Framework), CDT component

### SYS-109.2: FACILITY Class Implementation

**As a** security administrator, **I want** the FACILITY class to protect general system functions, **so that** subsystems can control access to privileged operations.

**Acceptance Criteria:**
- Given `RDEFINE FACILITY IRR.RADMIN.* UACC(NONE)`, when a user without READ access calls RACF admin functions, then access is denied
- Given `RDEFINE FACILITY BPX.SUPERUSER UACC(NONE)` and `PERMIT BPX.SUPERUSER CLASS(FACILITY) ID(SYSADM) ACCESS(READ)`, when SYSADM executes a USS privileged operation, then it succeeds
- Given `RDEFINE FACILITY IRR.PWRESET.* UACC(NONE)`, when password reset is attempted by unpermitted user, then it is denied
- Given profile names using wildcard patterns in FACILITY class, when authorization checks are performed, then generic profile matching works correctly

### SYS-109.3: OPERCMDS and STARTED Classes

**As a** security administrator, **I want** OPERCMDS and STARTED classes to control operator command authorization and started task identity, **so that** console commands and started procedures have proper access control.

**Acceptance Criteria:**
- Given `RDEFINE OPERCMDS MVS.DISPLAY.* UACC(NONE)` and `PERMIT MVS.DISPLAY.* CLASS(OPERCMDS) ID(OPER1) ACCESS(CONTROL)`, when OPER1 issues DISPLAY command, then it succeeds; when unpermitted user issues it, access denied
- Given `RDEFINE STARTED JES2.* STDATA(USER(JES2) GROUP(STC) TRUSTED(YES))`, when JES2 started task begins, then it runs under userid JES2 with trusted attribute
- Given operator command profiles follow IBM naming: MVS.verb.resource (e.g., MVS.VARY.TCPIP, MVS.CANCEL.JOB), when authorization checks are performed, then the OPERCMDS profile name format is enforced
- Given `SETROPTS CLASSACT(OPERCMDS STARTED) RACLIST(OPERCMDS STARTED)`, when classes are activated, then authorization checks use in-storage profiles

### SYS-109.4: SURROGAT and JESSPOOL Classes

**As a** security administrator, **I want** SURROGAT class to control surrogate job submission and JESSPOOL to control spool output access, **so that** job security is properly enforced.

**Acceptance Criteria:**
- Given `RDEFINE SURROGAT JSMITH.SUBMIT UACC(NONE)` and `PERMIT JSMITH.SUBMIT CLASS(SURROGAT) ID(BATCHUSR) ACCESS(READ)`, when BATCHUSR submits a job as JSMITH (USER= on JOB card), then it succeeds
- Given SURROGAT profile does not exist for the target user, when surrogate submission is attempted, then it is denied with ICH408I message
- Given `RDEFINE JESSPOOL JSMITH.*.*.** UACC(NONE)`, when a user attempts to view JSMITH's spool output, then access is checked against the JESSPOOL profile
- Given spool file naming convention userid.jobname.jobid.dsid, when JESSPOOL profiles are matched, then the 4-qualifier naming format is enforced

### SYS-109.5: UNIXPRIV and PROGRAM Classes

**As a** security administrator, **I want** UNIXPRIV class to control USS privileged operations and PROGRAM class for program control, **so that** USS and load module security works correctly.

**Acceptance Criteria:**
- Given `RDEFINE UNIXPRIV SUPERUSER.FILESYS UACC(NONE)`, when a USS user without SUPERUSER.FILESYS access attempts chown, then the operation is denied
- Given UNIXPRIV profiles: SUPERUSER.FILESYS, SUPERUSER.PROCESS, CHOWN.UNRESTRICTED, when corresponding USS operations are performed, then RACF checks the appropriate profile
- Given `RDEFINE PROGRAM MYPROG ADDMEM('LOAD.LIB'//NOPADCHK)`, when MYPROG is loaded from LOAD.LIB, then program control checking validates the library
- Given `SETROPTS WHEN(PROGRAM)`, when program control is activated, then only programs in controlled libraries execute with the calling user's authority

### SYS-109.6: Grouping Classes and Member Classes

**As a** security administrator, **I want** grouping classes (GFACILITY, GTERMINL, etc.) to provide member-level profiles, **so that** I can create hierarchical resource protection.

**Acceptance Criteria:**
- Given a member class FACILITY and its grouping class GFACILITY, when a profile is defined in GFACILITY with members, then each member inherits the access list of the grouping profile
- Given CDT specifies POSIT, MEMBER, and GROUP relationships between classes, when the class pair is activated, then member profiles are resolved through the grouping class
- Given `RDEFINE GFACILITY MYGROUP ADDMEM(RESOURCE1 RESOURCE2)`, when access to RESOURCE1 is checked, then the GFACILITY access list applies

### SYS-109.7: Integration Tests — Cross-Subsystem Authorization

**As a** developer, **I want** integration tests proving resource classes work with subsystem authorization, **so that** CICS, JES2, MQ, and USS security is validated.

**Acceptance Criteria:**
- Given CICS transaction security enabled, when CICSTRN profiles are defined, then CICS transaction authorization uses RACF general resource checking
- Given JES2 job submission, when SURROGAT and JESJOBS profiles are checked, then job-level security works end-to-end
- Given USS file access, when UNIXPRIV is active, then privileged operations respect RACF authorization
- Given `cargo test -p open-mainframe-racf`, then all SYS-109 tests pass

---

## SYS-110: RACF PassTickets and Digital Certificates

**Description:** Implement full PassTicket lifecycle (PTKTDATA profiles, generation, validation, replay protection) and enhance certificate management beyond S106 with automated renewal, OCSP stapling, and certificate chain validation.

**User Value:** Middleware (z/OSMF, CICS, DB2) can authenticate programmatically using PassTickets without storing passwords, and TLS connections use properly managed certificate chains.

**Size:** M | **Stories:** 7

### SYS-110.1: PTKTDATA Profile Management

**As a** security administrator, **I want** to define PTKTDATA profiles specifying which applications can use PassTickets, **so that** PassTicket generation is controlled per application.

**Acceptance Criteria:**
- Given `RDEFINE PTKTDATA CICSAPP1 SSIGNON(KEYMASKED(E001193519561977))`, when the profile is created, then the masked key is stored for PassTicket generation
- Given `RDEFINE PTKTDATA ZOSMF SSIGNON(KEYMASKED(key))`, when defined, then z/OSMF can generate/validate PassTickets
- Given `RLIST PTKTDATA CICSAPP1 ALL`, when executed, then the profile shows application name, key type, and timeout settings
- Given no PTKTDATA profile for an application, when PassTicket generation is attempted, then it fails with "PTKTDATA profile not found"

### SYS-110.2: PassTicket Generation

**As an** application developer, **I want** to generate PassTickets for a userid and application, **so that** I can authenticate to the application without transmitting the real password.

**Acceptance Criteria:**
- Given a valid PTKTDATA profile for application CICSAPP1 and userid JSMITH, when `generate_passticket("JSMITH", "CICSAPP1")` is called, then an 8-character PassTicket is returned
- Given the same userid and application, when two PassTickets are generated in the same second, then they are different (replay protection token ensures uniqueness)
- Given the generation algorithm, when a PassTicket is generated, then it uses the PTKTDATA key, current time (10-minute granularity), and userid as inputs
- Given a PassTicket, when it is used after 10 minutes, then validation fails (expired)

### SYS-110.3: PassTicket Validation and Replay Protection

**As a** security administrator, **I want** PassTicket validation with replay detection, **so that** each PassTicket can only be used once.

**Acceptance Criteria:**
- Given a valid PassTicket for JSMITH/CICSAPP1, when `validate_passticket("JSMITH", "CICSAPP1", ticket)` is called, then it returns success
- Given a previously validated PassTicket, when the same ticket is presented again, then it is rejected (replay detected)
- Given the replay cache, when it stores validated tickets, then entries older than 10 minutes are purged
- Given an invalid PassTicket (wrong application or expired), when validation is attempted, then it fails with appropriate reason code

### SYS-110.4: PassTicket Integration with RACROUTE

**As a** subsystem developer, **I want** PassTickets to work transparently through RACROUTE VERIFY, **so that** existing authentication flows support PassTickets without code changes.

**Acceptance Criteria:**
- Given `RACROUTE REQUEST=VERIFY,PASSWRD=passticket,USERID=JSMITH,APPL='CICSAPP1'`, when processed, then RACF detects the password is a PassTicket (by checking PTKTDATA profiles) and validates accordingly
- Given a PassTicket presented as a regular password (no APPL parameter), when the PTKTDATA profile has NOEVAL specified, then RACF evaluates it as a PassTicket
- Given PassTicket validation failure, when `RACROUTE VERIFY` processes it, then the return code distinguishes "expired" from "replay" from "invalid key"

### SYS-110.5: Digital Certificate Chain Validation

**As a** security administrator, **I want** certificate chain validation for TLS connections, **so that** only properly signed certificates are accepted.

**Acceptance Criteria:**
- Given a server certificate signed by an intermediate CA, when the keyring contains the intermediate and root CA certificates, then chain validation succeeds
- Given a certificate with an expired intermediate CA, when chain validation runs, then it fails with "certificate expired in chain"
- Given a self-signed certificate, when chain validation is attempted, then it succeeds only if the self-signed cert is explicitly trusted in the keyring
- Given `RACDCERT CHECKCERT(LABEL('MyCert'))`, when executed, then the full chain is validated and results are displayed

### SYS-110.6: Certificate-to-Userid Mapping

**As a** security administrator, **I want** to map X.509 certificate attributes to RACF userids, **so that** TLS client authentication identifies the z/OS user.

**Acceptance Criteria:**
- Given `RACDCERT MAP ID(JSMITH) SDNFILTER('CN=John Smith.O=MyOrg') WITHLABEL('JSmithMap')`, when a TLS client presents a certificate matching that Subject DN, then the connection is authenticated as JSMITH
- Given multiple MAP entries, when a certificate matches more than one, then the most specific match wins (longest filter match)
- Given no MAP entry matches, when a certificate is presented, then authentication fails with "no matching certificate map"
- Given `RACDCERT LISTMAP ID(*)`, when executed, then all certificate mappings are displayed

### SYS-110.7: Integration Tests — Authentication Flows

**As a** developer, **I want** end-to-end tests for PassTicket and certificate authentication, **so that** these features work with z/OSMF and subsystem authentication.

**Acceptance Criteria:**
- Given a z/OSMF authentication request using PassTicket, when processed through the auth middleware, then the user is authenticated and a JWT is issued
- Given a TLS connection with client certificate, when the certificate is mapped to a userid, then RACF authentication succeeds
- Given PassTicket replay protection under concurrent requests, when 100 simultaneous PassTicket validations occur, then each ticket is validated exactly once
- Given `cargo test -p open-mainframe-racf`, then all SYS-110 tests pass

---

## Updated Dependency Graph (Including v4.0 Epics)

```
S100 (Core Data Model)
  │
  ├─► S101 (Dataset Access)
  ├─► S102 (General Resources) ──► SYS-109 (Class Framework Extension)
  ├─► S103 (SETROPTS)
  ├─► S104 (SAF/RACROUTE) ──► SYS-110 (PassTicket + Certs Extension)
  ├─► S105 (Authentication)
  └─► S106 (RACDCERT)
```

## Coverage Notes

**Inventory components now covered:**
- General resource profiles (RDEFINE/RALTER) → S102 + SYS-109
- FACILITY class → SYS-109.2
- OPERCMDS class → SYS-109.3
- SURROGAT class → SYS-109.4
- STARTED class → SYS-109.3
- UNIXPRIV class → SYS-109.5
- PROGRAM class → SYS-109.5
- PTKTDATA class → SYS-110.1
- PassTickets → SYS-110.2, SYS-110.3, SYS-110.4
- Key rings → S106 (v4.0)
- Class Descriptor Table → SYS-109.1
- JESSPOOL class → SYS-109.4
- Grouping classes → SYS-109.6

**Remaining deferred (low priority):**
- SECLABEL class (MLS) — niche security feature
- CSFKEYS/CSFSERV classes — depends on ICSF implementation (PG-23)
- IRRUT100/IRRUT200/IRRUT400 utilities — operational tools
- DSMON report — monitoring tool
