# RACF Crate — Epics & Stories

## Epic S100: RACF Core Data Model

**Goal:** Implement the RACF database with user profiles, group profiles, and membership management.

**Crate:** `open-mainframe-racf`
**FRs:** FR-v4.0-S100

### Story S100.1: User Profile Management

As a **security administrator**,
I want **ADDUSER/ALTUSER/LISTUSER/DELUSER commands to manage user profiles**,
So that **I can create and maintain user accounts with attributes**.

**Acceptance Criteria:**

**Given** `ADDUSER JSMITH DFLTGRP(DEPT01) NAME('John Smith') OWNER(ADMIN1)`
**When** executed
**Then** a user profile is created with default group DEPT01 and owner ADMIN1

**Given** `ALTUSER JSMITH SPECIAL OPERATIONS`
**When** executed
**Then** JSMITH receives SPECIAL (full RACF admin) and OPERATIONS (universal access) attributes

**Given** `LISTUSER JSMITH`
**When** executed
**Then** all profile fields are displayed: name, default group, attributes, password info, connect groups

**Given** `DELUSER JSMITH`
**When** executed
**Then** the user profile and all connect entries are removed

Attributes: SPECIAL, OPERATIONS, AUDITOR, REVOKE, GRPACC, ADSP, RESTRICTED, CLAUTH(class)

**Complexity:** M

### Story S100.2: Group Profile Management

As a **security administrator**,
I want **ADDGROUP/ALTGROUP/LISTGRP/DELGROUP commands**,
So that **I can organize users into groups with a hierarchical structure**.

**Acceptance Criteria:**

**Given** `ADDGROUP DEPT01 SUPGROUP(SYS1) OWNER(ADMIN1)`
**When** executed
**Then** group DEPT01 is created under superior group SYS1

**Given** `CONNECT JSMITH GROUP(DEPT01) AUTH(USE)`
**When** executed
**Then** JSMITH is connected to DEPT01 with USE authority

**Given** `REMOVE JSMITH GROUP(DEPT01)`
**When** executed
**Then** JSMITH's connection to DEPT01 is removed

Connect AUTH levels: USE, CREATE, CONNECT, JOIN

**Complexity:** M

### Story S100.3: RACF Database Storage and Search

As a **security administrator**,
I want **a persistent RACF database with search capability**,
So that **profiles survive restarts and can be queried**.

**Acceptance Criteria:**

**Given** `SEARCH CLASS(USER) MASK(J*)`
**When** executed
**Then** all user profiles matching J* are listed

**Given** profiles have been created and the system restarts
**When** the system comes back up
**Then** all profiles are restored from persistent storage

**Complexity:** M

---

## Epic S101: Dataset Access Control

**Goal:** Implement RACF dataset profiles with generic patterns and access levels.

**Crate:** `open-mainframe-racf`
**FRs:** FR-v4.0-S101

### Story S101.1: Dataset Profile Commands

As a **security administrator**,
I want **ADDSD/ALTDSD/LISTDSD/DELDSD commands**,
So that **I can protect datasets with RACF profiles**.

**Acceptance Criteria:**

**Given** `ADDSD 'PROD.PAYROLL.**' UACC(NONE) OWNER(ADMIN1)`
**When** executed
**Then** a generic dataset profile is created protecting all datasets matching PROD.PAYROLL.**

**Given** `PERMIT 'PROD.PAYROLL.**' ID(JSMITH) ACCESS(READ)`
**When** executed
**Then** JSMITH is added to the access list with READ access

**Given** a user attempts to open `PROD.PAYROLL.DATA` for READ
**When** RACF authorization check runs
**Then** the matching generic profile is found and access is granted if the user has READ or higher

Access levels: NONE(0), READ(2), UPDATE(4), CONTROL(8), ALTER(12)

**Complexity:** M

### Story S101.2: Generic Profile Matching and HLQ Model

As a **security administrator**,
I want **wildcard patterns in dataset profiles and automatic HLQ-based protection**,
So that **I can protect groups of datasets efficiently**.

**Acceptance Criteria:**

**Given** profiles `SYS1.**` and `SYS1.PARMLIB.*`
**When** access to `SYS1.PARMLIB.MEMBER1` is checked
**Then** `SYS1.PARMLIB.*` matches (most specific generic profile wins)

**Given** PROTECTALL is active and no profile exists for `JSMITH.NEW.DATA`
**When** JSMITH creates the dataset
**Then** a discrete profile is auto-created with JSMITH as owner (HLQ model)

Wildcards: `*` (single qualifier), `**` (any number of qualifiers), `%` (single character)

**Complexity:** L

---

## Epic S102: General Resource Framework

**Goal:** Implement the RACF general resource class infrastructure supporting 100+ IBM-defined classes.

**Crate:** `open-mainframe-racf`
**FRs:** FR-v4.0-S102

### Story S102.1: General Resource Commands

As a **security administrator**,
I want **RDEFINE/RALTER/RLIST/RDELETE/PERMIT commands for general resources**,
So that **I can protect any resource type beyond datasets**.

**Acceptance Criteria:**

**Given** `RDEFINE FACILITY IRR.RADMIN.* UACC(NONE)`
**When** executed
**Then** a general resource profile in the FACILITY class is created

**Given** `PERMIT IRR.RADMIN.LISTUSER CLASS(FACILITY) ID(SECADM) ACCESS(READ)`
**When** executed
**Then** SECADM can use LISTUSER (RACF admin function protected by FACILITY class)

**Complexity:** L

### Story S102.2: IBM-Supplied Classes and Conditional Access

As a **security administrator**,
I want **support for key IBM resource classes and WHEN conditional access**,
So that **I can implement fine-grained access control**.

**Acceptance Criteria:**

**Given** `SETROPTS CLASSACT(FACILITY) RACLIST(FACILITY)`
**When** executed
**Then** the FACILITY class is activated and in-storage profiles are built

**Given** `PERMIT MYPROG CLASS(PROGRAM) ID(JSMITH) ACCESS(READ) WHEN(TERMINAL(T3270*))`
**When** JSMITH runs MYPROG from terminal T3270A
**Then** access is granted (WHEN condition matches)

Key classes: DATASET, FACILITY, PROGRAM, OPERCMDS, STARTED, SURROGAT, JESSPOOL, JESJOBS, CICSTRN, TCICSTRN, MQQUEUE, MQCMDS, UNIXPRIV, CSFSERV

**Complexity:** L

---

## Epic S103: SETROPTS System Administration

**Goal:** Implement RACF system-wide options controlling class activation, password policy, and audit settings.

**Crate:** `open-mainframe-racf`
**FRs:** FR-v4.0-S103

### Story S103.1: Class Activation and Password Policy

As a **security administrator**,
I want **SETROPTS to activate classes and enforce password rules**,
So that **I can control system-wide security behavior**.

**Acceptance Criteria:**

**Given** `SETROPTS CLASSACT(FACILITY PROGRAM) RACLIST(FACILITY PROGRAM)`
**When** executed
**Then** both classes are activated and profiles are loaded into memory for fast checking

**Given** `SETROPTS PASSWORD(MINCHANGE(1) WARNING(14) REVOKE(5) HISTORY(32) INTERVAL(90))`
**When** a user attempts to change password before MINCHANGE days
**Then** the change is rejected

**Given** a user enters the wrong password 5 times
**When** REVOKE(5) is active
**Then** the user profile is revoked

**Complexity:** M

---

## Epic S104: SAF / RACROUTE Interface

**Goal:** Implement the System Authorization Facility router and RACROUTE macro interface used by all z/OS subsystems.

**Crate:** `open-mainframe-racf`
**FRs:** FR-v4.0-S104

### Story S104.1: SAF Router and RACROUTE REQUEST=AUTH

As a **subsystem developer**,
I want **the SAF router to dispatch authorization requests to RACF**,
So that **CICS, DB2, JES2, and other subsystems can check access via a standard API**.

**Acceptance Criteria:**

**Given** a CICS transaction issues RACROUTE REQUEST=AUTH for resource TRXN01 CLASS(CICSTRN)
**When** SAF routes the request to RACF
**Then** RACF checks the profile, access list, and UACC; returns RC=0 (authorized) or RC=8 (denied)

**Given** RACROUTE REQUEST=FASTAUTH for an in-storage profile
**When** processed
**Then** authorization is checked against the RACLIST'd in-storage profiles (no I/O)

SAF return codes: 0 (request successful), 4 (RACF not active — use default), 8 (request failed)
RACF return codes: 0 (authorized), 8 (not authorized), 14 (no matching profile)

**Complexity:** L

### Story S104.2: RACROUTE REQUEST=VERIFY and EXTRACT

As a **subsystem developer**,
I want **user authentication via VERIFY and profile data extraction via EXTRACT**,
So that **subsystems can authenticate users and query profile data**.

**Acceptance Criteria:**

**Given** `RACROUTE REQUEST=VERIFY,PASSWRD=password,USERID=JSMITH`
**When** processed
**Then** RACF validates the password against the user profile; returns RC=0 if valid

**Given** `RACROUTE REQUEST=EXTRACT,TYPE=EXTRACT,CLASS='USER',ENTITY='JSMITH',FIELDS=NAME`
**When** processed
**Then** the user's NAME field value is returned

**Complexity:** L

---

## Epic S105: Authentication Services

**Goal:** Implement password, passphrase, PassTicket, and certificate-based authentication.

**Crate:** `open-mainframe-racf`
**FRs:** FR-v4.0-S105

### Story S105.1: Password and Passphrase Authentication

As a **z/OS user**,
I want **password and passphrase authentication with expiration and history**,
So that **I can securely log on to the system**.

**Acceptance Criteria:**

**Given** a user with password 'OLD123' and password interval 90 days
**When** 91 days have passed and the user logs on
**Then** the system prompts for a new password

**Given** HISTORY(32) is active and the user tries to reuse one of their last 32 passwords
**When** the password change is attempted
**Then** it is rejected with "password previously used"

**Complexity:** M

### Story S105.2: PassTicket and Certificate Authentication

As a **application developer**,
I want **PassTicket generation/validation and certificate-to-userid mapping**,
So that **I can implement SSO and certificate-based authentication**.

**Acceptance Criteria:**

**Given** a PTKTDATA profile for application CICSAPP1
**When** a PassTicket is generated for user JSMITH to application CICSAPP1
**Then** a one-time time-based ticket is produced that can be used instead of a password

**Given** a RACDCERT MAP profile connecting a certificate's Subject DN to userid JSMITH
**When** a TLS connection presents that certificate
**Then** the connection is authenticated as JSMITH

**Complexity:** M

---

## Epic S106: RACDCERT Certificate Management

**Goal:** Implement digital certificate and keyring management for TLS/SSL.

**Crate:** `open-mainframe-racf`
**FRs:** FR-v4.0-S106

### Story S106.1: Certificate and Keyring Operations

As a **security administrator**,
I want **RACDCERT commands to manage certificates and keyrings**,
So that **I can configure TLS for z/OS applications**.

**Acceptance Criteria:**

**Given** `RACDCERT GENCERT ID(WEBSRV) SUBJECTSDN(CN('myserver.example.com')) SIZE(2048) WITHLABEL('WebServerCert')`
**When** executed
**Then** a self-signed RSA certificate is generated and stored in WEBSRV's profile

**Given** `RACDCERT ADDRING(MyKeyring) ID(WEBSRV)`; then `RACDCERT CONNECT(ID(WEBSRV) LABEL('WebServerCert') RING(MyKeyring))`
**When** executed
**Then** the certificate is connected to the keyring for use by applications

**Given** `RACDCERT LIST ID(WEBSRV)`
**When** executed
**Then** all certificates owned by WEBSRV are displayed with label, subject, issuer, dates

Operations: GENCERT, GENREQ, ADD, DELETE, MAP, LIST, LISTCHAIN, CHECKCERT, ADDRING, DELRING, LISTRING, CONNECT, REMOVE, EXPORT, IMPORT, REKEY, ROLLOVER

**Complexity:** L

---
