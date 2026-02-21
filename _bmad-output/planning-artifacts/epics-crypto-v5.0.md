---
version: 'v5.0'
planningGroup: 'PG-23'
technology: 'Crypto, PKI & Advanced Security'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-crypto-v5.0.md'
  - 'architecture-crypto-v5.0.md'
totalEpics: 7
totalStories: 38
frCoverage: '12/12 (100%)'
nfrCoverage: '3/3 (100%)'
---

# Epics & Stories: Crypto, PKI & Advanced Security

## Epic Overview

| Epic | Title | Size | Stories | Phase | Crate |
|------|-------|------|---------|-------|-------|
| CRYPTO-100 | ICSF Symmetric & Hashing Services | M | 6 | D | open-mainframe-crypto |
| CRYPTO-101 | ICSF Asymmetric Key Operations | M | 5 | D | open-mainframe-crypto |
| CRYPTO-102 | Key Management & Key Stores | L | 6 | D | open-mainframe-crypto |
| CRYPTO-103 | CSFKEYS/CSFSERV Integration | S | 4 | D | open-mainframe-crypto + racf |
| SEC-107 | Security Labels (MAC) | S | 5 | D | open-mainframe-racf |
| SEC-108 | RACF Audit & SMF Integration | M | 6 | D | open-mainframe-racf |
| SEC-109 | RACF Exits, Utilities & Config | M | 6 | D | open-mainframe-racf |

---

## CRYPTO-100: ICSF Symmetric & Hashing Services

**User Value:** Applications can encrypt/decrypt data, compute hashes, and generate HMACs using z/OS-compatible ICSF callable services, enabling secure data handling.

### CRYPTO-100.1: Symmetric Encryption — CSNBSYE/CSNBSYD

**As a** z/OS application developer, **I want** ICSF CSNBSYE (encipher) and CSNBSYD (decipher) callable services for AES and 3DES.

**Acceptance Criteria:**
- Given a clear key label and plaintext, when CSNBSYE is called with AES-256-CBC, then the ciphertext is returned with correct ICSF return/reason codes
- Given ciphertext and the same key, when CSNBSYD is called, then the original plaintext is recovered
- Given an invalid key label, when called, then return_code=8 with appropriate reason_code

### CRYPTO-100.2: One-Way Hash — CSNBOWH

**As a** z/OS application developer, **I want** CSNBOWH for SHA-256/384/512 hashing.

**Acceptance Criteria:**
- Given input data and algorithm SHA-256, when CSNBOWH is called, then a 32-byte hash is returned
- Given the same input, when called multiple times, then the same hash is produced (deterministic)

### CRYPTO-100.3: HMAC — CSNBHMG

**As a** z/OS application developer, **I want** CSNBHMG for HMAC generation and verification.

**Acceptance Criteria:**
- Given a key label and message, when CSNBHMG GENERATE is called, then an HMAC is produced
- Given the HMAC and original message, when CSNBHMG VERIFY is called, then verification succeeds
- Given a tampered message, when VERIFY is called, then verification fails

### CRYPTO-100.4: Random Number Generation — CSNBRNGL

**As a** z/OS application developer, **I want** CSNBRNGL for cryptographically secure random bytes.

**Acceptance Criteria:**
- Given a request for 32 random bytes, when CSNBRNGL is called, then 32 bytes of cryptographically secure random data are returned
- Given successive calls, when compared, then outputs are statistically unique

### CRYPTO-100.5: ICSF Configuration — CSFPRMxx

**As a** system administrator, **I want** CSFPRMxx PARMLIB members parsed for ICSF configuration.

**Acceptance Criteria:**
- Given `CKDSN(SYS1.CSF.CKDS) PKDSN(SYS1.CSF.PKDS) FIPSMODE(YES)`, when parsed, then IcsfConfig fields are populated
- Given FIPS mode enabled, when non-FIPS algorithms (DES) are requested, then an error is returned

### CRYPTO-100.6: Symmetric & Hashing Tests

**Acceptance Criteria:**
- Given AES-128/192/256 and 3DES encrypt/decrypt round-trips, when tested, then all produce correct results
- Given SHA-1/256/384/512 known-answer tests, when run, then all produce NIST-specified outputs
- Given `cargo test -p open-mainframe-crypto` symmetric/hash tests, then all pass

---

## CRYPTO-101: ICSF Asymmetric Key Operations

**User Value:** Applications can generate RSA/EC key pairs, create digital signatures, and verify signatures for authentication, non-repudiation, and certificate operations.

### CRYPTO-101.1: RSA Key Generation — CSNDPKG

**As a** z/OS application developer, **I want** CSNDPKG to generate RSA key pairs (2048/4096-bit).

**Acceptance Criteria:**
- Given key type RSA and length 4096, when CSNDPKG is called, then a key pair is generated and stored in the PKDS
- Given the generated key, when used for sign/verify, then operations succeed

### CRYPTO-101.2: RSA Digital Signature — CSNDDSG/CSNDDSV

**As a** z/OS application developer, **I want** CSNDDSG (sign) and CSNDDSV (verify) for RSA signatures.

**Acceptance Criteria:**
- Given a private key label and hash, when CSNDDSG is called, then a PKCS#1 v1.5 or PSS signature is produced
- Given the signature and public key, when CSNDDSV is called, then verification succeeds
- Given a tampered hash, when CSNDDSV is called, then verification fails

### CRYPTO-101.3: ECDSA Operations

**As a** z/OS application developer, **I want** ECDSA key generation, sign, and verify for EC curves P-256/P-384/P-521.

**Acceptance Criteria:**
- Given curve P-256, when key generation is called, then an EC key pair is produced
- Given an EC private key and message hash, when sign is called, then an ECDSA signature is produced
- Given the signature and public key, when verify is called, then it succeeds

### CRYPTO-101.4: Diffie-Hellman Key Agreement

**As a** z/OS application developer, **I want** ECDH key agreement for establishing shared secrets.

**Acceptance Criteria:**
- Given two EC key pairs, when ECDH is performed, then both parties derive the same shared secret
- Given the shared secret, when used to derive an AES key, then encrypt/decrypt works

### CRYPTO-101.5: Asymmetric Operation Tests

**Acceptance Criteria:**
- Given RSA 2048/4096 sign/verify round-trips, when tested, then all succeed
- Given ECDSA P-256/P-384 sign/verify round-trips, when tested, then all succeed
- Given `cargo test -p open-mainframe-crypto` asymmetric tests, then all pass

---

## CRYPTO-102: Key Management & Key Stores

**User Value:** Cryptographic keys are securely stored, managed, and protected in z/OS-compatible key datasets, enabling enterprise key management.

### CRYPTO-102.1: CKDS — Cryptographic Key Data Set

**As a** security administrator, **I want** a CKDS to store symmetric keys with labels.

**Acceptance Criteria:**
- Given a key label "MY.AES.KEY" and AES-256 key material, when stored in CKDS, then it can be retrieved by label
- Given CKDS at-rest encryption, when the CKDS file is examined externally, then key material is not visible

### CRYPTO-102.2: PKDS — Public Key Data Set

**As a** security administrator, **I want** a PKDS to store RSA/EC key pairs with labels.

**Acceptance Criteria:**
- Given an RSA-4096 key pair, when stored in PKDS with label "MY.RSA.KEY", then private and public keys are retrievable
- Given the stored private key, when used for signing, then it produces valid signatures

### CRYPTO-102.3: TKDS — Token Key Data Set (PKCS#11)

**As a** security administrator, **I want** a TKDS for PKCS#11 token storage.

**Acceptance Criteria:**
- Given a PKCS#11 token definition, when stored in TKDS, then it is accessible via PKCS#11 interface
- Given token attributes (CKA_ENCRYPT, CKA_SIGN), when queried, then correct capabilities are reported

### CRYPTO-102.4: Key Lifecycle Operations

**As a** security administrator, **I want** key lifecycle management: generate, import, export, delete, rekey.

**Acceptance Criteria:**
- Given `ICSF GENERATE KEY(MY.NEW.KEY) ALGORITHM(AES) LENGTH(256)`, when called, then a new key is created in CKDS
- Given `ICSF DELETE KEY(MY.OLD.KEY)`, when called, then the key is zeroized and removed from the key store
- Given `ICSF REKEY KEY(MY.KEY)`, when called, then a new key replaces the old one with the same label

### CRYPTO-102.5: Master Key Management

**As a** security administrator, **I want** a master key to protect key stores at rest.

**Acceptance Criteria:**
- Given a master key provided at initialization, when key stores are opened, then key material is decrypted
- Given an incorrect master key, when key store open is attempted, then an error is returned

### CRYPTO-102.6: Key Management Tests

**Acceptance Criteria:**
- Given key CRUD operations across CKDS/PKDS/TKDS, when tested, then all operations succeed
- Given key store encryption round-trips, when tested, then stored and retrieved keys match
- Given `cargo test -p open-mainframe-crypto` key management tests, then all pass

---

## CRYPTO-103: CSFKEYS/CSFSERV RACF Integration

**User Value:** Access to cryptographic keys and services is controlled by RACF, preventing unauthorized use of sensitive cryptographic material.

### CRYPTO-103.1: CSFKEYS Resource Class

**As a** security administrator, **I want** CSFKEYS class profiles to control access to individual cryptographic keys.

**Acceptance Criteria:**
- Given RDEFINE CSFKEYS MY.AES.KEY UACC(NONE), when a user without PERMIT attempts to use the key, then access is denied
- Given PERMIT MY.AES.KEY CLASS(CSFKEYS) ID(APPUSER) ACCESS(READ), when APPUSER uses the key for encryption, then access is allowed

### CRYPTO-103.2: CSFSERV Resource Class

**As a** security administrator, **I want** CSFSERV class profiles to control access to ICSF callable services.

**Acceptance Criteria:**
- Given RDEFINE CSFSERV CSNBSYE UACC(NONE), when a user without PERMIT calls CSNBSYE, then access is denied
- Given PERMIT CSNBSYE CLASS(CSFSERV) ID(APPUSER) ACCESS(READ), when APPUSER calls CSNBSYE, then the service is allowed

### CRYPTO-103.3: GCSFKEYS — Grouping Class

**As a** security administrator, **I want** GCSFKEYS for generic key profiles protecting key groups.

**Acceptance Criteria:**
- Given RDEFINE GCSFKEYS PROD.** UACC(NONE), when a key matching PROD.AES.KEY is accessed, then the generic profile applies
- Given PERMIT PROD.** CLASS(GCSFKEYS) ID(PRODGRP) ACCESS(READ), when a PRODGRP member uses any PROD.* key, then access is allowed

### CRYPTO-103.4: RACF Integration Tests

**Acceptance Criteria:**
- Given CSFKEYS and CSFSERV profiles with various access levels, when crypto operations are attempted, then RACF enforcement is correct
- Given `cargo test` integration tests spanning crypto + racf crates, then all pass

---

## SEC-107: Security Labels (MAC)

**User Value:** Mandatory Access Control via security labels prevents unauthorized information flow, supporting classified and regulated environments.

### SEC-107.1: Security Level Hierarchy

**As a** security administrator, **I want** hierarchical security levels (e.g., UNCLASSIFIED < CONFIDENTIAL < SECRET < TOP SECRET).

**Acceptance Criteria:**
- Given levels defined with numeric values (0, 10, 20, 30), when compared, then TOP SECRET (30) dominates SECRET (20)
- Given RDEFINE SECLEVEL TOPSECRET UACC(NONE), when defined, then the level is available for label composition

### SEC-107.2: Security Categories

**As a** security administrator, **I want** non-hierarchical security categories (e.g., PAYROLL, PERSONNEL, FINANCIAL).

**Acceptance Criteria:**
- Given categories PAYROLL and FINANCIAL, when a label includes both, then access requires both categories in the subject's label
- Given a subject with categories {PAYROLL, FINANCIAL, HR} and an object with {PAYROLL, FINANCIAL}, when checked, then access is allowed (subject categories are a superset)

### SEC-107.3: SECLABEL Class — Label Definitions

**As a** security administrator, **I want** SECLABEL class profiles to define valid label combinations.

**Acceptance Criteria:**
- Given RDEFINE SECLABEL SECRET-PAYROLL with level=SECRET categories={PAYROLL}, when defined, then the label is available for assignment
- Given ALTUSER MYUSER SECLABEL(SECRET-PAYROLL), when assigned, then the user's security label is set

### SEC-107.4: Dominance Checking (Bell-LaPadula)

**As a** security system, **I want** automatic dominance checking for all resource access.

**Acceptance Criteria:**
- Given a user with label SECRET-PAYROLL and a dataset with label CONFIDENTIAL-PAYROLL, when READ is attempted, then access is allowed (user level dominates, categories match)
- Given a user with label CONFIDENTIAL and a dataset with label SECRET, when READ is attempted, then access is denied (user level does not dominate)
- Given MLACTIVE mode, when a violation occurs, then it is denied and audited

### SEC-107.5: Security Label Tests

**Acceptance Criteria:**
- Given various label combinations and access scenarios, when dominance is checked, then correct allow/deny decisions are made
- Given MLACTIVE/MLQUIET/MLS modes, when violations occur, then correct behavior (deny+audit / deny+quiet / deny+message) results
- Given `cargo test -p open-mainframe-racf` security label tests, then all pass

---

## SEC-108: RACF Audit & SMF Integration

**User Value:** All security-relevant events are recorded in SMF for compliance, forensics, and operational monitoring.

### SEC-108.1: SMF Type 80 — Authorization Events

**As a** security auditor, **I want** SMF Type 80 records generated for RACF authorization events.

**Acceptance Criteria:**
- Given a successful authorization check, when the resource has AUDIT(SUCCESS) set, then an SMF Type 80 record is written with event=AUTH_SUCCESS
- Given a failed authorization check, when logged, then Type 80 records include user, resource, class, access level, and failure reason

### SEC-108.2: SMF Type 80 — Profile Change Events

**As a** security auditor, **I want** Type 80 records for profile modifications.

**Acceptance Criteria:**
- Given ADDUSER NEWUSER, when executed, then a Type 80 record with event=ADDUSER is written
- Given PERMIT resource ID(user) ACCESS(READ), when executed, then a Type 80 record with event=PERMIT is written

### SEC-108.3: SMF Type 81 — Initialization Events

**As a** security auditor, **I want** Type 81 records for RACF initialization.

**Acceptance Criteria:**
- Given RACF initialization at system startup, when completed, then a Type 81 record is written with RACF database name and options

### SEC-108.4: SMF Type 83 — Database Changes

**As a** security auditor, **I want** Type 83 records tracking all RACF database modifications.

**Acceptance Criteria:**
- Given a profile added/modified/deleted, when the change is committed, then a Type 83 record captures before/after state

### SEC-108.5: IRRDBU00 — Database Unload

**As a** security auditor, **I want** IRRDBU00 to unload the RACF database to a flat file for reporting.

**Acceptance Criteria:**
- Given `IRRDBU00` executed, when completed, then all profiles (user, group, dataset, general resource, connect) are written as fixed-format records
- Given the unload output, when processed, then it can be loaded into a database for SQL-based reporting

### SEC-108.6: Audit & SMF Tests

**Acceptance Criteria:**
- Given various RACF operations, when auditing is enabled, then correct SMF records are produced
- Given `cargo test -p open-mainframe-racf` audit tests, then all pass

---

## SEC-109: RACF Exits, Utilities & Configuration

**User Value:** Security administrators can customize RACF behavior via exits, maintain database health via utilities, and verify database integrity.

### SEC-109.1: ICHRTX00 — Pre-Authorization Exit

**As a** security administrator, **I want** ICHRTX00 to intercept authorization requests before RACF processing.

**Acceptance Criteria:**
- Given ICHRTX00 registered, when an AUTH request is made, then the exit is called first
- Given the exit returns Allow, when processing continues, then the resource is granted without RACF checking
- Given the exit returns Continue, when processing continues, then normal RACF checking proceeds

### SEC-109.2: ICHPWX01/ICHPWX11 — Password Quality Exits

**As a** security administrator, **I want** password quality exits to enforce custom password rules.

**Acceptance Criteria:**
- Given ICHPWX01 registered, when a password change is attempted, then the exit validates the new password
- Given the exit rejects the password (e.g., contains userid), then the change is denied with a message

### SEC-109.3: IRREVX01 — Event Notification Exit

**As a** security administrator, **I want** IRREVX01 to receive notifications of RACF events.

**Acceptance Criteria:**
- Given IRREVX01 registered, when an authorization failure occurs, then the exit is called with event details
- Given the exit, when invoked, then it can log to an external system or trigger an alert

### SEC-109.4: IRRUT100/IRRUT200 — Search & Verify

**As a** security administrator, **I want** IRRUT100 to search the RACF database and IRRUT200 to verify integrity.

**Acceptance Criteria:**
- Given `IRRUT100 CLASS(DATASET) PROFILE(SYS1.**)`, when executed, then all matching profiles are listed
- Given `IRRUT200`, when executed against the RACF database, then integrity checks pass or errors are reported

### SEC-109.5: IRRUT400 — Split/Merge

**As a** security administrator, **I want** IRRUT400 to split or merge RACF databases.

**Acceptance Criteria:**
- Given two RACF databases, when IRRUT400 MERGE is executed, then profiles from both are combined into one database
- Given a large RACF database, when IRRUT400 SPLIT is executed with criteria, then profiles are distributed across two databases

### SEC-109.6: Exits & Utilities Tests

**Acceptance Criteria:**
- Given exit registration and invocation for all exit points, when tested, then correct dispatch and action handling occurs
- Given IRRUT100/200/400 operations on test databases, when executed, then correct results are produced
- Given `cargo test -p open-mainframe-racf` exit/utility tests, then all pass

---

## Dependency Graph

```
CRYPTO-100 (Symmetric/Hash) → CRYPTO-102 (Key Stores)
CRYPTO-101 (Asymmetric)     → CRYPTO-102 (Key Stores)
CRYPTO-102 (Key Stores)     → CRYPTO-103 (RACF Integration)

SEC-107 (Security Labels) — independent (extends RACF S100-S106)
SEC-108 (Audit/SMF)       — depends on SMF crate (PG-17 SMF-102)
SEC-109 (Exits/Utilities)  — depends on RACF S100 (core model)
```

## FR/NFR Coverage Matrix

| FR | Stories |
|----|---------|
| FR-CRYPTO-001 | CRYPTO-100.1, CRYPTO-100.2, CRYPTO-100.3, CRYPTO-100.4 |
| FR-CRYPTO-002 | CRYPTO-101.1, CRYPTO-101.2, CRYPTO-101.3, CRYPTO-101.4 |
| FR-CRYPTO-003 | CRYPTO-102.1, CRYPTO-102.2, CRYPTO-102.3, CRYPTO-102.4, CRYPTO-102.5 |
| FR-CRYPTO-004 | CRYPTO-103.1, CRYPTO-103.2, CRYPTO-103.3 |
| FR-CRYPTO-005 | (Builds on S106 from v4.0 — certificate lifecycle) |
| FR-CRYPTO-006 | (Builds on SYS-110 from v5.0 addendum — keyring management) |
| FR-CRYPTO-007 | SEC-107.1, SEC-107.2, SEC-107.3, SEC-107.4 |
| FR-CRYPTO-008 | SEC-108.1, SEC-108.2, SEC-108.3, SEC-108.4 |
| FR-CRYPTO-009 | SEC-109.1, SEC-109.2, SEC-109.3 |
| FR-CRYPTO-010 | SEC-108.5, SEC-109.4, SEC-109.5 |
| FR-CRYPTO-011 | CRYPTO-102.3 |
| FR-CRYPTO-012 | CRYPTO-100.5 |

| NFR | Stories |
|-----|---------|
| NFR-CRYPTO-001 | CRYPTO-100.6, CRYPTO-101.5, CRYPTO-102.6, CRYPTO-103.4, SEC-107.5, SEC-108.6, SEC-109.6 |
| NFR-CRYPTO-002 | CRYPTO-100.1 (uses ring), CRYPTO-101.2 (uses ring) |
| NFR-CRYPTO-003 | CRYPTO-102.5, CRYPTO-102.1 |

**Coverage: 12/12 FRs (100%), 3/3 NFRs (100%)**
