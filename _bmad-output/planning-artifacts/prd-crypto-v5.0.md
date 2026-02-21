---
version: 'v5.0'
planningGroup: 'PG-23'
technology: 'Crypto, PKI & Advanced Security'
date: '2026-02-21'
status: 'complete'
totalFRs: 12
totalNFRs: 3
---

# PRD: Crypto, PKI & Advanced Security

## 1. Overview

z/OS provides a comprehensive cryptographic infrastructure through ICSF (Integrated Cryptographic Service Facility), hardware crypto acceleration via CPACF/CEX, digital certificate lifecycle management via RACF RACDCERT, and advanced security features including Security Labels (MAC), RACF exits, and database utilities.

This PRD covers the remaining security gaps not addressed by the existing RACF v4.0 (S100-S106) and v5.0 addendum (SYS-109/SYS-110) planning. Specifically: ICSF cryptographic services, RACF Security Labels (MAC), audit/SMF integration, RACF exits and customization, and RACF database utilities.

## 2. Functional Requirements

### FR-CRYPTO-001: ICSF Callable Services
The system SHALL provide ICSF callable services for symmetric encryption (AES-128/192/256, DES/3DES), hashing (SHA-1, SHA-256, SHA-384, SHA-512), MAC generation/verification (HMAC), and random number generation. Services SHALL be invoked via CSNBSYE/CSNBSYD (symmetric encrypt/decrypt), CSNBOWH (one-way hash), CSNBHMG (HMAC), and CSNE/CSNBRNGL (random).

### FR-CRYPTO-002: Asymmetric Key Operations
The system SHALL support RSA key generation (CSNDPKG), RSA digital signature (CSNDDSG/CSNDDSV), ECDSA operations (CSNDDSG2/CSNDDSV2), and Diffie-Hellman key agreement. Key sizes SHALL include RSA 2048/4096 and EC P-256/P-384/P-521 curves.

### FR-CRYPTO-003: Key Management
The system SHALL manage cryptographic keys with the ICSF key store (CKDS — Cryptographic Key Data Set, PKDS — Public Key Data Set, TKDS — Token Key Data Set). Keys SHALL be identified by label and protected via RACF CSFKEYS/GCSFKEYS resource class.

### FR-CRYPTO-004: CSFKEYS/CSFSERV Resource Classes
The system SHALL enforce access to cryptographic keys via the CSFKEYS resource class and access to cryptographic services via the CSFSERV resource class. RACF PERMIT commands SHALL control which users can use which keys and services.

### FR-CRYPTO-005: Certificate Lifecycle Management
The system SHALL extend RACDCERT with full certificate lifecycle: GENCERT (self-signed and CA-signed), GENREQ (CSR), ADD/DELETE, IMPORT/EXPORT (PKCS#12, PEM, DER), REKEY, ROLLOVER, CHECKCERT (validation), and LISTCHAIN (chain verification). This builds on the foundation in S106.

### FR-CRYPTO-006: Keyring Management
The system SHALL manage keyrings containing ordered certificate/key pairs for AT-TLS and application use. Operations: ADDRING, DELRING, LISTRING, CONNECT (certificate to keyring), REMOVE. Virtual keyrings SHALL be supported for SAF-based key access.

### FR-CRYPTO-007: Security Labels (MAC)
The system SHALL implement Mandatory Access Control via security labels (SECLABEL class). Each label combines a hierarchical security level and non-hierarchical security categories. Access decisions SHALL enforce dominance: a subject's label must dominate an object's label for READ access, and be dominated by it for WRITE (Bell-LaPadula).

### FR-CRYPTO-008: RACF Audit & SMF Integration
The system SHALL generate SMF Type 80 records for RACF events (authorization successes/failures, profile changes, certificate operations), Type 81 for initialization events, and Type 83 for database changes. Per-profile and per-class audit settings SHALL control what is recorded.

### FR-CRYPTO-009: RACF Exit Framework
The system SHALL support RACF installation exits: ICHRTX00 (pre-authorization), ICHRCX01/02 (SAF callable services), ICHPWX01/11 (password quality), IRREVX01 (event notification), and IRRPX001 (PassTicket). Exits SHALL be implemented as trait objects registered at initialization.

### FR-CRYPTO-010: RACF Database Utilities
The system SHALL provide RACF database administration utilities: IRRUT100 (search), IRRUT200 (verify/repair), IRRUT400 (split/merge), and IRRDBU00 (unload to flat file for reporting).

### FR-CRYPTO-011: PKCS#11 Token Support
The system SHALL support PKCS#11 tokens via the TKDS (Token Key Data Set) for interoperability with industry-standard cryptographic interfaces. Tokens SHALL be accessible via ICSF PKCS#11 callable services.

### FR-CRYPTO-012: Crypto Configuration (CSFPRMxx)
The system SHALL parse CSFPRMxx PARMLIB members for ICSF configuration including: CKDS/PKDS/TKDS dataset names, default algorithms, FIPS mode settings, and crypto hardware preferences.

## 3. Non-Functional Requirements

### NFR-CRYPTO-001: Test Coverage
All cryptographic operations SHALL have unit tests achieving ≥90% code coverage. Tests SHALL verify encrypt/decrypt round-trips, hash consistency, and signature verify.

### NFR-CRYPTO-002: Standards Compliance
Cryptographic implementations SHALL use well-established Rust crates (ring, rustls, rcgen) rather than custom implementations, ensuring algorithmic correctness and resistance to timing attacks.

### NFR-CRYPTO-003: Key Protection
Private keys SHALL never be stored in cleartext outside of key datasets. In-memory key material SHALL be zeroized after use via the `zeroize` crate.
