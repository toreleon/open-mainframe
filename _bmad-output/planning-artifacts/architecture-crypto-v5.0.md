---
version: 'v5.0'
planningGroup: 'PG-23'
technology: 'Crypto, PKI & Advanced Security'
date: '2026-02-21'
status: 'complete'
inputDocuments:
  - 'prd-crypto-v5.0.md'
---

# Architecture: Crypto, PKI & Advanced Security

## 1. Crate Strategy

**New crate:** `open-mainframe-crypto`
**Extends:** `open-mainframe-racf` (for security labels, exits, utilities, audit)

Rationale: ICSF cryptographic services are a distinct subsystem with their own callable service API, key stores, and configuration. A dedicated `open-mainframe-crypto` crate houses ICSF services, key management, and PKCS#11 support. The RACF-specific extensions (security labels, exits, audit, utilities) are added to the existing `open-mainframe-racf` crate since they are tightly coupled to the RACF data model.

## 2. Module Layout

```
crates/open-mainframe-crypto/src/
├── lib.rs              # Crate root, public API
├── symmetric.rs        # AES, DES/3DES encrypt/decrypt (CSNBSYE/CSNBSYD)
├── hash.rs             # SHA-1/256/384/512 (CSNBOWH)
├── hmac.rs             # HMAC generation/verification (CSNBHMG)
├── random.rs           # Random number generation (CSNBRNGL)
├── asymmetric/
│   ├── mod.rs          # Key generation dispatch
│   ├── rsa.rs          # RSA keygen, sign, verify (CSNDPKG/CSNDDSG/CSNDDSV)
│   └── ecdsa.rs        # ECDSA operations (CSNDDSG2/CSNDDSV2), DH
├── keystore/
│   ├── mod.rs          # Key store abstraction
│   ├── ckds.rs         # Cryptographic Key Data Set (symmetric keys)
│   ├── pkds.rs         # Public Key Data Set (asymmetric keys)
│   └── tkds.rs         # Token Key Data Set (PKCS#11 tokens)
├── pkcs11.rs           # PKCS#11 token interface
├── config.rs           # CSFPRMxx parser
└── error.rs            # CryptoError types

crates/open-mainframe-racf/src/  (extensions to existing crate)
├── seclabel/
│   ├── mod.rs          # SECLABEL class implementation
│   ├── levels.rs       # Hierarchical security levels
│   ├── categories.rs   # Non-hierarchical categories
│   └── dominance.rs    # Bell-LaPadula dominance checking
├── audit/
│   ├── mod.rs          # Audit framework
│   ├── type80.rs       # SMF Type 80 record generation
│   ├── type81.rs       # SMF Type 81 initialization events
│   └── type83.rs       # SMF Type 83 database changes
├── exits/
│   ├── mod.rs          # Exit registry and dispatch
│   ├── ichrtx00.rs     # Pre-authorization exit
│   ├── ichrcx.rs       # SAF callable services exits
│   ├── ichpwx.rs       # Password quality exits
│   └── irrevx01.rs     # Event notification exit
└── utilities/
    ├── mod.rs          # Utility dispatch
    ├── irrut100.rs     # Database search
    ├── irrut200.rs     # Database verify/repair
    ├── irrut400.rs     # Database split/merge
    └── irrdbu00.rs     # Database unload
```

## 3. Key Types

```rust
// ── open-mainframe-crypto types ──

/// ICSF symmetric algorithm
pub enum SymmetricAlgorithm {
    Aes128, Aes192, Aes256,
    Des, TripleDes,
}

/// ICSF hash algorithm
pub enum HashAlgorithm {
    Sha1, Sha256, Sha384, Sha512,
}

/// Asymmetric key type
pub enum AsymmetricKeyType {
    Rsa2048, Rsa4096,
    EcP256, EcP384, EcP521,
}

/// ICSF callable service result
pub struct IcsfResult {
    pub return_code: i32,
    pub reason_code: i32,
    pub output: Vec<u8>,
}

/// Cryptographic key reference
pub struct KeyToken {
    pub label: String,
    pub key_type: KeyType,
    pub store: KeyStore,
}

pub enum KeyType {
    Symmetric(SymmetricAlgorithm),
    Asymmetric(AsymmetricKeyType),
    Pkcs11Token,
}

pub enum KeyStore { Ckds, Pkds, Tkds }

/// ICSF configuration (from CSFPRMxx)
pub struct IcsfConfig {
    pub ckds_dsname: String,
    pub pkds_dsname: String,
    pub tkds_dsname: Option<String>,
    pub fips_mode: bool,
    pub default_sym_algo: SymmetricAlgorithm,
}

// ── open-mainframe-racf extension types ──

/// Security label (SECLABEL)
pub struct SecurityLabel {
    pub name: String,
    pub level: SecurityLevel,
    pub categories: HashSet<String>,
}

pub struct SecurityLevel {
    pub name: String,
    pub numeric: u16,  // Higher = more restrictive
}

/// RACF exit trait
pub trait RacfExit: Send + Sync {
    fn name(&self) -> &str;
    fn invoke(&self, context: &ExitContext) -> ExitAction;
}

pub enum ExitAction {
    Allow,
    Deny,
    Continue,  // Let RACF make the decision
    ModifyRequest(Box<dyn std::any::Any + Send>),
}

/// SMF Type 80 RACF event record
pub struct SmfType80Record {
    pub event_code: u16,
    pub qualifier: u16,
    pub user_id: String,
    pub resource_name: String,
    pub class_name: String,
    pub access_intent: u8,
    pub access_result: AccessResult,
    pub timestamp: SystemTime,
}

pub enum AccessResult {
    Success,
    Failure,
    Warning,
}

/// IRRDBU00 unload record format
pub struct DbUnloadRecord {
    pub record_type: UnloadRecordType,
    pub fields: Vec<(String, String)>,
}

pub enum UnloadRecordType {
    UserBasic, UserDfp, UserTso, UserOmvs,
    GroupBasic,
    ConnectBasic,
    DatasetAccess, DatasetProfile,
    GeneralResource, GeneralAccess,
    Certificate,
}
```

## 4. Design Decisions

### DD-5.0-CRYPTO-01: Delegation to ring/rustls
**Decision:** All cryptographic primitives are delegated to the `ring` crate (for symmetric, hashing, HMAC, random) and `rcgen` (for certificate generation). The `open-mainframe-crypto` crate provides the z/OS ICSF API surface (callable service names, return/reason codes, key token formats) but calls `ring` for the actual crypto. This ensures algorithmic correctness without custom crypto implementations.

### DD-5.0-CRYPTO-02: Key Stores as Encrypted Files
**Decision:** CKDS, PKDS, and TKDS are implemented as encrypted files on the native filesystem. The encryption key for the key stores is derived from a master key provided at system initialization. This provides at-rest protection analogous to z/OS hardware master keys, without requiring actual crypto hardware.

### DD-5.0-CRYPTO-03: Security Labels as Profile Attributes
**Decision:** Security labels are stored as attributes on user profiles and resource profiles in the existing RACF database. The SECLABEL class defines valid label combinations. Dominance checking is a pure function of level comparison and category subset checking, implemented without any RACF database lookups beyond the initial label resolution.

### DD-5.0-CRYPTO-04: Exit Framework via Trait Objects
**Decision:** RACF exits are implemented as trait objects (`Box<dyn RacfExit>`) registered in a global exit registry. Each exit point has a named slot (ICHRTX00, ICHRCX01, etc.). When no exit is registered for a slot, the default behavior applies. This matches the real z/OS model where exits are optional customization points.
