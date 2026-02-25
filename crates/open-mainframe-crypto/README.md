# open-mainframe-crypto

z/OS **ICSF (Integrated Cryptographic Service Facility)** and RACF security
extensions — symmetric/asymmetric cryptographic operations, key store
management, RACF crypto profile authorization, Bell-LaPadula security labels,
audit/SMF records, and RACF exit/utility support.

## Overview

This crate provides a simulated implementation of the z/OS cryptographic
subsystem spanning seven modules:

| Module | Lines | Responsibility |
|---|---|---|
| `symmetric.rs` | 643 | AES/3DES encryption, SHA hashing, HMAC, PRNG, ICSF configuration |
| `asymmetric.rs` | 439 | RSA/EC key generation, RSA/ECDSA signatures, ECDH key agreement |
| `keystore.rs` | 588 | CKDS, PKDS, TKDS key stores, key lifecycle, master key wrapping |
| `racf_crypto.rs` | 379 | CSFKEYS/CSFSERV RACF profiles, wildcard matching, access checks |
| `seclabel.rs` | 409 | Bell-LaPadula security labels, dominance check, MLS modes |
| `audit.rs` | 781 | SMF Type 80/81/83 records, IRRDBU00 unload, audit trail queries |
| `exits.rs` | 755 | RACF exit points (ICHRTX00/ICHPWX01/IRREVX01), exit registry, IRRUT100/200/400 utilities |
| `error.rs` | 113 | `CryptoError` enum (12 variants) |

**Total**: ~4 150 lines of Rust.

All cryptographic operations use **simulated** algorithms (XOR-based
transforms) and are **not cryptographically secure**. The implementations
are structurally faithful to the z/OS ICSF callable service interfaces and
are intended for functional testing and demonstration.

## Architecture

```
  ┌─────────────────────────────────────────────────────────────┐
  │                    ICSF Callable Services                   │
  │                                                             │
  │  ┌──────────────┐  ┌───────────────┐  ┌──────────────────┐ │
  │  │  symmetric    │  │  asymmetric   │  │    keystore      │ │
  │  │  AES/3DES     │  │  RSA / EC     │  │  CKDS PKDS TKDS  │ │
  │  │  SHA hash     │  │  Sign/Verify  │  │  MasterKey       │ │
  │  │  HMAC / PRNG  │  │  ECDH agree   │  │  KeyLifecycle    │ │
  │  └──────────────┘  └───────────────┘  └──────────────────┘ │
  └─────────────────────────┬───────────────────────────────────┘
                            │
  ┌─────────────────────────▼───────────────────────────────────┐
  │                  RACF Security Integration                  │
  │                                                             │
  │  ┌──────────────┐  ┌───────────────┐  ┌──────────────────┐ │
  │  │ racf_crypto  │  │   seclabel    │  │     audit        │ │
  │  │ CSFKEYS/SERV │  │ Bell-LaPadula │  │ SMF 80/81/83     │ │
  │  │ Profile ACL  │  │ MAC dominance │  │ IRRDBU00         │ │
  │  │ Wildcard     │  │ MLS modes     │  │ AuditTrail       │ │
  │  └──────────────┘  └───────────────┘  └──────────────────┘ │
  │                                                             │
  │  ┌───────────────────────────────────────────────────────┐  │
  │  │                      exits                            │  │
  │  │  ICHRTX00 / ICHPWX01 / IRREVX01 exit registry        │  │
  │  │  IRRUT100 search / IRRUT200 verify / IRRUT400 split  │  │
  │  └───────────────────────────────────────────────────────┘  │
  └─────────────────────────────────────────────────────────────┘
```

## Key Types and Traits

### Symmetric Operations (`symmetric.rs`)

| Type | Description |
|---|---|
| `SymmetricAlgorithm` | Enum: `Aes128`, `Aes192`, `Aes256`, `TripleDes`; `key_len()` returns expected byte count |
| `CipherMode` | Enum: `Cbc`, `Ecb`, `Ctr` |
| `HashAlgorithm` | Enum: `Sha256` (32 bytes), `Sha384` (48), `Sha512` (64); `digest_len()` |
| `IcsfSymmetricKey` | Wrapper around raw key bytes |
| `IcsfResult` | Return/reason code pair + output data; `ok()`, `err()`, `is_ok()` |
| `IcsfConfig` | CSFPRMxx config: CKDS/PKDS dataset names, FIPS mode; `parse()` |

Functions: `encrypt()`, `decrypt()`, `one_way_hash()`, `hmac_generate()`,
`hmac_verify()`, `generate_random()`.

### Asymmetric Operations (`asymmetric.rs`)

| Type | Description |
|---|---|
| `SignScheme` | Enum: `Pkcs1v15`, `Pss` |
| `EcCurve` | Enum: `P256`, `P384`, `P521`; `key_size()`, `public_key_size()` |
| `RsaKeyPair` | public_key + private_key + modulus_bits (2048 or 4096) |
| `EcKeyPair` | private_key + public_key + curve |

Functions: `generate_rsa_keypair()`, `rsa_sign()`, `rsa_verify()`,
`generate_ec_keypair()`, `ecdsa_sign()`, `ecdsa_verify()`, `ecdh_agree()`.

### Key Stores (`keystore.rs`)

| Type | Description |
|---|---|
| `Ckds` | Cryptographic Key Data Set — symmetric keys by label |
| `Pkds` | Public Key Data Set — RSA/EC key pairs by label |
| `PkdsEntry` | public_key + private_key + key_type string |
| `Tkds` | Token Key Data Set — PKCS#11 objects with attributes |
| `TkdsEntry` | key_bytes + `HashMap<TokenAttribute, String>` |
| `TokenAttribute` | Enum: 11 CKA_* attributes (Class, KeyType, Label, Encrypt, Decrypt, Sign, Verify, Wrap, Unwrap, Extractable, Token) |
| `KeyLifecycle` | Manages CKDS + PKDS: generate, delete, rekey |
| `MasterKey` | XOR-based key wrapping/unwrapping |

### RACF Crypto Integration (`racf_crypto.rs`)

| Type | Description |
|---|---|
| `CryptoAccessLevel` | Ordered enum: None < Read < Update < Control < Alter |
| `CsfKeysProfile` | CSFKEYS class profile: key_label + access list + UACC |
| `CsfServProfile` | CSFSERV class profile: service_name + access list + UACC |
| `GcsfKeysProfile` | Generic CSFKEYS with wildcard pattern (`*`, `**`) matching |

Functions: `check_key_access()`, `check_service_access()`.

### Security Labels (`seclabel.rs`)

| Type | Description |
|---|---|
| `SecurityLevel` | Ordered enum: Unclassified < Confidential < Secret < TopSecret |
| `SecurityCategory` | Named compartment (e.g., PAYROLL, FINANCIAL) — uppercase normalized |
| `SecurityLabel` | Level + `BTreeSet<SecurityCategory>` |
| `SeclabelProfile` | Named SECLABEL profile mapping to a `SecurityLabel` |
| `MlsMode` | Enum: Active (enforce + audit), Quiet (audit only), Off |

Function: `dominance_check()` — Bell-LaPadula "no read up" rule.

### Audit / SMF Records (`audit.rs`)

| Type | Description |
|---|---|
| `SmfType80Record` | Authorization event: event_type, user, resource, class, access, result, timestamp |
| `SmfType80ProfileChange` | Profile modification: change_type (11 variants), issuer, target |
| `SmfType81Record` | Initialization event: Startup, DatabaseSwitch, SetroptsChange, ClassChange |
| `SmfType83Record` | Database change with before/after images |
| `Irrdbu00` | RACF database unload utility — flat-file record generation |
| `UnloadRecordType` | UserBasic (0200), GroupBasic (0100), ConnectData (0205), DatasetAccess (0400), GeneralResource (0500) |
| `AuditTrail` | Event collection with `events_for_user()`, `failures()`, `events_for_class()` queries |

### Exits and Utilities (`exits.rs`)

| Type | Description |
|---|---|
| `ExitPoint` | Enum: PreAuth (ICHRTX00), PasswordQuality (ICHPWX01), EventNotification (IRREVX01) |
| `ExitAction` | Enum: Allow, Deny, PassThrough |
| `ExitContext` | User + resource + data passed to exit handlers |
| `ExitRegistry` | HashMap-based registry of `Box<dyn Fn>` handlers per exit point |
| `RacfSearchUtil` | IRRUT100: field-based search with wildcard support |
| `RacfVerifyUtil` | IRRUT200: integrity checks (orphaned connections, duplicates) |
| `RacfSplitMergeUtil` | IRRUT400: round-robin database split and merge |

## Implementation Details

### Simulated Encryption (CRYPTO-100)

Encryption/decryption uses a repeating-key XOR transform:

```
ciphertext[i] = plaintext[i] XOR key[i % key_len]
```

This makes `encrypt` and `decrypt` symmetric (XOR is its own inverse). The
implementation validates key length against the algorithm (AES-128 → 16 bytes,
AES-256 → 32 bytes, 3DES → 24 bytes) and rejects empty data.

### Simulated Hashing (CRYPTO-100)

`one_way_hash` produces a deterministic digest using a Merkle-Damgard-like
construction:

1. Initialize state array with algorithm-specific constants
2. Process each input byte: `state[pos] = state[pos] * 31 + byte + idx`
3. Cascade diffusion to adjacent position: `state[next] += state[pos] * 7`
4. Finalization: 4 rounds of `state[i] = state[i] * 17 + state[prev] + round`

The HMAC construction follows RFC 2104 structure: `H(key⊕opad ∥ H(key⊕ipad ∥ data))`.

### RSA / EC Key Generation (CRYPTO-101)

Key pairs are populated with pseudo-random bytes from the LCG-based PRNG.
RSA supports 2048 and 4096-bit modulus sizes. EC supports P-256 (32-byte
scalar, 65-byte uncompressed public key), P-384 (48/97 bytes), and P-521
(66/133 bytes).

Signature operations use hash-like mixing of key and data bytes. In the
simulation, sign/verify round-trips succeed when the same bytes are used for
both halves (i.e., in test setups where public_key == private_key bytes).

### ECDH Key Agreement (CRYPTO-101)

`ecdh_agree` produces a shared secret by element-wise multiplication of the
private and public key bytes:

```
shared[i] = our_private[i] * their_public[i] + i
```

### Key Store Architecture (CRYPTO-102)

Three HashMap-backed stores model the z/OS ICSF key datasets:

- **CKDS** — Symmetric keys indexed by label (`String → Vec<u8>`)
- **PKDS** — Key pairs indexed by label (`String → PkdsEntry`)
- **TKDS** — PKCS#11 token objects with CKA_* attributes

All stores enforce unique labels and return sorted label lists. `MasterKey`
wraps/unwraps keys using XOR. `KeyLifecycle` provides generate/delete/rekey
workflows across CKDS and PKDS.

### RACF Crypto Profiles (CRYPTO-103)

CSFKEYS profiles control access to individual keys. CSFSERV profiles control
access to ICSF services (e.g., CSFENC, CSFDEC). Each profile has a per-user
access list and a Universal Access (UACC) default.

Generic CSFKEYS profiles support z/OS wildcard conventions:
- `*` matches exactly one qualifier (dot-separated segment)
- `**` matches zero or more qualifiers

Matching uses recursive backtracking: `wm_recursive(pat_parts, inp_parts)`.

### Bell-LaPadula Dominance (SEC-107)

`dominance_check` implements the "simple security property" (no read up):

- Subject level must be ≥ object level
- Subject categories must be a superset of object categories

Categories are stored in `BTreeSet` for deterministic ordering. MLS modes
control whether MAC is enforced (Active), audit-only (Quiet), or disabled (Off).

### SMF Record Types (SEC-108)

| SMF Type | Record | Description |
|---|---|---|
| 80 | `SmfType80Record` | Authorization success/failure (5 event types) |
| 80 | `SmfType80ProfileChange` | Administrative changes (11 change types) |
| 81 | `SmfType81Record` | RACF initialization events (4 event types) |
| 83 | `SmfType83Record` | Database changes with before/after images |

`Irrdbu00` converts RACF profiles to flat-file records with type prefixes
(0100=GroupBasic, 0200=UserBasic, 0205=ConnectData, 0400=DatasetAccess,
0500=GeneralResource).

`AuditTrail` provides indexed queries: by user, by failure status, by
resource class.

### RACF Exit Registry (SEC-109)

Exit handlers are `Box<dyn Fn(&ExitContext) -> ExitAction + Send + Sync>`
closures stored in a HashMap keyed by `ExitPoint`. Only one handler per exit
point is allowed. Unregistered points return `PassThrough`.

### RACF Utilities (SEC-109)

- **IRRUT100** (`RacfSearchUtil`) — Field-based search with trailing `*`
  wildcard support; all criteria must match (AND semantics)
- **IRRUT200** (`RacfVerifyUtil`) — Checks for orphaned connections (user or
  group missing) and duplicate entries
- **IRRUT400** (`RacfSplitMergeUtil`) — Round-robin partitioning of users
  and groups into N partitions; merge recombines all partitions

## Syntax / Feature Coverage

### ICSF Symmetric Services

| Service | Status | Notes |
|---|---|---|
| CSNBENC (Encrypt) | Implemented | AES-128/192/256, 3DES; CBC/ECB/CTR modes |
| CSNBDEC (Decrypt) | Implemented | Symmetric inverse of encrypt |
| CSNBOWH (One-Way Hash) | Implemented | SHA-256/384/512 |
| CSNBHMG (HMAC Generate) | Implemented | RFC 2104 structure |
| CSNBHMV (HMAC Verify) | Implemented | Constant-time-ish comparison |
| CSNBRNG (Random Generate) | Implemented | LCG-based PRNG |
| CSFPRMxx Config Parse | Implemented | CKDSN, PKDSN, FIPSMODE |

### ICSF Asymmetric Services

| Service | Status | Notes |
|---|---|---|
| CSNDPKG (RSA Key Generate) | Implemented | 2048 / 4096 bits |
| CSNDPKS (RSA Sign) | Implemented | PKCS#1 v1.5 and PSS schemes |
| CSNDPKV (RSA Verify) | Implemented | Scheme-aware verification |
| EC Key Generate | Implemented | P-256, P-384, P-521 |
| ECDSA Sign/Verify | Implemented | Deterministic simulation |
| ECDH Key Agreement | Implemented | Shared secret derivation |

### Key Stores

| Store | Status | Operations |
|---|---|---|
| CKDS | Implemented | insert, get, delete, list, len |
| PKDS | Implemented | insert, get, delete, list, len |
| TKDS | Implemented | insert (with CKA_* attributes), get, delete, list |
| Master Key wrap/unwrap | Implemented | XOR-based simulation |
| Key Lifecycle | Implemented | generate, delete, rekey (symmetric + asymmetric) |

### RACF Security

| Feature | Status | Notes |
|---|---|---|
| CSFKEYS profiles | Implemented | Discrete per-key ACLs |
| CSFSERV profiles | Implemented | Per-service ACLs |
| Generic wildcards (`*`, `**`) | Implemented | Recursive pattern matching |
| Access checks | Implemented | Discrete → generic fallback |
| Security labels (MAC) | Implemented | Bell-LaPadula dominance |
| SECLABEL profiles | Implemented | Named label definitions |
| MLS modes | Implemented | Active / Quiet / Off |
| SMF Type 80 auth | Implemented | 5 event types, 3 results |
| SMF Type 80 changes | Implemented | 11 change types |
| SMF Type 81 init | Implemented | 4 event types |
| SMF Type 83 db changes | Implemented | Before/after images |
| IRRDBU00 unload | Implemented | 5 record types with prefixes |
| Audit trail queries | Implemented | By user, failures, class |
| ICHRTX00 exit | Implemented | Pre-authorization |
| ICHPWX01 exit | Implemented | Password quality |
| IRREVX01 exit | Implemented | Event notification |
| IRRUT100 search | Implemented | Wildcard field matching |
| IRRUT200 verify | Implemented | Orphan/duplicate detection |
| IRRUT400 split/merge | Implemented | Round-robin partitioning |

## Usage Examples

### Symmetric Encryption

```rust
use open_mainframe_crypto::{
    symmetric::{encrypt, decrypt, IcsfSymmetricKey, SymmetricAlgorithm, CipherMode},
};

let key = IcsfSymmetricKey::new(vec![0xAA; 32]);
let plaintext = b"Sensitive data";

let result = encrypt(&key, plaintext, SymmetricAlgorithm::Aes256, CipherMode::Cbc)?;
assert!(result.is_ok());

let decrypted = decrypt(&key, &result.data, SymmetricAlgorithm::Aes256, CipherMode::Cbc)?;
assert_eq!(decrypted.data, plaintext);
```

### HMAC Generation and Verification

```rust
use open_mainframe_crypto::symmetric::{HashAlgorithm, hmac_generate, hmac_verify};

let key = b"shared-secret";
let data = b"message to authenticate";
let mac = hmac_generate(HashAlgorithm::Sha256, key, data);
assert!(hmac_verify(HashAlgorithm::Sha256, key, data, &mac));
```

### Key Store Management

```rust
use open_mainframe_crypto::keystore::{KeyLifecycle, PkdsEntry};

let mut lifecycle = KeyLifecycle::new();
lifecycle.generate_symmetric("AES.PROD.KEY1", vec![0x42; 32])?;
let key = lifecycle.ckds.get("AES.PROD.KEY1")?;

let old = lifecycle.rekey_symmetric("AES.PROD.KEY1", vec![0x99; 32])?;
```

### Bell-LaPadula Access Check

```rust
use open_mainframe_crypto::seclabel::*;

let user_label = SecurityLabel::with_categories(
    SecurityLevel::TopSecret,
    vec![SecurityCategory::new("PAYROLL"), SecurityCategory::new("FINANCIAL")],
);
let resource_label = SecurityLabel::with_categories(
    SecurityLevel::Secret,
    vec![SecurityCategory::new("PAYROLL")],
);
assert!(dominance_check(&user_label, &resource_label)); // user dominates resource
```

### RACF Exit Registration

```rust
use open_mainframe_crypto::exits::*;

let mut registry = ExitRegistry::new();
registry.register(ExitPoint::PasswordQuality, |ctx| {
    if ctx.data.len() >= 8 { ExitAction::Allow } else { ExitAction::Deny }
})?;

let ctx = ExitContext::new("USER1", "", "shortpw");
assert_eq!(registry.invoke(ExitPoint::PasswordQuality, &ctx), ExitAction::Deny);
```

## Dependencies

| Crate | Purpose |
|---|---|
| `thiserror` | Derive `Error` for `CryptoError` |
| `miette` | Diagnostic error reporting |

No external cryptography crate dependencies. All algorithms are simulated
internally for demonstration purposes.

## Testing

Each module includes extensive `#[cfg(test)]` sections:

| Module | Tests | Covers |
|---|---|---|
| `symmetric.rs` | 25 | Encrypt/decrypt round-trip, key length validation, empty data, hash consistency, HMAC verify/tamper, PRNG length, config parsing |
| `asymmetric.rs` | 20 | RSA 2048/4096 generation, invalid sizes, sign/verify PKCS1/PSS, wrong key/tampered data, EC P-256/384/521, ECDSA round-trip, ECDH |
| `keystore.rs` | 15 | CKDS/PKDS/TKDS insert/get/delete, duplicates, sorted lists, lifecycle generate/delete/rekey, master key wrap/unwrap |
| `racf_crypto.rs` | 16 | Profile permissions, overwrite, wildcard `*`/`**`, discrete/generic access checks, service access, case insensitivity, level ordering |
| `seclabel.rs` | 17 | Level ordering, category normalization, label display, add/remove categories, dominance checks (8 scenarios), MLS modes |
| `audit.rs` | 14 | SMF 80/81/83 display and fields, IRRDBU00 unload format, audit trail queries by user/class/failures |
| `exits.rs` | 18 | Exit registration/invocation, duplicate prevention, deregister, password quality, search exact/wildcard, verify clean/orphaned/duplicate, split/merge round-robin |

```sh
cargo test -p open-mainframe-crypto
```

## Limitations and Future Work

- All cryptographic operations use **simulated algorithms** (XOR transforms,
  LCG PRNG) — not suitable for production security
- RSA signature simulation requires identical bytes for public/private keys
  to round-trip; real asymmetric key math is not implemented
- ECDH shared secret computation is a simple element-wise product, not
  elliptic curve scalar multiplication
- HMAC uses the simulated hash, not a real SHA implementation
- TKDS does not enforce PKCS#11 attribute constraints (e.g., mutual
  exclusivity of CKA_ENCRYPT and CKA_DECRYPT)
- Key lifecycle does not implement key expiration or rotation policies
- RACF wildcard matching does not implement the full z/OS generic profile
  specificity ordering algorithm
- SMF records use string timestamps rather than binary STCK format
- IRRDBU00 output format is simplified compared to the real 80-byte
  fixed-record layout
- Exit handlers are closures without parameter block simulation (no RACROUTE
  work area)
- IRRUT200 verify checks are basic (orphans and duplicates only); missing
  cross-reference and index validation
