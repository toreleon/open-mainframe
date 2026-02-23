//! SYS-110.7 — Integration tests for PassTicket and certificate authentication flows.

use open_mainframe_racf::auth::{
    AuthService, PassTicketKeyType, PassTicketValidationResult,
};
use open_mainframe_racf::certificate::{
    CertType, CertUsage, CertificateManager, GencertParams, KeyAlgorithm, SubjectDN,
};
use open_mainframe_racf::database::RacfDatabase;
use open_mainframe_racf::saf::SafRouter;
use open_mainframe_racf::types::AccessLevel;

fn make_subject(cn: &str) -> SubjectDN {
    SubjectDN {
        cn: cn.to_string(),
        org: Some("TestOrg".to_string()),
        ..Default::default()
    }
}

fn setup_db() -> RacfDatabase {
    let mut db = RacfDatabase::new();
    db.add_group("DEPT01", "SYS1", "ADMIN1").unwrap();
    db.add_user("JSMITH", "DEPT01", "John Smith", "ADMIN1")
        .unwrap();

    let user = db.get_user_mut("JSMITH").unwrap();
    user.password_hash = Some("PASSWORD1".to_string());

    db
}

// ─────── PassTicket + RACROUTE VERIFY end-to-end ───────

/// Simulates z/OSMF authentication via PassTicket through SAF.
#[test]
fn test_zosmf_passticket_authentication() {
    let db = setup_db();
    let saf = SafRouter::new();
    let mut auth = AuthService::new();

    // Define PTKTDATA for z/OSMF.
    auth.rdefine_ptktdata(
        "ZOSMF",
        "ZOSMFSECRETKEY01",
        PassTicketKeyType::KeyMasked,
        None,
        false,
    );

    // z/OSMF generates a PassTicket for JSMITH.
    let ticket = auth.generate_passticket("JSMITH", "ZOSMF").unwrap();
    assert_eq!(ticket.ticket.len(), 8);

    // z/OSMF presents the PassTicket via RACROUTE VERIFY with APPL='ZOSMF'.
    let result = saf.verify_passticket(
        &mut auth,
        &db,
        "JSMITH",
        &ticket.ticket,
        Some("ZOSMF"),
    );
    assert!(
        result.is_authorized(),
        "z/OSMF PassTicket auth should succeed"
    );
}

/// Certificate-to-userid mapping authentication flow.
#[test]
fn test_tls_certificate_to_userid_mapping() {
    let mut cert_mgr = CertificateManager::new();

    // Set up certificate infrastructure.
    cert_mgr.gencert(&GencertParams {
        owner: "CERTAUTH".into(),
        label: "RootCA".into(),
        subject: make_subject("Enterprise Root CA"),
        algorithm: KeyAlgorithm::Rsa,
        key_size: 4096,
        not_before: "2024-01-01".into(),
        not_after: "2034-12-31".into(),
        cert_type: CertType::CertAuth,
    });

    // Generate a client certificate for JSMITH.
    cert_mgr.gencert(&GencertParams {
        owner: "JSMITH".into(),
        label: "ClientCert".into(),
        subject: SubjectDN {
            cn: "John Smith".to_string(),
            org: Some("MyOrg".to_string()),
            ou: Some("IT".to_string()),
            ..Default::default()
        },
        algorithm: KeyAlgorithm::Rsa,
        key_size: 2048,
        not_before: "2024-06-01".into(),
        not_after: "2026-06-01".into(),
        cert_type: CertType::Personal,
    });

    // Create a keyring and connect both certs.
    cert_mgr.addring("JSMITH", "TLSKeyring");
    cert_mgr.connect(
        "JSMITH",
        "TLSKeyring",
        "CERTAUTH",
        "RootCA",
        CertUsage::CertAuth,
        false,
    );
    cert_mgr.connect(
        "JSMITH",
        "TLSKeyring",
        "JSMITH",
        "ClientCert",
        CertUsage::Personal,
        true,
    );

    // Map the certificate subject to a RACF userid.
    cert_mgr.map("JSMITH", "CN=John Smith", None, "JSmithMap");

    // TLS client presents certificate → lookup the mapped userid.
    let userid = cert_mgr
        .lookup_mapping("CN=John Smith,OU=IT,O=MyOrg", "CN=Enterprise Root CA")
        .unwrap();
    assert_eq!(userid, "JSMITH");

    // Verify the keyring has both certificates.
    let ring = cert_mgr.get_ring("JSMITH", "TLSKeyring").unwrap();
    assert_eq!(ring.connections.len(), 2);
}

/// PassTicket replay protection under concurrent generation.
#[test]
fn test_passticket_concurrent_validation() {
    let mut auth = AuthService::new();
    auth.add_passticket_profile("CICSAPP1", "CONCURRENCYKEY");

    // Generate 100 PassTickets rapidly (simulates concurrent requests).
    let tickets: Vec<String> = (0..100)
        .map(|_| {
            auth.generate_passticket("JSMITH", "CICSAPP1")
                .unwrap()
                .ticket
        })
        .collect();

    // All tickets should be unique.
    let unique: std::collections::HashSet<&str> = tickets.iter().map(|t| t.as_str()).collect();
    assert_eq!(
        unique.len(),
        100,
        "all 100 tickets must be unique"
    );

    // Each ticket should validate exactly once.
    for ticket in &tickets {
        let r1 = auth.validate_passticket("JSMITH", "CICSAPP1", ticket);
        assert!(r1.is_valid(), "first validation should succeed: {:?}", r1);

        let r2 = auth.validate_passticket("JSMITH", "CICSAPP1", ticket);
        assert_eq!(
            r2,
            PassTicketValidationResult::Replay,
            "second validation should detect replay"
        );
    }
}

/// Full PTKTDATA lifecycle: define → generate → validate → replay → purge.
#[test]
fn test_ptktdata_full_lifecycle() {
    let mut auth = AuthService::new();

    // 1. No profile → generation fails.
    assert!(auth.generate_passticket("JSMITH", "MYAPP").is_none());

    // 2. RDEFINE PTKTDATA.
    auth.rdefine_ptktdata(
        "MYAPP",
        "E001193519561977",
        PassTicketKeyType::KeyMasked,
        None,
        false,
    );

    // 3. RLIST confirms profile.
    let listing = auth.rlist_ptktdata("MYAPP").unwrap();
    assert_eq!(listing.application, "MYAPP");
    assert_eq!(listing.key_type, PassTicketKeyType::KeyMasked);
    assert_eq!(listing.timeout_minutes, 10);

    // 4. Generate and validate.
    let ticket = auth.generate_passticket("JSMITH", "MYAPP").unwrap();
    assert!(
        auth.validate_passticket("JSMITH", "MYAPP", &ticket.ticket)
            .is_valid()
    );

    // 5. Replay is rejected.
    assert_eq!(
        auth.validate_passticket("JSMITH", "MYAPP", &ticket.ticket),
        PassTicketValidationResult::Replay
    );

    // 6. Invalid ticket.
    assert_eq!(
        auth.validate_passticket("JSMITH", "MYAPP", "BADTICKT"),
        PassTicketValidationResult::InvalidTicket
    );

    // 7. Purge expired replay entries (insert a fake expired entry).
    auth.used_passtickets.insert("fake:entry:ticket".into(), 0);
    auth.purge_expired_passtickets();
    assert!(!auth.used_passtickets.contains_key("fake:entry:ticket"));

    // 8. RDELETE PTKTDATA.
    assert!(auth.rdelete_ptktdata("MYAPP"));
    assert!(auth.generate_passticket("JSMITH", "MYAPP").is_none());
}

/// Certificate chain validation with valid chain.
#[test]
fn test_certificate_chain_validation_full_chain() {
    let mut mgr = CertificateManager::new();

    // Root CA (self-signed, trusted).
    mgr.gencert(&GencertParams {
        owner: "CERTAUTH".into(),
        label: "RootCA".into(),
        subject: make_subject("Root CA"),
        algorithm: KeyAlgorithm::Rsa,
        key_size: 4096,
        not_before: "2020-01-01".into(),
        not_after: "2040-12-31".into(),
        cert_type: CertType::CertAuth,
    });

    // Chain validation on the root itself.
    let result = mgr.checkcert_chain("CERTAUTH", "RootCA", "2025-06-01");
    assert!(result.valid);
    assert_eq!(result.chain.len(), 1);
}

/// Multi-subsystem PassTicket scenario: CICS and DB2 with different keys.
#[test]
fn test_multi_subsystem_passtickets() {
    let db = setup_db();
    let saf = SafRouter::new();
    let mut auth = AuthService::new();

    auth.add_passticket_profile("CICSAPP1", "CICS_KEY_001");
    auth.add_passticket_profile("DB2PROD", "DB2_KEY_002");

    // Generate tickets for both applications.
    let cics_ticket = auth.generate_passticket("JSMITH", "CICSAPP1").unwrap();
    let db2_ticket = auth.generate_passticket("JSMITH", "DB2PROD").unwrap();

    // Tickets should be different (different keys, different apps).
    assert_ne!(cics_ticket.ticket, db2_ticket.ticket);

    // Each validates against its own application.
    let r1 = saf.verify_passticket(
        &mut auth,
        &db,
        "JSMITH",
        &cics_ticket.ticket,
        Some("CICSAPP1"),
    );
    assert!(r1.is_authorized());

    let r2 = saf.verify_passticket(
        &mut auth,
        &db,
        "JSMITH",
        &db2_ticket.ticket,
        Some("DB2PROD"),
    );
    assert!(r2.is_authorized());

    // Cross-application fails.
    let r3 = saf.verify_passticket(
        &mut auth,
        &db,
        "JSMITH",
        &cics_ticket.ticket,
        Some("DB2PROD"),
    );
    assert!(!r3.is_authorized());
}

/// Mapping specificity: multiple maps, most specific wins.
#[test]
fn test_certificate_mapping_specificity_ranking() {
    let mut mgr = CertificateManager::new();

    // Broad org-level mapping.
    mgr.map("GENERIC", "O=MyOrg", None, "OrgMap");
    // Department-level mapping.
    mgr.map("DEPTUSER", "OU=Finance.O=MyOrg", None, "DeptMap");
    // User-specific mapping.
    mgr.map("JSMITH", "CN=John Smith.OU=Finance.O=MyOrg", None, "UserMap");

    // A cert with CN=John Smith.OU=Finance.O=MyOrg should match the user-specific mapping.
    let uid = mgr
        .lookup_mapping("CN=John Smith.OU=Finance.O=MyOrg.C=US", "CN=SomeCA")
        .unwrap();
    assert_eq!(uid, "JSMITH");

    // A cert with just OU=Finance.O=MyOrg matches the department mapping.
    let uid = mgr
        .lookup_mapping("OU=Finance.O=MyOrg.C=US", "CN=SomeCA")
        .unwrap();
    assert_eq!(uid, "DEPTUSER");

    // A cert with only O=MyOrg matches the org mapping.
    let uid = mgr.lookup_mapping("O=MyOrg.C=US", "CN=SomeCA").unwrap();
    assert_eq!(uid, "GENERIC");
}

/// RACROUTE AUTH + PassTicket VERIFY combined flow.
#[test]
fn test_auth_then_passticket_verify() {
    let mut db = setup_db();
    // Protect a FACILITY resource.
    db.add_dataset("PROD.DATA.**", AccessLevel::None, "ADMIN1").unwrap();
    db.permit_dataset("PROD.DATA.**", "JSMITH", AccessLevel::Read).unwrap();

    let saf = SafRouter::new();
    let mut auth = AuthService::new();
    auth.add_passticket_profile("ZOSMF", "ZOSMFKEY");

    // Step 1: Authenticate with PassTicket.
    let ticket = auth.generate_passticket("JSMITH", "ZOSMF").unwrap();
    let verify_result = saf.verify_passticket(
        &mut auth,
        &db,
        "JSMITH",
        &ticket.ticket,
        Some("ZOSMF"),
    );
    assert!(verify_result.is_authorized());

    // Step 2: Authorized user performs resource access check.
    let auth_result = saf.auth(
        &db,
        "DATASET",
        "PROD.DATA.FILE1",
        "JSMITH",
        AccessLevel::Read,
    );
    assert!(auth_result.is_authorized());
}
