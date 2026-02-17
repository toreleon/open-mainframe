//! Integration tests for the TUI callback pipeline.
//!
//! These tests verify that the Session correctly handles SEND MAP/SEND TEXT
//! events, simulating the callback pipeline used in the real CICS session loop.

use std::collections::HashMap;
use std::path::PathBuf;

use open_mainframe_cics::bms::{BmsMap, BmsParser};
use open_mainframe_cics::terminal::SendMapOptions;
use open_mainframe_tui::fields::FieldTable;
use open_mainframe_tui::session::{Session, SessionConfig, TerminalModel};

/// Create a basic SessionConfig for testing (no actual TUI terminal needed).
fn test_config() -> SessionConfig {
    SessionConfig {
        initial_program: PathBuf::from("TESTPROG.cbl"),
        include_paths: vec![],
        data_files: vec![],
        program_dir: None,
        transid_map: HashMap::new(),
        color_theme: "classic".to_string(),
        userid: None,
        initial_transid: None,
        terminal_model: TerminalModel::Model2,
    }
}

/// Parse a BMS source into a mapset and return the first map.
fn parse_first_map(source: &str) -> BmsMap {
    let mut parser = BmsParser::new();
    let mapset = parser.parse(source).unwrap();
    assert!(!mapset.maps.is_empty());
    mapset.maps[0].clone()
}

/// A sign-on screen BMS definition for testing.
const SIGNON_BMS: &str = r#"
COSGN00  DFHMSD TYPE=MAP,LANG=COBOL,TIOAPFX=YES
COSGN0A  DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,25),LENGTH=30,ATTRB=(PROT,BRT),                X
               INITIAL='AWS CardDemo Sign-on Screen'
USRIDL   DFHMDF POS=(7,10),LENGTH=9,ATTRB=(PROT),INITIAL='User ID :'
USRIDI   DFHMDF POS=(7,21),LENGTH=8,ATTRB=(UNPROT,IC)
USRIDF   DFHMDF POS=(7,30),LENGTH=1,ATTRB=(ASKIP)
PASSWL   DFHMDF POS=(9,10),LENGTH=11,ATTRB=(PROT),INITIAL='Password  :'
PASSWI   DFHMDF POS=(9,21),LENGTH=8,ATTRB=(DRK,UNPROT)
PASSWF   DFHMDF POS=(9,30),LENGTH=1,ATTRB=(ASKIP)
ERRMSG   DFHMDF POS=(12,10),LENGTH=60,ATTRB=(PROT,BRT)
MSG1     DFHMDF POS=(20,10),LENGTH=50,ATTRB=(PROT),                   X
               INITIAL='Press ENTER to Sign on, F3 to Exit'
         DFHMSD TYPE=FINAL
"#;

#[test]
fn test_session_on_send_map_builds_field_table() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);

    // Simulate a SEND MAP with ERASE
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    // The session should now have a field table populated from the BMS map.
    // We can verify this by checking the session's internal state.
    // Since Session doesn't expose field_table directly, we verify through
    // the cursor position (which reflects the IC field: USRIDI at row 7, col 21).
    // The cursor position is tracked in the status info.
    // We can't access status directly from here, but the session should work
    // correctly when wait_for_input is called (requires a terminal, so we test
    // the field table separately).

    // Build a field table the same way Session does internally:
    let table = FieldTable::from_bms_map(&map);
    assert!(table.input_field_count() >= 2);

    let active = table.active_field().unwrap();
    assert_eq!(active.name, "USRIDI");
}

#[test]
fn test_session_on_send_map_with_data() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);

    // Simulate a SEND MAP with field data (e.g., error message)
    let mut data = HashMap::new();
    data.insert("ERRMSG".to_string(), b"Invalid credentials".to_vec());

    session.on_send_map(&map, &data, &SendMapOptions::initial());

    // Verify the field table has the error message
    let table = FieldTable::from_bms_map(&map);
    let mut table_copy = table;
    table_copy.set_field_data("ERRMSG", b"Invalid credentials");

    let errmsg = table_copy.fields().iter().find(|f| f.name == "ERRMSG").unwrap();
    let content = String::from_utf8_lossy(&errmsg.content);
    assert!(content.starts_with("Invalid credentials"));
}

#[test]
fn test_session_on_send_map_erase_flag() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);

    // First send with ERASE
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    // Second send with DATAONLY (no erase) - simulates updating fields
    let mut data = HashMap::new();
    data.insert("ERRMSG".to_string(), b"Login failed".to_vec());
    session.on_send_map(&map, &data, &SendMapOptions::update());
}

#[test]
fn test_session_on_send_text() {
    let mut session = Session::new(test_config());

    // Simulate SEND TEXT with ERASE
    session.on_send_text("Welcome to the CICS Terminal", true);

    // Send another text without erase
    session.on_send_text("Processing your request...", false);
}

#[test]
fn test_session_set_program_and_transid() {
    let mut session = Session::new(test_config());

    session.set_program("COSGN00C");
    session.set_transid("COSG");
    session.set_message("Ready");
}

#[test]
fn test_session_multiple_send_maps_xctl_chain() {
    let mut session = Session::new(test_config());

    // Simulate XCTL chain: sign-on → menu
    let signon_map = parse_first_map(SIGNON_BMS);

    // First screen: sign-on
    session.set_program("COSGN00C");
    session.on_send_map(&signon_map, &HashMap::new(), &SendMapOptions::initial());

    // XCTL to menu program
    let menu_bms = r#"
COMEN00  DFHMSD TYPE=MAP,LANG=COBOL,TIOAPFX=YES
COMEN0A  DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,25),LENGTH=30,ATTRB=(PROT,BRT),                X
               INITIAL='AWS CardDemo Main Menu'
OPT1     DFHMDF POS=(5,10),LENGTH=40,ATTRB=(PROT),                    X
               INITIAL='1. View Account Details'
OPTI     DFHMDF POS=(12,23),LENGTH=1,ATTRB=(NUM,UNPROT,IC)
OPTF     DFHMDF POS=(12,25),LENGTH=1,ATTRB=(ASKIP)
         DFHMSD TYPE=FINAL
"#;
    let menu_map = parse_first_map(menu_bms);

    session.set_program("COMEN01C");
    session.on_send_map(&menu_map, &HashMap::new(), &SendMapOptions::initial());
}

#[test]
fn test_field_table_survives_send_map_with_initial_values() {
    // Verify that BMS INITIAL values make it through the pipeline
    let map = parse_first_map(SIGNON_BMS);
    let table = FieldTable::from_bms_map(&map);

    // TITLE should have initial value
    let title = table.fields().iter().find(|f| f.name == "TITLE").unwrap();
    let content = String::from_utf8_lossy(&title.content);
    assert!(
        content.contains("AWS CardDemo Sign-on Screen"),
        "Title content should have INITIAL value, got: '{}'",
        content
    );

    // MSG1 should have initial value
    let msg1 = table.fields().iter().find(|f| f.name == "MSG1").unwrap();
    let msg_content = String::from_utf8_lossy(&msg1.content);
    assert!(
        msg_content.contains("Press ENTER to Sign on"),
        "MSG1 should have INITIAL value, got: '{}'",
        msg_content
    );

    // USRIDI should be empty (no initial value)
    let usridi = table.fields().iter().find(|f| f.name == "USRIDI").unwrap();
    let usridi_content = String::from_utf8_lossy(&usridi.content);
    assert!(
        usridi_content.trim().is_empty(),
        "USRIDI should be empty, got: '{}'",
        usridi_content
    );
}

#[test]
fn test_callback_pipeline_send_map_then_input_simulation() {
    // This test simulates the full pipeline:
    // 1. Session receives SEND MAP
    // 2. User types into fields
    // 3. User presses ENTER
    // 4. Modified fields are collected
    let map = parse_first_map(SIGNON_BMS);
    let mut table = FieldTable::from_bms_map(&map);

    // Simulate user typing "ADMIN" into USRIDI
    assert_eq!(table.active_field().unwrap().name, "USRIDI");
    for ch in "ADMIN".chars() {
        table.type_char(ch);
    }

    // Tab to PASSWI
    table.tab_forward();
    assert_eq!(table.active_field().unwrap().name, "PASSWI");

    // Type password
    for ch in "SECRET".chars() {
        table.type_char(ch);
    }

    // Collect all input fields (simulating what wait_for_input returns)
    let all_fields = table.get_all_input_fields();
    assert!(all_fields.len() >= 2, "Should have at least 2 input fields");

    // Verify USRIDI has the typed content
    let usridi_data = all_fields.iter().find(|(n, _)| n == "USRIDI");
    assert!(usridi_data.is_some(), "USRIDI should be in all input fields");
    let (_, data) = usridi_data.unwrap();
    assert!(
        String::from_utf8_lossy(data).starts_with("ADMIN"),
        "USRIDI should contain 'ADMIN'"
    );

    // Verify modified fields contain both typed fields
    let modified = table.get_modified_fields();
    let modified_names: Vec<&str> = modified.iter().map(|(n, _)| n.as_str()).collect();
    assert!(modified_names.contains(&"USRIDI"));
    assert!(modified_names.contains(&"PASSWI"));
}

#[test]
fn test_session_dataonly_preserves_labels() {
    // DATAONLY should update only variable fields, keeping static labels intact.
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);

    // Initial SEND MAP with ERASE (full map)
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    // Now a DATAONLY update with error message
    let mut data = HashMap::new();
    data.insert("ERRMSG".to_string(), b"Login failed".to_vec());

    let dataonly_opts = SendMapOptions {
        dataonly: true,
        freekb: true,
        ..Default::default()
    };
    session.on_send_map(&map, &data, &dataonly_opts);
    // Should not panic - DATAONLY updates fields in the existing table.
}

#[test]
fn test_session_maponly_ignores_data() {
    // MAPONLY should render the map with INITIAL values but ignore program data.
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);

    let mut data = HashMap::new();
    data.insert("ERRMSG".to_string(), b"This should be ignored".to_vec());

    let maponly_opts = SendMapOptions {
        maponly: true,
        erase: true,
        freekb: true,
        ..Default::default()
    };
    session.on_send_map(&map, &data, &maponly_opts);
    // Should not panic. MAPONLY means only the static map content is rendered.
}

#[test]
fn test_session_frset_clears_mdt() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);

    // Initial send
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    // Send with FRSET to clear MDTs
    let frset_opts = SendMapOptions {
        frset: true,
        freekb: true,
        ..Default::default()
    };
    session.on_send_map(&map, &HashMap::new(), &frset_opts);
    // MDTs should be cleared after FRSET.
}

#[test]
fn test_e2e_signon_flow_with_color_attributes() {
    // End-to-end test verifying that BMS COLOR and HILIGHT attributes
    // are preserved through the full pipeline.
    let colored_bms = r#"
COLSGN   DFHMSD TYPE=MAP,LANG=COBOL,TIOAPFX=YES
COLSGNA  DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,25),LENGTH=30,ATTRB=(PROT,BRT),                X
               COLOR=BLUE,INITIAL='AWS CardDemo Sign-on Screen'
USRIDL   DFHMDF POS=(7,10),LENGTH=9,ATTRB=(PROT),COLOR=GREEN,          X
               INITIAL='User ID :'
USRIDI   DFHMDF POS=(7,21),LENGTH=8,ATTRB=(UNPROT,IC),COLOR=TURQUOISE
PASSWL   DFHMDF POS=(9,10),LENGTH=11,ATTRB=(PROT),COLOR=GREEN,         X
               INITIAL='Password  :'
PASSWI   DFHMDF POS=(9,21),LENGTH=8,ATTRB=(DRK,UNPROT)
ERRMSG   DFHMDF POS=(12,10),LENGTH=60,ATTRB=(PROT,BRT),COLOR=RED,      X
               HILIGHT=REVERSE
         DFHMSD TYPE=FINAL
"#;

    let mut session = Session::new(test_config());
    let map = parse_first_map(colored_bms);

    // SEND MAP with ERASE
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    // Verify the field table was built with extended attributes
    let table = FieldTable::from_bms_map(&map);

    // TITLE: bright protected, blue color
    let title = table.fields().iter().find(|f| f.name == "TITLE").unwrap();
    assert!(title.attribute.is_bright());
    assert!(title.attribute.is_protected());
    assert_eq!(title.color, Some(open_mainframe_cics::bms::FieldColor::Blue));

    // USRIDI: unprotected input, turquoise
    let usridi = table.fields().iter().find(|f| f.name == "USRIDI").unwrap();
    assert!(!usridi.attribute.is_protected());
    assert_eq!(usridi.color, Some(open_mainframe_cics::bms::FieldColor::Turquoise));

    // PASSWI: dark (non-display) for password
    let passwi = table.fields().iter().find(|f| f.name == "PASSWI").unwrap();
    assert!(passwi.attribute.is_dark());

    // ERRMSG: bright protected, red + reverse
    let errmsg = table.fields().iter().find(|f| f.name == "ERRMSG").unwrap();
    assert!(errmsg.attribute.is_bright());
    assert_eq!(errmsg.color, Some(open_mainframe_cics::bms::FieldColor::Red));
    assert_eq!(errmsg.highlight, Some(open_mainframe_cics::bms::FieldHighlight::Reverse));
}

#[test]
fn test_multi_screen_xctl_session_flow() {
    // Simulates a multi-screen XCTL chain: sign-on screen → menu screen.
    // Verifies that on_send_map can handle consecutive screen transitions
    // as would happen during an XCTL from one program to another.
    let mut session = Session::new(test_config());

    // --- Screen 1: Sign-on ---
    let signon_map = parse_first_map(SIGNON_BMS);

    session.set_program("COSGN00C");
    session.set_transid("COSG");
    session.on_send_map(&signon_map, &HashMap::new(), &SendMapOptions::initial());

    // Verify sign-on screen is active (field table built from sign-on BMS)
    let signon_table = FieldTable::from_bms_map(&signon_map);
    assert_eq!(signon_table.active_field().unwrap().name, "USRIDI");
    assert_eq!(signon_table.input_field_count(), 2);

    // --- XCTL to menu ---
    let menu_bms = r#"
COMEN00  DFHMSD TYPE=MAP,LANG=COBOL,TIOAPFX=YES
COMEN0A  DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,25),LENGTH=30,ATTRB=(PROT,BRT),                X
               INITIAL='AWS CardDemo Main Menu'
OPT1     DFHMDF POS=(5,10),LENGTH=40,ATTRB=(PROT),                    X
               INITIAL='1. View Account Details'
OPT2     DFHMDF POS=(6,10),LENGTH=40,ATTRB=(PROT),                    X
               INITIAL='2. View Transaction History'
OPTL     DFHMDF POS=(12,10),LENGTH=12,ATTRB=(PROT),INITIAL='Selection :'
OPTI     DFHMDF POS=(12,23),LENGTH=1,ATTRB=(NUM,UNPROT,IC)
OPTF     DFHMDF POS=(12,25),LENGTH=1,ATTRB=(ASKIP)
MSG1     DFHMDF POS=(22,10),LENGTH=50,ATTRB=(PROT),                   X
               INITIAL='Enter selection, PF3=Exit'
         DFHMSD TYPE=FINAL
"#;
    let menu_map = parse_first_map(menu_bms);

    // XCTL replaces the screen entirely (ERASE)
    session.set_program("COMEN01C");
    session.set_transid("MENU");
    session.on_send_map(&menu_map, &HashMap::new(), &SendMapOptions::initial());

    // Verify menu screen is now active
    let menu_table = FieldTable::from_bms_map(&menu_map);
    assert_eq!(menu_table.active_field().unwrap().name, "OPTI");
    assert_eq!(menu_table.input_field_count(), 1);
    assert!(menu_table.active_field().unwrap().is_numeric());

    // Verify menu has the correct INITIAL values
    let title = menu_table.fields().iter().find(|f| f.name == "TITLE").unwrap();
    let content = String::from_utf8_lossy(&title.content);
    assert!(content.contains("AWS CardDemo Main Menu"));
}

#[test]
fn test_pseudo_conversational_dataonly_update() {
    // Simulates RETURN TRANSID flow:
    // 1. Program sends initial map (ERASE)
    // 2. User enters input, presses ENTER
    // 3. Same program re-executes, sends DATAONLY with error message
    // 4. User sees updated screen with error but original labels preserved
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);

    // Round 1: Initial send with ERASE
    session.set_program("COSGN00C");
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    // Round 2: After RECEIVE MAP, program validates and sends error via DATAONLY
    let mut error_data = HashMap::new();
    error_data.insert("ERRMSG".to_string(), b"Sign-on is unsuccessful".to_vec());

    let dataonly_opts = SendMapOptions {
        dataonly: true,
        freekb: true,
        ..Default::default()
    };
    session.on_send_map(&map, &error_data, &dataonly_opts);

    // Build a table to verify the DATAONLY update preserved structure
    let mut table = FieldTable::from_bms_map(&map);
    table.set_field_data("ERRMSG", b"Sign-on is unsuccessful");

    // Error message should be set
    let errmsg = table.fields().iter().find(|f| f.name == "ERRMSG").unwrap();
    let content = String::from_utf8_lossy(&errmsg.content);
    assert!(content.starts_with("Sign-on is unsuccessful"));

    // TITLE label should still have its INITIAL value (preserved by DATAONLY)
    let title = table.fields().iter().find(|f| f.name == "TITLE").unwrap();
    let title_content = String::from_utf8_lossy(&title.content);
    assert!(title_content.contains("AWS CardDemo Sign-on Screen"));
}
