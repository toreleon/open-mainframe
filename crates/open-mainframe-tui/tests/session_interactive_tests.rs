//! Integration tests for interactive TUI session testing.
//!
//! These tests exercise `Session::wait_for_input` with a ratatui `TestBackend`
//! and a `MockEventSource`, proving that the full render + input loop can be
//! driven programmatically without a real terminal.

use std::collections::HashMap;
use std::path::PathBuf;

use crossterm::event::KeyCode;
use ratatui::backend::TestBackend;
use ratatui::Terminal;

use open_mainframe_cics::bms::{BmsMap, BmsParser};
use open_mainframe_cics::runtime::eib::aid;
use open_mainframe_cics::terminal::SendMapOptions;
use open_mainframe_tui::event::MockEventSource;
use open_mainframe_tui::session::{Session, SessionConfig, TerminalModel};

/// Create a basic SessionConfig for testing.
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

/// Sign-on screen BMS definition for testing.
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

/// Create a Terminal<TestBackend> sized for a 3270 Model 2 screen + status.
fn test_terminal() -> Terminal<TestBackend> {
    // 24 rows for screen + 2 rows for status line
    Terminal::new(TestBackend::new(80, 26)).unwrap()
}

// ---------- Tests ----------

#[test]
fn test_wait_for_input_enter_returns_aid() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();
    events.push_key(KeyCode::Enter);

    let (aid_code, _fields) = session.wait_for_input(&mut terminal, &mut events).unwrap();
    assert_eq!(aid_code, aid::ENTER);
}

#[test]
fn test_wait_for_input_pf3_returns_aid() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();
    events.push_key(KeyCode::F(3));

    let (aid_code, _fields) = session.wait_for_input(&mut terminal, &mut events).unwrap();
    assert_eq!(aid_code, aid::PF3);
}

#[test]
fn test_wait_for_input_type_and_submit() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();

    // Type "ADMIN" into USRIDI (IC field, so it's already active)
    events.push_text("ADMIN");
    events.push_key(KeyCode::Enter);

    let (aid_code, fields) = session.wait_for_input(&mut terminal, &mut events).unwrap();
    assert_eq!(aid_code, aid::ENTER);

    // USRIDI should contain "ADMIN" (possibly padded)
    let usridi = fields.get("USRIDI");
    assert!(usridi.is_some(), "USRIDI should be in returned fields");
    let usridi_str = String::from_utf8_lossy(usridi.unwrap());
    assert!(
        usridi_str.starts_with("ADMIN"),
        "USRIDI should start with 'ADMIN', got: '{}'",
        usridi_str
    );
}

#[test]
fn test_wait_for_input_tab_navigation_and_type() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();

    // Type "USER1" into USRIDI
    events.push_text("USER1");
    // Tab to PASSWI
    events.push_key(KeyCode::Tab);
    // Type "PASS1" into PASSWI
    events.push_text("PASS1");
    // Submit
    events.push_key(KeyCode::Enter);

    let (aid_code, fields) = session.wait_for_input(&mut terminal, &mut events).unwrap();
    assert_eq!(aid_code, aid::ENTER);

    // Verify both fields have data
    let usridi_str = String::from_utf8_lossy(fields.get("USRIDI").expect("USRIDI missing"));
    assert!(
        usridi_str.starts_with("USER1"),
        "USRIDI should start with 'USER1', got: '{}'",
        usridi_str
    );

    let passwi_str = String::from_utf8_lossy(fields.get("PASSWI").expect("PASSWI missing"));
    assert!(
        passwi_str.starts_with("PASS1"),
        "PASSWI should start with 'PASS1', got: '{}'",
        passwi_str
    );
}

#[test]
fn test_wait_for_input_renders_screen_content() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();
    events.push_key(KeyCode::Enter);

    let _ = session.wait_for_input(&mut terminal, &mut events).unwrap();

    // Verify the TestBackend buffer contains expected screen content.
    let buffer = terminal.backend().buffer().clone();
    let mut screen_text = String::new();
    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            let cell = &buffer[(x, y)];
            screen_text.push_str(cell.symbol());
        }
    }

    // The sign-on title should appear in the rendered output
    assert!(
        screen_text.contains("AWS CardDemo Sign-on Screen"),
        "Screen should contain the title text"
    );

    // Static labels should be rendered
    assert!(
        screen_text.contains("User ID"),
        "Screen should contain 'User ID' label"
    );

    assert!(
        screen_text.contains("Password"),
        "Screen should contain 'Password' label"
    );
}

#[test]
fn test_ctrl_c_returns_interrupted() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();
    events.push_key_mod(KeyCode::Char('c'), crossterm::event::KeyModifiers::CONTROL);

    let result = session.wait_for_input(&mut terminal, &mut events);
    assert!(result.is_err(), "Ctrl+C should return an error");
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("interrupted") || err.to_string().contains("Interrupted"),
        "Error should indicate interruption, got: '{}'",
        err
    );
}

#[test]
fn test_empty_event_source_returns_interrupted() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();
    // No events queued -- should return Interrupted when queue is empty

    let result = session.wait_for_input(&mut terminal, &mut events);
    assert!(result.is_err(), "Empty event source should return error");
}

#[test]
fn test_wait_for_input_clear_key() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);
    session.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();

    // Type into field, then press CLEAR (Esc)
    events.push_text("ADMIN");
    events.push_key(KeyCode::Esc);

    let (aid_code, _fields) = session.wait_for_input(&mut terminal, &mut events).unwrap();
    assert_eq!(aid_code, aid::CLEAR);
}

#[test]
fn test_wait_for_input_with_error_message_data() {
    let mut session = Session::new(test_config());
    let map = parse_first_map(SIGNON_BMS);

    // Simulate SEND MAP with error message data
    let mut data = HashMap::new();
    data.insert("ERRMSG".to_string(), b"Invalid credentials".to_vec());
    session.on_send_map(&map, &data, &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();
    events.push_key(KeyCode::Enter);

    let _ = session.wait_for_input(&mut terminal, &mut events).unwrap();

    // Verify the error message appears in the rendered buffer
    let buffer = terminal.backend().buffer().clone();
    let mut screen_text = String::new();
    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            let cell = &buffer[(x, y)];
            screen_text.push_str(cell.symbol());
        }
    }

    assert!(
        screen_text.contains("Invalid credentials"),
        "Screen should contain the error message"
    );
}

#[test]
fn test_multiple_screens_in_sequence() {
    // Simulates what happens during XCTL: one screen replaces another.
    let mut session = Session::new(test_config());

    // First screen: sign-on
    let signon_map = parse_first_map(SIGNON_BMS);
    session.on_send_map(&signon_map, &HashMap::new(), &SendMapOptions::initial());

    let mut terminal = test_terminal();
    let mut events = MockEventSource::new();
    events.push_key(KeyCode::Enter);

    let (aid1, _) = session.wait_for_input(&mut terminal, &mut events).unwrap();
    assert_eq!(aid1, aid::ENTER);

    // Second screen: menu (XCTL replaces screen entirely)
    let menu_bms = r#"
COMEN00  DFHMSD TYPE=MAP,LANG=COBOL,TIOAPFX=YES
COMEN0A  DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,25),LENGTH=30,ATTRB=(PROT,BRT),                X
               INITIAL='AWS CardDemo Main Menu'
OPTI     DFHMDF POS=(12,23),LENGTH=1,ATTRB=(NUM,UNPROT,IC)
OPTF     DFHMDF POS=(12,25),LENGTH=1,ATTRB=(ASKIP)
         DFHMSD TYPE=FINAL
"#;
    let menu_map = parse_first_map(menu_bms);
    session.set_program("COMEN01C");
    session.on_send_map(&menu_map, &HashMap::new(), &SendMapOptions::initial());

    let mut events2 = MockEventSource::new();
    events2.push_text("1");
    events2.push_key(KeyCode::Enter);

    let (aid2, fields2) = session.wait_for_input(&mut terminal, &mut events2).unwrap();
    assert_eq!(aid2, aid::ENTER);

    // Verify the menu screen is now rendered
    let buffer = terminal.backend().buffer().clone();
    let mut screen_text = String::new();
    for y in 0..buffer.area.height {
        for x in 0..buffer.area.width {
            let cell = &buffer[(x, y)];
            screen_text.push_str(cell.symbol());
        }
    }
    assert!(
        screen_text.contains("AWS CardDemo Main Menu"),
        "Screen should show menu title after XCTL"
    );

    // The typed "1" should be in OPTI
    let opti = fields2.get("OPTI");
    assert!(opti.is_some(), "OPTI should be in returned fields");
}
