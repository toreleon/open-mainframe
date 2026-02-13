//! Integration tests for MockTerminal and session flow.

use std::collections::HashMap;

use zos_cics::bms::BmsMap;
use zos_cics::runtime::eib::aid;
use zos_cics::terminal::SendMapOptions;
use zos_tui::mock::MockTerminal;

#[test]
fn test_mock_terminal_records_send_map() {
    let mut mock = MockTerminal::new();

    let map = BmsMap {
        name: "TESTMAP".to_string(),
        size: (24, 80),
        ..Default::default()
    };

    let mut data = HashMap::new();
    data.insert("FIELD1".to_string(), b"Hello".to_vec());

    let opts = SendMapOptions {
        erase: true,
        freekb: true,
        ..Default::default()
    };

    use zos_cics::terminal::TerminalCallback;
    mock.on_send_map(&map, &data, &opts);

    assert_eq!(mock.send_map_count(), 1);
    let event = mock.last_send_map().unwrap();
    assert_eq!(event.map_name, "TESTMAP");
    assert!(event.erase);
    assert_eq!(
        event.data.get("FIELD1").map(|v| v.as_slice()),
        Some(b"Hello".as_slice())
    );
}

#[test]
fn test_mock_terminal_queued_input() {
    let mut mock = MockTerminal::new();

    // Queue credentials input
    let mut fields = HashMap::new();
    fields.insert("USRIDI".to_string(), b"ADMIN   ".to_vec());
    fields.insert("PASSWI".to_string(), b"SECRET  ".to_vec());
    mock.queue_input(aid::ENTER, fields);

    // Then queue PF3 to exit
    mock.queue_pf3();

    let map = BmsMap::default();

    use zos_cics::terminal::TerminalCallback;

    // First receive: should get credentials with ENTER
    let (aid1, fields1) = mock.on_receive_map(&map).unwrap();
    assert_eq!(aid1, aid::ENTER);
    assert_eq!(fields1.get("USRIDI").map(|v| v.as_slice()), Some(b"ADMIN   ".as_slice()));

    // Second receive: should get PF3
    let (aid2, _) = mock.on_receive_map(&map).unwrap();
    assert_eq!(aid2, aid::PF3);
}

#[test]
fn test_mock_terminal_multiple_send_maps() {
    let mut mock = MockTerminal::new();

    use zos_cics::terminal::TerminalCallback;

    // Simulate XCTL chain: sign-on → menu → detail
    let screens = ["COSGN0A", "COMEN0A", "COACTVW"];
    for name in &screens {
        let map = BmsMap {
            name: name.to_string(),
            size: (24, 80),
            ..Default::default()
        };
        mock.on_send_map(&map, &HashMap::new(), &SendMapOptions::initial());
    }

    assert_eq!(mock.send_map_count(), 3);
    assert_eq!(mock.send_map_events[0].map_name, "COSGN0A");
    assert_eq!(mock.send_map_events[1].map_name, "COMEN0A");
    assert_eq!(mock.send_map_events[2].map_name, "COACTVW");
}

#[test]
fn test_mock_terminal_send_text() {
    let mut mock = MockTerminal::new();

    use zos_cics::terminal::TerminalCallback;

    mock.on_send_text("Welcome to CardDemo", true);
    mock.on_send_text("Processing complete", false);

    assert_eq!(mock.send_text_count(), 2);
    assert_eq!(mock.send_text_events[0].text, "Welcome to CardDemo");
    assert!(mock.send_text_events[0].erase);
    assert_eq!(mock.send_text_events[1].text, "Processing complete");
    assert!(!mock.send_text_events[1].erase);
}

#[test]
fn test_mock_terminal_clear_events() {
    let mut mock = MockTerminal::new();

    use zos_cics::terminal::TerminalCallback;

    mock.on_send_text("Test", false);
    assert_eq!(mock.send_text_count(), 1);

    mock.clear_events();
    assert_eq!(mock.send_text_count(), 0);
    assert_eq!(mock.send_map_count(), 0);
}

#[test]
fn test_mock_terminal_default_aid_when_queue_empty() {
    let mut mock = MockTerminal::new();
    let map = BmsMap::default();

    use zos_cics::terminal::TerminalCallback;

    // No input queued - should return default ENTER
    let (aid_code, fields) = mock.on_receive_map(&map).unwrap();
    assert_eq!(aid_code, aid::ENTER);
    assert!(fields.is_empty());
}
