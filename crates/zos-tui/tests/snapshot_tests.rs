//! Screen snapshot tests for BMS map rendering.
//!
//! These tests parse BMS map definitions, build field tables, and verify
//! that the rendered screen output matches expected patterns.

use zos_cics::bms::BmsParser;
use zos_tui::fields::FieldTable;

/// Parse a BMS source and return the first map.
fn parse_first_map(source: &str) -> zos_cics::bms::BmsMap {
    let mut parser = BmsParser::new();
    let mapset = parser.parse(source).unwrap();
    assert!(!mapset.maps.is_empty(), "BMS source must contain at least one map");
    mapset.maps[0].clone()
}

/// A simple sign-on screen BMS definition.
const SIGNON_BMS: &str = r#"
COSGN00  DFHMSD TYPE=MAP,LANG=COBOL,TIOAPFX=YES
COSGN0A  DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,25),LENGTH=30,ATTRB=(PROT,BRT),                X
               INITIAL='AWS CardDemo Sign-on Screen'
BLANK1   DFHMDF POS=(3,1),LENGTH=78,ATTRB=(PROT)
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

/// A menu screen BMS definition.
const MENU_BMS: &str = r#"
COMEN00  DFHMSD TYPE=MAP,LANG=COBOL,TIOAPFX=YES
COMEN0A  DFHMDI SIZE=(24,80)
TITLE    DFHMDF POS=(1,25),LENGTH=30,ATTRB=(PROT,BRT),                X
               INITIAL='AWS CardDemo Main Menu'
OPT1     DFHMDF POS=(5,10),LENGTH=40,ATTRB=(PROT),                    X
               INITIAL='1. View Account Details'
OPT2     DFHMDF POS=(6,10),LENGTH=40,ATTRB=(PROT),                    X
               INITIAL='2. View Transaction History'
OPT3     DFHMDF POS=(7,10),LENGTH=40,ATTRB=(PROT),                    X
               INITIAL='3. Update Account'
OPT4     DFHMDF POS=(8,10),LENGTH=40,ATTRB=(PROT),                    X
               INITIAL='4. Bill Payment'
OPTL     DFHMDF POS=(12,10),LENGTH=12,ATTRB=(PROT),INITIAL='Selection :'
OPTI     DFHMDF POS=(12,23),LENGTH=1,ATTRB=(NUM,UNPROT,IC)
OPTF     DFHMDF POS=(12,25),LENGTH=1,ATTRB=(ASKIP)
ERRMSG   DFHMDF POS=(15,10),LENGTH=60,ATTRB=(PROT,BRT)
MSG1     DFHMDF POS=(22,10),LENGTH=50,ATTRB=(PROT),                   X
               INITIAL='Enter selection, PF3=Exit'
         DFHMSD TYPE=FINAL
"#;

#[test]
fn test_signon_screen_field_table() {
    let map = parse_first_map(SIGNON_BMS);

    assert_eq!(map.name, "COSGN0A");
    assert_eq!(map.size, (24, 80));

    let table = FieldTable::from_bms_map(&map);

    // Should have fields (exact count depends on BMS parser's handling of filler fields)
    assert!(table.fields().len() >= 9, "Expected at least 9 fields, got {}", table.fields().len());

    // Should have 2 input fields (USRIDI and PASSWI)
    assert_eq!(table.input_field_count(), 2);

    // USRIDI field should have IC and be the active field
    let active = table.active_field().unwrap();
    assert_eq!(active.name, "USRIDI");
    assert_eq!(active.row, 7);
    assert_eq!(active.col, 21);
    assert_eq!(active.length, 8);
    assert!(!active.attribute.is_protected());
}

#[test]
fn test_signon_field_content_initial() {
    let map = parse_first_map(SIGNON_BMS);
    let table = FieldTable::from_bms_map(&map);

    // TITLE field should have initial text
    let title = table.fields().iter().find(|f| f.name == "TITLE").unwrap();
    let content_str = String::from_utf8_lossy(&title.content);
    assert!(content_str.starts_with("AWS CardDemo Sign-on Screen"));

    // USRIDL label
    let label = table.fields().iter().find(|f| f.name == "USRIDL").unwrap();
    let label_str = String::from_utf8_lossy(&label.content);
    assert_eq!(label_str.trim(), "User ID :");
}

#[test]
fn test_signon_field_attributes() {
    let map = parse_first_map(SIGNON_BMS);
    let table = FieldTable::from_bms_map(&map);

    // TITLE should be protected and bright
    let title = table.fields().iter().find(|f| f.name == "TITLE").unwrap();
    assert!(title.attribute.is_protected());
    assert!(title.attribute.is_bright());

    // PASSWI should be dark (hidden password)
    let passwi = table.fields().iter().find(|f| f.name == "PASSWI").unwrap();
    assert!(!passwi.attribute.is_protected());
    assert!(passwi.attribute.is_dark());

    // USRIDI should be unprotected (input)
    let usridi = table.fields().iter().find(|f| f.name == "USRIDI").unwrap();
    assert!(!usridi.attribute.is_protected());
}

#[test]
fn test_signon_user_input_simulation() {
    let map = parse_first_map(SIGNON_BMS);
    let mut table = FieldTable::from_bms_map(&map);

    // Type "ADMIN" into the User ID field
    assert!(table.type_char('A'));
    assert!(table.type_char('D'));
    assert!(table.type_char('M'));
    assert!(table.type_char('I'));
    assert!(table.type_char('N'));

    let userid = table.active_field().unwrap();
    assert_eq!(&userid.content[..5], b"ADMIN");
    assert!(userid.modified);

    // Tab to password field
    table.tab_forward();
    let password = table.active_field().unwrap();
    assert_eq!(password.name, "PASSWI");

    // Type password
    assert!(table.type_char('S'));
    assert!(table.type_char('E'));
    assert!(table.type_char('C'));
    assert!(table.type_char('R'));
    assert!(table.type_char('E'));
    assert!(table.type_char('T'));

    // Check modified fields
    let modified = table.get_modified_fields();
    assert_eq!(modified.len(), 2);

    // Should contain both USRIDI and PASSWI
    let names: Vec<&str> = modified.iter().map(|(n, _)| n.as_str()).collect();
    assert!(names.contains(&"USRIDI"));
    assert!(names.contains(&"PASSWI"));
}

#[test]
fn test_menu_screen_structure() {
    let map = parse_first_map(MENU_BMS);

    assert_eq!(map.name, "COMEN0A");

    let table = FieldTable::from_bms_map(&map);

    // Should have 1 input field (OPTI for menu selection)
    assert_eq!(table.input_field_count(), 1);

    // Input field should be numeric
    let opti = table.active_field().unwrap();
    assert_eq!(opti.name, "OPTI");
    assert!(opti.is_numeric());
    assert_eq!(opti.length, 1);
}

#[test]
fn test_menu_numeric_validation() {
    let map = parse_first_map(MENU_BMS);
    let mut table = FieldTable::from_bms_map(&map);

    // Should accept digit
    assert!(table.type_char('1'));

    // Should reject letter
    table.home();
    assert!(!table.type_char('A'));
}

#[test]
fn test_field_data_update() {
    let map = parse_first_map(SIGNON_BMS);
    let mut table = FieldTable::from_bms_map(&map);

    // Simulate setting field data (as SEND MAP DATAONLY would)
    table.set_field_data("ERRMSG", b"Invalid credentials");

    let errmsg = table.fields().iter().find(|f| f.name == "ERRMSG").unwrap();
    let content = String::from_utf8_lossy(&errmsg.content);
    assert!(content.starts_with("Invalid credentials"));
}

#[test]
fn test_clear_unprotected_fields() {
    let map = parse_first_map(SIGNON_BMS);
    let mut table = FieldTable::from_bms_map(&map);

    // Type something
    table.type_char('X');
    table.type_char('Y');

    // Clear unprotected
    table.clear_unprotected();

    // All input fields should be empty
    let fields = table.get_all_input_fields();
    for (_, data) in &fields {
        assert!(data.iter().all(|&b| b == b' '), "Field should be cleared");
    }
}

#[test]
fn test_screen_cursor_position() {
    let map = parse_first_map(SIGNON_BMS);
    let table = FieldTable::from_bms_map(&map);

    // Cursor should be at USRIDI field (row 7, col 21)
    let pos = table.cursor_position().unwrap();
    assert_eq!(pos.row, 7);
    assert_eq!(pos.col, 21);
}

#[test]
fn test_field_table_tab_wrapping() {
    let map = parse_first_map(SIGNON_BMS);
    let mut table = FieldTable::from_bms_map(&map);

    // Start at USRIDI (first input field with IC)
    assert_eq!(table.active_field().unwrap().name, "USRIDI");

    // Tab to PASSWI
    table.tab_forward();
    assert_eq!(table.active_field().unwrap().name, "PASSWI");

    // Tab wraps back to USRIDI
    table.tab_forward();
    assert_eq!(table.active_field().unwrap().name, "USRIDI");

    // Back-tab to PASSWI
    table.tab_backward();
    assert_eq!(table.active_field().unwrap().name, "PASSWI");
}
