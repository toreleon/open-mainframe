//! CICS terminal interface.
//!
//! Handles terminal I/O for CICS transactions including:
//! - SEND MAP / RECEIVE MAP for BMS maps
//! - SEND TEXT / RECEIVE for basic terminal I/O
//! - Screen buffer management

mod screen;
mod terminal;

pub use screen::{ScreenBuffer, ScreenPosition};
pub use terminal::{Terminal, TerminalState};

use crate::bms::{BmsMap, ScreenSize, MapRenderer};
use crate::{CicsError, CicsResult};
use std::collections::HashMap;

/// Callback trait for terminal I/O events.
///
/// Implement this trait to receive notifications when the CICS runtime
/// needs to send output to or receive input from a terminal. This enables
/// interactive TUI sessions to be driven by CICS program execution.
pub trait TerminalCallback: Send {
    /// Called when a SEND MAP command is executed.
    /// The implementation should render the map to the user's screen.
    fn on_send_map(
        &mut self,
        map: &BmsMap,
        data: &HashMap<String, Vec<u8>>,
        options: &SendMapOptions,
    );

    /// Called when a SEND TEXT command is executed.
    fn on_send_text(&mut self, text: &str, erase: bool);

    /// Called when a RECEIVE MAP command needs user input.
    /// Returns the AID key pressed and a map of field name -> field data.
    fn on_receive_map(
        &mut self,
        map: &BmsMap,
    ) -> CicsResult<(u8, HashMap<String, Vec<u8>>)>;

    /// Called when a RECEIVE command needs raw terminal input.
    /// Returns raw input data and the AID key.
    fn on_receive(&mut self, max_length: usize) -> CicsResult<(Vec<u8>, u8)>;
}

/// Terminal manager for CICS transactions.
pub struct TerminalManager {
    /// Active terminals
    terminals: HashMap<String, Terminal>,
    /// Default screen size
    default_size: ScreenSize,
}

impl TerminalManager {
    /// Create a new terminal manager.
    pub fn new() -> Self {
        Self {
            terminals: HashMap::new(),
            default_size: ScreenSize::Model2,
        }
    }

    /// Create or get a terminal.
    pub fn get_or_create(&mut self, terminal_id: &str) -> &mut Terminal {
        let size = self.default_size;
        self.terminals
            .entry(terminal_id.to_uppercase())
            .or_insert_with(|| Terminal::new(terminal_id, size))
    }

    /// Get terminal by ID.
    pub fn get(&self, terminal_id: &str) -> Option<&Terminal> {
        self.terminals.get(&terminal_id.to_uppercase())
    }

    /// Get terminal by ID (mutable).
    pub fn get_mut(&mut self, terminal_id: &str) -> Option<&mut Terminal> {
        self.terminals.get_mut(&terminal_id.to_uppercase())
    }

    /// Check if terminal exists.
    pub fn exists(&self, terminal_id: &str) -> bool {
        self.terminals.contains_key(&terminal_id.to_uppercase())
    }

    /// Execute SEND MAP.
    pub fn send_map(
        &mut self,
        terminal_id: &str,
        map: &BmsMap,
        data: &HashMap<String, Vec<u8>>,
        options: SendMapOptions,
    ) -> CicsResult<()> {
        let terminal = self.get_or_create(terminal_id);

        // Create renderer and set data
        let mut renderer = MapRenderer::new(terminal.screen_size());
        for (name, value) in data {
            renderer.set_field(name, value);
        }

        // Render map to screen
        let erase = options.erase || options.eraseaup;

        if erase {
            terminal.clear_screen();
        }

        // Apply map to terminal screen buffer
        terminal.apply_map(map, &renderer, options.cursor)?;

        // Record map name
        terminal.set_current_map(Some(map.name.clone()));

        Ok(())
    }

    /// Execute RECEIVE MAP.
    pub fn receive_map(
        &mut self,
        terminal_id: &str,
        map: &BmsMap,
    ) -> CicsResult<HashMap<String, Vec<u8>>> {
        let terminal = self.get_mut(terminal_id).ok_or_else(|| {
            CicsError::InvalidRequest(format!("Terminal {} not found", terminal_id))
        })?;

        // Extract field data from screen buffer
        terminal.extract_map_data(map)
    }

    /// Execute SEND TEXT.
    pub fn send_text(
        &mut self,
        terminal_id: &str,
        text: &str,
        options: SendTextOptions,
    ) -> CicsResult<()> {
        let terminal = self.get_or_create(terminal_id);

        if options.erase {
            terminal.clear_screen();
        }

        // Write text to screen
        terminal.write_text(text, options.header)?;

        Ok(())
    }

    /// Execute RECEIVE.
    pub fn receive(
        &mut self,
        terminal_id: &str,
        max_length: usize,
    ) -> CicsResult<(Vec<u8>, u8)> {
        let terminal = self.get_mut(terminal_id).ok_or_else(|| {
            CicsError::InvalidRequest(format!("Terminal {} not found", terminal_id))
        })?;

        // Get input data and AID
        let data = terminal.get_input_data(max_length)?;
        let aid = terminal.last_aid();

        Ok((data, aid))
    }

    /// Simulate terminal input for testing.
    pub fn simulate_input(
        &mut self,
        terminal_id: &str,
        aid: u8,
        fields: HashMap<String, Vec<u8>>,
    ) -> CicsResult<()> {
        let terminal = self.get_or_create(terminal_id);
        terminal.set_input(aid, fields);
        Ok(())
    }
}

impl Default for TerminalManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Options for SEND MAP command.
#[derive(Debug, Clone, Default)]
pub struct SendMapOptions {
    /// Erase screen before sending
    pub erase: bool,
    /// Erase all unprotected fields
    pub eraseaup: bool,
    /// Map only (no data)
    pub maponly: bool,
    /// Data only (no map formatting)
    pub dataonly: bool,
    /// Cursor position
    pub cursor: Option<ScreenPosition>,
    /// Free keyboard after send
    pub freekb: bool,
    /// Sound alarm
    pub alarm: bool,
    /// Reset MDT flags
    pub frset: bool,
}

impl SendMapOptions {
    /// Create options for initial map display.
    pub fn initial() -> Self {
        Self {
            erase: true,
            freekb: true,
            ..Default::default()
        }
    }

    /// Create options for map update.
    pub fn update() -> Self {
        Self {
            dataonly: true,
            freekb: true,
            ..Default::default()
        }
    }
}

/// Options for SEND TEXT command.
#[derive(Debug, Clone, Default)]
pub struct SendTextOptions {
    /// Erase screen before sending
    pub erase: bool,
    /// Include header
    pub header: bool,
    /// Free keyboard
    pub freekb: bool,
    /// Sound alarm
    pub alarm: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bms::BmsParser;

    fn create_test_map() -> BmsMap {
        let source = r#"
TEST     DFHMSD TYPE=MAP,LANG=COBOL
TESTM    DFHMDI SIZE=(24,80)
NAME     DFHMDF POS=(5,10),LENGTH=20,ATTRB=(UNPROT)
AMOUNT   DFHMDF POS=(6,10),LENGTH=10,ATTRB=(NUM,UNPROT)
STATUS   DFHMDF POS=(7,10),LENGTH=15,ATTRB=(PROT)
         DFHMSD TYPE=FINAL
"#;
        let mut parser = BmsParser::new();
        let mapset = parser.parse(source).unwrap();
        mapset.maps[0].clone()
    }

    #[test]
    fn test_terminal_manager_creation() {
        let manager = TerminalManager::new();
        assert!(!manager.exists("T001"));
    }

    #[test]
    fn test_get_or_create_terminal() {
        let mut manager = TerminalManager::new();
        let _term = manager.get_or_create("T001");

        assert!(manager.exists("T001"));
        assert!(manager.exists("t001")); // case insensitive
    }

    #[test]
    fn test_send_map() {
        let mut manager = TerminalManager::new();
        let map = create_test_map();

        let mut data = HashMap::new();
        data.insert("NAME".to_string(), b"John Doe".to_vec());
        data.insert("AMOUNT".to_string(), b"1000".to_vec());
        data.insert("STATUS".to_string(), b"ACTIVE".to_vec());

        let result = manager.send_map("T001", &map, &data, SendMapOptions::initial());
        assert!(result.is_ok());

        let terminal = manager.get("T001").unwrap();
        assert_eq!(terminal.current_map(), Some("TESTM"));
    }

    #[test]
    fn test_receive_map() {
        let mut manager = TerminalManager::new();
        let map = create_test_map();

        // First send the map
        let data = HashMap::new();
        manager.send_map("T001", &map, &data, SendMapOptions::initial()).unwrap();

        // Simulate user input
        let mut input = HashMap::new();
        input.insert("NAME".to_string(), b"Jane Smith".to_vec());
        input.insert("AMOUNT".to_string(), b"2500".to_vec());
        manager.simulate_input("T001", 0x7D, input).unwrap(); // ENTER

        // Receive map data
        let received = manager.receive_map("T001", &map).unwrap();

        assert_eq!(received.get("NAME").map(|v| v.as_slice()), Some(b"Jane Smith".as_slice()));
        assert_eq!(received.get("AMOUNT").map(|v| v.as_slice()), Some(b"2500".as_slice()));
    }

    #[test]
    fn test_send_text() {
        let mut manager = TerminalManager::new();

        let result = manager.send_text(
            "T001",
            "Welcome to the system",
            SendTextOptions { erase: true, ..Default::default() }
        );

        assert!(result.is_ok());
    }

    #[test]
    fn test_send_map_options() {
        let initial = SendMapOptions::initial();
        assert!(initial.erase);
        assert!(initial.freekb);

        let update = SendMapOptions::update();
        assert!(update.dataonly);
        assert!(update.freekb);
    }
}
