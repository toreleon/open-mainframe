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

/// An accumulated page entry for BMS page building.
#[derive(Debug, Clone)]
pub struct AccumulatedPage {
    /// Map name
    pub map_name: String,
    /// Field data
    pub data: HashMap<String, Vec<u8>>,
    /// Send options
    pub erase: bool,
}

/// Terminal manager for CICS transactions.
pub struct TerminalManager {
    /// Active terminals
    terminals: HashMap<String, Terminal>,
    /// Default screen size
    default_size: ScreenSize,
    /// Accumulated pages per terminal (for BMS page building)
    page_buffer: HashMap<String, Vec<AccumulatedPage>>,
    /// Delivered pages per terminal (from SEND PAGE)
    delivered_pages: HashMap<String, Vec<Vec<AccumulatedPage>>>,
}

impl TerminalManager {
    /// Create a new terminal manager.
    pub fn new() -> Self {
        Self {
            terminals: HashMap::new(),
            default_size: ScreenSize::Model2,
            page_buffer: HashMap::new(),
            delivered_pages: HashMap::new(),
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
    ///
    /// When `options.accum` is true, the map is accumulated in a page buffer
    /// instead of being sent immediately. Use `send_page()` to deliver
    /// accumulated maps.
    pub fn send_map(
        &mut self,
        terminal_id: &str,
        map: &BmsMap,
        data: &HashMap<String, Vec<u8>>,
        options: SendMapOptions,
    ) -> CicsResult<()> {
        let tid = terminal_id.to_uppercase();

        // If ACCUM is set, accumulate the map for page building
        if options.accum {
            let page = AccumulatedPage {
                map_name: map.name.clone(),
                data: data.clone(),
                erase: options.erase,
            };
            self.page_buffer.entry(tid).or_default().push(page);
            return Ok(());
        }

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

    /// Execute SEND PAGE — deliver accumulated maps to the terminal.
    ///
    /// All maps accumulated via SEND MAP ACCUM are collected into a single
    /// page and stored in `delivered_pages`. The page buffer is cleared.
    ///
    /// Returns the number of maps in the delivered page.
    pub fn send_page(&mut self, terminal_id: &str) -> CicsResult<usize> {
        let tid = terminal_id.to_uppercase();

        let pages = self.page_buffer.remove(&tid).unwrap_or_default();
        let count = pages.len();

        if count > 0 {
            self.delivered_pages
                .entry(tid)
                .or_default()
                .push(pages);
        }

        Ok(count)
    }

    /// Execute PURGE MESSAGE — clear all accumulated and delivered pages.
    pub fn purge_message(&mut self, terminal_id: &str) {
        let tid = terminal_id.to_uppercase();
        self.page_buffer.remove(&tid);
        self.delivered_pages.remove(&tid);
    }

    /// Get the accumulated page buffer for a terminal (for inspection/testing).
    pub fn page_buffer(&self, terminal_id: &str) -> Option<&[AccumulatedPage]> {
        self.page_buffer
            .get(&terminal_id.to_uppercase())
            .map(|v| v.as_slice())
    }

    /// Get delivered pages for a terminal (for inspection/testing).
    pub fn delivered_pages(&self, terminal_id: &str) -> Option<&[Vec<AccumulatedPage>]> {
        self.delivered_pages
            .get(&terminal_id.to_uppercase())
            .map(|v| v.as_slice())
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

    /// Set default screen size for new terminals.
    pub fn set_default_size(&mut self, size: ScreenSize) {
        self.default_size = size;
    }

    /// Get default screen size.
    pub fn default_size(&self) -> ScreenSize {
        self.default_size
    }

    /// Execute CONVERSE — combined SEND + RECEIVE.
    ///
    /// Sends text to the terminal, then waits for input. Returns
    /// received data and the AID key pressed. The terminal must
    /// have input pre-staged (via `simulate_input`) before the
    /// CONVERSE completes.
    pub fn converse(
        &mut self,
        terminal_id: &str,
        text: &str,
        erase: bool,
        max_receive_length: usize,
    ) -> CicsResult<(Vec<u8>, u8)> {
        // SEND phase — write output to the screen
        let terminal = self.get_or_create(terminal_id);

        if erase {
            terminal.clear_screen();
        }

        // Write text but preserve input state if already set
        let cursor = terminal.cursor();
        terminal.screen_write_string(cursor, text.as_bytes());

        // RECEIVE phase — read whatever input is staged
        let data = terminal.get_input_data(max_receive_length)?;
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
    /// Accumulate map for page building (BMS page building)
    pub accum: bool,
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

    // === Story 203.1: CONVERSE via terminal manager ===

    #[test]
    fn test_converse_terminal() {
        let mut manager = TerminalManager::new();

        // Create terminal and set raw input to simulate user response
        let term = manager.get_or_create("T001");
        term.set_raw_input(0x7D, b"USER REPLY".to_vec()); // ENTER key

        // Converse: send text then receive
        let (data, aid) = manager.converse("T001", "Enter data:", false, 80).unwrap();
        assert_eq!(aid, 0x7D);
        assert_eq!(data, b"USER REPLY");
    }

    // === Story 203.2: Dynamic terminal screen size ===

    #[test]
    fn test_terminal_manager_default_size() {
        let manager = TerminalManager::new();
        assert_eq!(manager.default_size(), ScreenSize::Model2);
    }

    #[test]
    fn test_terminal_manager_custom_size() {
        let mut manager = TerminalManager::new();
        manager.set_default_size(ScreenSize::Model4);

        let term = manager.get_or_create("T001");
        assert_eq!(term.screen_size(), ScreenSize::Model4);
        assert_eq!(term.screen_size().dimensions(), (43, 80));
    }

    // === Story 209.2: BMS Page Building ===

    #[test]
    fn test_send_map_accum() {
        // AC: Given multiple SEND MAP ACCUM calls
        // Then maps are accumulated in the page buffer
        let mut manager = TerminalManager::new();
        let map = create_test_map();

        let mut data1 = HashMap::new();
        data1.insert("NAME".to_string(), b"PAGE1 LINE1".to_vec());

        let mut data2 = HashMap::new();
        data2.insert("NAME".to_string(), b"PAGE1 LINE2".to_vec());

        // Send with ACCUM
        let opts = SendMapOptions {
            accum: true,
            ..Default::default()
        };

        manager.send_map("T001", &map, &data1, opts.clone()).unwrap();
        manager.send_map("T001", &map, &data2, opts).unwrap();

        // Should have 2 accumulated maps
        let buffer = manager.page_buffer("T001").unwrap();
        assert_eq!(buffer.len(), 2);
        assert_eq!(buffer[0].map_name, "TESTM");
        assert_eq!(buffer[1].map_name, "TESTM");
    }

    #[test]
    fn test_send_page() {
        // AC: When SEND PAGE is issued
        // Then accumulated maps are delivered to the terminal as pages
        let mut manager = TerminalManager::new();
        let map = create_test_map();

        let data = HashMap::new();
        let opts = SendMapOptions {
            accum: true,
            ..Default::default()
        };

        manager.send_map("T001", &map, &data, opts.clone()).unwrap();
        manager.send_map("T001", &map, &data, opts.clone()).unwrap();
        manager.send_map("T001", &map, &data, opts).unwrap();

        // Send page — deliver accumulated maps
        let count = manager.send_page("T001").unwrap();
        assert_eq!(count, 3);

        // Buffer should now be empty
        assert!(manager.page_buffer("T001").is_none());

        // Delivered pages should have one page with 3 maps
        let delivered = manager.delivered_pages("T001").unwrap();
        assert_eq!(delivered.len(), 1);
        assert_eq!(delivered[0].len(), 3);
    }

    #[test]
    fn test_send_page_empty() {
        let mut manager = TerminalManager::new();
        let count = manager.send_page("T001").unwrap();
        assert_eq!(count, 0);
    }

    #[test]
    fn test_purge_message() {
        let mut manager = TerminalManager::new();
        let map = create_test_map();
        let data = HashMap::new();

        let opts = SendMapOptions {
            accum: true,
            ..Default::default()
        };
        manager.send_map("T001", &map, &data, opts).unwrap();
        manager.send_page("T001").unwrap();

        // Now accumulate more
        let opts2 = SendMapOptions {
            accum: true,
            ..Default::default()
        };
        manager.send_map("T001", &map, &data, opts2).unwrap();

        // Purge all
        manager.purge_message("T001");

        assert!(manager.page_buffer("T001").is_none());
        assert!(manager.delivered_pages("T001").is_none());
    }

    #[test]
    fn test_multiple_send_pages() {
        let mut manager = TerminalManager::new();
        let map = create_test_map();
        let data = HashMap::new();

        let opts = SendMapOptions {
            accum: true,
            ..Default::default()
        };

        // First page: 2 maps
        manager.send_map("T001", &map, &data, opts.clone()).unwrap();
        manager.send_map("T001", &map, &data, opts.clone()).unwrap();
        manager.send_page("T001").unwrap();

        // Second page: 1 map
        manager.send_map("T001", &map, &data, opts).unwrap();
        manager.send_page("T001").unwrap();

        let delivered = manager.delivered_pages("T001").unwrap();
        assert_eq!(delivered.len(), 2);
        assert_eq!(delivered[0].len(), 2);
        assert_eq!(delivered[1].len(), 1);
    }

    #[test]
    fn test_accum_does_not_affect_terminal() {
        // ACCUM sends should NOT modify the terminal screen directly
        let mut manager = TerminalManager::new();
        let map = create_test_map();
        let data = HashMap::new();

        let opts = SendMapOptions {
            accum: true,
            ..Default::default()
        };
        manager.send_map("T001", &map, &data, opts).unwrap();

        // Terminal should not have been created (since ACCUM skips terminal render)
        assert!(!manager.exists("T001"));
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
