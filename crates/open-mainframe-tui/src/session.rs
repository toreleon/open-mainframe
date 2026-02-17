//! Session state machine for pseudo-conversational CICS sessions.
//!
//! Manages the lifecycle of an interactive CICS terminal session:
//! Execute program → Display screen → Wait for input → Process → Repeat.

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;

use crossterm::terminal;
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::Terminal;

use crate::event::EventSource;

/// Terminal type alias for the TUI backend.
pub type TuiTerminal = Terminal<CrosstermBackend<io::Stdout>>;

use open_mainframe_cics::bms::{BmsMap, ScreenSize};
use open_mainframe_cics::terminal::{ScreenBuffer, SendMapOptions};

/// 3270 terminal model determining screen dimensions.
///
/// Each model provides a default and (optionally) an alternate screen size.
/// `EWA` (Erase/Write Alternate) switches to the alternate size; `EW`
/// (Erase/Write) reverts to the default size.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TerminalModel {
    /// 24 rows x 80 columns.
    #[default]
    Model2,
    /// 32 rows x 80 columns.
    Model3,
    /// 43 rows x 80 columns.
    Model4,
    /// 27 rows x 132 columns (default 24x80, alternate 27x132).
    Model5,
}

impl TerminalModel {
    /// Default screen dimensions for this model.
    pub fn default_size(self) -> ScreenSize {
        ScreenSize::Model2
    }

    /// Alternate (EWA) screen dimensions for this model.
    pub fn alternate_size(self) -> ScreenSize {
        match self {
            TerminalModel::Model2 => ScreenSize::Model2,
            TerminalModel::Model3 => ScreenSize::Model3,
            TerminalModel::Model4 => ScreenSize::Model4,
            TerminalModel::Model5 => ScreenSize::Model5,
        }
    }

    /// Dimensions (rows, cols) for the given screen size variant.
    pub fn dimensions(self, alternate: bool) -> (usize, usize) {
        if alternate {
            self.alternate_size().dimensions()
        } else {
            self.default_size().dimensions()
        }
    }
}

use crate::color::ColorTheme;
use crate::error::SessionError;
use crate::fields::FieldTable;
use crate::input::{self, InputAction};
use crate::renderer::FieldRenderedScreen;
use crate::status::{StatusInfo, StatusLine};

/// Configuration for an interactive CICS session.
#[derive(Debug, Clone)]
pub struct SessionConfig {
    /// Path to the initial COBOL program.
    pub initial_program: PathBuf,
    /// Copybook include paths.
    pub include_paths: Vec<PathBuf>,
    /// VSAM data files (DDNAME=path:key_len:rec_len).
    pub data_files: Vec<String>,
    /// Directory to search for XCTL target programs.
    pub program_dir: Option<PathBuf>,
    /// TRANSID -> program name mapping.
    pub transid_map: HashMap<String, String>,
    /// Color theme name.
    pub color_theme: String,
    /// User ID for ASSIGN USERID.
    pub userid: Option<String>,
    /// Initial transaction ID.
    pub initial_transid: Option<String>,
    /// Terminal model (Model 2-5) controlling screen dimensions.
    pub terminal_model: TerminalModel,
}

/// Session state in the pseudo-conversational lifecycle.
#[derive(Debug, Clone)]
pub enum SessionState {
    /// A program is being executed.
    Executing {
        program: String,
    },
    /// Screen has been displayed, waiting for user to press an AID key.
    WaitingForInput {
        pending_transid: Option<String>,
        commarea: Option<Vec<u8>>,
    },
    /// User pressed an AID key, program will process input.
    InputReceived {
        aid: u8,
    },
    /// Session is ending.
    Ending {
        exit_code: i32,
    },
}

/// An interactive CICS terminal session.
pub struct Session {
    /// Session configuration.
    config: SessionConfig,
    /// Color theme.
    theme: ColorTheme,
    /// Current session state.
    state: SessionState,
    /// Current field table (from last SEND MAP).
    field_table: FieldTable,
    /// Screen buffer.
    screen: ScreenBuffer,
    /// Status line information.
    status: StatusInfo,
    /// Current program name.
    current_program: String,
    /// Whether the screen needs a redraw.
    needs_redraw: bool,
    /// Whether we are using the alternate (EWA) screen size.
    use_alternate: bool,
    /// Current screen rows.
    screen_rows: usize,
    /// Current screen columns.
    screen_cols: usize,
}

impl Session {
    /// Create a new session from configuration.
    pub fn new(config: SessionConfig) -> Self {
        let theme = ColorTheme::by_name(&config.color_theme);
        let program = config
            .initial_program
            .file_stem()
            .map(|s| s.to_string_lossy().to_uppercase())
            .unwrap_or_default();

        // Start with default screen size (always 24x80)
        let (rows, cols) = config.terminal_model.dimensions(false);

        Self {
            config,
            theme,
            state: SessionState::Executing {
                program: program.clone(),
            },
            field_table: FieldTable::new(),
            screen: ScreenBuffer::new(ScreenSize::Model2),
            status: StatusInfo {
                program: program.clone(),
                transid: String::new(),
                cursor_row: 1,
                cursor_col: 1,
                message: String::new(),
            },
            current_program: program,
            needs_redraw: true,
            use_alternate: false,
            screen_rows: rows,
            screen_cols: cols,
        }
    }

    /// Switch to the alternate (EWA) screen size.
    pub fn erase_write_alternate(&mut self) {
        if self.use_alternate {
            return;
        }
        self.use_alternate = true;
        let (rows, cols) = self.config.terminal_model.dimensions(true);
        self.screen_rows = rows;
        self.screen_cols = cols;
        let screen_size = self.config.terminal_model.alternate_size();
        self.screen = ScreenBuffer::new(screen_size);
        self.field_table = FieldTable::new();
        self.needs_redraw = true;
    }

    /// Switch back to the default (EW) screen size.
    pub fn erase_write(&mut self) {
        if !self.use_alternate {
            return;
        }
        self.use_alternate = false;
        let (rows, cols) = self.config.terminal_model.dimensions(false);
        self.screen_rows = rows;
        self.screen_cols = cols;
        self.screen = ScreenBuffer::new(ScreenSize::Model2);
        self.field_table = FieldTable::new();
        self.needs_redraw = true;
    }

    /// Get the current screen dimensions (rows, cols).
    pub fn screen_dimensions(&self) -> (usize, usize) {
        (self.screen_rows, self.screen_cols)
    }

    /// Update the screen from a SEND MAP operation.
    /// Called by the CICS bridge when a program issues SEND MAP.
    ///
    /// Handles three modes:
    /// - **Full (default):** Rebuilds the field table from the BMS map and applies data.
    /// - **DATAONLY:** Updates only variable data fields in the existing table,
    ///   preserving static labels and current field content.
    /// - **MAPONLY:** Rebuilds the field table from the BMS map with INITIAL values
    ///   but does not apply any data from the program.
    pub fn on_send_map(
        &mut self,
        map: &BmsMap,
        data: &HashMap<String, Vec<u8>>,
        options: &SendMapOptions,
    ) {
        if options.erase || options.eraseaup {
            self.screen.clear();
        }

        if options.dataonly {
            // DATAONLY: Update only variable fields in existing field table.
            // Don't rebuild the map - just merge in the new data values.
            for (name, value) in data {
                self.field_table.set_field_data(name, value);
            }
        } else {
            // Full send or MAPONLY: rebuild field table from BMS map.
            self.field_table = FieldTable::from_bms_map(map);

            if !options.maponly {
                // Apply data to field table (not for MAPONLY).
                for (name, value) in data {
                    self.field_table.set_field_data(name, value);
                }
            }
        }

        // Reset MDTs if FRSET
        if options.frset {
            self.field_table.reset_mdt();
        }

        // Apply CURSOR option if specified; otherwise use the IC field.
        if let Some(ref pos) = options.cursor {
            self.field_table.set_cursor_to_position(pos);
        }
        if let Some(pos) = self.field_table.cursor_position() {
            self.status.cursor_row = pos.row;
            self.status.cursor_col = pos.col;
        }

        self.needs_redraw = true;
    }

    /// Called when a program issues SEND TEXT.
    pub fn on_send_text(&mut self, text: &str, erase: bool) {
        if erase {
            self.screen.clear();
        }
        self.screen
            .write_string(self.screen.cursor(), text.as_bytes());
        self.field_table = FieldTable::new(); // No fields for plain text
        self.needs_redraw = true;
    }

    /// Set session status message.
    pub fn set_message(&mut self, msg: &str) {
        self.status.message = msg.to_string();
        self.needs_redraw = true;
    }

    /// Set the current program name in status.
    pub fn set_program(&mut self, name: &str) {
        self.current_program = name.to_uppercase();
        self.status.program = self.current_program.clone();
    }

    /// Set the transaction ID in status.
    pub fn set_transid(&mut self, transid: &str) {
        self.status.transid = transid.to_string();
    }

    /// Run the TUI input loop, rendering the screen and waiting for user input.
    /// Returns the AID key pressed and the modified field data.
    ///
    /// Generic over the ratatui backend and the event source so that tests
    /// can use `TestBackend` + `MockEventSource` instead of a real terminal.
    pub fn wait_for_input<B: ratatui::backend::Backend>(
        &mut self,
        terminal: &mut Terminal<B>,
        events: &mut dyn EventSource,
    ) -> Result<(u8, HashMap<String, Vec<u8>>), SessionError> {
        self.needs_redraw = true;

        loop {
            if self.needs_redraw {
                self.render(terminal)?;
                self.needs_redraw = false;
            }

            // Wait for keyboard event
            let evt = events.read_event()?;
            let action = input::map_key_event(&evt);

            match action {
                InputAction::Terminate => {
                    return Err(SessionError::Interrupted);
                }

                InputAction::AidKey(aid_code) => {
                    use open_mainframe_cics::runtime::eib::aid;

                    // CLEAR key: clear all unprotected fields first
                    if aid_code == aid::CLEAR {
                        self.field_table.clear_unprotected();
                        self.field_table.home();
                        self.update_cursor_status();
                    }

                    // PA keys are "short read" — return only AID + cursor,
                    // no field data is transmitted.
                    if aid_code == aid::PA1
                        || aid_code == aid::PA2
                        || aid_code == aid::PA3
                    {
                        return Ok((aid_code, HashMap::new()));
                    }

                    // Collect modified field data
                    let mut fields = HashMap::new();
                    for (name, data) in self.field_table.get_all_input_fields() {
                        fields.insert(name, data);
                    }
                    return Ok((aid_code, fields));
                }

                InputAction::Character(ch) => {
                    self.field_table.type_char(ch);
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                InputAction::Tab => {
                    self.field_table.tab_forward();
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                InputAction::BackTab => {
                    self.field_table.tab_backward();
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                InputAction::Home => {
                    self.field_table.home();
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                InputAction::Backspace => {
                    self.field_table.backspace();
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                InputAction::Delete => {
                    self.field_table.delete();
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                InputAction::EraseEof => {
                    self.field_table.erase_eof();
                    self.needs_redraw = true;
                }

                InputAction::CursorLeft => {
                    self.field_table.cursor_left();
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                InputAction::CursorRight => {
                    self.field_table.cursor_right();
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                InputAction::CursorUp => {
                    self.field_table.cursor_up();
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                InputAction::CursorDown => {
                    self.field_table.cursor_down();
                    self.update_cursor_status();
                    self.needs_redraw = true;
                }

                _ => {} // Ignore other actions
            }
        }
    }

    /// Update cursor position in status bar.
    fn update_cursor_status(&mut self) {
        if let Some(pos) = self.field_table.cursor_position() {
            self.status.cursor_row = pos.row;
            self.status.cursor_col = pos.col;
        }
    }

    /// Render the TUI screen.
    fn render<B: ratatui::backend::Backend>(
        &self,
        terminal: &mut Terminal<B>,
    ) -> Result<(), SessionError> {
        let theme = &self.theme;
        let field_table = &self.field_table;
        let status = &self.status;
        let rows = self.screen_rows;
        let cols = self.screen_cols;

        terminal
            .draw(|frame| {
                let size = frame.area();

                // Layout: screen rows for 3270 screen, remaining for status
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Length(rows as u16), // 3270 screen area
                        Constraint::Min(2),              // Status line
                    ])
                    .split(size);

                // Render 3270 screen
                let screen_widget = FieldRenderedScreen {
                    field_table,
                    theme,
                    rows,
                    cols,
                };
                frame.render_widget(screen_widget, chunks[0]);

                // Render status line
                let status_widget = StatusLine::new(status, theme);
                frame.render_widget(status_widget, chunks[1]);

                // Position terminal cursor
                if let Some(pos) = field_table.cursor_position() {
                    let x = chunks[0].x + (pos.col - 1).min(cols - 1) as u16;
                    let y = chunks[0].y + (pos.row - 1).min(rows - 1) as u16;
                    frame.set_cursor_position((x, y));
                }
            })
            .map_err(SessionError::TerminalInit)?;

        Ok(())
    }
}

/// Set up the terminal for TUI rendering.
pub fn setup_terminal() -> Result<Terminal<CrosstermBackend<io::Stdout>>, SessionError> {
    // Install panic handler that restores terminal
    let original_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let _ = terminal::disable_raw_mode();
        let _ = crossterm::execute!(
            io::stdout(),
            terminal::LeaveAlternateScreen,
            crossterm::cursor::Show
        );
        original_hook(info);
    }));

    terminal::enable_raw_mode().map_err(SessionError::TerminalInit)?;
    crossterm::execute!(
        io::stdout(),
        terminal::EnterAlternateScreen,
        crossterm::cursor::Hide
    )
    .map_err(SessionError::TerminalInit)?;

    let backend = CrosstermBackend::new(io::stdout());
    let terminal = Terminal::new(backend).map_err(SessionError::TerminalInit)?;

    Ok(terminal)
}

/// Restore the terminal to normal mode.
pub fn restore_terminal() -> Result<(), SessionError> {
    terminal::disable_raw_mode().map_err(SessionError::TerminalInit)?;
    crossterm::execute!(
        io::stdout(),
        terminal::LeaveAlternateScreen,
        crossterm::cursor::Show
    )
    .map_err(SessionError::TerminalInit)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_session_config_creation() {
        let config = SessionConfig {
            initial_program: PathBuf::from("COSGN00C.cbl"),
            include_paths: vec![],
            data_files: vec![],
            program_dir: None,
            transid_map: HashMap::new(),
            color_theme: "classic".to_string(),
            userid: None,
            initial_transid: None,
            terminal_model: TerminalModel::Model2,
        };
        assert_eq!(config.initial_program.to_str(), Some("COSGN00C.cbl"));
    }

    #[test]
    fn test_session_creation() {
        let config = SessionConfig {
            initial_program: PathBuf::from("COSGN00C.cbl"),
            include_paths: vec![],
            data_files: vec![],
            program_dir: None,
            transid_map: HashMap::new(),
            color_theme: "classic".to_string(),
            userid: None,
            initial_transid: None,
            terminal_model: TerminalModel::Model2,
        };
        let session = Session::new(config);
        assert_eq!(session.current_program, "COSGN00C");
        assert_eq!(session.status.program, "COSGN00C");
    }

    #[test]
    fn test_session_state_variants() {
        let _executing = SessionState::Executing {
            program: "COSGN00C".to_string(),
        };
        let _waiting = SessionState::WaitingForInput {
            pending_transid: Some("COSG".to_string()),
            commarea: None,
        };
        let _ending = SessionState::Ending { exit_code: 0 };
    }

    #[test]
    fn test_terminal_model_default_size() {
        let model = TerminalModel::Model2;
        assert_eq!(model.dimensions(false), (24, 80));
        assert_eq!(model.dimensions(true), (24, 80));
    }

    #[test]
    fn test_terminal_model_4_sizes() {
        let model = TerminalModel::Model4;
        // Default is always 24x80
        assert_eq!(model.dimensions(false), (24, 80));
        // Alternate is 43x80
        assert_eq!(model.dimensions(true), (43, 80));
    }

    #[test]
    fn test_terminal_model_5_sizes() {
        let model = TerminalModel::Model5;
        assert_eq!(model.dimensions(false), (24, 80));
        assert_eq!(model.dimensions(true), (27, 132));
    }

    #[test]
    fn test_session_model4_screen_dimensions() {
        let config = SessionConfig {
            initial_program: PathBuf::from("TEST.cbl"),
            include_paths: vec![],
            data_files: vec![],
            program_dir: None,
            transid_map: HashMap::new(),
            color_theme: "classic".to_string(),
            userid: None,
            initial_transid: None,
            terminal_model: TerminalModel::Model4,
        };
        let session = Session::new(config);
        // Starts with default size (24x80)
        assert_eq!(session.screen_dimensions(), (24, 80));
    }

    #[test]
    fn test_erase_write_alternate_switches_screen() {
        let config = SessionConfig {
            initial_program: PathBuf::from("TEST.cbl"),
            include_paths: vec![],
            data_files: vec![],
            program_dir: None,
            transid_map: HashMap::new(),
            color_theme: "classic".to_string(),
            userid: None,
            initial_transid: None,
            terminal_model: TerminalModel::Model5,
        };
        let mut session = Session::new(config);
        assert_eq!(session.screen_dimensions(), (24, 80));

        // Switch to alternate (27x132)
        session.erase_write_alternate();
        assert_eq!(session.screen_dimensions(), (27, 132));

        // Switch back to default (24x80)
        session.erase_write();
        assert_eq!(session.screen_dimensions(), (24, 80));
    }
}
