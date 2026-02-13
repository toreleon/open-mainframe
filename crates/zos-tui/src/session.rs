//! Session state machine for pseudo-conversational CICS sessions.
//!
//! Manages the lifecycle of an interactive CICS terminal session:
//! Execute program → Display screen → Wait for input → Process → Repeat.

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;

use crossterm::event;
use crossterm::terminal;
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout};
use ratatui::Terminal;

/// Terminal type alias for the TUI backend.
pub type TuiTerminal = Terminal<CrosstermBackend<io::Stdout>>;

use zos_cics::bms::{BmsMap, ScreenSize};
use zos_cics::terminal::{ScreenBuffer, SendMapOptions};

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
        }
    }

    /// Update the screen from a SEND MAP operation.
    /// Called by the CICS bridge when a program issues SEND MAP.
    pub fn on_send_map(
        &mut self,
        map: &BmsMap,
        data: &HashMap<String, Vec<u8>>,
        options: &SendMapOptions,
    ) {
        if options.erase || options.eraseaup {
            self.screen.clear();
        }

        // Build field table from BMS map
        self.field_table = FieldTable::from_bms_map(map);

        // Apply data to field table
        for (name, value) in data {
            self.field_table.set_field_data(name, value);
        }

        // Reset MDTs if FRSET
        if options.frset {
            self.field_table.reset_mdt();
        }

        // Update cursor position
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
    pub fn wait_for_input(
        &mut self,
        terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    ) -> Result<(u8, HashMap<String, Vec<u8>>), SessionError> {
        self.needs_redraw = true;

        loop {
            if self.needs_redraw {
                self.render(terminal)?;
                self.needs_redraw = false;
            }

            // Wait for keyboard event
            let evt = event::read().map_err(SessionError::TerminalInit)?;
            let action = input::map_key_event(&evt);

            match action {
                InputAction::Terminate => {
                    return Err(SessionError::Interrupted);
                }

                InputAction::AidKey(aid_code) => {
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
    fn render(
        &self,
        terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    ) -> Result<(), SessionError> {
        let theme = &self.theme;
        let field_table = &self.field_table;
        let status = &self.status;

        terminal
            .draw(|frame| {
                let size = frame.area();

                // Layout: 24 rows for screen, remaining for status
                let chunks = Layout::default()
                    .direction(Direction::Vertical)
                    .constraints([
                        Constraint::Length(24), // 3270 screen area
                        Constraint::Min(2),     // Status line
                    ])
                    .split(size);

                // Render 3270 screen
                let screen_widget = FieldRenderedScreen {
                    field_table,
                    theme,
                    rows: 24,
                    cols: 80,
                };
                frame.render_widget(screen_widget, chunks[0]);

                // Render status line
                let status_widget = StatusLine::new(status, theme);
                frame.render_widget(status_widget, chunks[1]);

                // Position terminal cursor
                if let Some(pos) = field_table.cursor_position() {
                    let x = chunks[0].x + (pos.col - 1).min(79) as u16;
                    let y = chunks[0].y + (pos.row - 1).min(23) as u16;
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
}
