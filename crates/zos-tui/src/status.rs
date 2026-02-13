//! Status line widget displayed below the 3270 screen.
//!
//! Shows program name, transaction ID, cursor position, and key hints.

use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::widgets::Widget;

use crate::color::ColorTheme;

/// Information to display in the status line.
#[derive(Debug, Clone, Default)]
pub struct StatusInfo {
    /// Current program name.
    pub program: String,
    /// Current transaction ID.
    pub transid: String,
    /// Cursor row (1-based).
    pub cursor_row: usize,
    /// Cursor col (1-based).
    pub cursor_col: usize,
    /// Status/error message.
    pub message: String,
}

/// Widget that renders the status line.
pub struct StatusLine<'a> {
    info: &'a StatusInfo,
    theme: &'a ColorTheme,
}

impl<'a> StatusLine<'a> {
    pub fn new(info: &'a StatusInfo, theme: &'a ColorTheme) -> Self {
        Self { info, theme }
    }
}

impl Widget for StatusLine<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        if area.height == 0 || area.width == 0 {
            return;
        }

        let style = Style::default()
            .fg(self.theme.status_fg)
            .bg(self.theme.status_bg);

        // Fill status line background
        for x in area.x..area.right() {
            buf[(x, area.y)].set_char(' ').set_style(style);
        }

        // Left side: key hints
        let hints = "F1=Help  F3=Exit  F7=Back  F8=Forward";

        // Right side: cursor position + program
        let right = format!(
            "{} {}  Row {} Col {}",
            self.info.transid, self.info.program, self.info.cursor_row, self.info.cursor_col
        );

        // Render left hints
        for (i, ch) in hints.chars().enumerate() {
            let x = area.x + i as u16 + 1;
            if x < area.right() {
                buf[(x, area.y)].set_char(ch).set_style(style);
            }
        }

        // Render right info
        let right_start = area.right().saturating_sub(right.len() as u16 + 1);
        for (i, ch) in right.chars().enumerate() {
            let x = right_start + i as u16;
            if x >= area.x && x < area.right() {
                buf[(x, area.y)]
                    .set_char(ch)
                    .set_style(style.add_modifier(Modifier::BOLD));
            }
        }

        // Render message line (second row if available)
        if area.height >= 2 && !self.info.message.is_empty() {
            let msg_y = area.y + 1;
            let msg_style = Style::default()
                .fg(self.theme.error)
                .bg(self.theme.status_bg);

            for x in area.x..area.right() {
                buf[(x, msg_y)].set_char(' ').set_style(msg_style);
            }

            for (i, ch) in self.info.message.chars().enumerate() {
                let x = area.x + i as u16 + 1;
                if x < area.right() {
                    buf[(x, msg_y)].set_char(ch).set_style(msg_style);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_status_info_default() {
        let info = StatusInfo::default();
        assert!(info.program.is_empty());
        assert!(info.message.is_empty());
        assert_eq!(info.cursor_row, 0);
    }
}
