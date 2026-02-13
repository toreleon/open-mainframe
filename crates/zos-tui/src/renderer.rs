//! ScreenBuffer to ratatui widget conversion.
//!
//! Renders the 3270 ScreenBuffer as a ratatui widget, mapping field
//! attributes and colors to terminal styles.

use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::widgets::Widget;

use zos_cics::bms::AttributeByte;
use zos_cics::terminal::{ScreenBuffer, ScreenPosition};

use crate::color::ColorTheme;
use crate::fields::FieldTable;
use crate::styles;

/// Widget that renders a 3270 screen from a ScreenBuffer and FieldTable.
pub struct ScreenWidget<'a> {
    /// The 3270 screen buffer to render.
    screen: &'a ScreenBuffer,
    /// The field table for attribute/style information (reserved for future color overlay).
    _field_table: &'a FieldTable,
    /// Color theme.
    theme: &'a ColorTheme,
}

impl<'a> ScreenWidget<'a> {
    pub fn new(
        screen: &'a ScreenBuffer,
        field_table: &'a FieldTable,
        theme: &'a ColorTheme,
    ) -> Self {
        Self {
            screen,
            _field_table: field_table,
            theme,
        }
    }
}

impl Widget for ScreenWidget<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let (rows, cols) = self.screen.dimensions();
        let render_rows = rows.min(area.height as usize);
        let render_cols = cols.min(area.width as usize);

        // Track the current field attribute as we scan through the buffer
        let mut current_attr: Option<AttributeByte> = None;

        for row in 0..render_rows {
            for col in 0..render_cols {
                let screen_pos = ScreenPosition::new(row + 1, col + 1);
                let cell = match self.screen.get(screen_pos) {
                    Some(c) => c,
                    None => continue,
                };

                // If this cell has an attribute, update current tracking
                if let Some(attr) = cell.attribute {
                    current_attr = Some(attr);
                    // Attribute byte position is not displayable - render as space
                    let style = Style::default().bg(self.theme.background);
                    let buf_x = area.x + col as u16;
                    let buf_y = area.y + row as u16;
                    if buf_x < area.right() && buf_y < area.bottom() {
                        buf[(buf_x, buf_y)].set_char(' ').set_style(style);
                    }
                    continue;
                }

                // Determine style from current field attribute
                let style = if let Some(ref attr) = current_attr {
                    styles::attribute_to_style(attr, self.theme)
                } else {
                    Style::default()
                        .bg(self.theme.background)
                        .fg(self.theme.unprotected)
                };

                // Convert character
                let ch = if (0x20..0x7F).contains(&cell.char) {
                    cell.char as char
                } else if cell.char == 0 {
                    ' '
                } else {
                    '.'
                };

                let buf_x = area.x + col as u16;
                let buf_y = area.y + row as u16;
                if buf_x < area.right() && buf_y < area.bottom() {
                    buf[(buf_x, buf_y)].set_char(ch).set_style(style);
                }
            }
        }
    }
}

/// Render the screen buffer using field table data (alternative approach).
/// This renders from the FieldTable directly rather than from the ScreenBuffer,
/// which gives more accurate field-level styling.
pub struct FieldRenderedScreen<'a> {
    pub field_table: &'a FieldTable,
    pub theme: &'a ColorTheme,
    pub rows: usize,
    pub cols: usize,
}

impl Widget for FieldRenderedScreen<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        // First, fill background
        let bg_style = Style::default().bg(self.theme.background).fg(self.theme.protected);
        for y in area.y..area.bottom() {
            for x in area.x..area.right() {
                buf[(x, y)].set_char(' ').set_style(bg_style);
            }
        }

        // Render each field
        for field in self.field_table.fields() {
            let style = styles::attribute_to_style(&field.attribute, self.theme);

            for (i, &ch_byte) in field.content.iter().enumerate() {
                let pos = field.position_at(i);
                let buf_x = area.x + (pos.col - 1) as u16;
                let buf_y = area.y + (pos.row - 1) as u16;

                if buf_x >= area.right() || buf_y >= area.bottom() {
                    continue;
                }

                let ch = if (0x20..0x7F).contains(&ch_byte) {
                    ch_byte as char
                } else if ch_byte == 0 {
                    ' '
                } else {
                    '.'
                };

                buf[(buf_x, buf_y)].set_char(ch).set_style(style);
            }
        }
    }
}

/// Convert a ScreenBuffer to a text string for testing/snapshot purposes.
pub fn screen_to_text(screen: &ScreenBuffer) -> String {
    screen.to_text()
}

#[cfg(test)]
mod tests {
    use super::*;
    use zos_cics::bms::ScreenSize;

    #[test]
    fn test_screen_to_text() {
        let mut screen = ScreenBuffer::new(ScreenSize::Model2);
        screen.write_string(ScreenPosition::new(1, 1), b"Hello, World!");
        let text = screen_to_text(&screen);
        assert!(text.starts_with("Hello, World!"));
    }
}
