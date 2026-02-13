//! 3270 field attribute to ratatui Style mapping.
//!
//! Converts 3270 field attributes (protected, bright, dark, numeric, etc.)
//! into ratatui terminal styles with appropriate colors and modifiers.

use ratatui::style::{Color, Modifier, Style};
use zos_cics::bms::{AttributeByte, FieldColor, FieldHighlight};

use crate::color::ColorTheme;

/// Convert a 3270 attribute byte to a ratatui Style.
pub fn attribute_to_style(attr: &AttributeByte, theme: &ColorTheme) -> Style {
    let mut style = Style::default().bg(theme.background);

    if attr.is_dark() {
        // Dark fields: invisible (same color as background)
        style = style.fg(theme.background);
    } else if attr.is_bright() {
        // Bright fields: bold/high-intensity
        style = style.fg(theme.bright).add_modifier(Modifier::BOLD);
    } else if attr.is_protected() {
        // Protected fields: normal dimmer color
        style = style.fg(theme.protected);
    } else {
        // Unprotected (input) fields
        style = style.fg(theme.unprotected);
    }

    // Unprotected fields get underline to show editability
    if !attr.is_protected() {
        style = style.add_modifier(Modifier::UNDERLINED);
    }

    style
}

/// Convert a 3270 extended color attribute to a ratatui Color.
pub fn field_color_to_ratatui(color: &FieldColor) -> Color {
    match color {
        FieldColor::Default => Color::Reset,
        FieldColor::Blue => Color::Blue,
        FieldColor::Red => Color::Red,
        FieldColor::Pink => Color::Magenta,
        FieldColor::Green => Color::Green,
        FieldColor::Turquoise => Color::Cyan,
        FieldColor::Yellow => Color::Yellow,
        FieldColor::White => Color::White,
    }
}

/// Convert a 3270 highlight attribute to ratatui Modifier.
pub fn highlight_to_modifier(highlight: &FieldHighlight) -> Modifier {
    match highlight {
        FieldHighlight::Normal => Modifier::empty(),
        FieldHighlight::Blink => Modifier::SLOW_BLINK,
        FieldHighlight::Reverse => Modifier::REVERSED,
        FieldHighlight::Underscore => Modifier::UNDERLINED,
    }
}

/// Build a complete style for a screen cell, considering both base attribute
/// and optional extended attributes (color, highlight).
pub fn cell_style(
    attr: Option<&AttributeByte>,
    color: Option<&FieldColor>,
    highlight: Option<&FieldHighlight>,
    theme: &ColorTheme,
) -> Style {
    let mut style = if let Some(a) = attr {
        attribute_to_style(a, theme)
    } else {
        Style::default().bg(theme.background).fg(theme.unprotected)
    };

    // Override foreground with extended color if present
    if let Some(c) = color {
        let fg = field_color_to_ratatui(c);
        if fg != Color::Reset {
            style = style.fg(fg);
        }
    }

    // Add highlight modifier
    if let Some(h) = highlight {
        style = style.add_modifier(highlight_to_modifier(h));
    }

    style
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_protected_style() {
        let theme = ColorTheme::classic();
        let attr = AttributeByte::new(true, false, false, false, false);
        let style = attribute_to_style(&attr, &theme);
        assert_eq!(style.fg, Some(theme.protected));
    }

    #[test]
    fn test_bright_style() {
        let theme = ColorTheme::classic();
        let attr = AttributeByte::new(false, false, true, false, false);
        let style = attribute_to_style(&attr, &theme);
        assert_eq!(style.fg, Some(theme.bright));
        assert!(style.add_modifier.contains(Modifier::BOLD));
    }

    #[test]
    fn test_dark_style() {
        let theme = ColorTheme::classic();
        let attr = AttributeByte::new(true, false, false, true, false);
        let style = attribute_to_style(&attr, &theme);
        // Dark fields: fg matches bg
        assert_eq!(style.fg, Some(theme.background));
    }

    #[test]
    fn test_unprotected_style() {
        let theme = ColorTheme::classic();
        let attr = AttributeByte::new(false, false, false, false, false);
        let style = attribute_to_style(&attr, &theme);
        assert_eq!(style.fg, Some(theme.unprotected));
        assert!(style.add_modifier.contains(Modifier::UNDERLINED));
    }

    #[test]
    fn test_field_color_mapping() {
        assert_eq!(field_color_to_ratatui(&FieldColor::Blue), Color::Blue);
        assert_eq!(field_color_to_ratatui(&FieldColor::Green), Color::Green);
        assert_eq!(field_color_to_ratatui(&FieldColor::Red), Color::Red);
    }
}
