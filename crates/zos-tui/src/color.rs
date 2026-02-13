//! Color themes and terminal capability detection.
//!
//! Maps 3270 field semantics to terminal colors, with support for
//! multiple named themes and graceful degradation.

use ratatui::style::Color;

/// A color theme for 3270 terminal rendering.
#[derive(Debug, Clone)]
pub struct ColorTheme {
    /// Screen background color.
    pub background: Color,
    /// Default text color (normal protected fields).
    pub protected: Color,
    /// Unprotected (input) field text color.
    pub unprotected: Color,
    /// Bright/intensified field color.
    pub bright: Color,
    /// Status line background.
    pub status_bg: Color,
    /// Status line text.
    pub status_fg: Color,
    /// Error/message color.
    pub error: Color,
    /// Input field underline/highlight color.
    pub input_highlight: Color,
}

impl ColorTheme {
    /// Classic green-screen 3270 theme.
    pub fn classic() -> Self {
        Self {
            background: Color::Black,
            protected: Color::Blue,
            unprotected: Color::Green,
            bright: Color::White,
            status_bg: Color::DarkGray,
            status_fg: Color::Cyan,
            error: Color::Red,
            input_highlight: Color::Green,
        }
    }

    /// Modern theme with softer colors.
    pub fn modern() -> Self {
        Self {
            background: Color::Black,
            protected: Color::Gray,
            unprotected: Color::White,
            bright: Color::Yellow,
            status_bg: Color::Rgb(30, 30, 40),
            status_fg: Color::Rgb(180, 180, 200),
            error: Color::Red,
            input_highlight: Color::Cyan,
        }
    }

    /// Monochrome theme for terminals without color support.
    pub fn monochrome() -> Self {
        Self {
            background: Color::Black,
            protected: Color::White,
            unprotected: Color::White,
            bright: Color::White,
            status_bg: Color::White,
            status_fg: Color::Black,
            error: Color::White,
            input_highlight: Color::White,
        }
    }

    /// Get a theme by name.
    pub fn by_name(name: &str) -> Self {
        match name.to_lowercase().as_str() {
            "classic" | "green" => Self::classic(),
            "modern" => Self::modern(),
            "mono" | "monochrome" => Self::monochrome(),
            _ => Self::classic(),
        }
    }
}

impl Default for ColorTheme {
    fn default() -> Self {
        Self::classic()
    }
}

/// Detect terminal color capability from environment.
pub fn detect_color_support() -> ColorSupport {
    if std::env::var("NO_COLOR").is_ok() {
        return ColorSupport::None;
    }

    match std::env::var("TERM").as_deref() {
        Ok("dumb") | Err(_) => ColorSupport::None,
        Ok(term) if term.contains("256color") => ColorSupport::TrueColor,
        Ok(_) => ColorSupport::Basic,
    }
}

/// Level of color support available.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorSupport {
    /// No color support.
    None,
    /// Basic 8/16 colors.
    Basic,
    /// 256 colors or true color.
    TrueColor,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_color_support_levels() {
        // Verify all enum variants exist and are distinct
        assert_ne!(ColorSupport::None, ColorSupport::Basic);
        assert_ne!(ColorSupport::Basic, ColorSupport::TrueColor);
        assert_ne!(ColorSupport::None, ColorSupport::TrueColor);
    }

    #[test]
    fn test_detect_color_support_returns_valid() {
        // The function should always return a valid ColorSupport variant
        // regardless of the current environment.
        let support = detect_color_support();
        assert!(
            support == ColorSupport::None
                || support == ColorSupport::Basic
                || support == ColorSupport::TrueColor
        );
    }

    #[test]
    fn test_theme_by_name() {
        let classic = ColorTheme::by_name("classic");
        assert_eq!(classic.background, Color::Black);
        assert_eq!(classic.unprotected, Color::Green);

        let modern = ColorTheme::by_name("modern");
        assert_eq!(modern.unprotected, Color::White);
    }

    #[test]
    fn test_default_theme() {
        let theme = ColorTheme::default();
        assert_eq!(theme.background, Color::Black);
        assert_eq!(theme.unprotected, Color::Green);
    }
}
