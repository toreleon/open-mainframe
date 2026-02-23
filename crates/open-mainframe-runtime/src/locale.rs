//! LE Locale Services — CEESETL/CEEQRYL/CEEFMON/CEEFTDS/CEELCNV/CEESCOL/CEEQDTC.
//!
//! Implements the z/OS Language Environment locale callable services for
//! locale-aware formatting, string comparison, and convention queries.

use crate::date_time::FeedbackCode;

/// Locale conventions structure (returned by CEELCNV).
#[derive(Debug, Clone)]
pub struct LocaleConventions {
    /// Decimal point character (e.g. '.' for US, ',' for EU).
    pub decimal_point: char,
    /// Thousands separator (e.g. ',' for US, '.' for EU).
    pub thousands_sep: char,
    /// Currency symbol (e.g. "$", "EUR").
    pub currency_symbol: String,
    /// International currency symbol (e.g. "USD", "EUR").
    pub int_currency_symbol: String,
    /// Number of fractional digits for monetary values.
    pub frac_digits: u8,
    /// Date format string (e.g. "MM/DD/YYYY", "DD.MM.YYYY").
    pub date_format: String,
    /// Time format string (e.g. "HH:MI:SS").
    pub time_format: String,
}

impl Default for LocaleConventions {
    fn default() -> Self {
        Self {
            decimal_point: '.',
            thousands_sep: ',',
            currency_symbol: "$".to_string(),
            int_currency_symbol: "USD".to_string(),
            frac_digits: 2,
            date_format: "MM/DD/YYYY".to_string(),
            time_format: "HH:MI:SS".to_string(),
        }
    }
}

/// A locale definition.
#[derive(Debug, Clone)]
pub struct LocaleDefinition {
    /// Locale name (e.g. "En_US.IBM-1047", "De_DE.IBM-1141").
    pub name: String,
    /// Language name.
    pub language: String,
    /// Country/territory.
    pub territory: String,
    /// Codeset.
    pub codeset: String,
    /// Conventions for this locale.
    pub conventions: LocaleConventions,
}

/// LE locale manager.
#[derive(Debug, Clone)]
pub struct LocaleManager {
    /// Current active locale name.
    current: String,
    /// Available locales.
    locales: Vec<LocaleDefinition>,
}

impl Default for LocaleManager {
    fn default() -> Self {
        Self::new()
    }
}

impl LocaleManager {
    /// Create a new locale manager with standard locales.
    pub fn new() -> Self {
        let mut mgr = Self {
            current: "En_US.IBM-1047".to_string(),
            locales: Vec::new(),
        };
        mgr.load_defaults();
        mgr
    }

    fn load_defaults(&mut self) {
        self.locales.push(LocaleDefinition {
            name: "En_US.IBM-1047".to_string(),
            language: "English".to_string(),
            territory: "US".to_string(),
            codeset: "IBM-1047".to_string(),
            conventions: LocaleConventions::default(),
        });
        self.locales.push(LocaleDefinition {
            name: "De_DE.IBM-1141".to_string(),
            language: "German".to_string(),
            territory: "DE".to_string(),
            codeset: "IBM-1141".to_string(),
            conventions: LocaleConventions {
                decimal_point: ',',
                thousands_sep: '.',
                currency_symbol: "EUR".to_string(),
                int_currency_symbol: "EUR".to_string(),
                frac_digits: 2,
                date_format: "DD.MM.YYYY".to_string(),
                time_format: "HH:MI:SS".to_string(),
            },
        });
        self.locales.push(LocaleDefinition {
            name: "Fr_FR.IBM-1147".to_string(),
            language: "French".to_string(),
            territory: "FR".to_string(),
            codeset: "IBM-1147".to_string(),
            conventions: LocaleConventions {
                decimal_point: ',',
                thousands_sep: ' ',
                currency_symbol: "EUR".to_string(),
                int_currency_symbol: "EUR".to_string(),
                frac_digits: 2,
                date_format: "DD/MM/YYYY".to_string(),
                time_format: "HH:MI:SS".to_string(),
            },
        });
        self.locales.push(LocaleDefinition {
            name: "Ja_JP.IBM-939".to_string(),
            language: "Japanese".to_string(),
            territory: "JP".to_string(),
            codeset: "IBM-939".to_string(),
            conventions: LocaleConventions {
                decimal_point: '.',
                thousands_sep: ',',
                currency_symbol: "JPY".to_string(),
                int_currency_symbol: "JPY".to_string(),
                frac_digits: 0,
                date_format: "YYYY/MM/DD".to_string(),
                time_format: "HH:MI:SS".to_string(),
            },
        });
    }

    /// CEESETL — Set the current locale.
    pub fn ceesetl(&mut self, locale_name: &str) -> FeedbackCode {
        if self.locales.iter().any(|l| l.name == locale_name) {
            self.current = locale_name.to_string();
            FeedbackCode::success()
        } else {
            FeedbackCode::error(2700)
        }
    }

    /// CEEQRYL — Query the current locale name.
    pub fn ceeqryl(&self) -> (String, FeedbackCode) {
        (self.current.clone(), FeedbackCode::success())
    }

    /// CEELCNV — Get locale conventions for the current locale.
    pub fn ceelcnv(&self) -> (LocaleConventions, FeedbackCode) {
        match self.locales.iter().find(|l| l.name == self.current) {
            Some(loc) => (loc.conventions.clone(), FeedbackCode::success()),
            None => (LocaleConventions::default(), FeedbackCode::error(2700)),
        }
    }

    /// CEEFMON — Format a monetary value per the current locale conventions.
    pub fn ceefmon(&self, amount: f64) -> (String, FeedbackCode) {
        let (conv, fc) = self.ceelcnv();
        if !fc.is_success() {
            return (String::new(), fc);
        }
        let abs = amount.abs();
        let sign = if amount < 0.0 { "-" } else { "" };
        let formatted = format_with_separators(abs, conv.frac_digits, conv.decimal_point, conv.thousands_sep);
        (
            format!("{}{}{}", sign, conv.currency_symbol, formatted),
            FeedbackCode::success(),
        )
    }

    /// CEEFTDS — Format date/time string per the current locale conventions.
    pub fn ceeftds(&self, year: i32, month: u32, day: u32, hour: u32, minute: u32, second: u32) -> (String, FeedbackCode) {
        let (conv, fc) = self.ceelcnv();
        if !fc.is_success() {
            return (String::new(), fc);
        }
        let date = conv.date_format
            .replace("YYYY", &format!("{:04}", year))
            .replace("MM", &format!("{:02}", month))
            .replace("DD", &format!("{:02}", day));
        let time = conv.time_format
            .replace("HH", &format!("{:02}", hour))
            .replace("MI", &format!("{:02}", minute))
            .replace("SS", &format!("{:02}", second));
        (format!("{} {}", date, time), FeedbackCode::success())
    }

    /// CEESCOL — Compare two strings per the current locale's collation.
    ///
    /// Returns -1 (a < b), 0 (a == b), or 1 (a > b).
    pub fn ceescol(&self, a: &str, b: &str) -> (i32, FeedbackCode) {
        // Simplified: use standard byte-order comparison.
        let cmp = a.cmp(b);
        let result = match cmp {
            std::cmp::Ordering::Less => -1,
            std::cmp::Ordering::Equal => 0,
            std::cmp::Ordering::Greater => 1,
        };
        (result, FeedbackCode::success())
    }

    /// CEEQDTC — Query date/time conventions for the current locale.
    pub fn ceeqdtc(&self) -> (String, String, FeedbackCode) {
        let (conv, fc) = self.ceelcnv();
        if !fc.is_success() {
            return (String::new(), String::new(), fc);
        }
        (conv.date_format, conv.time_format, FeedbackCode::success())
    }

    /// Register a custom locale.
    pub fn register_locale(&mut self, locale: LocaleDefinition) {
        self.locales.push(locale);
    }
}

fn format_with_separators(value: f64, frac_digits: u8, decimal_point: char, thousands_sep: char) -> String {
    let rounded = if frac_digits > 0 {
        let factor = 10f64.powi(frac_digits as i32);
        (value * factor).round() / factor
    } else {
        value.round()
    };

    let int_part = rounded as u64;
    let frac_part = ((rounded - int_part as f64) * 10f64.powi(frac_digits as i32)).round() as u64;

    // Format integer part with thousands separators.
    let int_str = int_part.to_string();
    let mut result = String::new();
    for (i, ch) in int_str.chars().rev().enumerate() {
        if i > 0 && i % 3 == 0 {
            result.push(thousands_sep);
        }
        result.push(ch);
    }
    let result: String = result.chars().rev().collect();

    if frac_digits > 0 {
        format!("{}{}{:0>width$}", result, decimal_point, frac_part, width = frac_digits as usize)
    } else {
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ─── LE107.1: Locale Management ───

    #[test]
    fn test_default_locale_is_us_english() {
        let mgr = LocaleManager::new();
        let (name, fc) = mgr.ceeqryl();
        assert!(fc.is_success());
        assert_eq!(name, "En_US.IBM-1047");
    }

    #[test]
    fn test_ceesetl_valid_locale() {
        let mut mgr = LocaleManager::new();
        let fc = mgr.ceesetl("De_DE.IBM-1141");
        assert!(fc.is_success());
        let (name, _) = mgr.ceeqryl();
        assert_eq!(name, "De_DE.IBM-1141");
    }

    #[test]
    fn test_ceesetl_invalid_locale() {
        let mut mgr = LocaleManager::new();
        let fc = mgr.ceesetl("Zz_ZZ.INVALID");
        assert!(!fc.is_success());
    }

    #[test]
    fn test_ceelcnv_us_defaults() {
        let mgr = LocaleManager::new();
        let (conv, fc) = mgr.ceelcnv();
        assert!(fc.is_success());
        assert_eq!(conv.decimal_point, '.');
        assert_eq!(conv.thousands_sep, ',');
        assert_eq!(conv.currency_symbol, "$");
    }

    #[test]
    fn test_ceelcnv_german() {
        let mut mgr = LocaleManager::new();
        mgr.ceesetl("De_DE.IBM-1141");
        let (conv, fc) = mgr.ceelcnv();
        assert!(fc.is_success());
        assert_eq!(conv.decimal_point, ',');
        assert_eq!(conv.thousands_sep, '.');
        assert_eq!(conv.currency_symbol, "EUR");
    }

    // ─── LE107.2: Locale Formatting ───

    #[test]
    fn test_ceefmon_us_format() {
        let mgr = LocaleManager::new();
        let (formatted, fc) = mgr.ceefmon(1234.56);
        assert!(fc.is_success());
        assert_eq!(formatted, "$1,234.56");
    }

    #[test]
    fn test_ceefmon_negative() {
        let mgr = LocaleManager::new();
        let (formatted, fc) = mgr.ceefmon(-99.99);
        assert!(fc.is_success());
        assert_eq!(formatted, "-$99.99");
    }

    #[test]
    fn test_ceefmon_german_format() {
        let mut mgr = LocaleManager::new();
        mgr.ceesetl("De_DE.IBM-1141");
        let (formatted, fc) = mgr.ceefmon(1234.56);
        assert!(fc.is_success());
        assert_eq!(formatted, "EUR1.234,56");
    }

    #[test]
    fn test_ceeftds_us_format() {
        let mgr = LocaleManager::new();
        let (formatted, fc) = mgr.ceeftds(2024, 3, 15, 14, 30, 0);
        assert!(fc.is_success());
        assert_eq!(formatted, "03/15/2024 14:30:00");
    }

    #[test]
    fn test_ceeftds_german_format() {
        let mut mgr = LocaleManager::new();
        mgr.ceesetl("De_DE.IBM-1141");
        let (formatted, fc) = mgr.ceeftds(2024, 3, 15, 14, 30, 0);
        assert!(fc.is_success());
        assert_eq!(formatted, "15.03.2024 14:30:00");
    }

    #[test]
    fn test_ceescol_compare() {
        let mgr = LocaleManager::new();
        let (result, fc) = mgr.ceescol("ABC", "DEF");
        assert!(fc.is_success());
        assert_eq!(result, -1);

        let (result, _) = mgr.ceescol("ABC", "ABC");
        assert_eq!(result, 0);

        let (result, _) = mgr.ceescol("DEF", "ABC");
        assert_eq!(result, 1);
    }

    #[test]
    fn test_ceeqdtc_us() {
        let mgr = LocaleManager::new();
        let (date_fmt, time_fmt, fc) = mgr.ceeqdtc();
        assert!(fc.is_success());
        assert_eq!(date_fmt, "MM/DD/YYYY");
        assert_eq!(time_fmt, "HH:MI:SS");
    }

    #[test]
    fn test_ceeqdtc_japanese() {
        let mut mgr = LocaleManager::new();
        mgr.ceesetl("Ja_JP.IBM-939");
        let (date_fmt, _, fc) = mgr.ceeqdtc();
        assert!(fc.is_success());
        assert_eq!(date_fmt, "YYYY/MM/DD");
    }

    #[test]
    fn test_register_custom_locale() {
        let mut mgr = LocaleManager::new();
        mgr.register_locale(LocaleDefinition {
            name: "Es_ES.IBM-1145".to_string(),
            language: "Spanish".to_string(),
            territory: "ES".to_string(),
            codeset: "IBM-1145".to_string(),
            conventions: LocaleConventions {
                decimal_point: ',',
                thousands_sep: '.',
                currency_symbol: "EUR".to_string(),
                int_currency_symbol: "EUR".to_string(),
                frac_digits: 2,
                date_format: "DD/MM/YYYY".to_string(),
                time_format: "HH:MI:SS".to_string(),
            },
        });
        let fc = mgr.ceesetl("Es_ES.IBM-1145");
        assert!(fc.is_success());
    }
}
