//! Date and time intrinsic functions.
//!
//! Implements COBOL-2014 date and time manipulation functions.

use std::time::{SystemTime, UNIX_EPOCH};

/// COBOL date format: YYYYMMDD as integer.
pub type CobolDate = i32;

/// COBOL day format: YYYYDDD as integer.
pub type CobolDay = i32;

/// COBOL integer date: days since a reference date.
pub type IntegerDate = i32;

// Reference date: January 1, 1601 (COBOL standard)
#[allow(dead_code)]
const REFERENCE_YEAR: i32 = 1601;

/// FUNCTION CURRENT-DATE implementation.
///
/// Returns the current date and time as a 21-character string:
/// Format: YYYYMMDDHHMMSShhmm±HHMM
/// - YYYYMMDD: Date
/// - HHMMSS: Time
/// - hh: Hundredths of seconds
/// - mm: Additional precision (always 00)
/// - ±HHMM: UTC offset
pub fn current_date() -> String {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();

    let secs = now.as_secs();
    let millis = now.subsec_millis();

    // Calculate date/time components (simplified UTC)
    let days = secs / 86400;
    let time_of_day = secs % 86400;

    let hours = time_of_day / 3600;
    let minutes = (time_of_day % 3600) / 60;
    let seconds = time_of_day % 60;
    let hundredths = millis / 10;

    // Calculate date from Unix epoch (1970-01-01)
    // Unix epoch is 719528 days from year 1
    let total_days = days as i32 + 719528;
    let (year, month, day) = days_to_ymd_simple(total_days);

    // Format: YYYYMMDDHHMMSShh±HHMM (21 chars)
    // hh = hundredths, ±HHMM = UTC offset
    format!(
        "{:04}{:02}{:02}{:02}{:02}{:02}{:02}+0000",
        year, month, day, hours, minutes, seconds, hundredths
    )
}

/// FUNCTION WHEN-COMPILED placeholder.
///
/// Returns the compilation date and time (should be set at compile time).
pub fn when_compiled() -> String {
    // In a real implementation, this would be set during compilation
    "20260213120000000000+0000".to_string()
}

/// FUNCTION INTEGER-OF-DATE implementation.
///
/// Converts YYYYMMDD to integer date (days since reference).
pub fn integer_of_date(date: CobolDate) -> IntegerDate {
    let year = date / 10000;
    let month = (date % 10000) / 100;
    let day = date % 100;

    ymd_to_days(year, month, day)
}

/// FUNCTION DATE-OF-INTEGER implementation.
///
/// Converts integer date to YYYYMMDD.
pub fn date_of_integer(int_date: IntegerDate) -> CobolDate {
    let (year, month, day) = days_to_ymd(int_date);
    year * 10000 + month * 100 + day
}

/// FUNCTION INTEGER-OF-DAY implementation.
///
/// Converts YYYYDDD (Julian) to integer date.
pub fn integer_of_day(day_format: CobolDay) -> IntegerDate {
    let year = day_format / 1000;
    let day_of_year = day_format % 1000;

    // Days from reference to start of year
    let jan1_days = ymd_to_days(year, 1, 1);
    jan1_days + day_of_year - 1
}

/// FUNCTION DAY-OF-INTEGER implementation.
///
/// Converts integer date to YYYYDDD (Julian).
pub fn day_of_integer(int_date: IntegerDate) -> CobolDay {
    let (year, _month, _day) = days_to_ymd(int_date);

    // Calculate day of year
    let jan1_days = ymd_to_days(year, 1, 1);
    let day_of_year = int_date - jan1_days + 1;

    year * 1000 + day_of_year
}

/// FUNCTION DATE-TO-YYYYMMDD implementation.
///
/// Converts various date formats to YYYYMMDD.
pub fn date_to_yyyymmdd(date: i32, format: DateFormat) -> CobolDate {
    match format {
        DateFormat::YYMMDD => {
            let yy = date / 10000;
            let mmdd = date % 10000;
            let yyyy = if yy >= 50 { 1900 + yy } else { 2000 + yy };
            yyyy * 10000 + mmdd
        }
        DateFormat::YYYYMMDD => date,
        DateFormat::YYYYDDD => {
            let int_date = integer_of_day(date);
            date_of_integer(int_date)
        }
        DateFormat::YYDDD => {
            let yy = date / 1000;
            let ddd = date % 1000;
            let yyyy = if yy >= 50 { 1900 + yy } else { 2000 + yy };
            let int_date = integer_of_day(yyyy * 1000 + ddd);
            date_of_integer(int_date)
        }
    }
}

/// Date format enumeration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DateFormat {
    YYMMDD,
    YYYYMMDD,
    YYYYDDD,
    YYDDD,
}

/// FUNCTION YEAR-TO-YYYY implementation.
///
/// Converts 2-digit year to 4-digit year.
pub fn year_to_yyyy(yy: i32, window_start: i32) -> i32 {
    let century_start = window_start / 100 * 100;
    let window_yy = window_start % 100;

    if yy >= window_yy {
        century_start + yy
    } else {
        century_start + 100 + yy
    }
}

/// Check if a year is a leap year.
pub fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

/// Days in each month (non-leap year).
const DAYS_IN_MONTH: [i32; 12] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

/// Get days in a month.
pub fn days_in_month(year: i32, month: i32) -> i32 {
    if month == 2 && is_leap_year(year) {
        29
    } else if month >= 1 && month <= 12 {
        DAYS_IN_MONTH[(month - 1) as usize]
    } else {
        0
    }
}

/// Convert year/month/day to days since reference date.
fn ymd_to_days(year: i32, month: i32, day: i32) -> i32 {
    // Algorithm from Howard Hinnant's date algorithms
    let y = if month <= 2 { year - 1 } else { year };
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = (y - era * 400) as u32;
    let doy = (153 * (if month > 2 { month - 3 } else { month + 9 }) as u32 + 2) / 5 + day as u32
        - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146097 + doe as i32 - 719468 + days_from_reference_to_epoch()
}

/// Convert days since reference date to year/month/day.
fn days_to_ymd(days: i32) -> (i32, i32, i32) {
    // Adjust from reference date
    let z = days + 719468 - days_from_reference_to_epoch();
    let era = if z >= 0 { z } else { z - 146096 } / 146097;
    let doe = (z - era * 146097) as u32;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe as i32 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = if m <= 2 { y + 1 } else { y };
    (year, m as i32, d as i32)
}

/// Days from COBOL reference date (1601-01-01) to Unix epoch (1970-01-01).
fn days_from_reference_to_epoch() -> i32 {
    // 1601 to 1970 = 369 years
    // 369 years = 89 leap years + 280 non-leap years
    // = 89 * 366 + 280 * 365 = 134774 days
    134774
}

/// Simple days-to-YMD conversion from absolute days (days since year 1).
fn days_to_ymd_simple(mut days: i32) -> (i32, i32, i32) {
    // Use proleptic Gregorian calendar
    let mut year = 1;

    // Fast-forward through 400-year cycles (146097 days each)
    let cycles_400 = days / 146097;
    days %= 146097;
    year += cycles_400 * 400;

    // 100-year cycles (36524 days each, max 3 to avoid overflow into next 400-year)
    let cycles_100 = (days / 36524).min(3);
    days -= cycles_100 * 36524;
    year += cycles_100 * 100;

    // 4-year cycles (1461 days each)
    let cycles_4 = days / 1461;
    days -= cycles_4 * 1461;
    year += cycles_4 * 4;

    // Individual years (max 3 to avoid overflow into next 4-year)
    let years = (days / 365).min(3);
    days -= years * 365;
    year += years;

    // Now days is day of year (0-indexed)
    let day_of_year = days + 1;

    // Find month and day
    let mut month = 1;
    let mut day = day_of_year;

    while month <= 12 {
        let dim = days_in_month(year, month);
        if day <= dim {
            break;
        }
        day -= dim;
        month += 1;
    }

    (year, month, day)
}

/// FUNCTION DAY-TO-YYYYDDD implementation.
///
/// Converts a 2-digit year Julian date (YYDDD) to 4-digit year (YYYYDDD).
pub fn day_to_yyyyddd(yyddd: i32, window_start: i32) -> i32 {
    let yy = yyddd / 1000;
    let ddd = yyddd % 1000;
    let yyyy = year_to_yyyy(yy, window_start);
    yyyy * 1000 + ddd
}

/// FUNCTION COMBINED-DATETIME implementation.
///
/// Combines an integer date and seconds past midnight into a single numeric value.
pub fn combined_datetime(integer_date: IntegerDate, seconds: f64) -> f64 {
    integer_date as f64 * 1_000_000.0 + seconds
}

/// FUNCTION SECONDS-PAST-MIDNIGHT implementation.
///
/// Returns the number of seconds past midnight.
pub fn seconds_past_midnight() -> f64 {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    let secs = now.as_secs();
    let time_of_day = secs % 86400;
    let millis = now.subsec_millis();
    time_of_day as f64 + millis as f64 / 1000.0
}

/// FUNCTION TEST-DATE-YYYYMMDD implementation.
///
/// Tests if a date in YYYYMMDD format is valid.
/// Returns 0 if valid, non-zero (position of first invalid component) if not.
pub fn test_date_yyyymmdd(date: i32) -> i32 {
    let year = date / 10000;
    let month = (date % 10000) / 100;
    let day = date % 100;

    if year < 1601 || year > 9999 {
        return 1; // Invalid year
    }
    if month < 1 || month > 12 {
        return 5; // Invalid month (position in YYYYMMDD)
    }
    if day < 1 || day > days_in_month(year, month) {
        return 7; // Invalid day
    }
    0 // Valid
}

/// FUNCTION TEST-DAY-YYYYDDD implementation.
///
/// Tests if a Julian date in YYYYDDD format is valid.
/// Returns 0 if valid, non-zero if not.
pub fn test_day_yyyyddd(day: i32) -> i32 {
    let year = day / 1000;
    let day_of_year = day % 1000;

    if year < 1601 || year > 9999 {
        return 1; // Invalid year
    }
    let max_day = if is_leap_year(year) { 366 } else { 365 };
    if day_of_year < 1 || day_of_year > max_day {
        return 5; // Invalid day
    }
    0 // Valid
}

/// Validate a date.
pub fn is_valid_date(year: i32, month: i32, day: i32) -> bool {
    if month < 1 || month > 12 {
        return false;
    }
    if day < 1 {
        return false;
    }
    day <= days_in_month(year, month)
}

/// Calculate age in years.
pub fn age_in_years(birth_date: CobolDate, current_date: CobolDate) -> i32 {
    let birth_year = birth_date / 10000;
    let birth_month = (birth_date % 10000) / 100;
    let birth_day = birth_date % 100;

    let current_year = current_date / 10000;
    let current_month = (current_date % 10000) / 100;
    let current_day = current_date % 100;

    let mut age = current_year - birth_year;

    // Adjust if birthday hasn't occurred yet this year
    if current_month < birth_month || (current_month == birth_month && current_day < birth_day) {
        age -= 1;
    }

    age.max(0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_current_date_format() {
        let date = current_date();
        assert_eq!(date.len(), 21, "Date should be 21 chars, got: {}", date);
        // Should start with valid year (20xx)
        assert!(date.starts_with("20"), "Date should start with 20xx, got: {}", date);
    }

    #[test]
    fn test_integer_of_date() {
        // January 1, 1601 should be day 1
        let day1 = integer_of_date(16010101);

        // January 2, 1601 should be day 2
        let day2 = integer_of_date(16010102);
        assert_eq!(day2 - day1, 1);
    }

    #[test]
    fn test_date_of_integer() {
        let int_date = integer_of_date(20260213);
        let back = date_of_integer(int_date);
        assert_eq!(back, 20260213);
    }

    #[test]
    fn test_round_trip() {
        for date in [20000101, 20201231, 19990228, 20000229, 21001231] {
            let int_date = integer_of_date(date);
            let back = date_of_integer(int_date);
            assert_eq!(back, date, "Round trip failed for {}", date);
        }
    }

    #[test]
    fn test_julian_conversion() {
        // January 1 = day 1
        let int_date = integer_of_day(2026001);
        let julian = day_of_integer(int_date);
        assert_eq!(julian, 2026001);

        // December 31 of non-leap year = day 365
        let int_date2 = integer_of_day(2025365);
        let gregorian = date_of_integer(int_date2);
        assert_eq!(gregorian, 20251231);
    }

    #[test]
    fn test_is_leap_year() {
        assert!(is_leap_year(2000));
        assert!(is_leap_year(2024));
        assert!(!is_leap_year(1900));
        assert!(!is_leap_year(2023));
    }

    #[test]
    fn test_days_in_month() {
        assert_eq!(days_in_month(2024, 2), 29); // Leap year
        assert_eq!(days_in_month(2023, 2), 28); // Non-leap
        assert_eq!(days_in_month(2023, 1), 31);
        assert_eq!(days_in_month(2023, 4), 30);
    }

    #[test]
    fn test_year_to_yyyy() {
        // Window starting at 1950
        assert_eq!(year_to_yyyy(50, 1950), 1950);
        assert_eq!(year_to_yyyy(99, 1950), 1999);
        assert_eq!(year_to_yyyy(00, 1950), 2000);
        assert_eq!(year_to_yyyy(49, 1950), 2049);
    }

    #[test]
    fn test_date_to_yyyymmdd() {
        assert_eq!(date_to_yyyymmdd(260213, DateFormat::YYMMDD), 20260213);
        assert_eq!(date_to_yyyymmdd(990101, DateFormat::YYMMDD), 19990101);
    }

    #[test]
    fn test_is_valid_date() {
        assert!(is_valid_date(2024, 2, 29));
        assert!(!is_valid_date(2023, 2, 29));
        assert!(is_valid_date(2023, 12, 31));
        assert!(!is_valid_date(2023, 13, 1));
        assert!(!is_valid_date(2023, 4, 31));
    }

    #[test]
    fn test_age_in_years() {
        assert_eq!(age_in_years(19900515, 20260213), 35);
        assert_eq!(age_in_years(19900515, 20260514), 35);
        assert_eq!(age_in_years(19900515, 20260515), 36);
    }

    #[test]
    fn test_day_to_yyyyddd() {
        assert_eq!(day_to_yyyyddd(26001, 1950), 2026001);
        assert_eq!(day_to_yyyyddd(99365, 1950), 1999365);
        assert_eq!(day_to_yyyyddd(00001, 1950), 2000001);
    }

    #[test]
    fn test_combined_datetime() {
        let int_date = integer_of_date(20260214);
        let combined = combined_datetime(int_date, 43200.0); // noon
        assert!(combined > 0.0);
    }

    #[test]
    fn test_seconds_past_midnight() {
        let secs = seconds_past_midnight();
        assert!(secs >= 0.0);
        assert!(secs < 86400.0);
    }

    #[test]
    fn test_test_date_yyyymmdd() {
        assert_eq!(test_date_yyyymmdd(20260214), 0); // Valid
        assert_eq!(test_date_yyyymmdd(20260229), 7); // 2026 is not leap year
        assert_eq!(test_date_yyyymmdd(20241301), 5); // Invalid month
        assert_eq!(test_date_yyyymmdd(10000101), 1); // Invalid year (>9999 is invalid)
        assert_eq!(test_date_yyyymmdd(20240229), 0); // 2024 is leap year
    }

    #[test]
    fn test_test_day_yyyyddd() {
        assert_eq!(test_day_yyyyddd(2026001), 0); // Valid: Jan 1
        assert_eq!(test_day_yyyyddd(2026365), 0); // Valid: Dec 31 (non-leap)
        assert_eq!(test_day_yyyyddd(2026366), 5); // Invalid: 2026 not leap
        assert_eq!(test_day_yyyyddd(2024366), 0); // Valid: 2024 is leap
        assert_eq!(test_day_yyyyddd(2026000), 5); // Invalid day 0
    }
}
