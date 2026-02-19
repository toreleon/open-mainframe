//! Calendar-accurate date/time library and LE callable services.
//!
//! Provides Lilian day calculations, date arithmetic, and LE date/time
//! callable services (CEEDAYS, CEEDATE, CEESECS) used by COBOL programs.
//!
//! The Lilian day number is the count of days since October 15, 1582
//! (the first day of the Gregorian calendar). Day 1 = October 15, 1582.

/// Lilian epoch: October 15, 1582 (Gregorian calendar adoption).
const LILIAN_EPOCH_YEAR: i32 = 1582;
const LILIAN_EPOCH_MONTH: u32 = 10;
const LILIAN_EPOCH_DAY: u32 = 15;

/// Check if a year is a leap year (Gregorian rules).
pub fn is_leap_year(year: i32) -> bool {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}

/// Get the number of days in a given month (1-based).
pub fn days_in_month(year: i32, month: u32) -> u32 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 => {
            if is_leap_year(year) {
                29
            } else {
                28
            }
        }
        _ => 0,
    }
}

/// Validate a Gregorian date.
pub fn is_valid_date(year: i32, month: u32, day: u32) -> bool {
    if !(LILIAN_EPOCH_YEAR..=9999).contains(&year) {
        return false;
    }
    if !(1..=12).contains(&month) {
        return false;
    }
    if day < 1 || day > days_in_month(year, month) {
        return false;
    }
    // Before the Lilian epoch
    if year == LILIAN_EPOCH_YEAR
        && (month < LILIAN_EPOCH_MONTH
            || (month == LILIAN_EPOCH_MONTH && day < LILIAN_EPOCH_DAY))
    {
        return false;
    }
    true
}

/// Convert a Gregorian date to a Julian Day Number (JDN).
///
/// Uses the standard algorithm from Meeus, "Astronomical Algorithms".
fn gregorian_to_jdn(year: i32, month: u32, day: u32) -> i64 {
    let y = year as i64;
    let m = month as i64;
    let d = day as i64;

    // Adjust for Jan/Feb being months 13/14 of the previous year
    let (a, b) = if m <= 2 { (y - 1, m + 12) } else { (y, m) };

    let jdn = (365.25 * (a + 4716) as f64) as i64
        + (30.6001 * (b + 1) as f64) as i64
        + d
        - 1524;

    // Gregorian correction
    let a2 = a / 100;
    let greg_correction = 2 - a2 + a2 / 4;

    jdn + greg_correction
}

/// Convert a Gregorian date to a Lilian day number.
///
/// Lilian day 1 = October 15, 1582.
///
/// # Arguments
/// * `year` - 4-digit year (1582–9999)
/// * `month` - Month (1–12)
/// * `day` - Day (1–31)
///
/// # Returns
/// The Lilian day number, or `None` if the date is invalid.
pub fn date_to_lilian(year: i32, month: u32, day: u32) -> Option<i64> {
    if !is_valid_date(year, month, day) {
        return None;
    }
    // Lilian day 1 = Oct 15, 1582 = JDN 2299161
    let jdn = gregorian_to_jdn(year, month, day);
    Some(jdn - 2299160) // JDN 2299161 -> Lilian 1
}

/// Convert a Lilian day number to a Gregorian date (year, month, day).
///
/// # Arguments
/// * `lilian` - Lilian day number (>= 1)
///
/// # Returns
/// `(year, month, day)` or `None` if the day number is invalid.
pub fn lilian_to_date(lilian: i64) -> Option<(i32, u32, u32)> {
    if lilian < 1 {
        return None;
    }
    // Convert Lilian to a Julian Day Number (JDN).
    // Lilian day 1 = Oct 15, 1582 = JDN 2299161.
    let jdn = lilian + 2299160;

    // Use the standard algorithm to convert JDN to Gregorian (y, m, d).
    // Reference: Meeus, "Astronomical Algorithms", Chapter 7.
    let a = jdn + 32044;
    let b = (4 * a + 3) / 146097;
    let c = a - (146097 * b) / 4;
    let d = (4 * c + 3) / 1461;
    let e = c - (1461 * d) / 4;
    let m = (5 * e + 2) / 153;

    let day = (e - (153 * m + 2) / 5 + 1) as u32;
    let month = (m + 3 - 12 * (m / 10)) as u32;
    let year = (100 * b + d - 4800 + m / 10) as i32;

    Some((year, month, day))
}

/// COBOL intrinsic: INTEGER-OF-DATE(yyyymmdd)
///
/// Converts a COBOL integer date (YYYYMMDD) to a Lilian day number.
pub fn integer_of_date(yyyymmdd: i32) -> Option<i64> {
    let year = yyyymmdd / 10000;
    let month = ((yyyymmdd % 10000) / 100) as u32;
    let day = (yyyymmdd % 100) as u32;
    date_to_lilian(year, month, day)
}

/// COBOL intrinsic: DATE-OF-INTEGER(lilian)
///
/// Converts a Lilian day number to COBOL integer date (YYYYMMDD).
pub fn date_of_integer(lilian: i64) -> Option<i32> {
    let (y, m, d) = lilian_to_date(lilian)?;
    Some(y * 10000 + m as i32 * 100 + d as i32)
}

/// COBOL intrinsic: INTEGER-OF-DAY(yyyyddd)
///
/// Converts a COBOL date in YYYYDDD format to a Lilian day number.
pub fn integer_of_day(yyyyddd: i32) -> Option<i64> {
    let year = yyyyddd / 1000;
    let doy = (yyyyddd % 1000) as u32;
    if doy < 1 {
        return None;
    }
    // Convert day-of-year to month/day
    let (month, day) = day_of_year_to_month_day(year, doy)?;
    date_to_lilian(year, month, day)
}

/// COBOL intrinsic: DAY-OF-INTEGER(lilian)
///
/// Converts a Lilian day number to COBOL YYYYDDD format.
pub fn day_of_integer(lilian: i64) -> Option<i32> {
    let (y, m, d) = lilian_to_date(lilian)?;
    let doy = month_day_to_day_of_year(y, m, d);
    Some(y * 1000 + doy as i32)
}

/// Convert day-of-year to (month, day).
fn day_of_year_to_month_day(year: i32, doy: u32) -> Option<(u32, u32)> {
    let max_doy = if is_leap_year(year) { 366 } else { 365 };
    if doy > max_doy {
        return None;
    }
    let mut remaining = doy;
    for month in 1..=12 {
        let dim = days_in_month(year, month);
        if remaining <= dim {
            return Some((month, remaining));
        }
        remaining -= dim;
    }
    None
}

/// Convert (month, day) to day-of-year.
fn month_day_to_day_of_year(year: i32, month: u32, day: u32) -> u32 {
    let mut doy = 0;
    for m in 1..month {
        doy += days_in_month(year, m);
    }
    doy + day
}

// =========================================================================
// LE Callable Services
// =========================================================================

/// Feedback code for LE callable services.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FeedbackCode {
    /// Condition ID (0 = success).
    pub condition_id: i32,
    /// Severity (0 = info, 1 = warning, 2 = error, 3 = severe, 4 = critical).
    pub severity: i32,
    /// Message number.
    pub msg_no: i32,
}

impl FeedbackCode {
    /// Success feedback.
    pub fn success() -> Self {
        Self {
            condition_id: 0,
            severity: 0,
            msg_no: 0,
        }
    }

    /// Error feedback.
    pub fn error(msg_no: i32) -> Self {
        Self {
            condition_id: 1,
            severity: 2,
            msg_no,
        }
    }

    /// Check if the feedback indicates success.
    pub fn is_success(&self) -> bool {
        self.severity < 2
    }
}

/// LE service: CEEDAYS — Convert date string to Lilian day number.
///
/// # Arguments
/// * `date_str` - Date string (e.g., "20240115")
/// * `picture` - Format picture (e.g., "YYYYMMDD")
///
/// # Returns
/// `(lilian_day, feedback_code)`
pub fn ceedays(date_str: &str, picture: &str) -> (i64, FeedbackCode) {
    let date_str = date_str.trim();
    match picture.to_uppercase().as_str() {
        "YYYYMMDD" => {
            if let Ok(yyyymmdd) = date_str.parse::<i32>() {
                if let Some(lilian) = integer_of_date(yyyymmdd) {
                    return (lilian, FeedbackCode::success());
                }
            }
            (0, FeedbackCode::error(2501))
        }
        "YYYYDDD" => {
            if let Ok(yyyyddd) = date_str.parse::<i32>() {
                if let Some(lilian) = integer_of_day(yyyyddd) {
                    return (lilian, FeedbackCode::success());
                }
            }
            (0, FeedbackCode::error(2501))
        }
        "MMDDYYYY" => {
            if date_str.len() == 8 {
                if let (Ok(mm), Ok(dd), Ok(yyyy)) = (
                    date_str[0..2].parse::<u32>(),
                    date_str[2..4].parse::<u32>(),
                    date_str[4..8].parse::<i32>(),
                ) {
                    if let Some(lilian) = date_to_lilian(yyyy, mm, dd) {
                        return (lilian, FeedbackCode::success());
                    }
                }
            }
            (0, FeedbackCode::error(2501))
        }
        "DDMMYYYY" => {
            if date_str.len() == 8 {
                if let (Ok(dd), Ok(mm), Ok(yyyy)) = (
                    date_str[0..2].parse::<u32>(),
                    date_str[2..4].parse::<u32>(),
                    date_str[4..8].parse::<i32>(),
                ) {
                    if let Some(lilian) = date_to_lilian(yyyy, mm, dd) {
                        return (lilian, FeedbackCode::success());
                    }
                }
            }
            (0, FeedbackCode::error(2501))
        }
        _ => (0, FeedbackCode::error(2502)), // unsupported picture
    }
}

/// LE service: CEEDATE — Convert Lilian day number to date string.
///
/// # Arguments
/// * `lilian` - Lilian day number
/// * `picture` - Output format picture (e.g., "YYYYMMDD")
///
/// # Returns
/// `(date_string, feedback_code)`
pub fn ceedate(lilian: i64, picture: &str) -> (String, FeedbackCode) {
    let date = match lilian_to_date(lilian) {
        Some(d) => d,
        None => return (String::new(), FeedbackCode::error(2503)),
    };
    let (year, month, day) = date;

    match picture.to_uppercase().as_str() {
        "YYYYMMDD" => {
            let s = format!("{:04}{:02}{:02}", year, month, day);
            (s, FeedbackCode::success())
        }
        "YYYYDDD" => {
            let doy = month_day_to_day_of_year(year, month, day);
            let s = format!("{:04}{:03}", year, doy);
            (s, FeedbackCode::success())
        }
        "MMDDYYYY" => {
            let s = format!("{:02}{:02}{:04}", month, day, year);
            (s, FeedbackCode::success())
        }
        "DDMMYYYY" => {
            let s = format!("{:02}{:02}{:04}", day, month, year);
            (s, FeedbackCode::success())
        }
        _ => (String::new(), FeedbackCode::error(2502)),
    }
}

/// LE service: CEESECS — Convert date/time to seconds since Lilian epoch.
///
/// The Lilian epoch for seconds is midnight, October 15, 1582.
///
/// # Arguments
/// * `date_time_str` - Date/time string (e.g., "20240115120000")
/// * `picture` - Format picture (e.g., "YYYYMMDDHHMMSS")
///
/// # Returns
/// `(seconds, feedback_code)`
pub fn ceesecs(date_time_str: &str, picture: &str) -> (f64, FeedbackCode) {
    let s = date_time_str.trim();
    match picture.to_uppercase().as_str() {
        "YYYYMMDDHHMMSS" => {
            if s.len() < 14 {
                return (0.0, FeedbackCode::error(2501));
            }
            let date_part = &s[0..8];
            let hh: u32 = s[8..10].parse().unwrap_or(99);
            let mm: u32 = s[10..12].parse().unwrap_or(99);
            let ss: u32 = s[12..14].parse().unwrap_or(99);

            if hh > 23 || mm > 59 || ss > 59 {
                return (0.0, FeedbackCode::error(2501));
            }

            let (lilian, fc) = ceedays(date_part, "YYYYMMDD");
            if !fc.is_success() {
                return (0.0, fc);
            }

            let day_seconds = (lilian - 1) as f64 * 86400.0;
            let time_seconds = hh as f64 * 3600.0 + mm as f64 * 60.0 + ss as f64;
            (day_seconds + time_seconds, FeedbackCode::success())
        }
        _ => (0.0, FeedbackCode::error(2502)),
    }
}

// =========================================================================
// LE103: Extended Date/Time Services
// =========================================================================

/// LE service: CEEDATM — Convert Lilian seconds to a formatted date/time string.
///
/// Supported picture tokens:
/// - `YYYY` — 4-digit year
/// - `MM` — 2-digit month
/// - `DD` — 2-digit day
/// - `HH` — 2-digit hour (00-23)
/// - `MI` — 2-digit minute
/// - `SS` — 2-digit second
///
/// Other characters in the picture are passed through literally (e.g. `-`, `:`, ` `).
pub fn ceedatm(lilian_seconds: f64, picture: &str) -> (String, FeedbackCode) {
    if lilian_seconds < 0.0 {
        return (String::new(), FeedbackCode::error(2503));
    }

    // Convert seconds to Lilian day + time-of-day.
    let total_secs = lilian_seconds as i64;
    let lilian_day = total_secs / 86400 + 1; // day 1 starts at second 0
    let time_of_day = total_secs % 86400;

    let (year, month, day) = match lilian_to_date(lilian_day) {
        Some(d) => d,
        None => return (String::new(), FeedbackCode::error(2503)),
    };

    let hour = (time_of_day / 3600) as u32;
    let minute = ((time_of_day % 3600) / 60) as u32;
    let second = (time_of_day % 60) as u32;

    let result = picture
        .replace("YYYY", &format!("{:04}", year))
        .replace("MM", &format!("{:02}", month))
        .replace("DD", &format!("{:02}", day))
        .replace("HH", &format!("{:02}", hour))
        .replace("MI", &format!("{:02}", minute))
        .replace("SS", &format!("{:02}", second));

    (result, FeedbackCode::success())
}

/// LE service: CEESECI — Decompose Lilian seconds into integer components.
///
/// Returns (year, month, day, hour, minute, second, millisecond, feedback).
pub fn ceeseci(
    lilian_seconds: f64,
) -> (i32, u32, u32, u32, u32, u32, u32, FeedbackCode) {
    if lilian_seconds < 0.0 {
        return (0, 0, 0, 0, 0, 0, 0, FeedbackCode::error(2503));
    }

    let total_secs = lilian_seconds as i64;
    let lilian_day = total_secs / 86400 + 1;
    let time_of_day = total_secs % 86400;

    let (year, month, day) = match lilian_to_date(lilian_day) {
        Some(d) => d,
        None => return (0, 0, 0, 0, 0, 0, 0, FeedbackCode::error(2503)),
    };

    let hour = (time_of_day / 3600) as u32;
    let minute = ((time_of_day % 3600) / 60) as u32;
    let second = (time_of_day % 60) as u32;
    // Milliseconds from fractional part.
    let ms = ((lilian_seconds - lilian_seconds.floor()) * 1000.0) as u32;

    (year, month, day, hour, minute, second, ms, FeedbackCode::success())
}

/// LE service: CEEISEC — Compose integer components into Lilian seconds.
pub fn ceeisec(
    year: i32,
    month: u32,
    day: u32,
    hour: u32,
    minute: u32,
    second: u32,
    millisecond: u32,
) -> (f64, FeedbackCode) {
    if hour > 23 || minute > 59 || second > 59 || millisecond > 999 {
        return (0.0, FeedbackCode::error(2501));
    }

    let lilian = match date_to_lilian(year, month, day) {
        Some(l) => l,
        None => return (0.0, FeedbackCode::error(2501)),
    };

    let day_seconds = (lilian - 1) as f64 * 86400.0;
    let time_seconds = hour as f64 * 3600.0 + minute as f64 * 60.0 + second as f64;
    let ms = millisecond as f64 / 1000.0;

    (day_seconds + time_seconds + ms, FeedbackCode::success())
}

/// LE service: CEEDYWK — Get day of week from Lilian day number.
///
/// Returns day of week: 1 = Sunday, 2 = Monday, ..., 7 = Saturday.
pub fn ceedywk(lilian_day: i64) -> (u32, FeedbackCode) {
    if lilian_day < 1 {
        return (0, FeedbackCode::error(2503));
    }
    // October 15, 1582 (Lilian day 1) was a Friday.
    // Friday = 6 in our 1-based Sun=1 system.
    // day_of_week = ((lilian - 1 + 5) % 7) + 1
    //   where 5 = Friday's offset (Fri is 6th day, 0-indexed = 5)
    let dow = ((lilian_day - 1 + 5) % 7 + 1) as u32;
    (dow, FeedbackCode::success())
}

/// LE service: CEEGMT — Get current GMT as Lilian seconds and Gregorian string.
///
/// Returns (lilian_seconds, gregorian_string, feedback).
pub fn ceegmt() -> (f64, String, FeedbackCode) {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();

    // Unix epoch (Jan 1, 1970) in Lilian days.
    let unix_epoch_lilian = date_to_lilian(1970, 1, 1).unwrap();
    let unix_epoch_seconds = (unix_epoch_lilian - 1) as f64 * 86400.0;
    let lilian_secs = unix_epoch_seconds + now.as_secs_f64();

    let (formatted, _) = ceedatm(lilian_secs, "YYYY-MM-DD HH:MI:SS");
    (lilian_secs, formatted, FeedbackCode::success())
}

/// LE service: CEELOCT — Get current local date/time.
///
/// Returns (lilian_day, lilian_seconds, gregorian_string, feedback).
/// Note: In this simplified implementation, local = GMT (no timezone offset).
pub fn ceeloct() -> (i64, f64, String, FeedbackCode) {
    let (lilian_secs, gregorian, fc) = ceegmt();
    if !fc.is_success() {
        return (0, 0.0, String::new(), fc);
    }
    let lilian_day = (lilian_secs / 86400.0) as i64 + 1;
    (lilian_day, lilian_secs, gregorian, FeedbackCode::success())
}

/// LE service: CEEGMTO — Get offset from GMT to local time.
///
/// Returns (offset_hours, offset_minutes, offset_seconds, feedback).
/// In this simplified implementation, offset is always 0 (local = GMT).
pub fn ceegmto() -> (i32, i32, f64, FeedbackCode) {
    // Simplified: no timezone support, offset = 0.
    (0, 0, 0.0, FeedbackCode::success())
}

/// Century window state (for CEESCEN/CEEQCEN).
///
/// A century window defines how 2-digit years are interpreted.
/// The window spans 100 years starting from (current_year - century_window).
#[derive(Debug, Clone)]
pub struct CenturyWindow {
    /// The century window value (0-100).
    value: u32,
}

impl CenturyWindow {
    /// Create a new century window with the given value.
    pub fn new(value: u32) -> Self {
        Self { value: value.min(100) }
    }

    /// CEESCEN — Set the century window value.
    pub fn ceescen(&mut self, value: u32) -> FeedbackCode {
        if value > 100 {
            return FeedbackCode::error(2504);
        }
        self.value = value;
        FeedbackCode::success()
    }

    /// CEEQCEN — Query the current century window value.
    pub fn ceeqcen(&self) -> (u32, FeedbackCode) {
        (self.value, FeedbackCode::success())
    }

    /// Interpret a 2-digit year using the century window.
    ///
    /// The window starts at (current_year - century_window_value).
    pub fn interpret_year(&self, two_digit_year: u32, current_year: i32) -> i32 {
        let start = current_year - self.value as i32;
        let century = start / 100 * 100;
        let candidate = century + two_digit_year as i32;
        if candidate < start {
            candidate + 100
        } else {
            candidate
        }
    }
}

impl Default for CenturyWindow {
    fn default() -> Self {
        Self { value: 80 }
    }
}

/// LE service: CEE3DLY — Delay (sleep) for the specified number of seconds.
///
/// Note: This actually sleeps the calling thread.
pub fn cee3dly(seconds: f64) -> FeedbackCode {
    if seconds < 0.0 {
        return FeedbackCode::error(2505);
    }
    if seconds > 0.0 {
        std::thread::sleep(std::time::Duration::from_secs_f64(seconds));
    }
    FeedbackCode::success()
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- Story 508.1: Calendar-Accurate Date Library ---

    #[test]
    fn test_integer_of_date_lilian_epoch() {
        // AC: Given INTEGER-OF-DATE(15821015) — the Lilian epoch
        // When evaluated, Then result is 1
        let result = integer_of_date(15821015);
        assert_eq!(result, Some(1));
    }

    #[test]
    fn test_integer_of_date_leap_year() {
        // AC: Given INTEGER-OF-DATE(20240229) — leap year date
        // When evaluated, Then result is correct (February 29, 2024 is valid)
        let result = integer_of_date(20240229);
        assert!(result.is_some());
        // Verify it's exactly one day after Feb 28
        let feb28 = integer_of_date(20240228).unwrap();
        assert_eq!(result.unwrap(), feb28 + 1);
    }

    #[test]
    fn test_integer_of_date_non_leap_year() {
        // Feb 29 in non-leap year should fail
        let result = integer_of_date(20230229);
        assert!(result.is_none());
    }

    #[test]
    fn test_date_of_integer_roundtrip() {
        let lilian = integer_of_date(20240115).unwrap();
        let back = date_of_integer(lilian).unwrap();
        assert_eq!(back, 20240115);
    }

    #[test]
    fn test_lilian_epoch_roundtrip() {
        let lilian = date_to_lilian(1582, 10, 15).unwrap();
        assert_eq!(lilian, 1);
        let (y, m, d) = lilian_to_date(1).unwrap();
        assert_eq!((y, m, d), (1582, 10, 15));
    }

    #[test]
    fn test_day_after_epoch() {
        let lilian = date_to_lilian(1582, 10, 16).unwrap();
        assert_eq!(lilian, 2);
    }

    #[test]
    fn test_known_date_2000_01_01() {
        // Jan 1, 2000 — well-known reference
        let lilian = date_to_lilian(2000, 1, 1).unwrap();
        // Reverse
        let (y, m, d) = lilian_to_date(lilian).unwrap();
        assert_eq!((y, m, d), (2000, 1, 1));
    }

    #[test]
    fn test_known_date_1970_01_01() {
        let lilian = date_to_lilian(1970, 1, 1).unwrap();
        let (y, m, d) = lilian_to_date(lilian).unwrap();
        assert_eq!((y, m, d), (1970, 1, 1));
    }

    #[test]
    fn test_is_leap_year() {
        assert!(is_leap_year(2000)); // div by 400
        assert!(!is_leap_year(1900)); // div by 100, not 400
        assert!(is_leap_year(2024)); // div by 4, not 100
        assert!(!is_leap_year(2023)); // not div by 4
    }

    #[test]
    fn test_days_in_month() {
        assert_eq!(days_in_month(2024, 2), 29); // leap
        assert_eq!(days_in_month(2023, 2), 28); // non-leap
        assert_eq!(days_in_month(2024, 1), 31);
        assert_eq!(days_in_month(2024, 4), 30);
    }

    #[test]
    fn test_integer_of_day() {
        // Jan 15, 2024 = day 15 of 2024
        let lilian_from_date = integer_of_date(20240115).unwrap();
        let lilian_from_day = integer_of_day(2024015).unwrap();
        assert_eq!(lilian_from_date, lilian_from_day);
    }

    #[test]
    fn test_day_of_integer() {
        let lilian = integer_of_date(20240115).unwrap();
        let yyyyddd = day_of_integer(lilian).unwrap();
        assert_eq!(yyyyddd, 2024015);
    }

    #[test]
    fn test_invalid_date() {
        assert!(integer_of_date(20241301).is_none()); // month 13
        assert!(integer_of_date(20240132).is_none()); // day 32
        assert!(integer_of_date(15820101).is_none()); // before epoch
    }

    #[test]
    fn test_lilian_to_date_invalid() {
        assert!(lilian_to_date(0).is_none());
        assert!(lilian_to_date(-1).is_none());
    }

    #[test]
    fn test_consecutive_days() {
        // Check 365 consecutive days starting from a known date
        let start = integer_of_date(20240101).unwrap();
        for i in 0..365 {
            let lilian = start + i;
            let date = lilian_to_date(lilian);
            assert!(date.is_some(), "Day {} from start should be valid", i);
        }
    }

    #[test]
    fn test_year_boundary() {
        let dec31 = integer_of_date(20231231).unwrap();
        let jan01 = integer_of_date(20240101).unwrap();
        assert_eq!(jan01, dec31 + 1);
    }

    // --- Story 508.2: LE Date/Time Callable Services ---

    #[test]
    fn test_ceedays_yyyymmdd() {
        // AC: CALL 'CEEDAYS' USING WS-DATE, 'YYYYMMDD', WS-LILIAN, FC
        // When WS-DATE = '20240115'
        // Then WS-LILIAN contains the correct Lilian day number and FC indicates success
        let (lilian, fc) = ceedays("20240115", "YYYYMMDD");
        assert!(fc.is_success());
        assert!(lilian > 0);

        // Cross-check with intrinsic
        let expected = integer_of_date(20240115).unwrap();
        assert_eq!(lilian, expected);
    }

    #[test]
    fn test_ceedays_yyyyddd() {
        let (lilian, fc) = ceedays("2024015", "YYYYDDD");
        assert!(fc.is_success());

        let expected = integer_of_day(2024015).unwrap();
        assert_eq!(lilian, expected);
    }

    #[test]
    fn test_ceedays_mmddyyyy() {
        let (lilian, fc) = ceedays("01152024", "MMDDYYYY");
        assert!(fc.is_success());

        let expected = integer_of_date(20240115).unwrap();
        assert_eq!(lilian, expected);
    }

    #[test]
    fn test_ceedays_invalid_date() {
        let (_, fc) = ceedays("20241301", "YYYYMMDD");
        assert!(!fc.is_success());
    }

    #[test]
    fn test_ceedays_unsupported_picture() {
        let (_, fc) = ceedays("2024-01-15", "YYYY-MM-DD");
        assert!(!fc.is_success());
        assert_eq!(fc.msg_no, 2502);
    }

    #[test]
    fn test_ceedate_yyyymmdd() {
        let lilian = integer_of_date(20240115).unwrap();
        let (date_str, fc) = ceedate(lilian, "YYYYMMDD");
        assert!(fc.is_success());
        assert_eq!(date_str, "20240115");
    }

    #[test]
    fn test_ceedate_yyyyddd() {
        let lilian = integer_of_date(20240115).unwrap();
        let (date_str, fc) = ceedate(lilian, "YYYYDDD");
        assert!(fc.is_success());
        assert_eq!(date_str, "2024015");
    }

    #[test]
    fn test_ceedate_mmddyyyy() {
        let lilian = integer_of_date(20240115).unwrap();
        let (date_str, fc) = ceedate(lilian, "MMDDYYYY");
        assert!(fc.is_success());
        assert_eq!(date_str, "01152024");
    }

    #[test]
    fn test_ceedate_invalid_lilian() {
        let (_, fc) = ceedate(0, "YYYYMMDD");
        assert!(!fc.is_success());
    }

    #[test]
    fn test_ceesecs() {
        let (secs, fc) = ceesecs("20240115120000", "YYYYMMDDHHMMSS");
        assert!(fc.is_success());
        assert!(secs > 0.0);

        // Cross-check: midnight of same day
        let (secs_midnight, fc2) = ceesecs("20240115000000", "YYYYMMDDHHMMSS");
        assert!(fc2.is_success());
        // Difference should be 12 hours = 43200 seconds
        assert!((secs - secs_midnight - 43200.0).abs() < 0.001);
    }

    #[test]
    fn test_ceesecs_invalid_time() {
        let (_, fc) = ceesecs("20240115250000", "YYYYMMDDHHMMSS"); // hour 25
        assert!(!fc.is_success());
    }

    #[test]
    fn test_ceedays_ceedate_roundtrip() {
        let (lilian, fc1) = ceedays("20240229", "YYYYMMDD");
        assert!(fc1.is_success());

        let (date_str, fc2) = ceedate(lilian, "YYYYMMDD");
        assert!(fc2.is_success());
        assert_eq!(date_str, "20240229");
    }

    #[test]
    fn test_feedback_code() {
        let fc = FeedbackCode::success();
        assert!(fc.is_success());
        assert_eq!(fc.severity, 0);

        let fc = FeedbackCode::error(2501);
        assert!(!fc.is_success());
        assert_eq!(fc.severity, 2);
    }

    // --- Story LE103.1: Date/Time Conversion Services ---

    #[test]
    fn test_ceedatm_formats_lilian_seconds() {
        // Compute Lilian seconds for 2024-03-15 14:30:00
        let (secs, fc) = ceeisec(2024, 3, 15, 14, 30, 0, 0);
        assert!(fc.is_success());

        let (formatted, fc2) = ceedatm(secs, "YYYY-MM-DD HH:MI:SS");
        assert!(fc2.is_success());
        assert_eq!(formatted, "2024-03-15 14:30:00");
    }

    #[test]
    fn test_ceeseci_decompose() {
        let (secs, _) = ceeisec(2024, 3, 15, 14, 30, 0, 0);
        let (year, month, day, hour, min, sec, _ms, fc) = ceeseci(secs);
        assert!(fc.is_success());
        assert_eq!((year, month, day), (2024, 3, 15));
        assert_eq!((hour, min, sec), (14, 30, 0));
    }

    #[test]
    fn test_ceeisec_compose() {
        let (secs, fc) = ceeisec(2024, 3, 15, 14, 30, 0, 0);
        assert!(fc.is_success());
        assert!(secs > 0.0);

        // Roundtrip.
        let (year, month, day, hour, min, sec, _, fc2) = ceeseci(secs);
        assert!(fc2.is_success());
        assert_eq!((year, month, day, hour, min, sec), (2024, 3, 15, 14, 30, 0));
    }

    #[test]
    fn test_ceeisec_invalid_time() {
        let (_, fc) = ceeisec(2024, 3, 15, 25, 0, 0, 0); // hour 25
        assert!(!fc.is_success());
    }

    #[test]
    fn test_ceedywk_day_of_week() {
        // 2024-03-15 is a Friday.
        let lilian = date_to_lilian(2024, 3, 15).unwrap();
        let (dow, fc) = ceedywk(lilian);
        assert!(fc.is_success());
        assert_eq!(dow, 6); // 6 = Friday (1=Sun, 2=Mon, ..., 6=Fri, 7=Sat)
    }

    #[test]
    fn test_ceedywk_lilian_epoch_friday() {
        // Oct 15, 1582 was a Friday.
        let (dow, fc) = ceedywk(1);
        assert!(fc.is_success());
        assert_eq!(dow, 6); // Friday
    }

    #[test]
    fn test_ceedywk_sunday() {
        // Oct 17, 1582 was a Sunday (day 3).
        let (dow, fc) = ceedywk(3);
        assert!(fc.is_success());
        assert_eq!(dow, 1); // Sunday
    }

    // --- Story LE103.2: Current Time and GMT Services ---

    #[test]
    fn test_ceegmt_returns_positive_seconds() {
        let (secs, greg, fc) = ceegmt();
        assert!(fc.is_success());
        assert!(secs > 0.0);
        assert!(!greg.is_empty());
    }

    #[test]
    fn test_ceeloct_returns_valid_day() {
        let (day, secs, greg, fc) = ceeloct();
        assert!(fc.is_success());
        assert!(day > 0);
        assert!(secs > 0.0);
        assert!(!greg.is_empty());
    }

    #[test]
    fn test_ceegmto_returns_offset() {
        let (hours, minutes, secs, fc) = ceegmto();
        assert!(fc.is_success());
        // Simplified: always 0.
        assert_eq!(hours, 0);
        assert_eq!(minutes, 0);
        assert_eq!(secs, 0.0);
    }

    // --- Story LE103.3: Century Window and Delay ---

    #[test]
    fn test_century_window_default() {
        let cw = CenturyWindow::default();
        let (val, fc) = cw.ceeqcen();
        assert!(fc.is_success());
        assert_eq!(val, 80);
    }

    #[test]
    fn test_ceescen_set_century_window() {
        let mut cw = CenturyWindow::default();
        let fc = cw.ceescen(60);
        assert!(fc.is_success());
        let (val, _) = cw.ceeqcen();
        assert_eq!(val, 60);
    }

    #[test]
    fn test_century_window_interpret_year() {
        let cw = CenturyWindow::new(80);
        // Current year 2025, window starts at 1945.
        // 2-digit 25 → 2025 (within window 1945-2044).
        assert_eq!(cw.interpret_year(25, 2025), 2025);
        // 2-digit 44 → 2044.
        assert_eq!(cw.interpret_year(44, 2025), 2044);
        // 2-digit 45 → 1945.
        assert_eq!(cw.interpret_year(45, 2025), 1945);
        // 2-digit 99 → 1999.
        assert_eq!(cw.interpret_year(99, 2025), 1999);
    }

    #[test]
    fn test_cee3dly_zero_delay() {
        // Zero delay should succeed immediately.
        let fc = cee3dly(0.0);
        assert!(fc.is_success());
    }

    #[test]
    fn test_cee3dly_negative_fails() {
        let fc = cee3dly(-1.0);
        assert!(!fc.is_success());
    }
}
