//! TIME — time-of-day retrieval in z/OS-compatible formats.

use chrono::{Datelike, Timelike, Utc};
use serde::{Deserialize, Serialize};

/// z/OS time representation in multiple formats.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MvsTime {
    /// STCK format — 64-bit TOD clock (bit 51 = 1 microsecond, epoch 1900-01-01).
    pub tod_clock: u64,
    /// Packed decimal Julian date: 0CYYDDDF (C=century, YY=year, DDD=Julian day, F=sign).
    pub date_packed: u32,
    /// Hundredths of seconds since midnight as 32-bit binary.
    pub time_binary: u32,
    /// Packed decimal time: HHMMSSTHxxxxxx (hours, minutes, seconds, tenths, hundredths).
    pub time_packed: u64,
    /// Gregorian date in packed decimal: YYYYMMDD.
    pub date_yyyymmdd: u32,
}

/// Retrieve the current time in all z/OS formats.
pub fn time_now() -> MvsTime {
    let now = Utc::now();
    from_chrono(&now)
}

/// Convert a chrono DateTime to MvsTime.
pub fn from_chrono(dt: &chrono::DateTime<Utc>) -> MvsTime {
    let year = dt.year();
    let month = dt.month();
    let day = dt.day();
    let hour = dt.hour();
    let minute = dt.minute();
    let second = dt.second();
    let nanos = dt.nanosecond();
    let hundredths = (nanos / 10_000_000) as u8;

    // Julian day of year (1-366)
    let julian_day = dt.ordinal();

    // Century indicator: 0 for 1900-1999, 1 for 2000-2099
    let century = if year >= 2000 { 1u8 } else { 0u8 };
    let yy = (year % 100) as u8;

    // date_packed: 0CYYDDDF in packed decimal
    // Format: nibbles: 0, C, Y1, Y2, D1, D2, D3, F(=0xC for positive)
    let date_packed = pack_julian_date(century, yy, julian_day as u16);

    // time_binary: hundredths of seconds since midnight
    let total_hundredths =
        hour * 360_000 + minute * 6_000 + second * 100 + hundredths as u32;

    // time_packed: HHMMSSTHxxxxxx in packed decimal (8 bytes)
    let time_packed = pack_time(hour as u8, minute as u8, second as u8, hundredths);

    // date_yyyymmdd: packed decimal YYYYMMDD
    let date_yyyymmdd = pack_yyyymmdd(year as u16, month as u8, day as u8);

    // TOD clock: microseconds since 1900-01-01 00:00:00 UTC, shifted left by 12 bits
    let tod_clock = compute_tod_clock(dt);

    MvsTime {
        tod_clock,
        date_packed,
        time_binary: total_hundredths,
        time_packed,
        date_yyyymmdd,
    }
}

/// Pack Julian date as 0CYYDDDF.
///
/// Each digit is a BCD nibble. F = 0xC (positive sign).
fn pack_julian_date(century: u8, year: u8, julian_day: u16) -> u32 {
    let c = century;
    let y1 = year / 10;
    let y2 = year % 10;
    let d1 = (julian_day / 100) as u8;
    let d2 = ((julian_day / 10) % 10) as u8;
    let d3 = (julian_day % 10) as u8;

    // 0CYYDDDF: 8 nibbles packed into 4 bytes
    let b0 = c & 0x0F;
    let b1 = (y1 << 4) | y2;
    let b2 = (d1 << 4) | d2;
    let b3 = (d3 << 4) | 0x0C; // F = positive sign

    ((b0 as u32) << 24) | ((b1 as u32) << 16) | ((b2 as u32) << 8) | (b3 as u32)
}

/// Pack time as HHMMSSTHxxxxxx (packed into 8 bytes, upper portion used).
fn pack_time(hours: u8, minutes: u8, seconds: u8, hundredths: u8) -> u64 {
    let hh1 = hours / 10;
    let hh2 = hours % 10;
    let mm1 = minutes / 10;
    let mm2 = minutes % 10;
    let ss1 = seconds / 10;
    let ss2 = seconds % 10;
    let th1 = hundredths / 10; // tenths
    let th2 = hundredths % 10; // hundredths

    // Pack into upper 4 bytes: HH MM SS TH
    let b0 = (hh1 << 4) | hh2;
    let b1 = (mm1 << 4) | mm2;
    let b2 = (ss1 << 4) | ss2;
    let b3 = (th1 << 4) | th2;

    ((b0 as u64) << 56) | ((b1 as u64) << 48) | ((b2 as u64) << 40) | ((b3 as u64) << 32)
}

/// Pack Gregorian date as YYYYMMDD in packed decimal form.
fn pack_yyyymmdd(year: u16, month: u8, day: u8) -> u32 {
    // Simple integer encoding: YYYY * 10000 + MM * 100 + DD
    (year as u32) * 10_000 + (month as u32) * 100 + (day as u32)
}

/// Compute the TOD clock value.
///
/// TOD clock epoch is 1900-01-01 00:00:00 UTC.
/// Bit 51 = 1 microsecond, so the value is microseconds << 12.
fn compute_tod_clock(dt: &chrono::DateTime<Utc>) -> u64 {
    // Seconds since 1900-01-01 (Unix epoch is 1970-01-01)
    // Difference: 70 years = 2208988800 seconds (accounting for leap years)
    const EPOCH_DIFF_SECS: i64 = 2_208_988_800;

    let unix_secs = dt.timestamp();
    let micros_since_epoch = (unix_secs + EPOCH_DIFF_SECS) as u64 * 1_000_000
        + (dt.nanosecond() / 1_000) as u64;

    // Shift left by 12 to place microsecond at bit 51
    micros_since_epoch << 12
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    #[test]
    fn time_now_returns_valid_time() {
        let t = time_now();
        assert!(t.tod_clock > 0);
        assert!(t.time_binary < 8_640_000); // max hundredths in a day
    }

    #[test]
    fn tod_clock_stck_format() {
        // 2026-01-15 10:30:00 UTC
        let dt = Utc.with_ymd_and_hms(2026, 1, 15, 10, 30, 0).unwrap();
        let t = from_chrono(&dt);
        // Just verify it's a reasonable nonzero value
        assert!(t.tod_clock > 0);
    }

    #[test]
    fn date_packed_julian_format() {
        // 2026-02-23 = Julian day 54, century=1, year=26
        let dt = Utc.with_ymd_and_hms(2026, 2, 23, 0, 0, 0).unwrap();
        let t = from_chrono(&dt);

        // 0CYYDDDF: 0x0126054C
        // 0=0, C=1, Y=26, DDD=054, F=C
        let packed = t.date_packed;
        // Extract nibbles
        let n0 = (packed >> 28) & 0xF; // 0
        let n1 = (packed >> 24) & 0xF; // C (century)
        let n2 = (packed >> 20) & 0xF; // Y tens
        let n3 = (packed >> 16) & 0xF; // Y units
        let n4 = (packed >> 12) & 0xF; // D hundreds
        let n5 = (packed >> 8) & 0xF;  // D tens
        let n6 = (packed >> 4) & 0xF;  // D units
        let n7 = packed & 0xF;         // F sign

        assert_eq!(n0, 0);
        assert_eq!(n1, 1); // century 2000s
        assert_eq!(n2 * 10 + n3, 26);
        assert_eq!(n4 * 100 + n5 * 10 + n6, 54);
        assert_eq!(n7, 0xC); // positive sign
    }

    #[test]
    fn time_binary_hundredths_since_midnight() {
        // 01:30:45.50 = 1*360000 + 30*6000 + 45*100 + 50 = 544550
        let dt = Utc.with_ymd_and_hms(2026, 1, 1, 1, 30, 45).unwrap();
        let t = from_chrono(&dt);
        // Without sub-second precision from chrono, hundredths = 0
        assert_eq!(t.time_binary, 1 * 360_000 + 30 * 6_000 + 45 * 100);
    }

    #[test]
    fn time_packed_format() {
        let dt = Utc.with_ymd_and_hms(2026, 1, 1, 14, 25, 36).unwrap();
        let t = from_chrono(&dt);
        // Upper 4 bytes should be 14253600 in BCD
        let upper = (t.time_packed >> 32) as u32;
        let b0 = (upper >> 24) & 0xFF;
        let b1 = (upper >> 16) & 0xFF;
        let b2 = (upper >> 8) & 0xFF;
        assert_eq!(b0, 0x14); // 14 hours
        assert_eq!(b1, 0x25); // 25 minutes
        assert_eq!(b2, 0x36); // 36 seconds
    }

    #[test]
    fn date_yyyymmdd_format() {
        let dt = Utc.with_ymd_and_hms(2026, 12, 31, 0, 0, 0).unwrap();
        let t = from_chrono(&dt);
        assert_eq!(t.date_yyyymmdd, 20261231);
    }

    #[test]
    fn date_19xx_century() {
        let dt = Utc.with_ymd_and_hms(1999, 12, 31, 0, 0, 0).unwrap();
        let t = from_chrono(&dt);
        let century = (t.date_packed >> 24) & 0xF;
        assert_eq!(century, 0); // 1900s
    }
}
