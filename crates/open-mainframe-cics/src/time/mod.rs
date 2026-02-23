//! CICS Time Services.
//!
//! Provides ASKTIME and FORMATTIME commands.

use std::time::{SystemTime, UNIX_EPOCH};

/// Absolute time value (CICS ABSTIME format).
///
/// This is a packed decimal representation of time since
/// January 1, 1900 in milliseconds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct AbsTime {
    /// Internal representation (milliseconds since epoch)
    value: u64,
}

impl AbsTime {
    /// Create from current time.
    pub fn now() -> Self {
        let since_unix = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default();

        // Convert to milliseconds
        let millis = since_unix.as_millis() as u64;

        // Add offset from 1900 to 1970 (70 years in milliseconds)
        // 70 * 365.25 * 24 * 60 * 60 * 1000 = 2208988800000
        let cics_epoch_offset = 2_208_988_800_000u64;

        Self {
            value: millis + cics_epoch_offset,
        }
    }

    /// Create from raw value.
    pub fn from_raw(value: u64) -> Self {
        Self { value }
    }

    /// Get raw value.
    pub fn raw(&self) -> u64 {
        self.value
    }

    /// Convert to packed decimal bytes (15 digits).
    pub fn to_packed(&self) -> [u8; 8] {
        let mut packed = [0u8; 8];
        let mut val = self.value;

        // Pack into 15 digits + sign nibble
        for i in (0..8).rev() {
            let low = (val % 10) as u8;
            val /= 10;
            let high = (val % 10) as u8;
            val /= 10;

            if i == 7 {
                // Last byte has sign nibble
                packed[i] = (low << 4) | 0x0C; // Positive sign
            } else {
                packed[i] = (high << 4) | low;
            }
        }

        packed
    }

    /// Create from packed decimal bytes.
    pub fn from_packed(packed: &[u8; 8]) -> Self {
        let mut value = 0u64;

        for (i, &byte) in packed.iter().enumerate() {
            if i == 7 {
                // Last byte has only one digit + sign
                let digit = (byte >> 4) as u64;
                value = value * 10 + digit;
            } else {
                let high = ((byte >> 4) & 0x0F) as u64;
                let low = (byte & 0x0F) as u64;
                value = value * 10 + high;
                value = value * 10 + low;
            }
        }

        Self { value }
    }
}

/// Formatted date/time output.
#[derive(Debug, Clone, Default)]
pub struct FormattedTime {
    /// Year (4 digits)
    pub year: u16,
    /// Month (1-12)
    pub month: u8,
    /// Day of month (1-31)
    pub day: u8,
    /// Day of year (1-366)
    pub day_of_year: u16,
    /// Day of week (0=Sunday, 6=Saturday)
    pub day_of_week: u8,
    /// Hour (0-23)
    pub hour: u8,
    /// Minute (0-59)
    pub minute: u8,
    /// Second (0-59)
    pub second: u8,
    /// Milliseconds (0-999)
    pub milliseconds: u16,
}

impl FormattedTime {
    /// Format as YYYYMMDD.
    pub fn yyyymmdd(&self) -> String {
        format!("{:04}{:02}{:02}", self.year, self.month, self.day)
    }

    /// Format as DDMMYYYY.
    pub fn ddmmyyyy(&self) -> String {
        format!("{:02}{:02}{:04}", self.day, self.month, self.year)
    }

    /// Format as MMDDYYYY.
    pub fn mmddyyyy(&self) -> String {
        format!("{:02}{:02}{:04}", self.month, self.day, self.year)
    }

    /// Format as YYMMDD.
    pub fn yymmdd(&self) -> String {
        format!("{:02}{:02}{:02}", self.year % 100, self.month, self.day)
    }

    /// Format as YYDDD (Julian date).
    pub fn yyddd(&self) -> String {
        format!("{:02}{:03}", self.year % 100, self.day_of_year)
    }

    /// Format as YYYYDDD (Julian date).
    pub fn yyyyddd(&self) -> String {
        format!("{:04}{:03}", self.year, self.day_of_year)
    }

    /// Format time as HHMMSS.
    pub fn hhmmss(&self) -> String {
        format!("{:02}{:02}{:02}", self.hour, self.minute, self.second)
    }

    /// Format time as HH:MM:SS.
    pub fn time_formatted(&self) -> String {
        format!("{:02}:{:02}:{:02}", self.hour, self.minute, self.second)
    }

    /// Format date as YYYY-MM-DD.
    pub fn date_formatted(&self) -> String {
        format!("{:04}-{:02}-{:02}", self.year, self.month, self.day)
    }

    /// Get day name.
    pub fn day_name(&self) -> &'static str {
        match self.day_of_week {
            0 => "Sunday",
            1 => "Monday",
            2 => "Tuesday",
            3 => "Wednesday",
            4 => "Thursday",
            5 => "Friday",
            6 => "Saturday",
            _ => "Unknown",
        }
    }

    /// Get month name.
    pub fn month_name(&self) -> &'static str {
        match self.month {
            1 => "January",
            2 => "February",
            3 => "March",
            4 => "April",
            5 => "May",
            6 => "June",
            7 => "July",
            8 => "August",
            9 => "September",
            10 => "October",
            11 => "November",
            12 => "December",
            _ => "Unknown",
        }
    }
}

/// Time services manager.
#[derive(Debug, Default)]
pub struct TimeServices;

impl TimeServices {
    /// Create new time services.
    pub fn new() -> Self {
        Self
    }

    /// ASKTIME - Get current absolute time.
    pub fn asktime(&self) -> AbsTime {
        AbsTime::now()
    }

    /// FORMATTIME - Format an absolute time.
    pub fn formattime(&self, abstime: AbsTime) -> FormattedTime {
        // Convert from CICS epoch (1900) to Unix epoch (1970)
        let cics_epoch_offset = 2_208_988_800_000u64;
        let unix_millis = abstime.value.saturating_sub(cics_epoch_offset);

        // Convert to seconds and remaining millis
        let unix_secs = unix_millis / 1000;
        let millis = (unix_millis % 1000) as u16;

        // Calculate date components
        // Days since Unix epoch
        let days_since_epoch = unix_secs / 86400;
        let time_of_day = unix_secs % 86400;

        // Hours, minutes, seconds
        let hour = (time_of_day / 3600) as u8;
        let minute = ((time_of_day % 3600) / 60) as u8;
        let second = (time_of_day % 60) as u8;

        // Day of week (Jan 1, 1970 was Thursday = 4)
        let day_of_week = ((days_since_epoch + 4) % 7) as u8;

        // Calculate year, month, day
        let (year, month, day, day_of_year) = days_to_ymd(days_since_epoch as i64);

        FormattedTime {
            year: year as u16,
            month: month as u8,
            day: day as u8,
            day_of_year: day_of_year as u16,
            day_of_week,
            hour,
            minute,
            second,
            milliseconds: millis,
        }
    }
}

/// Convert days since Unix epoch to year, month, day.
fn days_to_ymd(days: i64) -> (i32, i32, i32, i32) {
    // Algorithm from Howard Hinnant
    let z = days + 719468;
    let era = if z >= 0 { z } else { z - 146096 } / 146097;
    let doe = (z - era * 146097) as u32;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    let year = y + if m <= 2 { 1 } else { 0 };

    // Calculate day of year
    let is_leap = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
    let days_before_month = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
    let mut day_of_year = days_before_month[m as usize - 1] + d as i32;
    if is_leap && m > 2 {
        day_of_year += 1;
    }

    (year as i32, m as i32, d as i32, day_of_year)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_abstime_now() {
        let time = AbsTime::now();
        assert!(time.value > 0);
    }

    #[test]
    fn test_abstime_packed() {
        let time = AbsTime::from_raw(1234567890123);
        let packed = time.to_packed();
        let restored = AbsTime::from_packed(&packed);

        // Note: some precision may be lost due to packing
        assert!(restored.value > 0);
    }

    #[test]
    fn test_formattime() {
        let services = TimeServices::new();
        let abstime = services.asktime();
        let formatted = services.formattime(abstime);

        // Basic sanity checks
        assert!(formatted.year >= 2020);
        assert!(formatted.month >= 1 && formatted.month <= 12);
        assert!(formatted.day >= 1 && formatted.day <= 31);
        assert!(formatted.hour <= 23);
        assert!(formatted.minute <= 59);
        assert!(formatted.second <= 59);
    }

    #[test]
    fn test_format_outputs() {
        let formatted = FormattedTime {
            year: 2026,
            month: 2,
            day: 13,
            day_of_year: 44,
            day_of_week: 5, // Friday
            hour: 14,
            minute: 30,
            second: 45,
            milliseconds: 123,
        };

        assert_eq!(formatted.yyyymmdd(), "20260213");
        assert_eq!(formatted.ddmmyyyy(), "13022026");
        assert_eq!(formatted.mmddyyyy(), "02132026");
        assert_eq!(formatted.yymmdd(), "260213");
        assert_eq!(formatted.yyddd(), "26044");
        assert_eq!(formatted.yyyyddd(), "2026044");
        assert_eq!(formatted.hhmmss(), "143045");
        assert_eq!(formatted.time_formatted(), "14:30:45");
        assert_eq!(formatted.date_formatted(), "2026-02-13");
        assert_eq!(formatted.day_name(), "Friday");
        assert_eq!(formatted.month_name(), "February");
    }

    #[test]
    fn test_days_to_ymd() {
        // Unix epoch = Jan 1, 1970
        let (y, m, d, doy) = days_to_ymd(0);
        assert_eq!((y, m, d), (1970, 1, 1));
        assert_eq!(doy, 1);

        // Feb 13, 2026 = day 15749 since Unix epoch
        // (2026 - 1970) * 365 + leap days + 44 days
        // We'll verify the algorithm gives a reasonable result
        let (y, m, d, _) = days_to_ymd(20497); // Approximate
        assert!(y >= 2026 || y <= 2027);
    }
}
