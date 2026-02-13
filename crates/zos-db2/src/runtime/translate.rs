//! SQL translation from DB2 to PostgreSQL dialect.
//!
//! Handles syntax differences between DB2 and PostgreSQL.

use std::collections::HashMap;

/// SQL translator for DB2 to PostgreSQL conversion.
pub struct SqlTranslator {
    /// Function mappings
    function_map: HashMap<String, String>,
}

impl SqlTranslator {
    /// Create a new translator.
    pub fn new() -> Self {
        let mut function_map = HashMap::new();

        // DB2 to PostgreSQL function mappings
        function_map.insert("VALUE".to_string(), "COALESCE".to_string());
        function_map.insert("LOCATE".to_string(), "POSITION".to_string());
        function_map.insert("POSSTR".to_string(), "POSITION".to_string());
        function_map.insert("LENGTH".to_string(), "LENGTH".to_string());
        function_map.insert("STRIP".to_string(), "TRIM".to_string());
        function_map.insert("CHAR".to_string(), "CAST".to_string());
        function_map.insert("INTEGER".to_string(), "CAST".to_string());
        function_map.insert("DECIMAL".to_string(), "CAST".to_string());
        function_map.insert("DIGITS".to_string(), "TO_CHAR".to_string());
        function_map.insert("HEX".to_string(), "ENCODE".to_string());
        function_map.insert("DAYOFWEEK".to_string(), "EXTRACT(DOW FROM".to_string());
        function_map.insert("DAYOFYEAR".to_string(), "EXTRACT(DOY FROM".to_string());
        function_map.insert("DAYS".to_string(), "DATE".to_string());

        Self { function_map }
    }

    /// Translate DB2 SQL to PostgreSQL SQL.
    pub fn translate(&self, sql: &str) -> String {
        let mut result = sql.to_string();

        // Translate FETCH FIRST n ROWS ONLY -> LIMIT n
        result = self.translate_fetch_first(&result);

        // Translate OPTIMIZE FOR n ROWS -> (remove, PostgreSQL doesn't use this)
        result = self.translate_optimize_for(&result);

        // Translate WITH UR/CS/RS/RR -> (remove, handled at connection level)
        result = self.translate_isolation_clause(&result);

        // Translate CONCAT operator
        result = self.translate_concat(&result);

        // Translate SUBSTR
        result = self.translate_substr(&result);

        // Translate CURRENT TIMESTAMP
        result = self.translate_current_timestamp(&result);

        // Translate CURRENT DATE
        result = self.translate_current_date(&result);

        // Translate functions
        result = self.translate_functions(&result);

        // Translate data types in CAST expressions
        result = self.translate_data_types(&result);

        result
    }

    /// Translate FETCH FIRST n ROWS ONLY to LIMIT n.
    fn translate_fetch_first(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();

        if let Some(pos) = upper.find("FETCH FIRST") {
            // Find the number of rows
            let after_fetch = &sql[pos + 11..];
            let mut chars = after_fetch.trim_start().chars().peekable();
            let mut num = String::new();

            while let Some(&c) = chars.peek() {
                if c.is_ascii_digit() {
                    num.push(chars.next().unwrap());
                } else {
                    break;
                }
            }

            if !num.is_empty() {
                // Find "ROWS ONLY" or "ROW ONLY"
                let remaining: String = chars.collect();
                let remaining_upper = remaining.to_uppercase();

                if remaining_upper.contains("ROW") {
                    // Find the end of "ROWS ONLY" or "ROW ONLY"
                    let end_pos = if let Some(p) = remaining_upper.find("ONLY") {
                        p + 4
                    } else {
                        remaining.len()
                    };

                    // Build the result
                    let before = &sql[..pos];
                    let after = &remaining[end_pos..];
                    return format!("{} LIMIT {} {}", before.trim(), num, after.trim());
                }
            }
        }

        sql.to_string()
    }

    /// Remove OPTIMIZE FOR clause (PostgreSQL doesn't use it).
    fn translate_optimize_for(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();

        if let Some(pos) = upper.find("OPTIMIZE FOR") {
            // Find "ROWS" after OPTIMIZE FOR
            let after = &upper[pos..];
            if let Some(rows_pos) = after.find("ROWS") {
                let end = pos + rows_pos + 4;
                let before = &sql[..pos];
                let after = &sql[end..];
                return format!("{}{}", before.trim(), after);
            }
        }

        sql.to_string()
    }

    /// Remove isolation level clauses (WITH UR, CS, RS, RR).
    fn translate_isolation_clause(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();

        for clause in &["WITH UR", "WITH CS", "WITH RS", "WITH RR"] {
            if let Some(pos) = upper.find(clause) {
                let before = &sql[..pos];
                let after = &sql[pos + clause.len()..];
                return format!("{}{}", before.trim(), after);
            }
        }

        sql.to_string()
    }

    /// Translate CONCAT function to || operator.
    fn translate_concat(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();
        let mut result = sql.to_string();

        // Simple CONCAT(a, b) -> a || b
        // This is a simplified implementation
        let mut search_pos = 0;
        while let Some(pos) = upper[search_pos..].find("CONCAT(") {
            let actual_pos = search_pos + pos;

            // Find matching parentheses
            if let Some((args, end_pos)) = self.extract_function_args(&sql[actual_pos + 7..]) {
                // Split args by comma (simplified - doesn't handle nested functions well)
                let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
                if parts.len() == 2 {
                    let replacement = format!("({} || {})", parts[0], parts[1]);
                    result = format!(
                        "{}{}{}",
                        &result[..actual_pos],
                        replacement,
                        &result[actual_pos + 7 + end_pos + 1..]
                    );
                }
            }

            search_pos = actual_pos + 1;
            if search_pos >= upper.len() {
                break;
            }
        }

        result
    }

    /// Translate SUBSTR(str, pos, len) to SUBSTRING(str FROM pos FOR len).
    fn translate_substr(&self, sql: &str) -> String {
        let upper = sql.to_uppercase();
        let mut result = sql.to_string();

        let mut search_pos = 0;
        while let Some(pos) = upper[search_pos..].find("SUBSTR(") {
            let actual_pos = search_pos + pos;

            if let Some((args, end_pos)) = self.extract_function_args(&sql[actual_pos + 7..]) {
                let parts: Vec<&str> = args.split(',').map(|s| s.trim()).collect();
                if parts.len() >= 2 {
                    let replacement = if parts.len() == 3 {
                        format!(
                            "SUBSTRING({} FROM {} FOR {})",
                            parts[0], parts[1], parts[2]
                        )
                    } else {
                        format!("SUBSTRING({} FROM {})", parts[0], parts[1])
                    };
                    result = format!(
                        "{}{}{}",
                        &result[..actual_pos],
                        replacement,
                        &result[actual_pos + 7 + end_pos + 1..]
                    );
                }
            }

            search_pos = actual_pos + 1;
            if search_pos >= upper.len() {
                break;
            }
        }

        result
    }

    /// Translate CURRENT TIMESTAMP to CURRENT_TIMESTAMP.
    fn translate_current_timestamp(&self, sql: &str) -> String {
        sql.replace("CURRENT TIMESTAMP", "CURRENT_TIMESTAMP")
            .replace("current timestamp", "CURRENT_TIMESTAMP")
    }

    /// Translate CURRENT DATE to CURRENT_DATE.
    fn translate_current_date(&self, sql: &str) -> String {
        sql.replace("CURRENT DATE", "CURRENT_DATE")
            .replace("current date", "CURRENT_DATE")
    }

    /// Translate DB2 functions to PostgreSQL equivalents.
    fn translate_functions(&self, sql: &str) -> String {
        let mut result = sql.to_string();

        for (db2_func, pg_func) in &self.function_map {
            // Case-insensitive replacement
            let pattern = format!("{}(", db2_func);
            let replacement = format!("{}(", pg_func);

            // Simple case-insensitive find and replace
            let upper = result.to_uppercase();
            let pattern_upper = pattern.to_uppercase();

            if let Some(pos) = upper.find(&pattern_upper) {
                result = format!(
                    "{}{}{}",
                    &result[..pos],
                    replacement,
                    &result[pos + pattern.len()..]
                );
            }
        }

        result
    }

    /// Translate DB2 data types to PostgreSQL.
    fn translate_data_types(&self, sql: &str) -> String {
        sql.replace("VARCHAR FOR BIT DATA", "BYTEA")
            .replace("varchar for bit data", "BYTEA")
            .replace("GRAPHIC", "TEXT")
            .replace("VARGRAPHIC", "TEXT")
            .replace("LONG VARCHAR", "TEXT")
            .replace("CLOB", "TEXT")
            .replace("BLOB", "BYTEA")
            .replace("DBCLOB", "TEXT")
    }

    /// Extract function arguments (finds matching closing parenthesis).
    fn extract_function_args(&self, sql: &str) -> Option<(String, usize)> {
        let mut depth = 1;
        let mut end_pos = 0;

        for (i, c) in sql.chars().enumerate() {
            match c {
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        end_pos = i;
                        break;
                    }
                }
                _ => {}
            }
        }

        if depth == 0 {
            Some((sql[..end_pos].to_string(), end_pos))
        } else {
            None
        }
    }
}

impl Default for SqlTranslator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fetch_first_translation() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM T FETCH FIRST 10 ROWS ONLY";
        let result = translator.translate(sql);
        assert!(result.contains("LIMIT 10"));
        assert!(!result.contains("FETCH FIRST"));
    }

    #[test]
    fn test_fetch_first_single_row() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM T FETCH FIRST 1 ROW ONLY";
        let result = translator.translate(sql);
        assert!(result.contains("LIMIT 1"));
    }

    #[test]
    fn test_current_timestamp() {
        let translator = SqlTranslator::new();

        let sql = "SELECT CURRENT TIMESTAMP FROM SYSIBM.SYSDUMMY1";
        let result = translator.translate(sql);
        assert!(result.contains("CURRENT_TIMESTAMP"));
    }

    #[test]
    fn test_current_date() {
        let translator = SqlTranslator::new();

        let sql = "SELECT CURRENT DATE FROM T";
        let result = translator.translate(sql);
        assert!(result.contains("CURRENT_DATE"));
    }

    #[test]
    fn test_concat_translation() {
        let translator = SqlTranslator::new();

        let sql = "SELECT CONCAT(A, B) FROM T";
        let result = translator.translate(sql);
        assert!(result.contains("||"));
    }

    #[test]
    fn test_substr_translation() {
        let translator = SqlTranslator::new();

        let sql = "SELECT SUBSTR(NAME, 1, 5) FROM T";
        let result = translator.translate(sql);
        assert!(result.contains("SUBSTRING"));
        assert!(result.contains("FROM 1 FOR 5"));
    }

    #[test]
    fn test_isolation_removal() {
        let translator = SqlTranslator::new();

        let sql = "SELECT * FROM T WITH UR";
        let result = translator.translate(sql);
        assert!(!result.contains("WITH UR"));
    }

    #[test]
    fn test_data_type_translation() {
        let translator = SqlTranslator::new();

        let sql = "CAST(X AS VARCHAR FOR BIT DATA)";
        let result = translator.translate(sql);
        assert!(result.contains("BYTEA"));
    }

    #[test]
    fn test_value_to_coalesce() {
        let translator = SqlTranslator::new();

        let sql = "SELECT VALUE(A, B) FROM T";
        let result = translator.translate(sql);
        assert!(result.contains("COALESCE"));
    }
}
