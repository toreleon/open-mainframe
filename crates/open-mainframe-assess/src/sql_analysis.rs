//! DB2 SQL complexity analysis for embedded SQL in COBOL.
//!
//! Scans COBOL source for EXEC SQL statements, classifies them by
//! complexity, and produces PostgreSQL compatibility notes.

use std::collections::HashMap;

/// SQL statement complexity level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize)]
pub enum SqlComplexity {
    /// Simple single-table SELECT, INSERT, UPDATE, DELETE.
    Simple,
    /// JOIN queries or multi-table operations.
    Join,
    /// Subquery or correlated subquery.
    Subquery,
    /// Cursor declarations and operations.
    Cursor,
    /// Dynamic SQL (PREPARE, EXECUTE).
    Dynamic,
    /// DDL or utility statements.
    Utility,
}

impl SqlComplexity {
    /// Human-readable label.
    pub fn label(&self) -> &'static str {
        match self {
            SqlComplexity::Simple => "Simple Query",
            SqlComplexity::Join => "Join Query",
            SqlComplexity::Subquery => "Subquery",
            SqlComplexity::Cursor => "Cursor Operation",
            SqlComplexity::Dynamic => "Dynamic SQL",
            SqlComplexity::Utility => "Utility/DDL",
        }
    }

    /// Effort weight relative to simple query.
    pub fn effort_weight(&self) -> f64 {
        match self {
            SqlComplexity::Simple => 1.0,
            SqlComplexity::Join => 1.5,
            SqlComplexity::Subquery => 2.0,
            SqlComplexity::Cursor => 2.5,
            SqlComplexity::Dynamic => 3.0,
            SqlComplexity::Utility => 1.0,
        }
    }
}

/// A classified SQL statement.
#[derive(Debug, Clone, serde::Serialize)]
pub struct SqlStatement {
    /// The SQL verb (SELECT, INSERT, DECLARE CURSOR, etc.).
    pub verb: String,
    /// Complexity classification.
    pub complexity: SqlComplexity,
    /// Source line number.
    pub line: usize,
    /// PostgreSQL compatibility notes (if any).
    pub pg_notes: Vec<String>,
}

/// SQL analysis result for a program.
#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct SqlAnalysis {
    /// All SQL statements found.
    pub statements: Vec<SqlStatement>,
    /// Count by complexity level.
    pub by_complexity: HashMap<SqlComplexity, usize>,
    /// Total SQL statement count.
    pub total_count: usize,
    /// Estimated SQL migration effort (weighted sum).
    pub effort_score: f64,
}

impl SqlAnalysis {
    /// Analyze COBOL source for embedded SQL.
    pub fn from_source(source: &str) -> Self {
        let mut analysis = SqlAnalysis::default();
        let upper = source.to_uppercase();
        let lines: Vec<&str> = upper.lines().collect();

        // Collect EXEC SQL ... END-EXEC blocks
        let mut in_sql = false;
        let mut sql_start_line = 0;
        let mut sql_buf = String::new();

        for (idx, line) in lines.iter().enumerate() {
            let line_num = idx + 1;

            // Skip comments
            if line.len() > 6 && line.as_bytes().get(6) == Some(&b'*') {
                continue;
            }

            let trimmed = line.trim();

            if !in_sql {
                if let Some(pos) = trimmed.find("EXEC SQL") {
                    in_sql = true;
                    sql_start_line = line_num;
                    let after = &trimmed[pos + 8..];
                    sql_buf = after.to_string();

                    // Check for single-line statement
                    if sql_buf.contains("END-EXEC") {
                        sql_buf = sql_buf.replace("END-EXEC", "");
                        let stmt = classify_sql(sql_buf.trim(), sql_start_line);
                        analysis.statements.push(stmt);
                        in_sql = false;
                        sql_buf.clear();
                    }
                }
            } else if trimmed.contains("END-EXEC") {
                let before_end = trimmed.replace("END-EXEC", "");
                sql_buf.push(' ');
                sql_buf.push_str(before_end.trim());
                let stmt = classify_sql(sql_buf.trim(), sql_start_line);
                analysis.statements.push(stmt);
                in_sql = false;
                sql_buf.clear();
            } else {
                sql_buf.push(' ');
                sql_buf.push_str(trimmed);
            }
        }

        // Compute summary
        for stmt in &analysis.statements {
            *analysis.by_complexity.entry(stmt.complexity).or_insert(0) += 1;
            analysis.total_count += 1;
            analysis.effort_score += stmt.complexity.effort_weight();
        }

        analysis
    }

    /// Get simple query count.
    pub fn simple_count(&self) -> usize {
        self.by_complexity.get(&SqlComplexity::Simple).copied().unwrap_or(0)
    }

    /// Get cursor operation count.
    pub fn cursor_count(&self) -> usize {
        self.by_complexity.get(&SqlComplexity::Cursor).copied().unwrap_or(0)
    }

    /// Get all PostgreSQL compatibility notes.
    pub fn pg_compatibility_notes(&self) -> Vec<&str> {
        self.statements
            .iter()
            .flat_map(|s| s.pg_notes.iter().map(|n| n.as_str()))
            .collect()
    }
}

/// Classify a single SQL statement by complexity and add PG compatibility notes.
fn classify_sql(sql: &str, line: usize) -> SqlStatement {
    let trimmed = sql.trim();
    let mut pg_notes = Vec::new();

    // Determine verb and base complexity
    let (verb, mut complexity) = if trimmed.starts_with("DECLARE") && trimmed.contains("CURSOR") {
        // Check for WITH HOLD
        if trimmed.contains("WITH HOLD") {
            pg_notes.push(
                "DECLARE CURSOR WITH HOLD: Supported in PostgreSQL, but behavior differs across transaction boundaries".to_string()
            );
        }
        ("DECLARE CURSOR".to_string(), SqlComplexity::Cursor)
    } else if trimmed.starts_with("OPEN") {
        ("OPEN CURSOR".to_string(), SqlComplexity::Cursor)
    } else if trimmed.starts_with("FETCH") {
        ("FETCH".to_string(), SqlComplexity::Cursor)
    } else if trimmed.starts_with("CLOSE") {
        ("CLOSE CURSOR".to_string(), SqlComplexity::Cursor)
    } else if trimmed.starts_with("PREPARE") {
        ("PREPARE".to_string(), SqlComplexity::Dynamic)
    } else if trimmed.starts_with("EXECUTE") && !trimmed.starts_with("EXECUTE IMMEDIATE") {
        ("EXECUTE".to_string(), SqlComplexity::Dynamic)
    } else if trimmed.starts_with("EXECUTE IMMEDIATE") {
        ("EXECUTE IMMEDIATE".to_string(), SqlComplexity::Dynamic)
    } else if trimmed.starts_with("SELECT") {
        ("SELECT".to_string(), SqlComplexity::Simple)
    } else if trimmed.starts_with("INSERT") {
        ("INSERT".to_string(), SqlComplexity::Simple)
    } else if trimmed.starts_with("UPDATE") {
        ("UPDATE".to_string(), SqlComplexity::Simple)
    } else if trimmed.starts_with("DELETE") {
        ("DELETE".to_string(), SqlComplexity::Simple)
    } else if trimmed.starts_with("CREATE") || trimmed.starts_with("DROP")
        || trimmed.starts_with("ALTER") || trimmed.starts_with("GRANT")
    {
        let w = trimmed.split_whitespace().next().unwrap_or("DDL");
        (w.to_string(), SqlComplexity::Utility)
    } else if trimmed.starts_with("WHENEVER") {
        if trimmed.contains("SQLERROR") {
            pg_notes.push(
                "WHENEVER SQLERROR: Not directly supported in PostgreSQL; use exception handling in PL/pgSQL".to_string()
            );
        }
        if trimmed.contains("SQLWARNING") {
            pg_notes.push(
                "WHENEVER SQLWARNING: Not directly supported in PostgreSQL; check SQLSTATE manually".to_string()
            );
        }
        ("WHENEVER".to_string(), SqlComplexity::Utility)
    } else if trimmed.starts_with("INCLUDE") {
        ("INCLUDE".to_string(), SqlComplexity::Utility)
    } else if trimmed.starts_with("COMMIT") || trimmed.starts_with("ROLLBACK") {
        let w = trimmed.split_whitespace().next().unwrap_or("COMMIT");
        (w.to_string(), SqlComplexity::Utility)
    } else {
        let w = trimmed.split_whitespace().next().unwrap_or("UNKNOWN");
        (w.to_string(), SqlComplexity::Simple)
    };

    // Upgrade complexity for JOINs / subqueries in DML statements
    if matches!(complexity, SqlComplexity::Simple) {
        if trimmed.contains(" JOIN ") || trimmed.contains(" INNER JOIN ")
            || trimmed.contains(" LEFT JOIN ") || trimmed.contains(" RIGHT JOIN ")
            || trimmed.contains(" OUTER JOIN ")
        {
            complexity = SqlComplexity::Join;
        } else if trimmed.contains("(SELECT") || trimmed.contains("( SELECT")
            || trimmed.contains("EXISTS (") || trimmed.contains("EXISTS(")
        {
            complexity = SqlComplexity::Subquery;
        }
    }

    // DB2-specific notes
    if trimmed.contains("FOR UPDATE OF") {
        pg_notes.push("FOR UPDATE OF: Supported in PostgreSQL with same syntax".to_string());
    }
    if trimmed.contains("WITH UR") || trimmed.contains("WITH CS") || trimmed.contains("WITH RS")
        || trimmed.contains("WITH RR")
    {
        pg_notes.push(
            "DB2 isolation clause (WITH UR/CS/RS/RR): Use PostgreSQL SET TRANSACTION ISOLATION LEVEL instead".to_string()
        );
    }
    if trimmed.contains("OPTIMIZE FOR") {
        pg_notes.push(
            "OPTIMIZE FOR n ROWS: No direct PostgreSQL equivalent; consider cursor-based fetching".to_string()
        );
    }

    SqlStatement {
        verb,
        complexity,
        line,
        pg_notes,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_queries() {
        let source = r#"
       IDENTIFICATION DIVISION.
       PROCEDURE DIVISION.
           EXEC SQL SELECT NAME INTO :WS-NAME FROM CUSTOMER WHERE ID = :WS-ID END-EXEC.
           EXEC SQL INSERT INTO ORDERS (ID, CUST) VALUES (:WS-ID, :WS-CUST) END-EXEC.
           EXEC SQL UPDATE CUSTOMER SET NAME = :WS-NAME WHERE ID = :WS-ID END-EXEC.
           EXEC SQL DELETE FROM ORDERS WHERE ID = :WS-ID END-EXEC.
           EXEC SQL SELECT COUNT(*) INTO :WS-COUNT FROM CUSTOMER END-EXEC.
"#;
        let analysis = SqlAnalysis::from_source(source);
        assert_eq!(analysis.total_count, 5);
        assert_eq!(analysis.simple_count(), 5);
    }

    #[test]
    fn test_cursor_operations() {
        let source = r#"
           EXEC SQL DECLARE CUSTCUR CURSOR FOR
               SELECT NAME, ADDR FROM CUSTOMER
           END-EXEC.
           EXEC SQL OPEN CUSTCUR END-EXEC.
           EXEC SQL FETCH CUSTCUR INTO :WS-NAME, :WS-ADDR END-EXEC.
           EXEC SQL CLOSE CUSTCUR END-EXEC.
"#;
        let analysis = SqlAnalysis::from_source(source);
        assert_eq!(analysis.cursor_count(), 4);
    }

    #[test]
    fn test_join_detection() {
        let source = r#"
           EXEC SQL SELECT C.NAME, O.TOTAL
               FROM CUSTOMER C
               INNER JOIN ORDERS O ON C.ID = O.CUST_ID
               WHERE C.ACTIVE = 'Y'
           END-EXEC.
"#;
        let analysis = SqlAnalysis::from_source(source);
        assert_eq!(analysis.total_count, 1);
        assert_eq!(
            *analysis.by_complexity.get(&SqlComplexity::Join).unwrap(),
            1
        );
    }

    #[test]
    fn test_subquery_detection() {
        let source = r#"
           EXEC SQL SELECT NAME INTO :WS-NAME
               FROM CUSTOMER
               WHERE ID IN (SELECT CUST_ID FROM ORDERS)
           END-EXEC.
"#;
        let analysis = SqlAnalysis::from_source(source);
        assert_eq!(analysis.total_count, 1);
        assert_eq!(
            *analysis.by_complexity.get(&SqlComplexity::Subquery).unwrap(),
            1
        );
    }

    #[test]
    fn test_dynamic_sql() {
        let source = r#"
           EXEC SQL PREPARE STMT1 FROM :WS-SQL END-EXEC.
           EXEC SQL EXECUTE STMT1 END-EXEC.
           EXEC SQL EXECUTE IMMEDIATE :WS-DYNAMIC END-EXEC.
"#;
        let analysis = SqlAnalysis::from_source(source);
        assert_eq!(
            *analysis.by_complexity.get(&SqlComplexity::Dynamic).unwrap(),
            3
        );
    }

    #[test]
    fn test_pg_compatibility_cursor_with_hold() {
        let source = r#"
           EXEC SQL DECLARE MYCUR CURSOR WITH HOLD FOR
               SELECT * FROM ACCOUNTS
           END-EXEC.
"#;
        let analysis = SqlAnalysis::from_source(source);
        let notes = analysis.pg_compatibility_notes();
        assert!(!notes.is_empty());
        assert!(notes[0].contains("WITH HOLD"));
    }

    #[test]
    fn test_pg_compatibility_whenever() {
        let source = r#"
           EXEC SQL WHENEVER SQLERROR GOTO ERR-RTN END-EXEC.
"#;
        let analysis = SqlAnalysis::from_source(source);
        let notes = analysis.pg_compatibility_notes();
        assert!(!notes.is_empty());
        assert!(notes[0].contains("WHENEVER SQLERROR"));
    }

    #[test]
    fn test_db2_isolation_notes() {
        let source = r#"
           EXEC SQL SELECT NAME INTO :WS-NAME
               FROM CUSTOMER WHERE ID = :WS-ID WITH UR
           END-EXEC.
"#;
        let analysis = SqlAnalysis::from_source(source);
        let notes = analysis.pg_compatibility_notes();
        assert!(notes.iter().any(|n| n.contains("WITH UR")));
    }

    #[test]
    fn test_effort_score() {
        let source = r#"
           EXEC SQL SELECT X INTO :Y FROM T END-EXEC.
           EXEC SQL DECLARE C1 CURSOR FOR SELECT X FROM T END-EXEC.
"#;
        let analysis = SqlAnalysis::from_source(source);
        // Simple=1.0, Cursor=2.5 => 3.5
        assert!((analysis.effort_score - 3.5).abs() < 0.01);
    }

    #[test]
    fn test_empty_source() {
        let analysis = SqlAnalysis::from_source("");
        assert_eq!(analysis.total_count, 0);
        assert!(analysis.statements.is_empty());
    }

    #[test]
    fn test_complexity_labels() {
        assert_eq!(SqlComplexity::Simple.label(), "Simple Query");
        assert_eq!(SqlComplexity::Cursor.label(), "Cursor Operation");
        assert_eq!(SqlComplexity::Dynamic.label(), "Dynamic SQL");
    }
}
