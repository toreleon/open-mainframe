//! Database connection management.
//!
//! Provides connection configuration and pooling for PostgreSQL.

use crate::{Db2Error, Db2Result};
use std::env;

/// Connection configuration for PostgreSQL.
#[derive(Debug, Clone)]
pub struct Db2ConnectionConfig {
    /// Database host
    pub host: String,
    /// Database port
    pub port: u16,
    /// Database name
    pub database: String,
    /// Username
    pub user: String,
    /// Password
    pub password: Option<String>,
    /// Connection pool size
    pub pool_size: u32,
    /// Connection timeout in seconds
    pub connect_timeout: u32,
}

impl Db2ConnectionConfig {
    /// Create a new connection configuration.
    pub fn new(host: &str, port: u16, database: &str, user: &str) -> Self {
        Self {
            host: host.to_string(),
            port,
            database: database.to_string(),
            user: user.to_string(),
            password: None,
            pool_size: 10,
            connect_timeout: 30,
        }
    }

    /// Set the password.
    pub fn with_password(mut self, password: &str) -> Self {
        self.password = Some(password.to_string());
        self
    }

    /// Set the pool size.
    pub fn with_pool_size(mut self, size: u32) -> Self {
        self.pool_size = size;
        self
    }

    /// Set the connection timeout.
    pub fn with_timeout(mut self, seconds: u32) -> Self {
        self.connect_timeout = seconds;
        self
    }

    /// Create configuration from environment variables.
    ///
    /// Environment variables:
    /// - DB2_HOST: Database host (default: localhost)
    /// - DB2_PORT: Database port (default: 5432)
    /// - DB2_DATABASE: Database name (required)
    /// - DB2_USER: Username (required)
    /// - DB2_PASSWORD: Password (optional)
    /// - DB2_POOL_SIZE: Connection pool size (default: 10)
    pub fn from_env() -> Db2Result<Self> {
        let host = env::var("DB2_HOST").unwrap_or_else(|_| "localhost".to_string());
        let port: u16 = env::var("DB2_PORT")
            .unwrap_or_else(|_| "5432".to_string())
            .parse()
            .map_err(|_| Db2Error::SyntaxError {
                line: 0,
                message: "Invalid DB2_PORT value".to_string(),
            })?;

        let database = env::var("DB2_DATABASE").map_err(|_| Db2Error::SyntaxError {
            line: 0,
            message: "DB2_DATABASE environment variable not set".to_string(),
        })?;

        let user = env::var("DB2_USER").map_err(|_| Db2Error::SyntaxError {
            line: 0,
            message: "DB2_USER environment variable not set".to_string(),
        })?;

        let password = env::var("DB2_PASSWORD").ok();

        let pool_size: u32 = env::var("DB2_POOL_SIZE")
            .unwrap_or_else(|_| "10".to_string())
            .parse()
            .unwrap_or(10);

        Ok(Self {
            host,
            port,
            database,
            user,
            password,
            pool_size,
            connect_timeout: 30,
        })
    }

    /// Generate PostgreSQL connection string.
    pub fn connection_string(&self) -> String {
        let mut conn = format!(
            "host={} port={} dbname={} user={}",
            self.host, self.port, self.database, self.user
        );

        if let Some(ref password) = self.password {
            conn.push_str(&format!(" password={}", password));
        }

        conn.push_str(&format!(" connect_timeout={}", self.connect_timeout));

        conn
    }
}

impl Default for Db2ConnectionConfig {
    fn default() -> Self {
        Self {
            host: "localhost".to_string(),
            port: 5432,
            database: "db2".to_string(),
            user: "db2user".to_string(),
            password: None,
            pool_size: 10,
            connect_timeout: 30,
        }
    }
}

/// Database connection wrapper.
#[derive(Debug)]
pub struct Db2Connection {
    config: Db2ConnectionConfig,
    #[cfg(feature = "postgres")]
    pool: Option<r2d2::Pool<r2d2_postgres::PostgresConnectionManager<postgres::NoTls>>>,
}

impl Db2Connection {
    /// Create a new connection (does not connect immediately).
    pub fn new(config: Db2ConnectionConfig) -> Self {
        Self {
            config,
            #[cfg(feature = "postgres")]
            pool: None,
        }
    }

    /// Get the connection configuration.
    pub fn config(&self) -> &Db2ConnectionConfig {
        &self.config
    }

    /// Connect to the database.
    #[cfg(feature = "postgres")]
    pub fn connect(&mut self) -> Db2Result<()> {
        use r2d2_postgres::PostgresConnectionManager;

        let manager = PostgresConnectionManager::new(
            self.config.connection_string().parse().map_err(|e| {
                Db2Error::SyntaxError {
                    line: 0,
                    message: format!("Invalid connection string: {}", e),
                }
            })?,
            postgres::NoTls,
        );

        let pool = r2d2::Pool::builder()
            .max_size(self.config.pool_size)
            .build(manager)
            .map_err(|e| Db2Error::SyntaxError {
                line: 0,
                message: format!("Failed to create connection pool: {}", e),
            })?;

        self.pool = Some(pool);
        Ok(())
    }

    /// Connect to the database (no-op without postgres feature).
    #[cfg(not(feature = "postgres"))]
    pub fn connect(&mut self) -> Db2Result<()> {
        Ok(())
    }

    /// Check if connected.
    #[cfg(feature = "postgres")]
    pub fn is_connected(&self) -> bool {
        self.pool.is_some()
    }

    /// Check if connected (always false without postgres feature).
    #[cfg(not(feature = "postgres"))]
    pub fn is_connected(&self) -> bool {
        false
    }

    /// Execute a query and return rows (live PostgreSQL).
    #[cfg(feature = "postgres")]
    pub fn execute_query(
        &self,
        sql: &str,
        params: &[crate::runtime::SqlValue],
    ) -> Result<Vec<crate::runtime::SqlRow>, String> {
        use crate::runtime::{SqlRow, SqlValue};

        let pool = self.pool.as_ref().ok_or("Not connected")?;
        let mut client = pool.get().map_err(|e| format!("Pool error: {}", e))?;

        // Convert SqlValue params to postgres params
        let pg_params = sql_values_to_pg_params(params);
        let param_refs: Vec<&(dyn postgres::types::ToSql + Sync)> = pg_params
            .iter()
            .map(|p| p.as_ref() as &(dyn postgres::types::ToSql + Sync))
            .collect();

        let rows = client.query(sql, &param_refs).map_err(|e| {
            format!("PG:{}: {}", pg_error_state(&e), e)
        })?;

        let mut result = Vec::new();
        for row in &rows {
            let mut sql_row = SqlRow::new();
            for (idx, col) in row.columns().iter().enumerate() {
                let value = pg_value_to_sql_value(&row, idx, col.type_());
                sql_row.add_column(col.name(), value);
            }
            result.push(sql_row);
        }

        Ok(result)
    }

    /// Execute a non-query statement and return rows affected (live PostgreSQL).
    #[cfg(feature = "postgres")]
    pub fn execute_update(
        &self,
        sql: &str,
        params: &[crate::runtime::SqlValue],
    ) -> Result<u64, String> {
        let pool = self.pool.as_ref().ok_or("Not connected")?;
        let mut client = pool.get().map_err(|e| format!("Pool error: {}", e))?;

        let pg_params = sql_values_to_pg_params(params);
        let param_refs: Vec<&(dyn postgres::types::ToSql + Sync)> = pg_params
            .iter()
            .map(|p| p.as_ref() as &(dyn postgres::types::ToSql + Sync))
            .collect();

        let count = client.execute(sql, &param_refs).map_err(|e| {
            format!("PG:{}: {}", pg_error_state(&e), e)
        })?;

        Ok(count)
    }
}

/// Extract the SQLSTATE from a postgres error, defaulting to "HY000".
#[cfg(feature = "postgres")]
fn pg_error_state(err: &postgres::Error) -> String {
    err.as_db_error()
        .and_then(|db_err| db_err.code().map(|c| c.code().to_string()))
        .unwrap_or_else(|| "HY000".to_string())
}

/// Convert `SqlValue` slice to boxed postgres params.
#[cfg(feature = "postgres")]
fn sql_values_to_pg_params(
    params: &[crate::runtime::SqlValue],
) -> Vec<Box<dyn postgres::types::ToSql + Sync>> {
    use crate::runtime::SqlValue;

    params
        .iter()
        .map(|v| -> Box<dyn postgres::types::ToSql + Sync> {
            match v {
                SqlValue::Null => Box::new(Option::<String>::None),
                SqlValue::String(s) => Box::new(s.clone()),
                SqlValue::Integer(i) => Box::new(*i),
                SqlValue::Float(f) => Box::new(*f),
                SqlValue::Boolean(b) => Box::new(*b),
                SqlValue::Binary(b) => Box::new(b.clone()),
            }
        })
        .collect()
}

/// Convert a single PostgreSQL column value to `SqlValue`.
#[cfg(feature = "postgres")]
fn pg_value_to_sql_value(
    row: &postgres::Row,
    idx: usize,
    col_type: &postgres::types::Type,
) -> crate::runtime::SqlValue {
    use crate::runtime::SqlValue;
    use postgres::types::Type;

    // Try to extract based on column type
    match *col_type {
        Type::BOOL => row
            .try_get::<_, bool>(idx)
            .map(SqlValue::Boolean)
            .unwrap_or(SqlValue::Null),
        Type::INT2 => row
            .try_get::<_, i16>(idx)
            .map(|v| SqlValue::Integer(v as i64))
            .unwrap_or(SqlValue::Null),
        Type::INT4 => row
            .try_get::<_, i32>(idx)
            .map(|v| SqlValue::Integer(v as i64))
            .unwrap_or(SqlValue::Null),
        Type::INT8 => row
            .try_get::<_, i64>(idx)
            .map(SqlValue::Integer)
            .unwrap_or(SqlValue::Null),
        Type::FLOAT4 => row
            .try_get::<_, f32>(idx)
            .map(|v| SqlValue::Float(v as f64))
            .unwrap_or(SqlValue::Null),
        Type::FLOAT8 => row
            .try_get::<_, f64>(idx)
            .map(SqlValue::Float)
            .unwrap_or(SqlValue::Null),
        Type::BYTEA => row
            .try_get::<_, Vec<u8>>(idx)
            .map(SqlValue::Binary)
            .unwrap_or(SqlValue::Null),
        _ => {
            // Default: try as string
            row.try_get::<_, String>(idx)
                .map(SqlValue::String)
                .unwrap_or(SqlValue::Null)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_default() {
        let config = Db2ConnectionConfig::default();
        assert_eq!(config.host, "localhost");
        assert_eq!(config.port, 5432);
    }

    #[test]
    fn test_config_builder() {
        let config = Db2ConnectionConfig::new("dbhost", 5433, "mydb", "admin")
            .with_password("secret")
            .with_pool_size(20);

        assert_eq!(config.host, "dbhost");
        assert_eq!(config.port, 5433);
        assert_eq!(config.database, "mydb");
        assert_eq!(config.user, "admin");
        assert_eq!(config.password, Some("secret".to_string()));
        assert_eq!(config.pool_size, 20);
    }

    #[test]
    fn test_connection_string() {
        let config = Db2ConnectionConfig::new("localhost", 5432, "testdb", "user")
            .with_password("pass");
        let conn_str = config.connection_string();

        assert!(conn_str.contains("host=localhost"));
        assert!(conn_str.contains("port=5432"));
        assert!(conn_str.contains("dbname=testdb"));
        assert!(conn_str.contains("user=user"));
        assert!(conn_str.contains("password=pass"));
    }

    #[test]
    fn test_connection_creation() {
        let config = Db2ConnectionConfig::default();
        let conn = Db2Connection::new(config);
        assert!(!conn.is_connected());
    }
}
