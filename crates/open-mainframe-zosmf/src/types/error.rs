//! z/OSMF error response types.

use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use serde::{Deserialize, Serialize};

/// z/OSMF error response body — matches IBM z/OSMF JSON error format.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZosmfErrorBody {
    /// Return code.
    pub rc: i32,
    /// Reason code.
    pub reason: i32,
    /// Error category.
    pub category: i32,
    /// Human-readable error message.
    pub message: String,
    /// Additional error details.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub details: Vec<String>,
    /// Stack trace information.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stack: Option<String>,
}

/// z/OSMF error response — combines HTTP status with z/OSMF error body.
#[derive(Debug)]
pub struct ZosmfErrorResponse {
    /// HTTP status code.
    pub status: StatusCode,
    /// z/OSMF error body.
    pub body: ZosmfErrorBody,
}

impl ZosmfErrorResponse {
    /// Create a 400 Bad Request error.
    pub fn bad_request(message: impl Into<String>) -> Self {
        Self {
            status: StatusCode::BAD_REQUEST,
            body: ZosmfErrorBody {
                rc: 4,
                reason: 0,
                category: 1,
                message: message.into(),
                details: Vec::new(),
                stack: None,
            },
        }
    }

    /// Create a 401 Unauthorized error.
    pub fn unauthorized(message: impl Into<String>) -> Self {
        Self {
            status: StatusCode::UNAUTHORIZED,
            body: ZosmfErrorBody {
                rc: 4,
                reason: 0,
                category: 2,
                message: message.into(),
                details: Vec::new(),
                stack: None,
            },
        }
    }

    /// Create a 403 Forbidden error.
    pub fn forbidden(message: impl Into<String>) -> Self {
        Self {
            status: StatusCode::FORBIDDEN,
            body: ZosmfErrorBody {
                rc: 8,
                reason: 0,
                category: 3,
                message: message.into(),
                details: Vec::new(),
                stack: None,
            },
        }
    }

    /// Create a 404 Not Found error.
    pub fn not_found(message: impl Into<String>) -> Self {
        Self {
            status: StatusCode::NOT_FOUND,
            body: ZosmfErrorBody {
                rc: 8,
                reason: 0,
                category: 4,
                message: message.into(),
                details: Vec::new(),
                stack: None,
            },
        }
    }

    /// Create a 500 Internal Server Error.
    pub fn internal(message: impl Into<String>) -> Self {
        Self {
            status: StatusCode::INTERNAL_SERVER_ERROR,
            body: ZosmfErrorBody {
                rc: 12,
                reason: 0,
                category: 5,
                message: message.into(),
                details: Vec::new(),
                stack: None,
            },
        }
    }
}

impl IntoResponse for ZosmfErrorResponse {
    fn into_response(self) -> Response {
        if self.status.is_server_error() {
            tracing::error!(
                status = %self.status.as_u16(),
                rc = self.body.rc,
                message = %self.body.message,
                "Server error response"
            );
        } else if self.status.is_client_error() {
            tracing::warn!(
                status = %self.status.as_u16(),
                rc = self.body.rc,
                message = %self.body.message,
                "Client error response"
            );
        }

        let body = serde_json::to_string(&self.body).unwrap_or_else(|_| {
            r#"{"rc":12,"reason":0,"category":5,"message":"internal serialization error"}"#
                .to_string()
        });
        (
            self.status,
            [(
                axum::http::header::CONTENT_TYPE,
                "application/json",
            )],
            body,
        )
            .into_response()
    }
}

impl std::fmt::Display for ZosmfErrorResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "z/OSMF error {}: rc={}, reason={}, {}",
            self.status, self.body.rc, self.body.reason, self.body.message
        )
    }
}

impl std::error::Error for ZosmfErrorResponse {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_body_serialization() {
        let body = ZosmfErrorBody {
            rc: 4,
            reason: 0,
            category: 1,
            message: "test error".to_string(),
            details: Vec::new(),
            stack: None,
        };
        let json = serde_json::to_string(&body).unwrap();
        assert!(json.contains(r#""rc":4"#));
        assert!(json.contains(r#""reason":0"#));
        assert!(json.contains(r#""message":"test error""#));
    }

    #[test]
    fn test_error_constructors() {
        let err = ZosmfErrorResponse::bad_request("bad");
        assert_eq!(err.status, StatusCode::BAD_REQUEST);
        assert_eq!(err.body.rc, 4);

        let err = ZosmfErrorResponse::unauthorized("unauth");
        assert_eq!(err.status, StatusCode::UNAUTHORIZED);

        let err = ZosmfErrorResponse::forbidden("forbid");
        assert_eq!(err.status, StatusCode::FORBIDDEN);
        assert_eq!(err.body.rc, 8);

        let err = ZosmfErrorResponse::not_found("missing");
        assert_eq!(err.status, StatusCode::NOT_FOUND);

        let err = ZosmfErrorResponse::internal("broken");
        assert_eq!(err.status, StatusCode::INTERNAL_SERVER_ERROR);
        assert_eq!(err.body.rc, 12);
    }
}
