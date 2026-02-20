//! CSRF validation middleware — requires X-CSRF-ZOSMF-HEADER on mutating requests.
//!
//! z/OSMF requires this header on all PUT, POST, and DELETE requests to prevent
//! cross-site request forgery. The header value can be anything (even empty),
//! as long as the header is present.

use axum::extract::Request;
use axum::http::Method;
use axum::middleware::Next;
use axum::response::{IntoResponse, Response};

use crate::types::error::ZosmfErrorResponse;

/// The required header name for CSRF protection.
pub const CSRF_HEADER: &str = "x-csrf-zosmf-header";

/// CSRF validation middleware layer.
///
/// Rejects PUT, POST, and DELETE requests that do not include the
/// `X-CSRF-ZOSMF-HEADER` header with a 403 Forbidden response.
pub async fn csrf_middleware(request: Request, next: Next) -> Response {
    let method = request.method().clone();

    // Only validate CSRF on mutating methods.
    if (method == Method::PUT || method == Method::POST || method == Method::DELETE)
        && !request.headers().contains_key(CSRF_HEADER)
    {
        return ZosmfErrorResponse::forbidden(
            "CSRF validation failed: X-CSRF-ZOSMF-HEADER required on mutating requests",
        )
        .into_response();
    }

    next.run(request).await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_csrf_header_constant() {
        assert_eq!(CSRF_HEADER, "x-csrf-zosmf-header");
    }
}
