//! Authentication middleware — extracts credentials from Basic Auth, Bearer token, or cookie.
//!
//! Implements an Axum extractor for `AuthContext` that checks:
//! 1. `Authorization: Bearer <jwt>` header
//! 2. `jwtToken` cookie
//! 3. `Authorization: Basic base64(userid:password)` header (for initial auth)

use std::sync::Arc;

use axum::extract::FromRequestParts;
use axum::http::request::Parts;
use base64::Engine;

use crate::jwt;
use crate::state::AppState;
use crate::types::auth::AuthContext;
use crate::types::error::ZosmfErrorResponse;

/// Extract `AuthContext` from the request.
///
/// This is an Axum extractor that validates authentication on every request.
/// It checks Bearer token, cookie, and Basic Auth in that order.
impl FromRequestParts<Arc<AppState>> for AuthContext {
    type Rejection = ZosmfErrorResponse;

    async fn from_request_parts(
        parts: &mut Parts,
        state: &Arc<AppState>,
    ) -> std::result::Result<Self, Self::Rejection> {
        // 1. Check Authorization: Bearer <token>
        if let Some(auth_header) = parts.headers.get("authorization") {
            let auth_str = auth_header.to_str().unwrap_or("");

            if let Some(token) = auth_str.strip_prefix("Bearer ") {
                return validate_jwt(token.trim(), state);
            }

            // 3. Check Authorization: Basic <base64>
            if let Some(encoded) = auth_str.strip_prefix("Basic ") {
                return validate_basic(encoded.trim(), state);
            }
        }

        // 2. Check jwtToken cookie
        if let Some(cookie_header) = parts.headers.get("cookie") {
            if let Ok(cookie_str) = cookie_header.to_str() {
                for cookie in cookie_str.split(';') {
                    let cookie = cookie.trim();
                    if let Some(token) = cookie.strip_prefix("jwtToken=") {
                        return validate_jwt(token.trim(), state);
                    }
                }
            }
        }

        Err(ZosmfErrorResponse::unauthorized(
            "Authentication required: provide Authorization header or jwtToken cookie",
        ))
    }
}

/// Validate a JWT token and return AuthContext.
fn validate_jwt(token: &str, state: &Arc<AppState>) -> std::result::Result<AuthContext, ZosmfErrorResponse> {
    // Check if token is in the token store (not invalidated).
    if let Some(user) = state.token_store.get(token) {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();

        if user.expires_at <= now {
            state.token_store.remove(token);
            return Err(ZosmfErrorResponse::unauthorized("Token expired"));
        }

        return Ok(AuthContext {
            userid: user.userid.clone(),
        });
    }

    // Try to decode the JWT directly (stateless validation).
    match jwt::decode(token, &state.config.auth.token_secret) {
        Ok(claims) => {
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs();

            if claims.exp <= now {
                return Err(ZosmfErrorResponse::unauthorized("Token expired"));
            }

            Ok(AuthContext {
                userid: claims.sub,
            })
        }
        Err(_) => Err(ZosmfErrorResponse::unauthorized("Invalid token")),
    }
}

/// Validate Basic Auth credentials against RACF.
fn validate_basic(
    encoded: &str,
    state: &Arc<AppState>,
) -> std::result::Result<AuthContext, ZosmfErrorResponse> {
    let decoded = base64::engine::general_purpose::STANDARD
        .decode(encoded)
        .map_err(|_| ZosmfErrorResponse::unauthorized("Invalid Basic Auth encoding"))?;

    let decoded_str = String::from_utf8(decoded)
        .map_err(|_| ZosmfErrorResponse::unauthorized("Invalid Basic Auth encoding"))?;

    let (userid, password) = decoded_str
        .split_once(':')
        .ok_or_else(|| ZosmfErrorResponse::unauthorized("Invalid Basic Auth format"))?;

    // Use SAF router to verify credentials.
    let result = state.saf.verify(&state.racf, userid, password);

    if result.is_authorized() {
        Ok(AuthContext {
            userid: userid.trim().to_uppercase(),
        })
    } else {
        Err(ZosmfErrorResponse::unauthorized(format!(
            "Authentication failed for user '{}'",
            userid.to_uppercase()
        )))
    }
}
