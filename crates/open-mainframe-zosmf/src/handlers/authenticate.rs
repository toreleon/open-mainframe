//! POST/DELETE /zosmf/services/authenticate — login and logout endpoints.

use std::sync::Arc;

use axum::extract::State;
use axum::http::{header, StatusCode};
use axum::response::IntoResponse;
use axum::routing::{delete, post};
use axum::Router;
use base64::Engine;

use crate::jwt;
use crate::state::AppState;
use crate::types::auth::{AuthContext, AuthenticatedUser};
use crate::types::error::ZosmfErrorResponse;

/// Register authentication routes.
pub fn routes() -> Router<Arc<AppState>> {
    Router::new()
        .route("/zosmf/services/authenticate", post(login))
        .route("/zosmf/services/authenticate", delete(logout))
}

/// POST /zosmf/services/authenticate — authenticate with Basic Auth, return JWT.
async fn login(
    State(state): State<Arc<AppState>>,
    headers: axum::http::HeaderMap,
) -> std::result::Result<impl IntoResponse, ZosmfErrorResponse> {
    // Extract Basic Auth header.
    let auth_header = headers
        .get("authorization")
        .and_then(|v| v.to_str().ok())
        .ok_or_else(|| {
            ZosmfErrorResponse::unauthorized("Authorization header required")
        })?;

    let encoded = auth_header
        .strip_prefix("Basic ")
        .ok_or_else(|| {
            ZosmfErrorResponse::unauthorized("Basic authentication required")
        })?;

    let decoded = base64::engine::general_purpose::STANDARD
        .decode(encoded.trim())
        .map_err(|_| ZosmfErrorResponse::unauthorized("Invalid Basic Auth encoding"))?;

    let decoded_str = String::from_utf8(decoded)
        .map_err(|_| ZosmfErrorResponse::unauthorized("Invalid Basic Auth encoding"))?;

    let (userid, password) = decoded_str
        .split_once(':')
        .ok_or_else(|| ZosmfErrorResponse::unauthorized("Invalid Basic Auth format"))?;

    // Verify credentials via SAF router.
    let result = state.saf.verify(&state.racf, userid, password);

    if !result.is_authorized() {
        tracing::warn!(userid = %userid.to_uppercase(), "Login failed");
        return Err(ZosmfErrorResponse::unauthorized(format!(
            "Authentication failed for user '{}'",
            userid.to_uppercase()
        )));
    }

    let userid = userid.trim().to_uppercase();
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    let claims = jwt::Claims {
        sub: userid.clone(),
        iat: now,
        exp: now + state.config.auth.token_ttl_seconds,
    };

    let token = jwt::encode(&claims, &state.config.auth.token_secret);

    // Store in token store.
    state.token_store.insert(
        token.clone(),
        AuthenticatedUser {
            userid: userid.clone(),
            token: token.clone(),
            expires_at: claims.exp,
        },
    );

    let cookie = format!(
        "jwtToken={}; Path=/; HttpOnly; Secure; SameSite=Strict",
        token
    );

    tracing::info!(userid = %userid, "Login successful");

    // IBM z/OSMF returns JWT only via Set-Cookie; body is empty JSON.
    Ok((
        StatusCode::OK,
        [
            (header::SET_COOKIE, cookie),
            (header::CONTENT_TYPE, "application/json".to_string()),
        ],
        "{}",
    ))
}

/// DELETE /zosmf/services/authenticate — logout and invalidate token.
async fn logout(
    State(state): State<Arc<AppState>>,
    _auth: AuthContext,
    headers: axum::http::HeaderMap,
) -> std::result::Result<StatusCode, ZosmfErrorResponse> {
    // Find and remove the token from the store.
    // Extract token from Bearer header or cookie.
    let token = extract_token(&headers);

    if let Some(token) = token {
        state.token_store.remove(&token);
    }

    tracing::info!(userid = %_auth.userid, "Logout");
    Ok(StatusCode::OK)
}

/// Extract the JWT token string from request headers.
fn extract_token(headers: &axum::http::HeaderMap) -> Option<String> {
    // Check Authorization: Bearer <token>
    if let Some(auth) = headers.get("authorization") {
        if let Ok(auth_str) = auth.to_str() {
            if let Some(token) = auth_str.strip_prefix("Bearer ") {
                return Some(token.trim().to_string());
            }
        }
    }

    // Check jwtToken cookie
    if let Some(cookie) = headers.get("cookie") {
        if let Ok(cookie_str) = cookie.to_str() {
            for part in cookie_str.split(';') {
                let part = part.trim();
                if let Some(token) = part.strip_prefix("jwtToken=") {
                    return Some(token.trim().to_string());
                }
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_token_from_bearer() {
        let mut headers = axum::http::HeaderMap::new();
        headers.insert("authorization", "Bearer mytoken123".parse().unwrap());

        assert_eq!(extract_token(&headers), Some("mytoken123".to_string()));
    }

    #[test]
    fn test_extract_token_from_cookie() {
        let mut headers = axum::http::HeaderMap::new();
        headers.insert("cookie", "jwtToken=mytoken456; other=val".parse().unwrap());

        assert_eq!(extract_token(&headers), Some("mytoken456".to_string()));
    }

    #[test]
    fn test_extract_token_missing() {
        let headers = axum::http::HeaderMap::new();
        assert_eq!(extract_token(&headers), None);
    }
}
