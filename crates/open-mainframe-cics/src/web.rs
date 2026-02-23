//! # CICS Web Services & REST/JSON
//!
//! Provides WEB RECEIVE/SEND/OPEN/CONVERSE/CLOSE commands,
//! TRANSFORM DATATOJSON/JSONTODATA, URIMAP, and PIPELINE support.

use std::collections::HashMap;

// ─────────────────────── HTTP Types ───────────────────────

/// HTTP method.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HttpMethod {
    Get,
    Post,
    Put,
    Delete,
    Patch,
    Options,
    Head,
}

impl HttpMethod {
    /// Parse from string.
    pub fn parse_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "GET" => Some(Self::Get),
            "POST" => Some(Self::Post),
            "PUT" => Some(Self::Put),
            "DELETE" => Some(Self::Delete),
            "PATCH" => Some(Self::Patch),
            "OPTIONS" => Some(Self::Options),
            "HEAD" => Some(Self::Head),
            _ => None,
        }
    }

    /// As string.
    pub fn as_str(&self) -> &str {
        match self {
            Self::Get => "GET",
            Self::Post => "POST",
            Self::Put => "PUT",
            Self::Delete => "DELETE",
            Self::Patch => "PATCH",
            Self::Options => "OPTIONS",
            Self::Head => "HEAD",
        }
    }
}

/// URL scheme.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HttpScheme {
    Http,
    Https,
}

/// An inbound HTTP request received via WEB RECEIVE.
#[derive(Debug, Clone)]
pub struct WebRequest {
    /// HTTP method.
    pub method: HttpMethod,
    /// Request path.
    pub path: String,
    /// Query string parameters.
    pub query_params: HashMap<String, String>,
    /// HTTP headers.
    pub headers: HashMap<String, String>,
    /// Request body.
    pub body: Vec<u8>,
    /// Content type.
    pub content_type: String,
}

impl WebRequest {
    /// Create a new request.
    pub fn new(method: HttpMethod, path: &str) -> Self {
        Self {
            method,
            path: path.to_string(),
            query_params: HashMap::new(),
            headers: HashMap::new(),
            body: Vec::new(),
            content_type: String::new(),
        }
    }

    /// Get body as UTF-8 string.
    pub fn body_string(&self) -> Option<String> {
        String::from_utf8(self.body.clone()).ok()
    }

    /// Content length.
    pub fn content_length(&self) -> usize {
        self.body.len()
    }
}

/// An outbound HTTP response built via WEB SEND.
#[derive(Debug, Clone)]
pub struct WebResponse {
    /// HTTP status code.
    pub status_code: u16,
    /// Status reason phrase.
    pub reason: String,
    /// Response headers.
    pub headers: HashMap<String, String>,
    /// Response body.
    pub body: Vec<u8>,
    /// Media type.
    pub media_type: String,
}

impl WebResponse {
    /// Create a new response.
    pub fn new(status_code: u16) -> Self {
        let reason = match status_code {
            200 => "OK",
            201 => "Created",
            204 => "No Content",
            400 => "Bad Request",
            404 => "Not Found",
            500 => "Internal Server Error",
            _ => "Unknown",
        };
        Self {
            status_code,
            reason: reason.to_string(),
            headers: HashMap::new(),
            body: Vec::new(),
            media_type: String::new(),
        }
    }

    /// Set JSON body.
    pub fn json(mut self, json_body: &str) -> Self {
        self.body = json_body.as_bytes().to_vec();
        self.media_type = "application/json".to_string();
        self.headers
            .insert("Content-Type".to_string(), "application/json".to_string());
        self
    }

    /// Set text body.
    pub fn text(mut self, text_body: &str) -> Self {
        self.body = text_body.as_bytes().to_vec();
        self.media_type = "text/plain".to_string();
        self
    }
}

// ─────────────────────── WEB OPEN/CONVERSE/CLOSE ───────────────────────

/// An outbound HTTP client session.
#[derive(Debug)]
pub struct WebSession {
    /// Session token.
    pub token: u64,
    /// Target host.
    pub host: String,
    /// Target port.
    pub port: u16,
    /// URL scheme.
    pub scheme: HttpScheme,
    /// Whether the session is open.
    pub open: bool,
}

/// Result of WEB CONVERSE.
#[derive(Debug, Clone)]
pub struct ConverseResult {
    /// HTTP status code received.
    pub status_code: u16,
    /// Response body.
    pub body: Vec<u8>,
    /// Response headers.
    pub headers: HashMap<String, String>,
}

/// HTTP client for outbound requests.
#[derive(Debug)]
pub struct WebClient {
    sessions: HashMap<u64, WebSession>,
    next_token: u64,
}

impl WebClient {
    /// Create a new client.
    pub fn new() -> Self {
        Self {
            sessions: HashMap::new(),
            next_token: 1,
        }
    }

    /// WEB OPEN — create an outbound session.
    pub fn open(&mut self, host: &str, port: u16, scheme: HttpScheme) -> u64 {
        let token = self.next_token;
        self.next_token += 1;
        self.sessions.insert(
            token,
            WebSession {
                token,
                host: host.to_string(),
                port,
                scheme,
                open: true,
            },
        );
        token
    }

    /// WEB CONVERSE — send request and receive response.
    pub fn converse(
        &self,
        token: u64,
        method: HttpMethod,
        path: &str,
        body: &[u8],
    ) -> Result<ConverseResult, WebError> {
        let session = self
            .sessions
            .get(&token)
            .ok_or(WebError::SessionNotFound)?;
        if !session.open {
            return Err(WebError::SessionClosed);
        }

        // Simulated response (in a real system, this would make an HTTP call).
        Ok(ConverseResult {
            status_code: 200,
            body: format!(
                "{{\"simulated\":true,\"method\":\"{}\",\"path\":\"{}\",\"host\":\"{}\",\"bodyLen\":{}}}",
                method.as_str(),
                path,
                session.host,
                body.len()
            )
            .into_bytes(),
            headers: HashMap::new(),
        })
    }

    /// WEB CLOSE — terminate a session.
    pub fn close(&mut self, token: u64) -> Result<(), WebError> {
        match self.sessions.get_mut(&token) {
            Some(session) => {
                session.open = false;
                Ok(())
            }
            None => Err(WebError::SessionNotFound),
        }
    }
}

impl Default for WebClient {
    fn default() -> Self {
        Self::new()
    }
}

/// Web service errors.
#[derive(Debug, thiserror::Error)]
pub enum WebError {
    #[error("session not found")]
    SessionNotFound,
    #[error("session is closed")]
    SessionClosed,
    #[error("invalid URI: {0}")]
    InvalidUri(String),
    #[error("transform error: {0}")]
    TransformError(String),
}

// ─────────────────────── TRANSFORM ───────────────────────

/// A JSON field mapping for TRANSFORM.
#[derive(Debug, Clone)]
pub struct JsonFieldMapping {
    /// JSON field name.
    pub json_name: String,
    /// COBOL field name.
    pub cobol_name: String,
    /// Data type hint.
    pub data_type: JsonDataType,
}

/// JSON data type for TRANSFORM mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JsonDataType {
    String,
    Number,
    Boolean,
}

/// Transform COBOL data to JSON using field mappings.
pub fn transform_data_to_json(
    fields: &[JsonFieldMapping],
    values: &HashMap<String, String>,
) -> Result<String, WebError> {
    let mut pairs = Vec::new();
    for field in fields {
        let value = values
            .get(&field.cobol_name)
            .cloned()
            .unwrap_or_default();
        let json_value = match field.data_type {
            JsonDataType::String => format!("\"{}\"", value.replace('\"', "\\\"")),
            JsonDataType::Number => {
                if value.is_empty() {
                    "0".to_string()
                } else {
                    value
                }
            }
            JsonDataType::Boolean => {
                if value == "Y" || value == "1" || value.to_uppercase() == "TRUE" {
                    "true".to_string()
                } else {
                    "false".to_string()
                }
            }
        };
        pairs.push(format!("\"{}\":{}", field.json_name, json_value));
    }
    Ok(format!("{{{}}}", pairs.join(",")))
}

/// Transform JSON to COBOL data using field mappings.
pub fn transform_json_to_data(
    json: &str,
    fields: &[JsonFieldMapping],
) -> Result<HashMap<String, String>, WebError> {
    let mut result = HashMap::new();

    // Simple JSON parser for flat objects.
    let trimmed = json.trim();
    if !trimmed.starts_with('{') || !trimmed.ends_with('}') {
        return Err(WebError::TransformError("Expected JSON object".to_string()));
    }

    let inner = &trimmed[1..trimmed.len() - 1];

    for field in fields {
        let search = format!("\"{}\":", field.json_name);
        if let Some(pos) = inner.find(&search) {
            let value_start = pos + search.len();
            let value_str = &inner[value_start..].trim();
            let value = if let Some(stripped) = value_str.strip_prefix('"') {
                // String value.
                let end = stripped.find('"').unwrap_or(stripped.len());
                stripped[..end].to_string()
            } else {
                // Number or boolean.
                let end = value_str
                    .find([',', '}'])
                    .unwrap_or(value_str.len());
                value_str[..end].trim().to_string()
            };
            result.insert(field.cobol_name.clone(), value);
        }
    }

    Ok(result)
}

// ─────────────────────── URIMAP ───────────────────────

/// A URIMAP definition for routing inbound requests.
#[derive(Debug, Clone)]
pub struct UriMap {
    /// URIMAP name.
    pub name: String,
    /// URI path pattern.
    pub path: String,
    /// Target program.
    pub program: String,
    /// Allowed HTTP method.
    pub method: HttpMethod,
    /// Whether the URIMAP is enabled.
    pub enabled: bool,
}

impl UriMap {
    /// Check if a request path matches this URIMAP.
    pub fn matches(&self, request_path: &str, request_method: HttpMethod) -> bool {
        if self.method != request_method || !self.enabled {
            return false;
        }
        if self.path.ends_with('*') {
            let prefix = &self.path[..self.path.len() - 1];
            request_path.starts_with(prefix)
        } else {
            self.path == request_path
        }
    }
}

/// A URIMAP router.
#[derive(Debug)]
pub struct UriRouter {
    maps: Vec<UriMap>,
}

impl UriRouter {
    /// Create a new router.
    pub fn new() -> Self {
        Self { maps: Vec::new() }
    }

    /// Add a URIMAP.
    pub fn add(&mut self, map: UriMap) {
        self.maps.push(map);
    }

    /// Route a request to a program.
    pub fn route(&self, path: &str, method: HttpMethod) -> Option<&UriMap> {
        self.maps.iter().find(|m| m.matches(path, method))
    }
}

impl Default for UriRouter {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────── PIPELINE ───────────────────────

/// A PIPELINE step.
#[derive(Debug, Clone)]
pub struct PipelineStep {
    /// Step name.
    pub name: String,
    /// Handler program.
    pub program: String,
    /// Step order.
    pub order: u32,
}

/// A PIPELINE definition for SOAP/REST processing.
#[derive(Debug, Clone)]
pub struct Pipeline {
    /// Pipeline name.
    pub name: String,
    /// WSDL binding name, if SOAP.
    pub wsdl_binding: Option<String>,
    /// Processing steps.
    pub steps: Vec<PipelineStep>,
}

impl Pipeline {
    /// Create a new pipeline.
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            wsdl_binding: None,
            steps: Vec::new(),
        }
    }

    /// Add a processing step.
    pub fn add_step(&mut self, step_name: &str, program: &str) {
        let order = self.steps.len() as u32 + 1;
        self.steps.push(PipelineStep {
            name: step_name.to_string(),
            program: program.to_string(),
            order,
        });
    }
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ─── SYS-116.1: WEB RECEIVE ───

    #[test]
    fn test_web_request() {
        let mut req = WebRequest::new(HttpMethod::Post, "/api/employees");
        req.body = b"{\"empId\":\"12345\"}".to_vec();
        req.content_type = "application/json".to_string();
        req.query_params
            .insert("format".to_string(), "json".to_string());

        assert_eq!(req.method, HttpMethod::Post);
        assert_eq!(req.content_length(), 17);
        assert!(req.body_string().unwrap().contains("empId"));
    }

    #[test]
    fn test_web_request_query_params() {
        let mut req = WebRequest::new(HttpMethod::Get, "/api/search");
        req.query_params
            .insert("name".to_string(), "Smith".to_string());
        assert_eq!(req.query_params.get("name").unwrap(), "Smith");
    }

    // ─── SYS-116.2: WEB SEND ───

    #[test]
    fn test_web_response_json() {
        let resp = WebResponse::new(200).json("{\"status\":\"ok\"}");
        assert_eq!(resp.status_code, 200);
        assert_eq!(resp.media_type, "application/json");
        assert!(!resp.body.is_empty());
    }

    #[test]
    fn test_web_response_404() {
        let resp = WebResponse::new(404);
        assert_eq!(resp.status_code, 404);
        assert_eq!(resp.reason, "Not Found");
    }

    // ─── SYS-116.3: WEB OPEN/CONVERSE/CLOSE ───

    #[test]
    fn test_web_open_converse_close() {
        let mut client = WebClient::new();
        let token = client.open("api.example.com", 443, HttpScheme::Https);

        let result = client
            .converse(token, HttpMethod::Post, "/api/data", b"{\"key\":\"val\"}")
            .unwrap();
        assert_eq!(result.status_code, 200);
        assert!(!result.body.is_empty());

        client.close(token).unwrap();
    }

    #[test]
    fn test_web_converse_closed_session() {
        let mut client = WebClient::new();
        let token = client.open("host", 80, HttpScheme::Http);
        client.close(token).unwrap();
        assert!(client
            .converse(token, HttpMethod::Get, "/", &[])
            .is_err());
    }

    #[test]
    fn test_web_invalid_session() {
        let client = WebClient::new();
        assert!(client
            .converse(999, HttpMethod::Get, "/", &[])
            .is_err());
    }

    // ─── SYS-116.4: TRANSFORM ───

    #[test]
    fn test_transform_data_to_json() {
        let fields = vec![
            JsonFieldMapping {
                json_name: "empId".to_string(),
                cobol_name: "WS-EMP-ID".to_string(),
                data_type: JsonDataType::String,
            },
            JsonFieldMapping {
                json_name: "name".to_string(),
                cobol_name: "WS-NAME".to_string(),
                data_type: JsonDataType::String,
            },
        ];
        let mut values = HashMap::new();
        values.insert("WS-EMP-ID".to_string(), "12345".to_string());
        values.insert("WS-NAME".to_string(), "Smith".to_string());

        let json = transform_data_to_json(&fields, &values).unwrap();
        assert!(json.contains("\"empId\":\"12345\""));
        assert!(json.contains("\"name\":\"Smith\""));
    }

    #[test]
    fn test_transform_json_to_data() {
        let fields = vec![
            JsonFieldMapping {
                json_name: "empId".to_string(),
                cobol_name: "WS-EMP-ID".to_string(),
                data_type: JsonDataType::String,
            },
            JsonFieldMapping {
                json_name: "name".to_string(),
                cobol_name: "WS-NAME".to_string(),
                data_type: JsonDataType::String,
            },
        ];
        let json = r#"{"empId":"12345","name":"Smith"}"#;

        let data = transform_json_to_data(json, &fields).unwrap();
        assert_eq!(data.get("WS-EMP-ID").unwrap(), "12345");
        assert_eq!(data.get("WS-NAME").unwrap(), "Smith");
    }

    // ─── SYS-116.5: URIMAP ───

    #[test]
    fn test_urimap_exact_match() {
        let map = UriMap {
            name: "EMPAPI".to_string(),
            path: "/api/employees".to_string(),
            program: "EMPPROG".to_string(),
            method: HttpMethod::Post,
            enabled: true,
        };
        assert!(map.matches("/api/employees", HttpMethod::Post));
        assert!(!map.matches("/api/employees", HttpMethod::Get));
        assert!(!map.matches("/api/other", HttpMethod::Post));
    }

    #[test]
    fn test_urimap_wildcard() {
        let map = UriMap {
            name: "EMPAPI".to_string(),
            path: "/api/employees/*".to_string(),
            program: "EMPPROG".to_string(),
            method: HttpMethod::Post,
            enabled: true,
        };
        assert!(map.matches("/api/employees/123", HttpMethod::Post));
        assert!(map.matches("/api/employees/abc/def", HttpMethod::Post));
    }

    #[test]
    fn test_uri_router() {
        let mut router = UriRouter::new();
        router.add(UriMap {
            name: "EMP_GET".to_string(),
            path: "/api/employees/*".to_string(),
            program: "EMPGET".to_string(),
            method: HttpMethod::Get,
            enabled: true,
        });
        router.add(UriMap {
            name: "EMP_POST".to_string(),
            path: "/api/employees".to_string(),
            program: "EMPPOST".to_string(),
            method: HttpMethod::Post,
            enabled: true,
        });

        let result = router.route("/api/employees/123", HttpMethod::Get);
        assert_eq!(result.unwrap().program, "EMPGET");

        let result = router.route("/api/employees", HttpMethod::Post);
        assert_eq!(result.unwrap().program, "EMPPOST");

        assert!(router.route("/api/other", HttpMethod::Get).is_none());
    }

    // ─── SYS-116.6: PIPELINE ───

    #[test]
    fn test_pipeline() {
        let mut pipeline = Pipeline::new("SOAPAPI");
        pipeline.wsdl_binding = Some("EmpService".to_string());
        pipeline.add_step("PARSE", "XMLPARSE");
        pipeline.add_step("PROCESS", "EMPPROG");
        pipeline.add_step("FORMAT", "XMLFMT");

        assert_eq!(pipeline.steps.len(), 3);
        assert_eq!(pipeline.steps[0].order, 1);
        assert_eq!(pipeline.steps[2].order, 3);
    }

    // ─── SYS-116.7: Web Services Integration Tests ───

    #[test]
    fn test_full_rest_workflow() {
        // 1. Define URIMAP.
        let mut router = UriRouter::new();
        router.add(UriMap {
            name: "EMPAPI".to_string(),
            path: "/api/employees/*".to_string(),
            program: "EMPPROG".to_string(),
            method: HttpMethod::Get,
            enabled: true,
        });

        // 2. Route inbound request.
        let req = WebRequest::new(HttpMethod::Get, "/api/employees/123");
        let matched = router.route(&req.path, req.method).unwrap();
        assert_eq!(matched.program, "EMPPROG");

        // 3. Build response.
        let resp = WebResponse::new(200).json("{\"empId\":\"123\",\"name\":\"Smith\"}");
        assert_eq!(resp.status_code, 200);

        // 4. Transform JSON to data.
        let fields = vec![JsonFieldMapping {
            json_name: "empId".to_string(),
            cobol_name: "WS-ID".to_string(),
            data_type: JsonDataType::String,
        }];
        let body_str = String::from_utf8(resp.body).unwrap();
        let data = transform_json_to_data(&body_str, &fields).unwrap();
        assert_eq!(data.get("WS-ID").unwrap(), "123");
    }
}
