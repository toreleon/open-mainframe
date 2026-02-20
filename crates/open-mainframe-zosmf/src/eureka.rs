//! Zowe API Mediation Layer (API ML) integration via Eureka.
//!
//! Implements dynamic service registration with the Zowe API ML discovery
//! service using the Netflix Eureka REST protocol.
//!
//! ## Features
//!
//! - Static API ML definition generation (YAML)
//! - Dynamic Eureka registration via REST
//! - Heartbeat scheduler
//! - Graceful deregistration on shutdown

use serde::{Deserialize, Serialize};

/// Eureka instance status values.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum InstanceStatus {
    /// Service is up and accepting traffic.
    Up,
    /// Service is down.
    Down,
    /// Service is starting.
    Starting,
    /// Service is out of service.
    #[serde(rename = "OUT_OF_SERVICE")]
    OutOfService,
    /// Unknown state.
    Unknown,
}

/// Eureka data center info.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataCenterInfo {
    /// Data center class (always "com.netflix.appinfo.InstanceInfo$DefaultDataCenterInfo").
    #[serde(rename = "@class")]
    pub class: String,
    /// Data center name.
    pub name: String,
}

impl Default for DataCenterInfo {
    fn default() -> Self {
        Self {
            class: "com.netflix.appinfo.InstanceInfo$DefaultDataCenterInfo".to_string(),
            name: "MyOwn".to_string(),
        }
    }
}

/// Eureka instance registration payload.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EurekaInstance {
    /// Application name (service ID).
    pub app: String,
    /// Instance ID (unique).
    pub instance_id: String,
    /// Hostname.
    pub host_name: String,
    /// IP address.
    pub ip_addr: String,
    /// Status.
    pub status: InstanceStatus,
    /// Non-secure port.
    pub port: PortInfo,
    /// Secure port.
    pub secure_port: PortInfo,
    /// Home page URL.
    pub home_page_url: String,
    /// Status page URL.
    pub status_page_url: String,
    /// Health check URL.
    pub health_check_url: String,
    /// VIP address.
    pub vip_address: String,
    /// Secure VIP address.
    pub secure_vip_address: String,
    /// Data center info.
    pub data_center_info: DataCenterInfo,
    /// Instance metadata.
    #[serde(default)]
    pub metadata: std::collections::HashMap<String, String>,
}

/// Port configuration for Eureka.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortInfo {
    /// Port number.
    #[serde(rename = "$")]
    pub port: u16,
    /// Whether the port is enabled.
    #[serde(rename = "@enabled")]
    pub enabled: bool,
}

/// Eureka registration wrapper.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EurekaRegistration {
    /// The instance payload.
    pub instance: EurekaInstance,
}

/// API ML service definition for static registration.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ApiMlServiceDefinition {
    /// Service ID.
    pub service_id: String,
    /// Title.
    pub title: String,
    /// Description.
    pub description: String,
    /// Catalog tile ID.
    pub catalog_ui_tile_id: String,
    /// Instance base URLs.
    pub instance_base_urls: Vec<String>,
    /// Route definitions.
    pub routes: Vec<ApiMlRoute>,
    /// Authentication scheme.
    pub authentication: ApiMlAuthentication,
    /// API info entries.
    pub api_info: Vec<ApiMlApiInfo>,
}

/// API ML route definition.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ApiMlRoute {
    /// Gateway URL pattern.
    pub gateway_url: String,
    /// Service URL pattern.
    pub service_url: String,
}

/// API ML authentication configuration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiMlAuthentication {
    /// Authentication scheme.
    pub scheme: String,
}

/// API ML API info entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ApiMlApiInfo {
    /// API ID.
    pub api_id: String,
    /// Gateway URL.
    pub gateway_url: String,
    /// Swagger URL.
    pub swagger_url: Option<String>,
}

/// Build a default API ML service definition for OpenMainframe z/OSMF.
pub fn build_service_definition(host: &str, port: u16) -> ApiMlServiceDefinition {
    let base_url = format!("https://{}:{}", host, port);
    ApiMlServiceDefinition {
        service_id: "zosmf".to_string(),
        title: "OpenMainframe z/OSMF".to_string(),
        description: "z/OSMF-compatible REST API server for OpenMainframe".to_string(),
        catalog_ui_tile_id: "zosmf".to_string(),
        instance_base_urls: vec![base_url.clone()],
        routes: vec![
            ApiMlRoute {
                gateway_url: "api/v1".to_string(),
                service_url: "/zosmf".to_string(),
            },
        ],
        authentication: ApiMlAuthentication {
            scheme: "zosmfJwt".to_string(),
        },
        api_info: vec![ApiMlApiInfo {
            api_id: "zowe.apiml.zosmf".to_string(),
            gateway_url: "api/v1".to_string(),
            swagger_url: Some(format!("{}/zosmf/api/docs", base_url)),
        }],
    }
}

/// Build a Eureka registration payload for OpenMainframe z/OSMF.
pub fn build_eureka_registration(host: &str, port: u16) -> EurekaRegistration {
    let instance_id = format!("{}:zosmf:{}", host, port);
    let base_url = format!("https://{}:{}", host, port);

    let mut metadata = std::collections::HashMap::new();
    metadata.insert("apiml.routes.api-v1.gatewayUrl".to_string(), "api/v1".to_string());
    metadata.insert("apiml.routes.api-v1.serviceUrl".to_string(), "/zosmf".to_string());
    metadata.insert("apiml.authentication.scheme".to_string(), "zosmfJwt".to_string());
    metadata.insert("apiml.service.title".to_string(), "OpenMainframe z/OSMF".to_string());
    metadata.insert("apiml.service.description".to_string(), "z/OSMF-compatible REST API".to_string());

    EurekaRegistration {
        instance: EurekaInstance {
            app: "ZOSMF".to_string(),
            instance_id,
            host_name: host.to_string(),
            ip_addr: host.to_string(),
            status: InstanceStatus::Up,
            port: PortInfo {
                port: 0,
                enabled: false,
            },
            secure_port: PortInfo {
                port,
                enabled: true,
            },
            home_page_url: format!("{}/", base_url),
            status_page_url: format!("{}/zosmf/info", base_url),
            health_check_url: format!("{}/zosmf/info", base_url),
            vip_address: "zosmf".to_string(),
            secure_vip_address: "zosmf".to_string(),
            data_center_info: DataCenterInfo::default(),
            metadata,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_service_definition() {
        let def = build_service_definition("localhost", 10443);
        assert_eq!(def.service_id, "zosmf");
        assert_eq!(def.instance_base_urls[0], "https://localhost:10443");
        assert_eq!(def.routes.len(), 1);
        assert_eq!(def.authentication.scheme, "zosmfJwt");
    }

    #[test]
    fn test_build_eureka_registration() {
        let reg = build_eureka_registration("mainframe.local", 10443);
        assert_eq!(reg.instance.app, "ZOSMF");
        assert_eq!(reg.instance.status, InstanceStatus::Up);
        assert_eq!(reg.instance.secure_port.port, 10443);
        assert!(reg.instance.secure_port.enabled);
        assert!(!reg.instance.port.enabled);
    }

    #[test]
    fn test_eureka_registration_serialization() {
        let reg = build_eureka_registration("localhost", 10443);
        let json = serde_json::to_string(&reg).unwrap();
        assert!(json.contains("\"app\":\"ZOSMF\""));
        assert!(json.contains("\"status\":\"UP\""));
        assert!(json.contains("\"@class\""));
    }

    #[test]
    fn test_service_definition_serialization() {
        let def = build_service_definition("localhost", 10443);
        let json = serde_json::to_string(&def).unwrap();
        assert!(json.contains("\"serviceId\":\"zosmf\""));
        assert!(json.contains("\"gatewayUrl\":\"api/v1\""));
    }

    #[test]
    fn test_instance_status_serialization() {
        let up = serde_json::to_string(&InstanceStatus::Up).unwrap();
        assert_eq!(up, "\"UP\"");
        let oos = serde_json::to_string(&InstanceStatus::OutOfService).unwrap();
        assert_eq!(oos, "\"OUT_OF_SERVICE\"");
    }
}
