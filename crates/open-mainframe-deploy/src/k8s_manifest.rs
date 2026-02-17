//! Kubernetes manifest generation from configuration.
//!
//! Auto-generates Deployment, Service, and ConfigMap YAML manifests
//! from the OpenMainframe [`Config`](crate::Config), with support
//! for health probes, metrics annotations, and custom overrides.

use crate::config::Config;

/// Overrides for generated Kubernetes manifests.
#[derive(Debug, Clone)]
pub struct ManifestOverrides {
    /// Number of replicas (default: 1).
    pub replicas: u32,
    /// Memory limit (e.g., "256Mi", "512Mi").
    pub memory_limit: String,
    /// CPU limit (e.g., "500m", "1").
    pub cpu_limit: String,
    /// Memory request.
    pub memory_request: String,
    /// CPU request.
    pub cpu_request: String,
    /// Container image name.
    pub image: String,
    /// Namespace for all resources.
    pub namespace: String,
    /// Application name label.
    pub app_name: String,
}

impl Default for ManifestOverrides {
    fn default() -> Self {
        Self {
            replicas: 1,
            memory_limit: "256Mi".to_string(),
            cpu_limit: "500m".to_string(),
            memory_request: "128Mi".to_string(),
            cpu_request: "250m".to_string(),
            image: "open-mainframe:latest".to_string(),
            namespace: "default".to_string(),
            app_name: "open-mainframe".to_string(),
        }
    }
}

/// A generated set of Kubernetes manifests.
#[derive(Debug, Clone)]
pub struct GeneratedManifests {
    /// Deployment YAML.
    pub deployment: String,
    /// Service YAML.
    pub service: String,
    /// ConfigMap YAML.
    pub configmap: String,
}

/// Generate Kubernetes manifests from config with optional overrides.
pub fn generate_manifests(config: &Config, overrides: &ManifestOverrides) -> GeneratedManifests {
    GeneratedManifests {
        deployment: generate_deployment(config, overrides),
        service: generate_service(config, overrides),
        configmap: generate_configmap(config, overrides),
    }
}

/// Generate a Kubernetes Deployment manifest.
fn generate_deployment(config: &Config, ov: &ManifestOverrides) -> String {
    format!(
        r#"apiVersion: apps/v1
kind: Deployment
metadata:
  name: {name}
  namespace: {namespace}
  labels:
    app: {name}
spec:
  replicas: {replicas}
  selector:
    matchLabels:
      app: {name}
  template:
    metadata:
      labels:
        app: {name}
      annotations:
        prometheus.io/scrape: "true"
        prometheus.io/port: "{metrics_port}"
        prometheus.io/path: "/metrics"
    spec:
      containers:
        - name: {name}
          image: {image}
          ports:
            - name: http
              containerPort: {app_port}
              protocol: TCP
            - name: metrics
              containerPort: {metrics_port}
              protocol: TCP
          livenessProbe:
            httpGet:
              path: /health
              port: {app_port}
            initialDelaySeconds: 10
            periodSeconds: 15
            timeoutSeconds: 5
          readinessProbe:
            httpGet:
              path: /ready
              port: {app_port}
            initialDelaySeconds: 5
            periodSeconds: 10
            timeoutSeconds: 5
          resources:
            requests:
              memory: "{memory_request}"
              cpu: "{cpu_request}"
            limits:
              memory: "{memory_limit}"
              cpu: "{cpu_limit}"
          envFrom:
            - configMapRef:
                name: {name}-config
          env:
            - name: OPEN_MAINFRAME_DB_URL
              valueFrom:
                secretKeyRef:
                  name: open-mainframe-db-credentials
                  key: url
                  optional: true
            - name: OPEN_MAINFRAME_DB_USERNAME
              valueFrom:
                secretKeyRef:
                  name: open-mainframe-db-credentials
                  key: username
                  optional: true
            - name: OPEN_MAINFRAME_DB_PASSWORD
              valueFrom:
                secretKeyRef:
                  name: open-mainframe-db-credentials
                  key: password
                  optional: true
          volumeMounts:
            - name: db-credentials
              mountPath: /etc/open-mainframe/secrets
              readOnly: true
      volumes:
        - name: db-credentials
          secret:
            secretName: open-mainframe-db-credentials
            optional: true"#,
        name = ov.app_name,
        namespace = ov.namespace,
        replicas = ov.replicas,
        image = ov.image,
        app_port = config.server.port,
        metrics_port = config.server.metrics_port,
        memory_request = ov.memory_request,
        cpu_request = ov.cpu_request,
        memory_limit = ov.memory_limit,
        cpu_limit = ov.cpu_limit,
    )
}

/// Generate a Kubernetes Service manifest.
fn generate_service(config: &Config, ov: &ManifestOverrides) -> String {
    format!(
        r#"apiVersion: v1
kind: Service
metadata:
  name: {name}
  namespace: {namespace}
  labels:
    app: {name}
spec:
  type: ClusterIP
  selector:
    app: {name}
  ports:
    - name: http
      port: {app_port}
      targetPort: http
      protocol: TCP
    - name: metrics
      port: {metrics_port}
      targetPort: metrics
      protocol: TCP"#,
        name = ov.app_name,
        namespace = ov.namespace,
        app_port = config.server.port,
        metrics_port = config.server.metrics_port,
    )
}

/// Generate a Kubernetes ConfigMap manifest.
fn generate_configmap(config: &Config, ov: &ManifestOverrides) -> String {
    format!(
        r#"apiVersion: v1
kind: ConfigMap
metadata:
  name: {name}-config
  namespace: {namespace}
  labels:
    app: {name}
data:
  OPEN_MAINFRAME_SERVER_HOST: "{host}"
  OPEN_MAINFRAME_SERVER_PORT: "{app_port}"
  OPEN_MAINFRAME_METRICS_PORT: "{metrics_port}"
  OPEN_MAINFRAME_LOG_LEVEL: "{log_level}"
  OPEN_MAINFRAME_LOG_FORMAT: "{log_format}"
  OPEN_MAINFRAME_DB_POOL_SIZE: "{pool_size}"
  OPEN_MAINFRAME_CICS_MAX_TASKS: "{max_tasks}"
  OPEN_MAINFRAME_CICS_TIMEOUT: "{cics_timeout}""#,
        name = ov.app_name,
        namespace = ov.namespace,
        host = config.server.host,
        app_port = config.server.port,
        metrics_port = config.server.metrics_port,
        log_level = config.observability.log_level,
        log_format = config.observability.log_format,
        pool_size = config.database.pool_size,
        max_tasks = config.cics.max_tasks,
        cics_timeout = config.cics.timeout,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_deployment_defaults() {
        let config = Config::default();
        let ov = ManifestOverrides::default();
        let manifests = generate_manifests(&config, &ov);

        // Check deployment has required fields
        assert!(manifests.deployment.contains("kind: Deployment"));
        assert!(manifests.deployment.contains("replicas: 1"));
        assert!(manifests.deployment.contains("containerPort: 8080"));
        assert!(manifests.deployment.contains("containerPort: 9090"));
    }

    #[test]
    fn test_generate_deployment_health_probes() {
        let config = Config::default();
        let ov = ManifestOverrides::default();
        let manifests = generate_manifests(&config, &ov);

        // Liveness probe at /health on app port
        assert!(manifests.deployment.contains("livenessProbe:"));
        assert!(manifests.deployment.contains("path: /health"));

        // Readiness probe at /ready on app port
        assert!(manifests.deployment.contains("readinessProbe:"));
        assert!(manifests.deployment.contains("path: /ready"));
    }

    #[test]
    fn test_generate_deployment_metrics_annotation() {
        let config = Config::default();
        let ov = ManifestOverrides::default();
        let manifests = generate_manifests(&config, &ov);

        assert!(manifests.deployment.contains("prometheus.io/scrape: \"true\""));
        assert!(manifests.deployment.contains("prometheus.io/port: \"9090\""));
        assert!(manifests.deployment.contains("prometheus.io/path: \"/metrics\""));
    }

    #[test]
    fn test_generate_deployment_custom_overrides() {
        let config = Config::default();
        let ov = ManifestOverrides {
            replicas: 3,
            memory_limit: "512Mi".to_string(),
            ..ManifestOverrides::default()
        };
        let manifests = generate_manifests(&config, &ov);

        assert!(manifests.deployment.contains("replicas: 3"));
        assert!(manifests.deployment.contains("\"512Mi\""));
    }

    #[test]
    fn test_generate_deployment_custom_ports() {
        let mut config = Config::default();
        config.server.port = 3000;
        config.server.metrics_port = 9191;
        let ov = ManifestOverrides::default();
        let manifests = generate_manifests(&config, &ov);

        assert!(manifests.deployment.contains("containerPort: 3000"));
        assert!(manifests.deployment.contains("containerPort: 9191"));
        assert!(manifests.deployment.contains("prometheus.io/port: \"9191\""));
    }

    #[test]
    fn test_generate_service() {
        let config = Config::default();
        let ov = ManifestOverrides::default();
        let manifests = generate_manifests(&config, &ov);

        assert!(manifests.service.contains("kind: Service"));
        assert!(manifests.service.contains("type: ClusterIP"));
        assert!(manifests.service.contains("port: 8080"));
        assert!(manifests.service.contains("port: 9090"));
        assert!(manifests.service.contains("app: open-mainframe"));
    }

    #[test]
    fn test_generate_configmap() {
        let config = Config::default();
        let ov = ManifestOverrides::default();
        let manifests = generate_manifests(&config, &ov);

        assert!(manifests.configmap.contains("kind: ConfigMap"));
        assert!(manifests.configmap.contains("OPEN_MAINFRAME_SERVER_PORT: \"8080\""));
        assert!(manifests.configmap.contains("OPEN_MAINFRAME_METRICS_PORT: \"9090\""));
        assert!(manifests.configmap.contains("OPEN_MAINFRAME_LOG_LEVEL: \"info\""));
    }

    #[test]
    fn test_generate_configmap_custom_namespace() {
        let config = Config::default();
        let ov = ManifestOverrides {
            namespace: "production".to_string(),
            ..ManifestOverrides::default()
        };
        let manifests = generate_manifests(&config, &ov);

        assert!(manifests.configmap.contains("namespace: production"));
        assert!(manifests.deployment.contains("namespace: production"));
        assert!(manifests.service.contains("namespace: production"));
    }

    #[test]
    fn test_deployment_secret_refs() {
        let config = Config::default();
        let ov = ManifestOverrides::default();
        let manifests = generate_manifests(&config, &ov);

        // Should reference K8s secrets for DB credentials
        assert!(manifests.deployment.contains("open-mainframe-db-credentials"));
        assert!(manifests.deployment.contains("secretKeyRef"));
        assert!(manifests.deployment.contains("key: url"));
        assert!(manifests.deployment.contains("key: username"));
        assert!(manifests.deployment.contains("key: password"));
    }

    #[test]
    fn test_deployment_secret_volume_mount() {
        let config = Config::default();
        let ov = ManifestOverrides::default();
        let manifests = generate_manifests(&config, &ov);

        assert!(manifests.deployment.contains("/etc/open-mainframe/secrets"));
        assert!(manifests.deployment.contains("readOnly: true"));
    }

    #[test]
    fn test_manifest_overrides_default() {
        let ov = ManifestOverrides::default();
        assert_eq!(ov.replicas, 1);
        assert_eq!(ov.memory_limit, "256Mi");
        assert_eq!(ov.namespace, "default");
        assert_eq!(ov.app_name, "open-mainframe");
    }
}
