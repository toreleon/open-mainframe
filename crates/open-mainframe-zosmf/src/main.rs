//! z/OSMF server binary — starts the z/OSMF-compatible REST API server.

use std::sync::Arc;

use open_mainframe_zosmf::config::ZosmfConfig;
use open_mainframe_zosmf::handlers::build_router;
use open_mainframe_zosmf::mounts::{MountType, parse_mount_arg};
use open_mainframe_zosmf::state::AppState;
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() {
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| {
        EnvFilter::new("open_mainframe_zosmf=info,tower_http=debug,warn")
    });
    tracing_subscriber::fmt().with_env_filter(filter).init();

    let args: Vec<String> = std::env::args().collect();

    let port: u16 = std::env::var("ZOSMF_PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(10443);

    // Check for --config flag to load TOML config
    let mut config = if let Some(config_path) = get_arg(&args, "--config") {
        ZosmfConfig::from_file(&config_path).unwrap_or_else(|e| {
            tracing::warn!(path = %config_path, error = %e, "Failed to load config, using defaults");
            ZosmfConfig::default()
        })
    } else {
        ZosmfConfig::default()
    };

    config.server.port = port;
    config.server.host = "127.0.0.1".to_string();

    // Set up USS root in a temp directory.
    let uss_dir = std::env::temp_dir().join("openmainframe-uss");
    let _ = std::fs::create_dir_all(&uss_dir);
    config.uss.root_directory = uss_dir.to_string_lossy().to_string();

    let mut state = AppState::new(config);

    // Set up dataset catalog in a persistent directory.
    let ds_dir = std::env::temp_dir().join("openmainframe-datasets");
    let _ = std::fs::create_dir_all(&ds_dir);
    *state.catalog.get_mut().unwrap() = open_mainframe_dataset::Catalog::new(&ds_dir);

    // Process CLI mount arguments
    let dataset_mounts = get_all_args(&args, "--mount-dataset");
    let uss_mounts = get_all_args(&args, "--mount-uss");

    {
        let mount_table = state.mount_table.get_mut().unwrap();
        for arg in &dataset_mounts {
            if let Some((mt, host_path, virtual_path)) = parse_mount_arg(arg, MountType::DatasetPds) {
                let id = mount_table.add_mount(mt.clone(), host_path.clone(), virtual_path.clone(), false, None);
                let type_str = match mt {
                    MountType::DatasetPds => "PDS",
                    MountType::DatasetSeq => "SEQ",
                    MountType::Uss => "USS",
                };
                tracing::info!(mount_id = %id, mount_type = type_str, host = %host_path.display(), virtual_path = %virtual_path, "Dataset mount added");
            } else {
                tracing::warn!(arg = %arg, "Invalid --mount-dataset argument");
            }
        }
        for arg in &uss_mounts {
            if let Some((mt, host_path, virtual_path)) = parse_mount_arg(arg, MountType::Uss) {
                let id = mount_table.add_mount(mt, host_path.clone(), virtual_path.clone(), false, None);
                tracing::info!(mount_id = %id, host = %host_path.display(), virtual_path = %virtual_path, "USS mount added");
            } else {
                tracing::warn!(arg = %arg, "Invalid --mount-uss argument");
            }
        }
    }

    // Create default IBMUSER with password SYS1.
    state
        .racf
        .add_user("IBMUSER", "SYS1", "IBM Default User", "SYS1")
        .expect("Failed to create IBMUSER");
    state
        .racf
        .get_user_mut("IBMUSER")
        .expect("IBMUSER not found")
        .password_hash = Some("SYS1".to_string());

    let sysplex_name = state.sysplex.read().unwrap().name.clone();
    let system_count = state.sysplex.read().unwrap().system_count();
    let mount_count = state.mount_table.read().unwrap().list().len();

    let state = Arc::new(state);
    let router = build_router(state);

    let bind_addr = format!("127.0.0.1:{}", port);
    tracing::info!(
        bind_addr = %bind_addr,
        datasets_dir = %ds_dir.display(),
        uss_root = %uss_dir.display(),
        sysplex = %sysplex_name,
        systems = system_count,
        mounts = mount_count,
        "z/OSMF server starting"
    );

    let listener = tokio::net::TcpListener::bind(&bind_addr)
        .await
        .expect("Failed to bind");

    axum::serve(listener, router)
        .await
        .expect("Server error");
}

/// Get the value of a CLI argument (e.g., --config path).
fn get_arg(args: &[String], flag: &str) -> Option<String> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|i| args.get(i + 1).cloned())
}

/// Get all values for a repeatable CLI argument (e.g., multiple --mount-dataset).
fn get_all_args(args: &[String], flag: &str) -> Vec<String> {
    let mut results = Vec::new();
    let mut i = 0;
    while i < args.len() {
        if args[i] == flag {
            if let Some(val) = args.get(i + 1) {
                results.push(val.clone());
                i += 2;
                continue;
            }
        }
        i += 1;
    }
    results
}
