//! GDG (Generation Data Group) CLI commands.

use std::path::PathBuf;

use miette::Result;
use zos_dataset::{GdgBase, GdgOptions};

/// GDG subcommands.
#[derive(clap::Subcommand, Debug)]
pub enum GdgAction {
    /// Create a new GDG base
    Create {
        /// GDG base name (e.g., MY.GDG.BASE)
        name: String,

        /// Maximum number of generations to keep (1-255)
        #[arg(short, long, default_value = "255")]
        limit: u8,

        /// Delete old generations when rolling off (default: true)
        #[arg(long, default_value = "true")]
        scratch: bool,

        /// Base directory for datasets
        #[arg(long)]
        dataset_dir: Option<PathBuf>,
    },

    /// List GDG generations
    List {
        /// GDG base name
        name: String,

        /// Base directory for datasets
        #[arg(long)]
        dataset_dir: Option<PathBuf>,
    },

    /// Delete a GDG base and all generations
    Delete {
        /// GDG base name
        name: String,

        /// Force deletion even if generations exist
        #[arg(short, long)]
        force: bool,

        /// Base directory for datasets
        #[arg(long)]
        dataset_dir: Option<PathBuf>,
    },

    /// Create a new generation
    NewGen {
        /// GDG base name
        name: String,

        /// Base directory for datasets
        #[arg(long)]
        dataset_dir: Option<PathBuf>,
    },
}

/// Get the dataset directory.
fn get_dataset_dir(dataset_dir: Option<PathBuf>) -> PathBuf {
    dataset_dir.unwrap_or_else(|| {
        std::env::var("ZOS_CLONE_DATASET_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|_| {
                std::env::current_dir()
                    .unwrap_or_else(|_| PathBuf::from("."))
                    .join("datasets")
            })
    })
}

/// Run a GDG command.
pub fn run(action: GdgAction) -> Result<()> {
    match action {
        GdgAction::Create {
            name,
            limit,
            scratch,
            dataset_dir,
        } => {
            let base_dir = get_dataset_dir(dataset_dir);
            let options = GdgOptions {
                limit,
                scratch,
                ..Default::default()
            };

            let base = GdgBase::create(&name, &base_dir, options)
                .map_err(|e| miette::miette!("Failed to create GDG: {}", e))?;

            println!("Created GDG: {}", base.name());
            println!("  Limit: {}", base.options().limit);
            println!("  Scratch: {}", base.options().scratch);

            Ok(())
        }

        GdgAction::List { name, dataset_dir } => {
            let base_dir = get_dataset_dir(dataset_dir);

            let base = GdgBase::open(&name, &base_dir)
                .map_err(|e| miette::miette!("Failed to open GDG: {}", e))?;

            let info = base.list_info();

            println!("GDG: {}", info.name);
            println!("  Limit: {}", info.limit);
            println!("  Scratch: {}", info.scratch);
            println!("  Empty: {}", info.empty);
            println!("  Generations: {}", info.generation_count);
            println!();

            if info.generations.is_empty() {
                println!("  (no generations)");
            } else {
                println!("  {:4}  {:25}  {:>12}  EXISTS", "REL", "NAME", "CREATED");
                println!("  {:-<4}  {:-<25}  {:-<12}  ------", "", "", "");

                for (i, gen) in info.generations.iter().enumerate() {
                    let rel = i as i32 - info.generation_count as i32 + 1;
                    let rel_str = if rel == 0 {
                        "(0)".to_string()
                    } else {
                        format!("({})", rel)
                    };
                    let exists_str = if gen.exists { "yes" } else { "no" };
                    println!(
                        "  {:>4}  {:<25}  {:>12}  {}",
                        rel_str, gen.name, gen.created, exists_str
                    );
                }
            }

            Ok(())
        }

        GdgAction::Delete {
            name,
            force,
            dataset_dir,
        } => {
            let base_dir = get_dataset_dir(dataset_dir);

            let base = GdgBase::open(&name, &base_dir)
                .map_err(|e| miette::miette!("Failed to open GDG: {}", e))?;

            if !force && base.generation_count() > 0 {
                return Err(miette::miette!(
                    "GDG {} has {} generation(s). Use --force to delete anyway.",
                    name,
                    base.generation_count()
                ));
            }

            base.delete()
                .map_err(|e| miette::miette!("Failed to delete GDG: {}", e))?;

            println!("Deleted GDG: {}", name);

            Ok(())
        }

        GdgAction::NewGen { name, dataset_dir } => {
            let base_dir = get_dataset_dir(dataset_dir);

            let mut base = GdgBase::open(&name, &base_dir)
                .map_err(|e| miette::miette!("Failed to open GDG: {}", e))?;

            let gen = base
                .new_generation()
                .map_err(|e| miette::miette!("Failed to create generation: {}", e))?;

            println!("Created generation: {}", gen.name());
            println!("  Path: {}", gen.path().display());

            Ok(())
        }
    }
}
