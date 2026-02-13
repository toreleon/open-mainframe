//! Generation Data Group (GDG) support.
//!
//! GDGs provide dataset versioning by maintaining a catalog of generations
//! under a base name. Each generation has a unique name in the format:
//! `BASE.GxxxxVyy` where xxxx is the generation number and yy is the version.
//!
//! # Example
//!
//! ```ignore
//! use zos_dataset::gdg::{GdgBase, GdgOptions};
//!
//! // Create a GDG base with limit 10
//! let base = GdgBase::create("MY.GDG.BASE", GdgOptions {
//!     limit: 10,
//!     scratch: true,
//!     ..Default::default()
//! })?;
//!
//! // Create a new generation
//! let gen = base.new_generation()?;
//! assert_eq!(gen.name(), "MY.GDG.BASE.G0001V00");
//! ```

mod base;
mod generation;

pub use base::{GdgBase, GdgGenerationInfo, GdgListInfo, GdgOptions};
pub use generation::{GdgGeneration, GenerationNumber};
