//! # GDG-to-ICF Catalog Integration
//!
//! Wires the GDG (Generation Data Group) implementation into the ICF catalog:
//!
//! - GDG base stored as a BCS `Gdg` entry with LIMIT/SCRATCH attributes
//! - Each generation stored as a BCS `NonVsam` entry linked to its base
//! - Relative references (+1, 0, -1, -2, …) resolved through catalog
//! - Rolloff removes oldest generation catalog entries when LIMIT exceeded
//! - LISTCAT shows GDG base and all generation details

use crate::icf::{BcsEntry, BcsEntryType, IcfCatalogSystem};

/// GDG-ICF integration layer.
///
/// Manages GDG lifecycle operations through the ICF catalog.
pub struct GdgIcfManager<'a> {
    catalog: &'a mut IcfCatalogSystem,
}

impl<'a> GdgIcfManager<'a> {
    /// Create a new GDG-ICF manager.
    pub fn new(catalog: &'a mut IcfCatalogSystem) -> Self {
        Self { catalog }
    }

    /// DEFINE GDG — create a GDG base entry in the catalog.
    ///
    /// Equivalent to `DEFINE GDG (NAME(dsn) LIMIT(limit) SCRATCH|NOSCRATCH)`.
    pub fn define_gdg(&mut self, dsn: &str, limit: u8, scratch: bool) {
        let mut entry = BcsEntry::gdg_base(dsn);
        entry
            .attributes
            .insert("LIMIT".to_string(), limit.to_string());
        entry.attributes.insert(
            "SCRATCH".to_string(),
            if scratch { "YES" } else { "NO" }.to_string(),
        );
        self.catalog.catalog_dataset(entry);
    }

    /// Catalog a new GDG generation.
    ///
    /// Creates a NonVsam BCS entry for the generation (e.g., `MY.GDG.G0001V00`)
    /// and enforces the GDG LIMIT by rolling off old generations.
    pub fn catalog_generation(
        &mut self,
        gdg_base_dsn: &str,
        generation_dsn: &str,
        volume: &str,
    ) -> Vec<String> {
        let base_upper = gdg_base_dsn.to_uppercase();
        let gen_upper = generation_dsn.to_uppercase();

        // Catalog the new generation as NonVsam
        let entry = BcsEntry::non_vsam(&gen_upper, vec![volume.to_uppercase()]);
        self.catalog.catalog_dataset(entry);

        // Enforce LIMIT by rolling off oldest generations
        let limit = self.get_gdg_limit(&base_upper);
        if limit == 0 {
            return Vec::new();
        }

        let generations = self.list_generations(&base_upper);
        if generations.len() <= limit as usize {
            return Vec::new();
        }

        // Roll off oldest generations
        let to_remove = generations.len() - limit as usize;
        let mut removed = Vec::new();
        for gen in generations.into_iter().take(to_remove) {
            self.catalog.uncatalog_dataset(&gen);
            removed.push(gen);
        }
        removed
    }

    /// Resolve a relative GDG reference through the catalog.
    ///
    /// - `BASE(0)` → current (most recent) generation
    /// - `BASE(-1)` → previous generation
    /// - `BASE(-N)` → N generations back
    /// - `BASE(+1)` → next new generation name (computed, not cataloged yet)
    pub fn resolve_relative(
        &self,
        gdg_base_dsn: &str,
        relative: i32,
    ) -> Option<String> {
        let base_upper = gdg_base_dsn.to_uppercase();
        let generations = self.list_generations(&base_upper);

        if relative > 0 {
            // Compute the next generation name
            let next_num = if let Some(last) = generations.last() {
                parse_generation_number(last).unwrap_or(0) + 1
            } else {
                1
            };
            return Some(format!("{}.G{:04}V00", base_upper, next_num));
        }

        // relative <= 0: index from end
        let count = generations.len();
        if count == 0 {
            return None;
        }
        let idx = count as i32 - 1 + relative;
        if idx >= 0 && (idx as usize) < count {
            Some(generations[idx as usize].clone())
        } else {
            None
        }
    }

    /// List all cataloged generations for a GDG base, sorted by generation number.
    pub fn list_generations(&self, gdg_base_dsn: &str) -> Vec<String> {
        let base_upper = gdg_base_dsn.to_uppercase();
        let prefix = format!("{}.G", base_upper);

        // Search all catalogs for generation entries
        let results = self.catalog.listcat_level(&prefix, None);
        let mut gens: Vec<String> = results
            .into_iter()
            .filter(|r| {
                r.entry.entry_type == BcsEntryType::NonVsam
                    && r.entry.dsn.starts_with(&prefix)
                    && is_generation_dsn(&r.entry.dsn, &base_upper)
            })
            .map(|r| r.entry.dsn)
            .collect();

        gens.sort_by_key(|dsn| parse_generation_number(dsn).unwrap_or(0));
        gens
    }

    /// LISTCAT for a GDG — returns base info and all generation details.
    pub fn listcat_gdg(&self, gdg_base_dsn: &str) -> Option<GdgCatalogInfo> {
        let base_upper = gdg_base_dsn.to_uppercase();

        // Find the GDG base entry
        let base_result = self.catalog.lookup(&base_upper)?;
        if base_result.entry.entry_type != BcsEntryType::Gdg {
            return None;
        }

        let limit = base_result
            .entry
            .attributes
            .get("LIMIT")
            .and_then(|v| v.parse().ok())
            .unwrap_or(255u8);
        let scratch = base_result
            .entry
            .attributes
            .get("SCRATCH")
            .map(|v| v == "YES")
            .unwrap_or(true);

        let generations = self.list_generations(&base_upper);
        let gen_details: Vec<GdgGenerationCatalogInfo> = generations
            .iter()
            .map(|dsn| {
                let result = self.catalog.lookup(dsn);
                GdgGenerationCatalogInfo {
                    dsn: dsn.clone(),
                    generation_number: parse_generation_number(dsn).unwrap_or(0),
                    catalog_name: result
                        .as_ref()
                        .map(|r| r.catalog_name.clone())
                        .unwrap_or_default(),
                    volumes: result
                        .map(|r| r.entry.volumes)
                        .unwrap_or_default(),
                }
            })
            .collect();

        Some(GdgCatalogInfo {
            base_dsn: base_upper,
            catalog_name: base_result.catalog_name,
            limit,
            scratch,
            generation_count: gen_details.len(),
            generations: gen_details,
        })
    }

    /// Delete a GDG and all its generations from the catalog.
    pub fn delete_gdg(&mut self, gdg_base_dsn: &str) -> bool {
        let base_upper = gdg_base_dsn.to_uppercase();

        // Remove all generation entries
        let generations = self.list_generations(&base_upper);
        for gen in generations {
            self.catalog.uncatalog_dataset(&gen);
        }

        // Remove the base entry
        self.catalog.uncatalog_dataset(&base_upper)
    }

    /// Get the LIMIT attribute of a GDG base.
    fn get_gdg_limit(&self, gdg_base_dsn: &str) -> u8 {
        self.catalog
            .lookup(gdg_base_dsn)
            .and_then(|r| r.entry.attributes.get("LIMIT").and_then(|v| v.parse().ok()))
            .unwrap_or(255)
    }
}

/// LISTCAT output for a GDG.
#[derive(Debug, Clone)]
pub struct GdgCatalogInfo {
    /// GDG base dataset name.
    pub base_dsn: String,
    /// Catalog where the base is defined.
    pub catalog_name: String,
    /// Generation limit.
    pub limit: u8,
    /// Scratch mode.
    pub scratch: bool,
    /// Number of current generations.
    pub generation_count: usize,
    /// Generation details.
    pub generations: Vec<GdgGenerationCatalogInfo>,
}

/// LISTCAT output for a single GDG generation.
#[derive(Debug, Clone)]
pub struct GdgGenerationCatalogInfo {
    /// Full generation DSN (e.g., `MY.GDG.G0001V00`).
    pub dsn: String,
    /// Generation number (extracted from name).
    pub generation_number: u16,
    /// Catalog where this generation is cataloged.
    pub catalog_name: String,
    /// Volume serials.
    pub volumes: Vec<String>,
}

/// Check if a DSN is a generation entry for the given GDG base.
fn is_generation_dsn(dsn: &str, base_dsn: &str) -> bool {
    let suffix = match dsn.strip_prefix(base_dsn) {
        Some(s) => s,
        None => return false,
    };
    // Must be .GnnnnVnn format
    if !suffix.starts_with(".G") {
        return false;
    }
    let gen_part = &suffix[1..]; // "GnnnnVnn"
    if gen_part.len() < 5 {
        return false;
    }
    gen_part[1..5].chars().all(|c| c.is_ascii_digit())
}

/// Parse the generation number from a generation DSN.
fn parse_generation_number(dsn: &str) -> Option<u16> {
    // Find the last ".G" and parse the number
    let g_pos = dsn.rfind(".G")?;
    let after_g = &dsn[g_pos + 2..];
    let num_str: String = after_g.chars().take_while(|c| c.is_ascii_digit()).collect();
    num_str.parse().ok()
}

// ─────────────────────── Tests ───────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::icf::IcfCatalogSystem;

    fn setup() -> IcfCatalogSystem {
        let mut sys = IcfCatalogSystem::new("MASTER.CATALOG");
        sys.define_user_catalog("UCAT.PROD", "UCVOL1");
        sys.define_alias("PROD", "UCAT.PROD");
        sys
    }

    // ─── DFSMS-110.1: GDG Base as ICF Catalog Entry ───

    #[test]
    fn test_define_gdg_creates_catalog_entry() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
        }

        let result = sys.lookup("PROD.MY.GDG").unwrap();
        assert_eq!(result.entry.entry_type, BcsEntryType::Gdg);
        assert_eq!(result.entry.attributes.get("LIMIT").unwrap(), "10");
        assert_eq!(result.entry.attributes.get("SCRATCH").unwrap(), "YES");
    }

    #[test]
    fn test_define_gdg_noscratch() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 5, false);
        }

        let result = sys.lookup("PROD.MY.GDG").unwrap();
        assert_eq!(result.entry.attributes.get("SCRATCH").unwrap(), "NO");
    }

    #[test]
    fn test_define_gdg_routed_to_user_catalog() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
        }

        let result = sys.lookup("PROD.MY.GDG").unwrap();
        assert_eq!(result.catalog_name, "UCAT.PROD");
    }

    #[test]
    fn test_listcat_shows_gdg_base_and_generations() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0001V00", "VOL001");
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0002V00", "VOL001");
        }
        let mgr = GdgIcfManager::new(&mut sys);
        let info = mgr.listcat_gdg("PROD.MY.GDG").unwrap();
        assert_eq!(info.base_dsn, "PROD.MY.GDG");
        assert_eq!(info.limit, 10);
        assert!(info.scratch);
        assert_eq!(info.generation_count, 2);
        assert_eq!(info.generations[0].dsn, "PROD.MY.GDG.G0001V00");
        assert_eq!(info.generations[1].dsn, "PROD.MY.GDG.G0002V00");
    }

    // ─── DFSMS-110.2: Generation Catalog Entries ───

    #[test]
    fn test_catalog_generation_creates_nonvsam_entry() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0001V00", "VOL001");
        }

        let result = sys.lookup("PROD.MY.GDG.G0001V00").unwrap();
        assert_eq!(result.entry.entry_type, BcsEntryType::NonVsam);
        assert!(result.entry.volumes.contains(&"VOL001".to_string()));
    }

    #[test]
    fn test_resolve_relative_zero() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0001V00", "VOL001");
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0002V00", "VOL001");
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0003V00", "VOL001");
        }
        let mgr = GdgIcfManager::new(&mut sys);

        // (0) = current = most recent
        let resolved = mgr.resolve_relative("PROD.MY.GDG", 0).unwrap();
        assert_eq!(resolved, "PROD.MY.GDG.G0003V00");
    }

    #[test]
    fn test_resolve_relative_negative() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0001V00", "VOL001");
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0002V00", "VOL001");
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0003V00", "VOL001");
        }
        let mgr = GdgIcfManager::new(&mut sys);

        // (-1) = previous
        let resolved = mgr.resolve_relative("PROD.MY.GDG", -1).unwrap();
        assert_eq!(resolved, "PROD.MY.GDG.G0002V00");

        // (-2) = two back
        let resolved = mgr.resolve_relative("PROD.MY.GDG", -2).unwrap();
        assert_eq!(resolved, "PROD.MY.GDG.G0001V00");

        // (-3) = out of range
        assert!(mgr.resolve_relative("PROD.MY.GDG", -3).is_none());
    }

    #[test]
    fn test_resolve_relative_plus_one() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0001V00", "VOL001");
            mgr.catalog_generation("PROD.MY.GDG", "PROD.MY.GDG.G0002V00", "VOL001");
        }
        let mgr = GdgIcfManager::new(&mut sys);

        // (+1) = next new generation name
        let resolved = mgr.resolve_relative("PROD.MY.GDG", 1).unwrap();
        assert_eq!(resolved, "PROD.MY.GDG.G0003V00");
    }

    // ─── DFSMS-110.3: Integration Tests ───

    #[test]
    fn test_gdg_limit_5_with_7_generations_rolloff() {
        let mut sys = setup();
        let mut removed_all = Vec::new();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 5, true);

            // Create 7 generations
            for i in 1..=7 {
                let dsn = format!("PROD.MY.GDG.G{:04}V00", i);
                let removed = mgr.catalog_generation("PROD.MY.GDG", &dsn, "VOL001");
                removed_all.extend(removed);
            }
        }

        // Should have rolled off G0001 and G0002
        assert_eq!(removed_all.len(), 2);
        assert!(removed_all.contains(&"PROD.MY.GDG.G0001V00".to_string()));
        assert!(removed_all.contains(&"PROD.MY.GDG.G0002V00".to_string()));

        // Only 5 generations should remain
        let mgr = GdgIcfManager::new(&mut sys);
        let gens = mgr.list_generations("PROD.MY.GDG");
        assert_eq!(gens.len(), 5);
        assert_eq!(gens[0], "PROD.MY.GDG.G0003V00");
        assert_eq!(gens[4], "PROD.MY.GDG.G0007V00");

        // Rolled-off generations should not be in catalog
        assert!(sys.lookup("PROD.MY.GDG.G0001V00").is_none());
        assert!(sys.lookup("PROD.MY.GDG.G0002V00").is_none());
    }

    #[test]
    fn test_listcat_all_shows_gdg_details() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
            for i in 1..=3 {
                let dsn = format!("PROD.MY.GDG.G{:04}V00", i);
                mgr.catalog_generation("PROD.MY.GDG", &dsn, &format!("VOL{:03}", i));
            }
        }
        let mgr = GdgIcfManager::new(&mut sys);

        let info = mgr.listcat_gdg("PROD.MY.GDG").unwrap();
        assert_eq!(info.base_dsn, "PROD.MY.GDG");
        assert_eq!(info.generation_count, 3);

        // Each generation should have its volume
        assert_eq!(info.generations[0].volumes, vec!["VOL001"]);
        assert_eq!(info.generations[1].volumes, vec!["VOL002"]);
        assert_eq!(info.generations[2].volumes, vec!["VOL003"]);

        // Generation numbers
        assert_eq!(info.generations[0].generation_number, 1);
        assert_eq!(info.generations[1].generation_number, 2);
        assert_eq!(info.generations[2].generation_number, 3);
    }

    #[test]
    fn test_delete_gdg_removes_all() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
            for i in 1..=3 {
                let dsn = format!("PROD.MY.GDG.G{:04}V00", i);
                mgr.catalog_generation("PROD.MY.GDG", &dsn, "VOL001");
            }
        }

        // Verify all exist
        assert!(sys.lookup("PROD.MY.GDG").is_some());
        assert!(sys.lookup("PROD.MY.GDG.G0001V00").is_some());

        // Delete GDG
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            assert!(mgr.delete_gdg("PROD.MY.GDG"));
        }

        // All should be gone
        assert!(sys.lookup("PROD.MY.GDG").is_none());
        assert!(sys.lookup("PROD.MY.GDG.G0001V00").is_none());
        assert!(sys.lookup("PROD.MY.GDG.G0002V00").is_none());
        assert!(sys.lookup("PROD.MY.GDG.G0003V00").is_none());
    }

    #[test]
    fn test_listcat_nonexistent_gdg() {
        let mut sys = setup();
        let mgr = GdgIcfManager::new(&mut sys);
        assert!(mgr.listcat_gdg("PROD.NO.SUCH.GDG").is_none());
    }

    #[test]
    fn test_resolve_empty_gdg() {
        let mut sys = setup();
        {
            let mut mgr = GdgIcfManager::new(&mut sys);
            mgr.define_gdg("PROD.MY.GDG", 10, true);
        }
        let mgr = GdgIcfManager::new(&mut sys);

        // No generations — relative 0 should be None
        assert!(mgr.resolve_relative("PROD.MY.GDG", 0).is_none());

        // (+1) on empty GDG should give G0001V00
        let next = mgr.resolve_relative("PROD.MY.GDG", 1).unwrap();
        assert_eq!(next, "PROD.MY.GDG.G0001V00");
    }
}
