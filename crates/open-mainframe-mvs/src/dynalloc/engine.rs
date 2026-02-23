//! DYNALLOC engine — processes SVC 99 allocation requests.

use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::debug;

use super::dd_table::DdTable;
use super::types::*;
use crate::error::Result;

/// DYNALLOC engine — processes SVC 99 requests against a DD table.
#[derive(Debug, Clone)]
pub struct DynallocEngine {
    dd_table: Arc<RwLock<DdTable>>,
}

impl DynallocEngine {
    /// Create a new engine operating on the given DD table.
    pub fn new(dd_table: Arc<RwLock<DdTable>>) -> Self {
        Self { dd_table }
    }

    /// Execute a DYNALLOC request (SVC 99).
    pub async fn execute(&self, request: &DynallocRequest) -> Result<DynallocResponse> {
        debug!(verb = ?request.verb, "DYNALLOC SVC 99");

        match request.verb {
            DynallocVerb::Allocate => self.do_allocate(request).await,
            DynallocVerb::Unallocate => self.do_unallocate(request).await,
            DynallocVerb::Concatenate => self.do_concatenate(request).await,
            DynallocVerb::Deconcatenate => self.do_deconcatenate(request).await,
            DynallocVerb::InfoRetrieval => self.do_info_retrieval(request).await,
            _ => Ok(DynallocResponse::error(0x0438, 0x0000)),
        }
    }

    async fn do_allocate(&self, request: &DynallocRequest) -> Result<DynallocResponse> {
        let dsname = match request.find_text_unit(TextUnitKey::DalDsnam) {
            Some(tu) => match tu.first_string() {
                Some(s) => s,
                None => return Ok(DynallocResponse::error(0x0438, 0x0002)),
            },
            None => return Ok(DynallocResponse::error(0x0438, 0x0001)),
        };

        let ddname = request
            .find_text_unit(TextUnitKey::DalDdnam)
            .and_then(|tu| tu.first_string());

        let status = request
            .find_text_unit(TextUnitKey::DalStats)
            .and_then(|tu| tu.first_byte())
            .and_then(DatasetStatus::from_byte)
            .unwrap_or(DatasetStatus::Shr);

        let normal_disp = request
            .find_text_unit(TextUnitKey::DalNdisp)
            .and_then(|tu| tu.first_byte())
            .and_then(Disposition::from_byte)
            .unwrap_or(Disposition::Keep);

        let mut table = self.dd_table.write().await;
        match table.allocate(ddname.as_deref(), &dsname, status, normal_disp) {
            Ok(assigned_ddname) => {
                let wants_rtddn = request.find_text_unit(TextUnitKey::DalRtddn).is_some();
                let mut return_tus = Vec::new();
                if wants_rtddn {
                    return_tus.push(TextUnit::string(TextUnitKey::DalRtddn, &assigned_ddname));
                }
                debug!(ddname = %assigned_ddname, dsname = %dsname, "DYNALLOC allocate OK");
                Ok(DynallocResponse::success_with(return_tus))
            }
            Err(e) => {
                debug!(error = %e, "DYNALLOC allocate failed");
                Ok(DynallocResponse::error(0x0438, 0x0004))
            }
        }
    }

    async fn do_unallocate(&self, request: &DynallocRequest) -> Result<DynallocResponse> {
        let ddname = match request.find_text_unit(TextUnitKey::DalDdnam) {
            Some(tu) => match tu.first_string() {
                Some(s) => s,
                None => return Ok(DynallocResponse::error(0x0438, 0x0002)),
            },
            None => return Ok(DynallocResponse::error(0x0438, 0x0001)),
        };

        let mut table = self.dd_table.write().await;
        match table.unallocate(&ddname) {
            Ok(()) => {
                debug!(ddname = %ddname, "DYNALLOC unallocate OK");
                Ok(DynallocResponse::success())
            }
            Err(_) => Ok(DynallocResponse::error(0x0438, 0x0003)),
        }
    }

    async fn do_concatenate(&self, request: &DynallocRequest) -> Result<DynallocResponse> {
        let dsname_tus = request.find_all_text_units(TextUnitKey::DalDsnam);
        if dsname_tus.is_empty() {
            return Ok(DynallocResponse::error(0x0438, 0x0001));
        }

        let dsnames: Vec<String> = dsname_tus
            .iter()
            .filter_map(|tu| tu.first_string())
            .collect();

        if dsnames.is_empty() {
            return Ok(DynallocResponse::error(0x0438, 0x0002));
        }

        let ddname = request
            .find_text_unit(TextUnitKey::DalDdnam)
            .and_then(|tu| tu.first_string());

        let status = request
            .find_text_unit(TextUnitKey::DalStats)
            .and_then(|tu| tu.first_byte())
            .and_then(DatasetStatus::from_byte)
            .unwrap_or(DatasetStatus::Shr);

        let normal_disp = request
            .find_text_unit(TextUnitKey::DalNdisp)
            .and_then(|tu| tu.first_byte())
            .and_then(Disposition::from_byte)
            .unwrap_or(Disposition::Keep);

        let dsname_refs: Vec<&str> = dsnames.iter().map(|s| s.as_str()).collect();

        let mut table = self.dd_table.write().await;
        match table.concatenate(ddname.as_deref(), &dsname_refs, status, normal_disp) {
            Ok(assigned_ddname) => {
                debug!(ddname = %assigned_ddname, count = dsnames.len(), "DYNALLOC concatenate OK");
                let mut return_tus = Vec::new();
                if request.find_text_unit(TextUnitKey::DalRtddn).is_some() {
                    return_tus.push(TextUnit::string(TextUnitKey::DalRtddn, &assigned_ddname));
                }
                Ok(DynallocResponse::success_with(return_tus))
            }
            Err(_) => Ok(DynallocResponse::error(0x0438, 0x0004)),
        }
    }

    async fn do_deconcatenate(&self, request: &DynallocRequest) -> Result<DynallocResponse> {
        let ddname = match request.find_text_unit(TextUnitKey::DalDdnam) {
            Some(tu) => match tu.first_string() {
                Some(s) => s,
                None => return Ok(DynallocResponse::error(0x0438, 0x0002)),
            },
            None => return Ok(DynallocResponse::error(0x0438, 0x0001)),
        };

        let mut table = self.dd_table.write().await;
        match table.deconcatenate(&ddname) {
            Ok(_ddnames) => {
                debug!(ddname = %ddname, "DYNALLOC deconcatenate OK");
                Ok(DynallocResponse::success())
            }
            Err(_) => Ok(DynallocResponse::error(0x0438, 0x0003)),
        }
    }

    async fn do_info_retrieval(&self, request: &DynallocRequest) -> Result<DynallocResponse> {
        let ddname = match request.find_text_unit(TextUnitKey::DalDdnam) {
            Some(tu) => match tu.first_string() {
                Some(s) => s,
                None => return Ok(DynallocResponse::error(0x0438, 0x0002)),
            },
            None => return Ok(DynallocResponse::error(0x0438, 0x0001)),
        };

        let table = self.dd_table.read().await;
        match table.lookup(&ddname) {
            Some(entry) => {
                let mut return_tus = Vec::new();
                if request.find_text_unit(TextUnitKey::DalRtdsn).is_some() {
                    return_tus.push(TextUnit::string(TextUnitKey::DalRtdsn, &entry.dsname));
                }
                Ok(DynallocResponse::success_with(return_tus))
            }
            None => Ok(DynallocResponse::error(0x0438, 0x0003)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_engine() -> (DynallocEngine, Arc<RwLock<DdTable>>) {
        let dd = Arc::new(RwLock::new(DdTable::new()));
        let engine = DynallocEngine::new(dd.clone());
        (engine, dd)
    }

    #[tokio::test]
    async fn allocate_creates_dd_entry() {
        let (engine, dd) = make_engine();
        let req = DynallocRequest {
            verb: DynallocVerb::Allocate,
            flags: 0,
            text_units: vec![
                TextUnit::string(TextUnitKey::DalDsnam, "MY.DATASET"),
                TextUnit::string(TextUnitKey::DalDdnam, "SYSUT1"),
                TextUnit::byte(TextUnitKey::DalStats, DatasetStatus::Shr.to_byte()),
            ],
        };
        let resp = engine.execute(&req).await.unwrap();
        assert!(resp.is_success());

        let table = dd.read().await;
        let entry = table.lookup("SYSUT1").unwrap();
        assert_eq!(entry.dsname, "MY.DATASET");
    }

    #[tokio::test]
    async fn allocate_with_return_ddname() {
        let (engine, _) = make_engine();
        let req = DynallocRequest {
            verb: DynallocVerb::Allocate,
            flags: 0,
            text_units: vec![
                TextUnit::string(TextUnitKey::DalDsnam, "MY.DATASET"),
                TextUnit::string(TextUnitKey::DalRtddn, ""),
            ],
        };
        let resp = engine.execute(&req).await.unwrap();
        assert!(resp.is_success());
        let rtddn = resp
            .return_text_units
            .iter()
            .find(|tu| tu.key == TextUnitKey::DalRtddn)
            .unwrap();
        let assigned = rtddn.first_string().unwrap();
        assert!(assigned.starts_with("SYS"));
    }

    #[tokio::test]
    async fn allocate_missing_dsname_returns_error() {
        let (engine, _) = make_engine();
        let req = DynallocRequest {
            verb: DynallocVerb::Allocate,
            flags: 0,
            text_units: vec![TextUnit::string(TextUnitKey::DalDdnam, "DD01")],
        };
        let resp = engine.execute(&req).await.unwrap();
        assert!(!resp.is_success());
    }

    #[tokio::test]
    async fn unallocate_removes_dd_entry() {
        let (engine, dd) = make_engine();
        // Allocate first
        let alloc_req = DynallocRequest {
            verb: DynallocVerb::Allocate,
            flags: 0,
            text_units: vec![
                TextUnit::string(TextUnitKey::DalDsnam, "MY.DATA"),
                TextUnit::string(TextUnitKey::DalDdnam, "DD01"),
            ],
        };
        engine.execute(&alloc_req).await.unwrap();

        // Unallocate
        let free_req = DynallocRequest {
            verb: DynallocVerb::Unallocate,
            flags: 0,
            text_units: vec![TextUnit::string(TextUnitKey::DalDdnam, "DD01")],
        };
        let resp = engine.execute(&free_req).await.unwrap();
        assert!(resp.is_success());

        let table = dd.read().await;
        assert!(table.lookup("DD01").is_none());
    }

    #[tokio::test]
    async fn invalid_dsname_returns_error_response() {
        let (engine, _) = make_engine();
        let long_name = "A".repeat(45);
        let req = DynallocRequest {
            verb: DynallocVerb::Allocate,
            flags: 0,
            text_units: vec![
                TextUnit::string(TextUnitKey::DalDsnam, &long_name),
                TextUnit::string(TextUnitKey::DalDdnam, "DD01"),
            ],
        };
        let resp = engine.execute(&req).await.unwrap();
        assert!(!resp.is_success());
    }

    #[tokio::test]
    async fn concatenate_and_deconcatenate() {
        let (engine, dd) = make_engine();
        let concat_req = DynallocRequest {
            verb: DynallocVerb::Concatenate,
            flags: 0,
            text_units: vec![
                TextUnit::string(TextUnitKey::DalDdnam, "STEPLIB"),
                TextUnit::string(TextUnitKey::DalDsnam, "MY.LOAD1"),
                TextUnit::string(TextUnitKey::DalDsnam, "MY.LOAD2"),
                TextUnit::string(TextUnitKey::DalDsnam, "SYS1.LINKLIB"),
            ],
        };
        let resp = engine.execute(&concat_req).await.unwrap();
        assert!(resp.is_success());

        {
            let table = dd.read().await;
            let entry = table.lookup("STEPLIB").unwrap();
            assert_eq!(entry.dsname, "MY.LOAD1");
            assert_eq!(entry.concatenation.len(), 2);
        }

        // Now deconcatenate
        let deconcat_req = DynallocRequest {
            verb: DynallocVerb::Deconcatenate,
            flags: 0,
            text_units: vec![TextUnit::string(TextUnitKey::DalDdnam, "STEPLIB")],
        };
        let resp = engine.execute(&deconcat_req).await.unwrap();
        assert!(resp.is_success());

        let table = dd.read().await;
        let entry = table.lookup("STEPLIB").unwrap();
        assert!(entry.concatenation.is_empty());
        assert_eq!(table.len(), 3); // primary + 2 deconcat'd entries
    }

    #[tokio::test]
    async fn info_retrieval_returns_dsname() {
        let (engine, _) = make_engine();
        // Allocate first
        let alloc_req = DynallocRequest {
            verb: DynallocVerb::Allocate,
            flags: 0,
            text_units: vec![
                TextUnit::string(TextUnitKey::DalDsnam, "MY.DATA.FILE"),
                TextUnit::string(TextUnitKey::DalDdnam, "MYDD"),
            ],
        };
        engine.execute(&alloc_req).await.unwrap();

        // Info retrieval
        let info_req = DynallocRequest {
            verb: DynallocVerb::InfoRetrieval,
            flags: 0,
            text_units: vec![
                TextUnit::string(TextUnitKey::DalDdnam, "MYDD"),
                TextUnit::string(TextUnitKey::DalRtdsn, ""),
            ],
        };
        let resp = engine.execute(&info_req).await.unwrap();
        assert!(resp.is_success());
        let rtdsn = resp
            .return_text_units
            .iter()
            .find(|tu| tu.key == TextUnitKey::DalRtdsn)
            .unwrap();
        assert_eq!(rtdsn.first_string().unwrap(), "MY.DATA.FILE");
    }
}
