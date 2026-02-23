//! SYS-101.8 — Integration tests covering cross-module flows.

use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::Arc;
use std::time::Duration;

use open_mainframe_mvs::console::message::{DescriptorCode, RoutingCode};
use open_mainframe_mvs::console::wto::{wto, Console};
use open_mainframe_mvs::dynalloc::dd_table::DdTable;
use open_mainframe_mvs::dynalloc::engine::DynallocEngine;
use open_mainframe_mvs::dynalloc::types::{
    DatasetStatus, DynallocRequest, DynallocVerb, TextUnit, TextUnitKey,
};
use open_mainframe_mvs::program::link::link;
use open_mainframe_mvs::program::load::ModuleManager;
use open_mainframe_mvs::program::search::ProgramSearchOrder;
use open_mainframe_mvs::recovery::estae::{EstaeAction, EstaeEntry, EstaeManager};
use open_mainframe_mvs::recovery::sdwa::Sdwa;
use open_mainframe_mvs::sync::ecb::Ecb;
use open_mainframe_mvs::sync::enq::{EnqManager, EnqMode, EnqResource, EnqScope};
use open_mainframe_mvs::task::abend::AbendCode;
use open_mainframe_mvs::task::attach::{attach, complete_subtask, detach};
use open_mainframe_mvs::task::tcb::Tcb;
use open_mainframe_mvs::timer::stimer::{stimer_exit, stimer_wait, StimerInterval};

use tokio::sync::RwLock;

/// Test: DYNALLOC → ENQ → WTO sequence works without errors.
#[tokio::test]
async fn dynalloc_enq_wto_sequence() {
    // Step 1: DYNALLOC — allocate a dataset
    let dd_table = Arc::new(RwLock::new(DdTable::new()));
    let engine = DynallocEngine::new(dd_table.clone());

    let request = DynallocRequest {
        verb: DynallocVerb::Allocate,
        flags: 0,
        text_units: vec![
            TextUnit::string(TextUnitKey::DalDdnam, "SYSUT1"),
            TextUnit::string(TextUnitKey::DalDsnam, "MY.TEST.DATA"),
            TextUnit::byte(TextUnitKey::DalStats, DatasetStatus::New.to_byte()),
        ],
    };

    let response = engine.execute(&request).await.unwrap();
    assert!(response.is_success(), "DYNALLOC should succeed");

    // Step 2: ENQ — serialize access to the dataset
    let enq = EnqManager::new();
    let resource = EnqResource::new("DATASET", "MY.TEST.DATA").unwrap();
    enq.enq(&resource, EnqMode::Exclusive, EnqScope::Step, 1)
        .await
        .unwrap();

    // Step 3: WTO — report success
    let console = Console::new(100);
    let sender = console.sender();
    let id = wto(
        &sender,
        "IGD100I DATASET MY.TEST.DATA ALLOCATED",
        RoutingCode::OPERATOR_INFO,
        DescriptorCode::INFORMATIONAL,
    )
    .await;
    assert!(id > 0);

    // Cleanup: DEQ
    enq.deq(&resource, 1);

    // Verify DD table has our entry
    let table = dd_table.read().await;
    assert!(table.lookup("SYSUT1").is_some());
}

/// Test: ATTACH subtask → subtask issues WTO → subtask ABENDs → parent ESTAE intercepts.
#[tokio::test]
async fn attach_wto_abend_estae_recovery() {
    let dd_table = Arc::new(RwLock::new(DdTable::new()));
    let mut parent = Tcb::new("MAINPGM", dd_table);

    // Attach a subtask
    let token = attach(&mut parent, "SUBTASK1", None);
    assert!(parent.children.contains(&token.tcb_id));

    // Subtask issues WTO (simulated)
    let console = Console::new(100);
    let sender = console.sender();
    let id = wto(
        &sender,
        "IEF142I SUBTASK1 - STEP WAS EXECUTED",
        RoutingCode::OPERATOR_INFO,
        DescriptorCode::JOB_STATUS,
    )
    .await;
    assert!(id > 0);

    // Subtask ABENDs — create an SDWA from the abend
    let abend_code = AbendCode::User(100);
    let sdwa = Sdwa::new(abend_code, 0);

    // Parent has an ESTAE established
    let estae_invoked = Arc::new(AtomicBool::new(false));
    let estae_invoked_clone = estae_invoked.clone();
    let mut estae_mgr = EstaeManager::new();
    estae_mgr.establish(EstaeEntry {
        handler: Arc::new(move |sdwa_ref| {
            assert_eq!(format!("{}", sdwa_ref.abend_code), "U0100");
            estae_invoked_clone.store(true, Ordering::Release);
            EstaeAction::Retry { address: 0 }
        }),
        parameter: 0,
    });

    // Process the ABEND through ESTAE chain
    let action = estae_mgr.process(&sdwa);
    assert!(matches!(action, Some(EstaeAction::Retry { .. })));
    assert!(estae_invoked.load(Ordering::Acquire));

    // Complete and detach the subtask
    complete_subtask(&token, 4); // CC=4 from abend recovery
    assert!(token.completion_ecb.is_complete());
    detach(&mut parent, &token).unwrap();
    assert!(parent.children.is_empty());
}

/// Test: LINK to program → program issues STIMER WAIT → timer expires → program returns.
#[tokio::test]
async fn link_stimer_wait_returns() {
    // Set up program search order with a known program
    let mut search = ProgramSearchOrder::new();
    search.steplib.push("SYS1.LINKLIB".to_string());
    search.register_program("TIMERPGM", "SYS1.LINKLIB");
    let mut mgr = ModuleManager::new(search);

    // LINK finds the program and returns RC=0
    let rc = link(&mut mgr, "TIMERPGM", None).unwrap();
    assert_eq!(rc, 0);

    // The program issues STIMER WAIT (simulated as 10ms)
    let interval = StimerInterval::BinaryTime(1); // 10ms
    stimer_wait(interval).await;
    // Timer expired — program flow continues successfully
}

/// Test: ProgramSearchOrder STEPLIB-first verification.
#[test]
fn program_search_order_steplib_first() {
    let mut search = ProgramSearchOrder::new();

    // Add LNKLST first, then JOBLIB, then STEPLIB
    search.lnklst.push("SYS1.LINKLIB".to_string());
    search.joblib.push("USER.JOBLIB".to_string());
    search.steplib.push("USER.STEPLIB".to_string());

    // Register program in STEPLIB
    search.register_program("MYPROG", "USER.STEPLIB");
    let found = search.search("MYPROG");
    assert!(found.is_some());
    assert_eq!(found.unwrap(), "USER.STEPLIB");

    // Search path order: STEPLIB → JOBLIB → LNKLST
    let path = search.search_path();
    assert_eq!(path[0], "USER.STEPLIB");
    assert_eq!(path[1], "USER.JOBLIB");
    assert_eq!(path[2], "SYS1.LINKLIB");
}

/// Test: STIMER EXIT with callback → fires callback → ENQ serialization.
#[tokio::test]
async fn stimer_exit_then_enq_serialization() {
    let callback_done = Arc::new(AtomicBool::new(false));
    let callback_done_clone = callback_done.clone();

    // Set a very short timer with a callback that signals completion
    let _handle = stimer_exit(
        StimerInterval::BinaryTime(1), // 10ms
        move || {
            callback_done_clone.store(true, Ordering::Release);
        },
    );

    // Wait for callback to fire
    tokio::time::sleep(Duration::from_millis(50)).await;
    assert!(callback_done.load(Ordering::Acquire));

    // After timer fires, ENQ serialization still works
    let enq = EnqManager::new();
    let resource = EnqResource::new("SYSDSN", "SYS1.LINKLIB").unwrap();
    enq.enq(&resource, EnqMode::Shared, EnqScope::System, 1)
        .await
        .unwrap();
    enq.deq(&resource, 1);
}

/// Test: Multiple services — GETMAIN storage + DYNALLOC + ECB wait/post.
#[tokio::test]
async fn storage_dynalloc_ecb_flow() {
    use open_mainframe_mvs::storage::getmain::{freemain, getmain};
    use open_mainframe_mvs::storage::subpool::SubpoolManager;

    // GETMAIN — allocate working storage
    let mut storage = SubpoolManager::new();
    let addr = getmain(&mut storage, 0, 4096).unwrap();
    assert!(addr > 0);

    // DYNALLOC — allocate output dataset
    let dd_table = Arc::new(RwLock::new(DdTable::new()));
    let engine = DynallocEngine::new(dd_table.clone());
    let request = DynallocRequest {
        verb: DynallocVerb::Allocate,
        flags: 0,
        text_units: vec![
            TextUnit::string(TextUnitKey::DalDdnam, "SYSPRINT"),
            TextUnit::string(TextUnitKey::DalDsnam, "OUTPUT.DATA"),
            TextUnit::byte(TextUnitKey::DalStats, DatasetStatus::New.to_byte()),
        ],
    };
    let response = engine.execute(&request).await.unwrap();
    assert!(response.is_success());

    // ECB — signal processing complete
    let ecb = Arc::new(Ecb::new());
    let ecb_clone = ecb.clone();
    tokio::spawn(async move {
        tokio::task::yield_now().await;
        ecb_clone.post(0);
    });
    ecb.wait_on().await;
    assert!(ecb.is_complete());
    assert_eq!(ecb.completion_code(), 0);

    // FREEMAIN — release working storage
    assert!(freemain(&mut storage, addr).is_ok());
}

/// Test: ESTAE chain with multiple levels — percolate then retry.
#[test]
fn estae_multilevel_recovery() {
    let mut estae = EstaeManager::new();
    let level_reached = Arc::new(AtomicU32::new(0));

    // First ESTAE (established first, invoked last) — percolate
    let lr1 = level_reached.clone();
    estae.establish(EstaeEntry {
        handler: Arc::new(move |_sdwa| {
            lr1.store(1, Ordering::Release);
            EstaeAction::Percolate
        }),
        parameter: 1,
    });

    // Second ESTAE (established second, invoked second) — also percolate
    let lr2 = level_reached.clone();
    estae.establish(EstaeEntry {
        handler: Arc::new(move |_sdwa| {
            lr2.store(2, Ordering::Release);
            EstaeAction::Percolate
        }),
        parameter: 2,
    });

    // Third ESTAE (established last, invoked first) — retry
    let lr3 = level_reached.clone();
    estae.establish(EstaeEntry {
        handler: Arc::new(move |_sdwa| {
            lr3.store(3, Ordering::Release);
            EstaeAction::Retry { address: 0x1000 }
        }),
        parameter: 3,
    });

    let sdwa = Sdwa::new(AbendCode::System(0x0C7), 0);
    let action = estae.process(&sdwa);

    // Third ESTAE should fire and retry (LIFO — last established, first invoked)
    assert!(matches!(action, Some(EstaeAction::Retry { address: 0x1000 })));
    assert_eq!(level_reached.load(Ordering::Acquire), 3);
}

/// Test: Full lifecycle — create TCB, attach, WTO, complete, detach.
#[tokio::test]
async fn full_task_lifecycle() {
    let dd_table = Arc::new(RwLock::new(DdTable::new()));
    let mut parent = Tcb::new("IEFBR14", dd_table);
    assert_eq!(parent.program_name, "IEFBR14");

    // Attach two subtasks
    let t1 = attach(&mut parent, "STEP1", None);
    let t2 = attach(&mut parent, "STEP2", None);
    assert_eq!(parent.children.len(), 2);

    // Both subtasks issue WTO
    let console = Console::new(100);
    let sender = console.sender();
    let _msg1 = wto(
        &sender,
        "IEF142I STEP1 STARTED",
        RoutingCode::OPERATOR_INFO,
        DescriptorCode::JOB_STATUS,
    )
    .await;
    let _msg2 = wto(
        &sender,
        "IEF142I STEP2 STARTED",
        RoutingCode::OPERATOR_INFO,
        DescriptorCode::JOB_STATUS,
    )
    .await;

    // STEP1 completes successfully
    complete_subtask(&t1, 0);
    assert!(t1.completion_ecb.is_complete());
    assert_eq!(t1.completion_ecb.completion_code(), 0);

    // STEP2 completes with RC=4
    complete_subtask(&t2, 4);
    assert!(t2.completion_ecb.is_complete());
    assert_eq!(t2.completion_ecb.completion_code(), 4);

    // Wait for both ECBs (already posted, should return immediately)
    t1.completion_ecb.wait_on().await;
    t2.completion_ecb.wait_on().await;

    // Detach both
    detach(&mut parent, &t1).unwrap();
    detach(&mut parent, &t2).unwrap();
    assert!(parent.children.is_empty());
}
