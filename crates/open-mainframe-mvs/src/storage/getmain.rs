//! GETMAIN/FREEMAIN — storage allocation system services.

use crate::error::{MvsError, Result};
use crate::storage::SubpoolManager;

/// GETMAIN — allocate storage from a subpool.
///
/// Returns the address of the allocated block.
pub fn getmain(storage: &mut SubpoolManager, subpool: u8, length: u32) -> Result<u64> {
    if length == 0 {
        return Err(MvsError::GetmainFailed { subpool, length });
    }
    Ok(storage.allocate(subpool, length))
}

/// FREEMAIN — free a previously allocated storage block.
pub fn freemain(storage: &mut SubpoolManager, address: u64) -> Result<()> {
    if storage.free(address) {
        Ok(())
    } else {
        Err(MvsError::FreemainFailed { address })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn getmain_allocates_storage() {
        let mut storage = SubpoolManager::new();
        let addr = getmain(&mut storage, 0, 1024).unwrap();
        assert!(addr > 0);
        assert!(storage.get_block(addr).is_some());
    }

    #[test]
    fn getmain_zero_length_fails() {
        let mut storage = SubpoolManager::new();
        let err = getmain(&mut storage, 0, 0).unwrap_err();
        assert!(matches!(err, MvsError::GetmainFailed { .. }));
    }

    #[test]
    fn freemain_releases_storage() {
        let mut storage = SubpoolManager::new();
        let addr = getmain(&mut storage, 0, 256).unwrap();
        assert!(freemain(&mut storage, addr).is_ok());
        assert!(storage.get_block(addr).is_none());
    }

    #[test]
    fn freemain_invalid_address_fails() {
        let mut storage = SubpoolManager::new();
        let err = freemain(&mut storage, 0xBAD).unwrap_err();
        assert!(matches!(err, MvsError::FreemainFailed { .. }));
    }

    #[test]
    fn getmain_different_subpools() {
        let mut storage = SubpoolManager::new();
        let a1 = getmain(&mut storage, 0, 100).unwrap();
        let a2 = getmain(&mut storage, 230, 200).unwrap();
        assert_ne!(a1, a2);
        assert_eq!(storage.subpool_allocated(0), 100);
        assert_eq!(storage.subpool_allocated(230), 200);
    }
}
