//! RACF error types.

use miette::Diagnostic;
use thiserror::Error;

/// Errors produced by the RACF subsystem.
#[derive(Debug, Error, Diagnostic)]
pub enum RacfError {
    /// User profile already exists.
    #[error("user '{userid}' already defined to RACF")]
    #[diagnostic(code(racf::user_exists))]
    UserExists {
        /// The duplicate user ID.
        userid: String,
    },

    /// User profile not found.
    #[error("user '{userid}' is not defined to RACF")]
    #[diagnostic(code(racf::user_not_found))]
    UserNotFound {
        /// The missing user ID.
        userid: String,
    },

    /// Group profile already exists.
    #[error("group '{group}' already defined to RACF")]
    #[diagnostic(code(racf::group_exists))]
    GroupExists {
        /// The duplicate group name.
        group: String,
    },

    /// Group profile not found.
    #[error("group '{group}' is not defined to RACF")]
    #[diagnostic(code(racf::group_not_found))]
    GroupNotFound {
        /// The missing group name.
        group: String,
    },

    /// User is not connected to group.
    #[error("user '{userid}' is not connected to group '{group}'")]
    #[diagnostic(code(racf::not_connected))]
    NotConnected {
        /// The user ID.
        userid: String,
        /// The group name.
        group: String,
    },

    /// User is already connected to group.
    #[error("user '{userid}' is already connected to group '{group}'")]
    #[diagnostic(code(racf::already_connected))]
    AlreadyConnected {
        /// The user ID.
        userid: String,
        /// The group name.
        group: String,
    },

    /// Cannot delete group that still has connected users.
    #[error("group '{group}' has connected users and cannot be deleted")]
    #[diagnostic(code(racf::group_not_empty), help("Remove all users from the group first"))]
    GroupNotEmpty {
        /// The group name.
        group: String,
    },

    /// Cannot delete the user's default group connection.
    #[error("cannot remove user '{userid}' from default group '{group}'")]
    #[diagnostic(
        code(racf::cannot_remove_default),
        help("Use ALTUSER to change the default group first")
    )]
    CannotRemoveDefaultGroup {
        /// The user ID.
        userid: String,
        /// The group name.
        group: String,
    },

    /// Persistent storage I/O error.
    #[error("RACF database I/O error: {message}")]
    #[diagnostic(code(racf::io_error))]
    IoError {
        /// The error description.
        message: String,
    },

    /// Invalid user ID format.
    #[error("invalid user ID '{userid}': must be 1-8 alphanumeric characters")]
    #[diagnostic(code(racf::invalid_userid))]
    InvalidUserId {
        /// The invalid user ID.
        userid: String,
    },

    /// Invalid group name format.
    #[error("invalid group name '{group}': must be 1-8 alphanumeric characters")]
    #[diagnostic(code(racf::invalid_group))]
    InvalidGroupName {
        /// The invalid group name.
        group: String,
    },

    /// Dataset profile already exists.
    #[error("dataset profile '{name}' already defined to RACF")]
    #[diagnostic(code(racf::dataset_exists))]
    DatasetProfileExists {
        /// The duplicate profile name.
        name: String,
    },

    /// Dataset profile not found.
    #[error("dataset profile '{name}' is not defined to RACF")]
    #[diagnostic(code(racf::dataset_not_found))]
    DatasetProfileNotFound {
        /// The missing profile name.
        name: String,
    },

    /// No matching dataset profile found for authorization check.
    #[error("no profile found covering dataset '{dataset}'")]
    #[diagnostic(code(racf::no_covering_profile))]
    NoCoveringProfile {
        /// The dataset name with no covering profile.
        dataset: String,
    },
}
