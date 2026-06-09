use crate::logging::LogLevel;
use log::Level;
/// from https://github.com/dramforever/nix-json-progress/blob/main/src/log_item.rs
use num_enum::{IntoPrimitive, TryFromPrimitive};
use remoc::rtc::{Deserialize, Serialize};

#[derive(
    Clone,
    Copy,
    Debug,
    TryFromPrimitive,
    IntoPrimitive,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
)]
#[repr(i64)]
pub enum Verbosity {
    Error = 0,
    Warn = 1,
    Notice = 2,
    Info = 3,
    Talkative = 4,
    Chatty = 5,
    Debug = 6,
    Vomit = 7,
}

impl From<Verbosity> for LogLevel {
    fn from(value: Verbosity) -> Self {
        match value {
            Verbosity::Error => LogLevel::Error,
            Verbosity::Warn => LogLevel::Warning,
            Verbosity::Notice => LogLevel::Info,
            Verbosity::Info => LogLevel::Debug,
            Verbosity::Talkative => LogLevel::Trace,
            Verbosity::Chatty => LogLevel::Trace,
            Verbosity::Debug => LogLevel::Trace,
            Verbosity::Vomit => LogLevel::Trace,
        }
    }
}

impl From<Verbosity> for Level {
    fn from(value: Verbosity) -> Self {
        match value {
            Verbosity::Error => Level::Error,
            Verbosity::Warn => Level::Info,
            Verbosity::Notice => Level::Info,
            Verbosity::Info => Level::Debug,
            Verbosity::Talkative => Level::Trace,
            Verbosity::Chatty => Level::Trace,
            Verbosity::Debug => Level::Trace,
            Verbosity::Vomit => Level::Trace,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum Activity {
    Unknown,
    CopyPath {
        path: String,
        from: String,
        to: String,
    },
    FileTransfer {
        uri: String,
    },
    Realise,
    CopyPaths,
    Builds,
    Build {
        path: String,
        machine: String,
        round: i64,
        total_rounds: i64,
    },
    OptimiseStore,
    VerifyPaths,
    Substitute {
        path: String,
        uri: String,
    },
    QueryPathInfo {
        path: String,
        uri: String,
    },
    PostBuildHook {
        path: String,
    },
    BuildWaiting,
    FetchTree,
}

// impl Activity {
//     pub fn to_type(&self) -> ActivityType {
//         match self {
//             Activity::Unknown => ActivityType::Unknown,
//             Activity::CopyPath { .. } => ActivityType::CopyPath,
//             Activity::FileTransfer { .. } => ActivityType::FileTransfer,
//             Activity::Realise => ActivityType::Realise,
//             Activity::CopyPaths => ActivityType::CopyPaths,
//             Activity::Builds => ActivityType::Builds,
//             Activity::Build { .. } => ActivityType::Build,
//             Activity::OptimiseStore => ActivityType::OptimiseStore,
//             Activity::VerifyPaths => ActivityType::VerifyPaths,
//             Activity::Substitute { .. } => ActivityType::Substitute,
//             Activity::QueryPathInfo { .. } => ActivityType::QueryPathInfo,
//             Activity::PostBuildHook { .. } => ActivityType::PostBuildHook,
//             Activity::BuildWaiting { .. } => ActivityType::BuildWaiting,
//             Activity::FetchTree { .. } => ActivityType::FetchTree,
//         }
//     }
// }

#[derive(
    Clone, Copy, Debug, TryFromPrimitive, IntoPrimitive, PartialEq, Eq, Serialize, Deserialize,
)]
#[repr(i64)]
// #[non_exhaustive]
pub enum ActivityType {
    Unknown = 0,
    CopyPath = 100,
    FileTransfer = 101,
    Realise = 102,
    CopyPaths = 103,
    Builds = 104,
    Build = 105,
    OptimiseStore = 106,
    VerifyPaths = 107,
    Substitute = 108,
    QueryPathInfo = 109,
    PostBuildHook = 110,
    BuildWaiting = 111,
    FetchTree = 112,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum LogResult {
    FileLinked {
        size: i64,
        blocks: i64,
    },
    BuildLogLine {
        line: String,
    },
    UntrustedPath {
        path: String,
    },
    CorruptedPath {
        path: String,
    },
    SetPhase {
        phase: String,
    },
    Progress {
        done: i64,
        expected: i64,
        running: i64,
        failed: i64,
    },
    SetExpected {
        activity_type: ActivityType,
        expected: i64,
    },
    PostBuildLogLine {
        line: String,
    },
    FetchStatus {
        line: String,
    },
}

#[derive(Clone, Copy, Debug, TryFromPrimitive, IntoPrimitive)]
#[repr(i64)]
#[non_exhaustive]
pub enum ResultType {
    FileLinked = 100,
    BuildLogLine = 101,
    UntrustedPath = 102,
    CorruptedPath = 103,
    SetPhase = 104,
    Progress = 105,
    SetExpected = 106,
    PostBuildLogLine = 107,
    FetchStatus = 108,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum LogItem {
    Msg {
        level: Verbosity,
        msg: String,
    },
    Start {
        id: i64,
        level: Verbosity,
        text: String,
        activity: Activity,
    },
    Stop {
        id: i64,
    },
    Result {
        id: i64,
        result: LogResult,
    },
}
