// from https://github.com/dramforever/nix-json-progress/blob/main/src/log_item.rs

use log::Level;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use serde_json::Value;

#[derive(Clone, Copy, Debug, TryFromPrimitive, IntoPrimitive, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug, Clone)]
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

#[derive(Clone, Copy, Debug, TryFromPrimitive, IntoPrimitive, PartialEq, Eq)]
#[repr(i64)]
#[non_exhaustive]
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

#[derive(Debug)]
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

#[derive(Debug)]
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
    OutputLine(String),
    UnknownItem(Value),
}

pub fn parse_log_item(val: &Value) -> Option<LogItem> {
    use LogItem::*;
    let val = val.as_object()?;

    match val.get("action")?.as_str()? {
        "msg" => Some(Msg {
            level: val.get("level")?.as_i64()?.try_into().ok()?,
            msg: val.get("msg")?.as_str()?.to_owned(),
        }),
        "start" => {
            let activity_type: ActivityType = val.get("type")?.as_i64()?.try_into().ok()?;
            let fields = val
                .get("fields")
                .map_or(Value::Array(Vec::new()), |x| x.clone());

            let activity = match activity_type {
                ActivityType::Unknown => Activity::Unknown,
                ActivityType::CopyPath => {
                    let (path, from, to) = serde_json::from_value(fields).ok()?;
                    Activity::CopyPath { path, from, to }
                }
                ActivityType::FileTransfer => {
                    let (uri,): (String,) = serde_json::from_value(fields).ok()?;
                    Activity::FileTransfer { uri }
                }
                ActivityType::Realise => Activity::Realise,
                ActivityType::CopyPaths => Activity::CopyPaths,
                ActivityType::Builds => Activity::Builds,
                ActivityType::Build => {
                    let (path, machine, round, total_rounds): (String, String, i64, i64) =
                        serde_json::from_value(fields).ok()?;
                    Activity::Build {
                        path,
                        machine,
                        round,
                        total_rounds,
                    }
                }
                ActivityType::OptimiseStore => Activity::OptimiseStore,
                ActivityType::VerifyPaths => Activity::VerifyPaths,
                ActivityType::Substitute => {
                    let (path, uri): (String, String) = serde_json::from_value(fields).ok()?;
                    Activity::Substitute { path, uri }
                }
                ActivityType::QueryPathInfo => {
                    let (path, uri): (String, String) = serde_json::from_value(fields).ok()?;
                    Activity::QueryPathInfo { path, uri }
                }
                ActivityType::PostBuildHook => {
                    let (path,): (String,) = serde_json::from_value(fields).ok()?;
                    Activity::PostBuildHook { path }
                }
                ActivityType::BuildWaiting => Activity::BuildWaiting,
                ActivityType::FetchTree => Activity::FetchTree,
            };

            Some(Start {
                id: val.get("id")?.as_i64()?,
                level: val.get("level")?.as_i64()?.try_into().ok()?,
                text: val.get("text")?.as_str()?.to_owned(),
                activity,
            })
        }
        "stop" => Some(Stop {
            id: val.get("id")?.as_i64()?,
        }),
        "result" => {
            let result_type: ResultType = val.get("type")?.as_i64()?.try_into().ok()?;
            let fields = val
                .get("fields")
                .map_or(Value::Array(Vec::new()), |x| x.clone());

            let result = match result_type {
                ResultType::FileLinked => {
                    let (blocks, size): (i64, i64) = serde_json::from_value(fields).ok()?;
                    LogResult::FileLinked { blocks, size }
                }
                ResultType::BuildLogLine => {
                    let (line,): (String,) = serde_json::from_value(fields).ok()?;
                    LogResult::BuildLogLine { line }
                }
                ResultType::UntrustedPath => {
                    let (path,): (String,) = serde_json::from_value(fields).ok()?;
                    LogResult::UntrustedPath { path }
                }
                ResultType::CorruptedPath => {
                    let (path,): (String,) = serde_json::from_value(fields).ok()?;
                    LogResult::CorruptedPath { path }
                }
                ResultType::SetPhase => {
                    let (phase,): (String,) = serde_json::from_value(fields).ok()?;
                    LogResult::SetPhase { phase }
                }
                ResultType::Progress => {
                    let (done, expected, running, failed): (i64, i64, i64, i64) =
                        serde_json::from_value(fields).ok()?;
                    LogResult::Progress {
                        done,
                        expected,
                        running,
                        failed,
                    }
                }
                ResultType::SetExpected => {
                    let (activity_type, expected): (i64, i64) =
                        serde_json::from_value(fields).ok()?;
                    let activity_type = activity_type.try_into().ok()?;
                    LogResult::SetExpected {
                        activity_type,
                        expected,
                    }
                }
                ResultType::PostBuildLogLine => {
                    let (line,): (String,) = serde_json::from_value(fields).ok()?;
                    LogResult::PostBuildLogLine { line }
                }
                ResultType::FetchStatus => {
                    let (line,): (String,) = serde_json::from_value(fields).ok()?;
                    LogResult::FetchStatus { line }
                }
            };

            Some(Result {
                id: val.get("id")?.as_i64()?,
                result,
            })
        }
        _ => None,
    }
}

pub fn parse_line(line: &str) -> serde_json::Result<LogItem> {
    use LogItem::*;

    if !line.starts_with("@nix ") {
        Ok(OutputLine(line.to_string()))
    } else {
        let (_, line) = line.split_at("@nix ".len());
        let val = serde_json::from_str::<Value>(line)?;
        Ok(parse_log_item(&val).unwrap_or(UnknownItem(val)))
    }
}
