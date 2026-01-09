use ipc::logging::nix::{Activity, ActivityType, LogItem, LogResult, ResultType};
use serde_json::Value;

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
                result_type => unreachable!(
                    "Help! An unknown nix log result type has appeared: {:?}",
                    result_type
                ),
            };

            Some(ipc::logging::nix::LogItem::Result {
                id: val.get("id")?.as_i64()?,
                result,
            })
        }
        _ => None,
    }
}

