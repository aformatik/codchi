use ipc::logging::nix::{LogItem, Verbosity};
use ipc::logging::{LogLevel, LogLine, LogMessage, LogTopic};
use serde_json::Value;

mod nix;

pub fn parse_container_log(
    fallback_level: LogLevel,
    fallback_topic: LogTopic,
    line: String,
) -> LogLine {
    let parsed_line = if line.starts_with("@nix ") {
        let topic = LogTopic::Nix;
        let (_, line) = line.split_at("@nix ".len());
        let item_result = serde_json::from_str::<Value>(line).map(|val| nix::parse_log_item(&val));
        match item_result {
            Ok(Some(item)) => Some(LogLine {
                topic,
                level: match item {
                    LogItem::Msg { level, .. } => level.into(),
                    // ignore nonsense Error verbosity for LogItem::Start
                    LogItem::Start { level: Verbosity::Error, .. } => fallback_level.clone(),
                    LogItem::Start { level, .. } => level.into(),
                    _ => fallback_level.clone(),
                },
                msg: LogMessage::Nix(item),
            }),
            other => {
                tracing::error!("Failed parsing nix log line: {line}. Result: {other:?}");
                None
            }
        }
    } else {
        None
    };

    parsed_line.unwrap_or_else(|| LogLine {
        topic: fallback_topic,
        level: fallback_level,
        msg: LogMessage::Text(line),
    })
}
