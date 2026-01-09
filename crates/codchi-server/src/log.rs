use crate::server::GLOBAL_LOGGER;
use ipc::logging::{LogLevel, LogLine};
use shared::util::{ResultExt, UtilExt};

pub fn codchi_log(line: LogLine) {
    let topic = format!("{:?}", line.topic);
    let message = line.msg.clone();
    match line.level {
        LogLevel::Error => tracing::error!(codchi_topic = topic, "{message:?}"),
        LogLevel::Warning => tracing::warn!(codchi_topic = topic, "{message:?}"),
        LogLevel::Info => tracing::info!(codchi_topic = topic, "{message:?}"),
        LogLevel::Debug => tracing::debug!(codchi_topic = topic, "{message:?}"),
        LogLevel::Trace => tracing::trace!(codchi_topic = topic, "{message:?}"),
    }
    if let Some(log) = GLOBAL_LOGGER.get() {
        // if log.receiver_count() > 0 {
        log.send(line)
            // .trace_err("Failed sending log line to global logger")
            .ignore();
        // }
    } else {
        tracing::error!("Failed to log {line:?}. GLOBAL_LOGGER is not initialized yet.")
    }
}

/// Calls tracing::$level() and also broadcasts it to potentially listening clients
#[macro_export]
macro_rules! codchi_log {
    ($level:ident, $topic:ident, $($arg:tt)*) => {
        crate::log::codchi_log(LogLine {
            level: ipc::logging::LogLevel::$level,
            topic: ipc::logging::LogTopic::$topic,
            msg: ipc::logging::LogMessage::Text(format!($($arg)*)),
        })
    };
}
