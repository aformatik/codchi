pub mod nix;

use remoc::rtc::{Deserialize, Serialize};
use tracing::Level;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LogLine {
    pub level: LogLevel,
    pub topic: LogTopic,
    pub msg: LogMessage,
}

// impl Display for LogTopic {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         f.write_str(&self.to_string())
//     }
// }

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum LogLevel {
    Error,
    Warning,
    Info,
    Debug,
    Trace,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogTopic {
    Nix,
    StoreContainer,
    MachineContainer { name: String },
    Server,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LogMessage {
    Nix(nix::LogItem),
    Text(String),
}

// impl Into<Level> for LogLevel {
//     fn into(self) -> Level {
//         match self {
//             LogLevel::Error => Level::Error,
//             LogLevel::Warning => Level::Warn,
//             LogLevel::Info => Level::Info,
//             LogLevel::Debug => Level::Debug,
//             LogLevel::Trace => Level::Trace,
//         }
//     }
// }
//
// impl From<Level> for LogLevel {
//     fn from(value: Level) -> Self {
//         match value {
//             Level::Error => LogLevel::Error,
//             Level::Warn => LogLevel::Warning,
//             Level::Info => LogLevel::Info,
//             Level::Debug => LogLevel::Debug,
//             Level::Trace => LogLevel::Trace,
//         }
//     }
// }
//
impl Into<Level> for LogLevel {
    fn into(self) -> Level {
        match self {
            LogLevel::Error => Level::ERROR,
            LogLevel::Warning => Level::WARN,
            LogLevel::Info => Level::INFO,
            LogLevel::Debug => Level::DEBUG,
            LogLevel::Trace => Level::TRACE,
        }
    }
}
