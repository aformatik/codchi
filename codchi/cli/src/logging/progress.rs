use crate::util::store_path_base;

use super::nix::{self, Activity, ActivityType, LogItem, LogResult};
use console::style;
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use itertools::Itertools;
use log::Level;
use number_prefix::NumberPrefix;
use std::{borrow::Cow, collections::HashMap, sync::LazyLock, time::Duration};
use throttle::Throttle;

pub static ROOT_BAR: LazyLock<MultiProgress> = LazyLock::new(|| {
    let bar = MultiProgress::new();
    bar.set_move_cursor(true);
    bar
});

#[derive(Debug, Clone)]
pub enum NixActivity {
    Root {
        dl_bytes_expected: u64,
        unpack_bytes_expected: u64,
    },
    // count builds
    BuildRoot {
        done: u64,
        expected: u64,
    },
    Build {
        name: String,
        phase: Option<String>,
    },
    Unpack {
        done: u64,
    },
    Download {
        done: u64,
    },
}

// #[derive(Debug, Clone)]
pub struct Progress {
    activities: HashMap<i64, NixActivity>,
    status_bar: ProgressBar,
    throttle: Throttle,
}

impl Drop for Progress {
    fn drop(&mut self) {
        self.status_bar.finish_and_clear();
        ROOT_BAR.remove(&self.status_bar);
    }
}

impl Progress {
    pub fn new() -> Self {
        let status = ProgressBar::new_spinner().with_style(
            ProgressStyle::with_template("\r{spinner:.cyan} {wide_msg} {prefix:>}")
                .expect("Failed creating indicatif template."),
        );
        let fps = Duration::from_millis(100);
        status.enable_steady_tick(fps);

        Self {
            activities: HashMap::default(),
            status_bar: ROOT_BAR.add(status),
            throttle: Throttle::new(fps, 1),
        }
    }

    pub fn set_status<M>(&self, msg: M)
    where
        M: Into<Cow<'static, str>>,
    {
        self.status_bar.set_message(msg);
    }

    pub fn with_status<M>(self, msg: M) -> Self
    where
        M: Into<Cow<'static, str>>,
    {
        self.status_bar.set_message(msg);
        self
    }

    pub fn log(&mut self, fallback_target: &str, fallback_level: Level, msg: &str) {
        let result = nix::parse_line(msg);
        match result {
            Err(err) => {
                log::error!("Failed parsing log line from nix: {err}. Original line: {msg}")
            }
            Ok(LogItem::OutputLine(line)) => {
                log::log!(target:fallback_target, fallback_level, "{line}");
            }
            Ok(LogItem::UnknownItem(line)) => {
                log::warn!("Unknown message from nix: {}", line);
            }
            Ok(LogItem::Msg { level, msg }) => {
                let msg = msg.lines().join("\r\n");
                log::log!(target: "nix", level.into(), "{msg}")
            }
            Ok(LogItem::Start {
                id,
                level: _,
                text,
                activity,
            }) => {
                match activity {
                    // root activity with total expected dl / unpack
                    Activity::Realise => {
                        self.activities.clear();
                        self.activities.insert(
                            id,
                            NixActivity::Root {
                                dl_bytes_expected: 0,
                                unpack_bytes_expected: 0,
                            },
                        );
                        log::log!(target: "nix", fallback_level, "{text}");
                    }
                    // root activity for build count
                    Activity::Builds => {
                        self.activities.insert(
                            id,
                            NixActivity::BuildRoot {
                                done: 0,
                                expected: 0,
                            },
                        );
                    }
                    // individual build activities
                    Activity::Build { path, .. } => {
                        let name = store_path_base(&path);
                        self.activities
                            .insert(id, NixActivity::Build { name, phase: None });
                        log::log!(target: "nix", fallback_level, "{text}");
                    }
                    // unpack activity count
                    // Activity::CopyPaths => {
                    //     self.activities.insert(id, NixActivity::UnpackRoot(0));
                    // }
                    // individual unpack activities
                    Activity::CopyPath { .. } => {
                        self.activities.insert(id, NixActivity::Unpack { done: 0 });
                        log::log!(target: "nix", fallback_level, "{text}");
                    }
                    // individual download activities
                    Activity::FileTransfer { .. } => {
                        self.activities
                            .insert(id, NixActivity::Download { done: 0 });
                        // log::log!(target: "nix", fallback_level, "{text}");
                    }
                    _else => {}
                }
            }
            Ok(LogItem::Stop { id }) => {
                if let Some(NixActivity::Root { .. }) = self.activities.get(&id) {
                    self.activities.clear();
                };
                // self.activities.remove(&id);
            }
            Ok(LogItem::Result { id, result }) => {
                use LogResult::*;
                if let Some(activity) = self.activities.get_mut(&id) {
                    match result {
                        BuildLogLine { line } => {
                            if let NixActivity::Build { name, phase } = activity {
                                let phase = phase
                                    .to_owned()
                                    .map(|phase| format!(" ({phase})"))
                                    .unwrap_or_default();
                                log::log!(target: "nix", fallback_level, "{name}{phase}> {line}");
                            }
                        }
                        SetExpected {
                            expected,
                            activity_type,
                        } => match activity {
                            NixActivity::Root {
                                dl_bytes_expected,
                                unpack_bytes_expected,
                            } => {
                                if activity_type == ActivityType::FileTransfer {
                                    *dl_bytes_expected = expected as u64;
                                } else if activity_type == ActivityType::CopyPath {
                                    *unpack_bytes_expected = expected as u64;
                                }
                            }
                            NixActivity::BuildRoot {
                                expected: total, ..
                            } => {
                                *total = expected as u64;
                            }
                            _else => {}
                        },
                        SetPhase { phase } => {
                            if let NixActivity::Build {
                                phase: build_phase, ..
                            } = activity
                            {
                                *build_phase = Some(phase)
                            }
                        }
                        Progress {
                            done,
                            expected,
                            running: _,
                            failed: _,
                        } => match activity {
                            NixActivity::BuildRoot {
                                done: done_total,
                                expected: expected_total,
                            } => {
                                *done_total = done as u64;
                                *expected_total = expected as u64;
                            }
                            NixActivity::Unpack { done: done_total } => {
                                *done_total = done as u64;
                            }
                            NixActivity::Download { done: done_total } => {
                                // log::error!("{done}, {done_total}");
                                *done_total = done as u64;
                            }
                            _else => {}
                        },
                        _else => {}
                    }
                }
            }
        }
        if self.throttle.accept().is_ok() {
            self.render();
        }
    }

    pub fn render(&self) {
        let fmt = |prefix: &str, is_bytes: bool, done: u64, expected: u64| {
            if expected == 0 {
                None
            } else if is_bytes {
                let expected_prefix = NumberPrefix::binary(expected as f64);
                let (expected_value, expected_unit) = match expected_prefix {
                    NumberPrefix::Standalone(val) => (val, "".to_string()),
                    NumberPrefix::Prefixed(unit, val) => (val, unit.to_string()),
                };
                let divider = match expected_prefix {
                    NumberPrefix::Standalone(_) => 1,
                    NumberPrefix::Prefixed(prefix, _) => {
                        use number_prefix::Prefix::*;
                        match prefix {
                            Kilo => 1000_i64.pow(1),
                            Mega => 1000_i64.pow(2),
                            Giga => 1000_i64.pow(3),
                            Tera => 1000_i64.pow(4),
                            Peta => 1000_i64.pow(5),
                            Exa => 1000_i64.pow(6),
                            Zetta => 1000_i64.pow(7),
                            Yotta => 1000_i64.pow(8),
                            Kibi => 1024_i64.pow(1),
                            Mebi => 1024_i64.pow(2),
                            Gibi => 1024_i64.pow(3),
                            Tebi => 1024_i64.pow(4),
                            Pebi => 1024_i64.pow(5),
                            Exbi => 1024_i64.pow(6),
                            Zebi => 1024_i64.pow(7),
                            Yobi => 1024_i64.pow(8),
                        }
                    }
                };

                let done_divided = (done as f64) / (divider as f64);

                Some(format!(
                    "{prefix} {}/{expected_value:.1} {expected_unit}B",
                    style(format!("{done_divided:.1}")).green()
                ))
            } else {
                Some(format!("{prefix} {}/{expected}", style(done).green()))
            }
        };

        let this = self;
        // if this.status_bar.is_finished() || this.status_bar.is_hidden() {
        //     continue;
        // }
        let (build_done, build_expected) = this
            .activities
            .iter()
            .filter_map(|(_, a)| match a {
                NixActivity::BuildRoot { done, expected } => Some((done, expected)),
                _else => None,
            })
            .fold((0, 0), |acc, (done, expected)| {
                (done + acc.0, expected + acc.1)
            });

        let prefices = [
            fmt("building", false, build_done, build_expected),
            fmt(
                "unpacking",
                true,
                this.activities
                    .values()
                    .map(|a| match a {
                        NixActivity::Unpack { done } => *done,
                        _else => 0,
                    })
                    .sum(),
                this.activities
                    .values()
                    .map(|a| match a {
                        NixActivity::Root {
                            unpack_bytes_expected,
                            ..
                        } => *unpack_bytes_expected,
                        _else => 0,
                    })
                    .sum(),
            ),
            fmt(
                "downloading",
                true,
                this.activities
                    .values()
                    .map(|a| match a {
                        NixActivity::Download { done } => *done,
                        _else => 0,
                    })
                    .sum(),
                this.activities
                    .values()
                    .map(|a| match a {
                        NixActivity::Root {
                            dl_bytes_expected, ..
                        } => *dl_bytes_expected,
                        _else => 0,
                    })
                    .sum(),
            ),
        ]
        .iter()
        .flatten()
        .join(", ");
        let prefix = if prefices.is_empty() {
            String::new()
        } else {
            format!("{}{prefices}{}", style('[').bold(), style(']').bold())
        };

        this.status_bar.set_prefix(prefix);
        // this.status_bar.tick();
    }
}

impl Default for Progress {
    fn default() -> Self {
        Self::new()
    }
}
