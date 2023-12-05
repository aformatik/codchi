use fs2::FileExt;
use log::*;
use std::{
    fs::{self, File},
    io::Read,
    process::exit,
};

use anyhow::{Context, Result};
use sysinfo::{Pid, PidExt, ProcessExt, ProcessRefreshKind, RefreshKind, System, SystemExt};

use crate::{
    cli::{Cli, ControllerCmd},
    config::state_dir,
};

mod tray;

impl Cli {
    pub fn controller(&self, ctrl_args: &ControllerCmd) -> Result<()> {
        let sys = System::new_with_specifics(
            RefreshKind::new().with_processes(ProcessRefreshKind::everything()),
        );
        let ctrl_proc = {
            let pid_path = state_dir().join("pidfile");
            if !pid_path.exists() {
                None
            } else {
                let mut file = File::open(pid_path.as_path())?;
                let mut pid_content = String::new();
                file.read_to_string(&mut pid_content)
                    .expect("Can't read pidfile");
                let pid_content = pid_content.trim();
                if pid_content.is_empty() {
                    warn!("Found empty pidfile!");
                    drop(file); // close file
                    fs::remove_file(pid_path)?;
                    None
                } else {
                    let pid = pid_content
                        .parse::<u32>()
                        .context("while parsing pidfile")?;
                    match sys.process(Pid::from_u32(pid)) {
                        Some(proc) => {
                            if let Ok(()) = file.try_lock_exclusive() {
                                warn!(
                                "Found running controller from pidfile, but pidfile is not locked!"
                            );
                            }
                            Some(proc)
                        }
                        x => x,
                    }
                }
            }
        };
        match ctrl_args {
            ControllerCmd::Stop {} => match ctrl_proc {
                None => {
                    error!("Codchi controller is not running.");
                    exit(1);
                }
                Some(proc) => {
                    if proc.kill() {
                        info!("Killed running controller");
                        Ok(())
                    } else {
                        error!("Could not kill running controller with pid {}", proc.pid());
                        exit(1);
                    }
                }
            },
            ControllerCmd::Start { run_in_foreground } => {
                if let Some(_) = ctrl_proc {
                    error!("Codchi controller is already running.");
                    exit(1);
                }

                // start_native should daemonize and continue with the main loop. On windows this isn't
                // as eas, so we create a detached process with --foreground and exit
                self.start_native(run_in_foreground, || tray::run())
            }
        }
    }

    #[cfg(target_family = "windows")]
    fn start_native(&self, run_in_foreground: &bool, cont: fn() -> Result<()>) -> Result<()> {
        use crate::cli::*;
        use std::env;
        use std::io::Write;
        use std::os::windows::process::CommandExt;
        use std::process::Command;
        use windows::Win32::System::Threading::*;

        if !run_in_foreground {
            let exe = env::current_exe()?;
            let stdout = File::create(state_dir().join("ctrl.out.log"))?;
            let stderr = File::create(state_dir().join("ctrl.err.log"))?;
            let cli = Cli {
                verbose: self.verbose.clone(),
                command: Cmd::Controller(ControllerCmd::Start {
                    run_in_foreground: true,
                }),
            };
            Command::new(exe)
                .args(cli.to_args())
                .stdout(stdout)
                .stderr(stderr)
                .creation_flags(CREATE_NEW_PROCESS_GROUP.0 | CREATE_NO_WINDOW.0)
                .spawn()?;
            exit(0);
        } else {
            let pid_path = state_dir().join("pidfile");
            let pid = std::process::id();
            {
                let mut file = File::create(pid_path.as_path())?;
                file.write_all(format!("{pid}").as_bytes())?;
            }

            let mut file = File::open(pid_path.as_path())?;
            file.lock_shared()?;
            let pid_from_file = {
                let mut cont = String::new();
                file.read_to_string(&mut cont)?;
                cont.trim().to_owned()
            };

            assert_eq!(pid.to_string(), pid_from_file, "Writing to pidfile failed.");

            // We should still have a shared lock on pidfile, because `file` is not dropped yet
            cont()
        }
    }

    #[cfg(target_family = "unix")]
    fn start_native(&self, run_in_foreground: &bool, cont: fn() -> Result<()>) -> Result<()> {
        use daemonize::Daemonize;
        if !run_in_foreground {
            let stdout = File::create(state_dir().join("ctrl.out.log"))?;
            let stderr = File::create(state_dir().join("ctrl.err.log"))?;
            let daemonize = Daemonize::new()
                .pid_file(state_dir().join("pidfile")) // Every method except `new` and `start`
                .stdout(stdout) // Redirect stdout to `/tmp/daemon.out`.
                .stderr(stderr); // Redirect stderr to `/tmp/daemon.err`.

            if let Err(e) = daemonize.start() {
                error!("Could not daemonize: {}", e);
                exit(1);
            };
        }
        cont()
    }
}
