use directories::BaseDirs;
use log::*;
use std::{
    fs::{self, File},
    io::Read,
    path::{Path, PathBuf},
    process::exit,
    thread::sleep,
    time::Duration,
};

use anyhow::Result;
use lazy_static::lazy_static;
use sysinfo::{Pid, PidExt, ProcessExt, ProcessRefreshKind, RefreshKind, System, SystemExt};

use crate::{build, cli::ControllerCmd};

lazy_static! {
    static ref BASE_DIRS: BaseDirs = BaseDirs::new().unwrap();
    static ref CODCHI_CONFIG_DIR: PathBuf = BASE_DIRS.config_dir().join(build::PROJECT_NAME);
    static ref CODCHI_STATE_DIR: PathBuf = BASE_DIRS.data_local_dir().join(build::PROJECT_NAME);
}

fn get_or_create_dir(path: &Path) {
    match fs::create_dir_all(path) {
        Err(e) => {
            error!("Could not create dir {}: {}", path.display(), e);
            exit(1);
        }
        Ok(()) => {}
    }
}

// pub fn config_dir() -> &'static Path {
//     let p = CODCHI_CONFIG_DIR.as_path();
//     get_or_create_dir(p);
//     p
// }

pub fn state_dir() -> &'static Path {
    let p = CODCHI_STATE_DIR.as_path();
    get_or_create_dir(p);
    p
}

pub fn execute(ctrl_args: &ControllerCmd) -> Result<()> {
    let sys = System::new_with_specifics(
        RefreshKind::new().with_processes(ProcessRefreshKind::everything()),
    );
    let ctrl_proc = {
        let pid_path = state_dir().join("pidfile");
        if !pid_path.exists() {
            None
        } else {
            let mut file = File::open(pid_path)?;
            let mut content = String::new();
            file.read_to_string(&mut content)
                .expect("Can't read pidfile");
            while content.ends_with('\n') || content.ends_with('\r') {
                content.pop();
            }
            let pid = content.parse::<u32>()?;
            sys.process(Pid::from_u32(pid))
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
            start_native(run_in_foreground, || loop {
                error!("Running...");
                sleep(Duration::from_secs(1));
            })
        }
    }
}

#[cfg(target_family = "windows")]
fn start_native(run_in_foreground: &bool, cont: fn() -> Result<()>) -> Result<()> {
    use fs2::FileExt;
    use std::env;
    use std::io::Write;
    use std::os::windows::process::CommandExt;
    use std::process::Command;
    use windows::Win32::System::Threading::*;

    if !run_in_foreground {
        let exe = env::current_exe()?;
        let stdout = File::create(state_dir().join("ctrl.out.log"))?;
        let stderr = File::create(state_dir().join("ctrl.err.log"))?;
        Command::new(exe)
            .args(["controller", "start", "--foreground"])
            .stdout(stdout)
            .stderr(stderr)
            .creation_flags(CREATE_NEW_PROCESS_GROUP.0 | CREATE_NO_WINDOW.0)
            .spawn()?;
        exit(0);
    } else {
        let pid = std::process::id();
        let mut file = File::create(state_dir().join("pidfile"))?;
        file.lock_shared()?;
        file.write_all(format!("{pid}").as_bytes())?;
        cont()?;
        file.unlock()?;
        Ok(())
    }
}

#[cfg(target_family = "unix")]
fn start_native(run_in_foreground: &bool, cont: fn() -> Result<()>) -> Result<()> {
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
