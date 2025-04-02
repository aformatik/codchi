use std::{env, path::PathBuf, process::Command};

#[derive(Debug, Clone)]
pub enum CodchiExe {
    Cli,
    Server,
    GUI,
    /// codchi cli compiled with windows subsystem to open graphical apps without console window
    #[cfg(target_os = "windows")]
    Codchiw,
}

impl CodchiExe {
    pub fn get_path(&self) -> anyhow::Result<PathBuf> {
        let cur_exe = env::current_exe()?;
        let cur_exe_dir = cur_exe
            .parent()
            .ok_or(anyhow::anyhow!("This executable has no parent dir."))?;

        #[cfg(target_os = "windows")]
        let cur_exe_dir = if matches!(self, CodchiExe::Cli | CodchiExe::Codchiw) {
            get_known_folder_path(KnownFolder::LocalAppData)
                .expect("FOLDERID_LocalAppData missing")
                .join("Microsoft")
                .join("WindowsApps")
        } else {
            cur_exe_dir
        };
        let exe = match self {
            CodchiExe::Cli => "codchi",
            CodchiExe::Server => "codchi-server",
            CodchiExe::GUI => "codchi-gui",
            #[cfg(target_os = "windows")]
            CodchiExe::Codchiw => "codchiw",
        };

        #[cfg(target_os = "windows")]
        let exe = format!("{exe}.exe");

        Ok(cur_exe_dir.join(exe))
    }
}

pub trait CommandExt {
    fn spawn_daemonized(&mut self) -> anyhow::Result<()>;
}

impl CommandExt for Command {
    fn spawn_daemonized(&mut self) -> anyhow::Result<()> {
        #[cfg(target_os = "windows")]
        {
            use std::os::windows::process::CommandExt;
            use windows::Win32::System::Threading::*;
            self.creation_flags(CREATE_NEW_PROCESS_GROUP.0 | CREATE_NO_WINDOW.0)
                .spawn()?;
            Ok(())
        }

        #[cfg(target_family = "unix")]
        {
            use daemonize::{Daemonize, Outcome};
            match Daemonize::new().execute() {
                Outcome::Parent(Ok(_)) => Ok(()),
                Outcome::Parent(Err(err)) => {
                    anyhow::bail!("Failed to daemonize (in parent): {err}")
                }
                Outcome::Child(Err(err)) => anyhow::bail!("Failed to daemonize (in child): {err}"),
                Outcome::Child(Ok(_)) => {
                    self.spawn()?;
                    std::process::exit(0)
                }
            }
        }
    }
}
