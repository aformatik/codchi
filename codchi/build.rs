use std::{error::Error, fs, path::PathBuf};

use clap::CommandFactory;
use clap_complete::{generate_to, Shell::*};

#[path = "src/cli.rs"]
mod cli;

fn main() -> Result<(), Box<dyn Error>> {
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());

    let man_dir = out_dir.join("man");
    fs::create_dir_all(&man_dir).unwrap();

    let cmd = cli::Cli::command();
    let name = cmd.get_name();

    let man = clap_mangen::Man::new(cmd.to_owned());
    let mut buffer: Vec<u8> = Default::default();
    man.render(&mut buffer).expect("Man page generation failed");
    fs::write(man_dir.join(format!("{}.1", name)), buffer).expect("Failed to write man page");

    let comp_dir = out_dir.join("completions");
    fs::create_dir_all(&comp_dir).unwrap();

    let mut mut_cmd = cmd.clone();

    for shell in [Bash, Elvish, Fish, PowerShell, Zsh] {
        generate_to(shell, &mut mut_cmd, name, &comp_dir).unwrap();
    }

    {
        shadow_rs::new()?;
    }

    #[cfg(target_os = "linux")]
    {}

    Ok(())
}
