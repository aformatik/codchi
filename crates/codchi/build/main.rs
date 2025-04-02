#![deny(unused_crate_dependencies)]

use clap::{CommandFactory, ValueEnum};
use clap_complete::{generate_to, Shell};
use clap_complete_fig::Fig;
use clap_complete_nushell::Nushell;
use cli::Cli;
use embed_manifest::{
    manifest::{ActiveCodePage, DpiAwareness, Setting},
    new_manifest,
};
use std::{
    env,
    error::Error,
    fs::{self},
    path::{Path, PathBuf},
};

mod cli;
mod docs;

pub type Result<A> = core::result::Result<A, Box<dyn Error>>;

fn main() -> Result<()> {
    let out_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR")?)
        .join("..") // we're in a cargo workspace
        .join("target")
        .join("codchi");
    docs::render(&out_dir, Cli::command())?;
    gen_completions(&out_dir)?;

    if std::env::var_os("CARGO_CFG_WINDOWS").is_some() {
        embed_manifest::embed_manifest(
            new_manifest("Codchi")
                .dpi_awareness(DpiAwareness::PerMonitorV2)
                .gdi_scaling(Setting::Enabled)
                .active_code_page(ActiveCodePage::Utf8),
        )
        .expect("unable to embed manifest file");
    }

    {
        if let Ok(wsl_ver_min) = env::var("CODCHI_WSL_VERSION_MIN") {
            println!("cargo:rustc-env=CODCHI_WSL_VERSION_MIN={wsl_ver_min}",);
        }

        if let Ok(wsl_ver_max) = env::var("CODCHI_WSL_VERSION_MAX") {
            println!("cargo:rustc-env=CODCHI_WSL_VERSION_MAX={wsl_ver_max}",);
        }

        if let Ok(profile) = env::var("PROFILE") {
            println!("cargo:rustc-cfg=profile={:?}", profile);
        }
        build_data::no_debug_rebuilds();
    }

    println!("cargo:rerun-if-changed=build/main.rs");
    println!("cargo:rerun-if-changed=build/filter.lua");

    Ok(())
}

fn gen_completions(out_dir: &Path) -> Result<()> {
    let comp_dir = out_dir.join("completions");
    fs::create_dir_all(&comp_dir).unwrap();

    let mut mut_cmd = Cli::command();
    let name = mut_cmd.clone();

    for shell in Shell::value_variants() {
        generate_to(*shell, &mut mut_cmd, name.get_name(), &comp_dir)?;
    }
    generate_to(Nushell, &mut mut_cmd, name.get_name(), &comp_dir)?;
    generate_to(Fig, &mut mut_cmd, name.get_name(), &comp_dir)?;

    Ok(())
}
