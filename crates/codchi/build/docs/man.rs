use crate::Result;
use clap_mangen::roff::Roff;
use duct::cmd;
use std::{fs::File, io::Write, path::PathBuf};

pub fn render(man_path: &PathBuf, cmd: &clap::Command) -> Result<()> {
    let mut out = File::create(man_path)?;
    let man = clap_mangen::Man::new(cmd.clone());

    man.render_title(&mut out)?;
    render_name_section(&mut out, cmd)?;
    man.render_synopsis_section(&mut out)?;
    render_description_section(&mut out, cmd)?;

    if app_has_subcommands(cmd) {
        man.render_subcommands_section(&mut out)?;
    }

    if app_has_arguments(cmd) {
        man.render_options_section(&mut out)?;
    }

    if cmd.get_after_long_help().is_some() || cmd.get_after_help().is_some() {
        render_example_section(&mut out, cmd)?;
    }

    // remove double 'v' in `vvX.X.X`
    if app_has_version(cmd) {
        let mut roff = clap_mangen::roff::Roff::default();
        let version = clap_mangen::roff::roman(
            cmd.get_long_version()
                .or_else(|| cmd.get_version())
                .unwrap(),
        );
        roff.control("SH", ["VERSION"]);
        roff.text([version]);
        roff.to_writer(&mut out)?;
    }

    if cmd.get_author().is_some() {
        man.render_authors_section(&mut out)?;
    }

    out.flush()?;
    Ok(())
}

// Does the application have a version?
fn app_has_version(cmd: &clap::Command) -> bool {
    cmd.get_version()
        .or_else(|| cmd.get_long_version())
        .is_some()
}

// Does the application have any command line arguments?
fn app_has_arguments(cmd: &clap::Command) -> bool {
    cmd.get_arguments().any(|i| !i.is_hide_set())
}

// Does the application have any subcommands?
pub fn app_has_subcommands(cmd: &clap::Command) -> bool {
    cmd.get_subcommands().any(|i| !i.is_hide_set())
}

fn render_name_section(out: &mut File, cmd: &clap::Command) -> Result<()> {
    let mut roff = Roff::default();
    roff.control("SH", ["NAME"]);
    roff.to_writer(out)?;

    let name = cmd.get_display_name().unwrap_or_else(|| cmd.get_name());
    let s = match cmd.get_about().or_else(|| cmd.get_long_about()) {
        Some(about) => format!("{} - {}", name, about),
        None => name.to_owned(),
    };

    render_md_to_roff(out, s)?;

    Ok(())
}

fn render_description_section(out: &mut File, cmd: &clap::Command) -> Result<()> {
    if let Some(about) = cmd.get_long_about() {
        let mut roff = Roff::default();
        roff.control("SH", ["DESCRIPTION"]);
        roff.to_writer(out)?;
        render_md_to_roff(out, about.to_string())?;
    }
    Ok(())
}

fn render_example_section(out: &mut File, cmd: &clap::Command) -> Result<()> {
    if let Some(examples) = cmd.get_after_long_help().or_else(|| cmd.get_after_help()) {
        let examples = examples.to_string();
        if !examples.trim_start().starts_with("# EXAMPLES") {
            let mut roff = Roff::default();
            roff.control("SH", ["EXTRA"]);
            roff.to_writer(out)?;
        }
        render_md_to_roff(out, examples)?;
    }
    Ok(())
}

fn render_md_to_roff(out: &mut File, md: String) -> Result<()> {
    let roff = cmd!("pandoc", "-f", "markdown", "-t", "man")
        .stdin_bytes(md)
        .stdout_capture()
        .read()?;
    writeln!(out, "{roff}")?;
    Ok(())
}
