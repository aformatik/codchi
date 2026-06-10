use std::{fs, path::Path};

mod man;
mod md;

use crate::Result;

pub fn render(out_dir: &Path, mut cmd: clap::Command) -> Result<()> {
    let man_dir = out_dir.join("man");
    fs::create_dir_all(&man_dir)?;

    let md_dir = out_dir.join("md");
    fs::create_dir_all(&md_dir)?;

    cmd.build();
    render_man_md(&man_dir, &md_dir, &cmd)
}

fn render_man_md(man_dir: &Path, md_dir: &Path, cmd: &clap::Command) -> Result<()> {
    // `get_display_name()` is `Some` for all instances, except the root.
    let name = cmd.get_display_name().unwrap_or_else(|| cmd.get_name());
    let man_path = man_dir.join(format!("{name}.1"));
    man::render(&man_path, cmd)?;

    let md_dir = if man::app_has_subcommands(cmd) {
        let md_dir_sub = md_dir.join(cmd.get_name());
        fs::create_dir_all(&md_dir_sub)?;

        for sub in cmd.get_subcommands().filter(|sub| !sub.is_hide_set()) {
            render_man_md(man_dir, &md_dir_sub, sub)?;
        }
        md_dir_sub
    } else {
        md_dir.to_path_buf()
    };
    let md_path = md_dir.join(format!("{}.md", cmd.get_name()));
    md::render(md_path, &man_path, cmd)?;

    Ok(())
}
