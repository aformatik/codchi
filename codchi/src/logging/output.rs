use std::{fmt::Display, io::stdout};

use itertools::Itertools;
use serde::Serialize;
use crate::config::{MachineModules, MachineStatus, Mod, ModLsOutput, StatusOutput};

use crate::platform::{ConfigStatus, Machine};

pub trait CodchiOutput<A: Serialize> {
    fn to_output(&self) -> A;
    fn human_output(x: A) -> impl Display;
    fn print(&self, json: bool) {
        let output = self.to_output();
        if json {
            serde_json::to_writer(stdout(), &output).expect("Failed serializing to JSON...")
        } else {
            println!("{}", Self::human_output(self.to_output()))
        }
    }
}

impl CodchiOutput<StatusOutput> for Vec<Machine> {
    fn to_output(&self) -> StatusOutput {
        self.iter()
            .map(|m| MachineStatus {
                name: m.config.name.clone(),
                status: m.config_status.clone(),
                running: m.platform_status == crate::platform::PlatformStatus::Running,
            })
            .collect()
    }

    fn human_output(out: StatusOutput) -> impl Display {
        use comfy_table::*;
        let mut table = Table::new();
        table.load_preset(presets::UTF8_FULL).set_header(vec![
            Cell::new("Machine"),
            Cell::new("Status"),
            Cell::new("Running?"),
        ]);

        for machine in out.iter() {
            table.add_row(vec![
                Cell::new(&machine.name),
                match machine.status {
                    ConfigStatus::NotInstalled => Cell::new("Not installed yet").fg(Color::Red),
                    ConfigStatus::Modified => Cell::new("Modified").fg(Color::Yellow),
                    ConfigStatus::UpdatesAvailable => {
                        Cell::new("Updates available").fg(Color::Yellow)
                    }
                    ConfigStatus::UpToDate => Cell::new("Up to date").fg(Color::Green),
                },
                Cell::new(if machine.running { "✅" } else { "❌" }),
            ]);
        }

        table
    }
}

impl CodchiOutput<ModLsOutput> for MachineModules {
    fn to_output(&self) -> ModLsOutput {
        self.iter()
            .map(|(name, module)| Mod {
                name: name.to_string(),
                url: module.pretty_print(),
                flake_module: module.flake_attr.to_string(),
            })
            .sorted_by(|a, b| a.name.cmp(&b.name))
            .collect()
    }
    fn human_output(out: ModLsOutput) -> impl Display {
        use comfy_table::*;

        let mut table = Table::new();
        table.load_preset(presets::UTF8_FULL).set_header(vec![
            Cell::new("Name"),
            Cell::new("Url"),
            Cell::new("Flake Module"),
        ]);

        // TODO add protocol column if SSH is added

        for m in out {
            table.add_row(vec![
                Cell::new(&m.name),
                Cell::new(&m.url),
                Cell::new(&m.flake_module),
            ]);
        }
        table
    }
}
