use std::{fmt::Display, str::FromStr};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ModuleAttrPath {
    /// codchiModules or nixosModules
    pub base: String,
    pub module: String,
}

impl Display for ModuleAttrPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.base, self.module)
    }
}

impl FromStr for ModuleAttrPath {
    type Err = String;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        match lazy_regex::regex_captures!(
            r"^(nixosModules|codchiModules)\.([a-zA-Z\_][a-zA-Z0-9\_\'\-]*)$",
            string
        ) {
            Some((_, base, module)) => Ok(ModuleAttrPath {
                base: base.to_string(),
                module: module.to_string(),
            }),
            None => Err("Please refer to a NixOS module as 'codchiModules.<name>' \
                    or 'nixosModules.<name>'"
                .to_string()),
        }
    }
}
