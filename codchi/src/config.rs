use crate::consts::Dir;
use crate::util::Finally;
use anyhow::Context;
use anyhow::Result;
use fs4::FileExt;
use itertools::Itertools;
use serde::Deserialize;
use serde::Serialize;
use serde_with::serde_as;
use serde_with::DisplayFromStr;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::fs;
use std::io::Read;
use std::io::Seek;
use std::io::Write;
use std::str::FromStr;
use strum::EnumString;
use toml_edit::ser::to_document;
use toml_edit::table;

use self::flake_attr::Type;

pub struct MutableConfig {
    pub doc: toml_edit::Document,
    file: fs::File,
}

impl MutableConfig {
    pub fn open() -> Result<Self> {
        let path = Dir::Config.get_or_create()?.join("config.toml");
        let mut file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .open(&path)?;
        file.lock_exclusive()?;
        let doc = {
            let size = file.metadata().map(|m| m.len() as usize).ok();
            let mut content = String::with_capacity(size.unwrap_or(0));
            file.read_to_string(&mut content)?;
            file.rewind()?;
            if content.is_empty() {
                Ok(to_document(&CodchiConfig::default())?)
            } else {
                toml_edit::Document::from_str(&content)
                    .with_context(|| format!("Failed parsing config at {path:?}"))
            }
        }
        .map_err(|err| {
            file.unlock().expect("Failed unlocking config.toml.");
            err
        })?;

        Ok(Self { doc, file })
    }

    pub fn write(mut self) -> Result<()> {
        let res = self.doc.to_string();
        self.file.write_all(res.as_bytes())?;
        drop(self);
        Ok(())
    }

    pub fn get_machines(&mut self) -> &mut toml_edit::Table {
        if !self.doc.contains_table("machines") {
            self.doc["machines"] = table();
        }
        let table = self.doc["machines"]
            .as_table_mut()
            .expect("Config toml doesn't contain key 'machines'");
        table.set_implicit(true);
        table
    }
}

impl Drop for MutableConfig {
    fn drop(&mut self) {
        self.file.unlock().expect("Failed unlocking config.toml.");
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, Default)]
pub struct CodchiConfig {
    pub machines: HashMap<String, CodeMachine>,
}

impl CodchiConfig {
    pub fn read_config() -> Result<Self> {
        let path = Dir::Config.get_or_create()?.join("config.toml");

        if fs::metadata(&path).is_ok() {
            let mut file = fs::File::open(&path)?;
            {
                let size = file.metadata().map(|m| m.len() as usize).ok();
                let mut content = String::with_capacity(size.unwrap_or(0));
                file.read_to_string(&mut content)?;
                toml::from_str(&content)
                    .with_context(|| format!("Failed parsing config at {path:?}"))
            }
            .finally(|| file.unlock().expect("Failed unlocking config file."))
        } else {
            Ok(Self::default())
        }
    }
}

#[serde_as]
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CodeMachine {
    #[serde(skip_serializing_if = "Option::is_none")]
    #[serde_as(as = "Option<DisplayFromStr>")]
    pub nixpkgs_from: Option<FlakeUrl<flake_attr::Without>>,
    #[serde_as(as = "Vec<DisplayFromStr>")]
    pub modules: Vec<CodchiModule>,
}

pub type CodchiModule = FlakeUrl<flake_attr::With>;

// impl FromStr for CodchiModule {
//     type Err = String;

//     fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
//         let url = FlakeUrl::from_str(s)?;
//         let flake_module = url
//             .flake_attr
//             .as_ref()
//             .ok_or_else(|| format!("Missing flake module"))
//             .and_then(|s| ModuleAttrPath::from_str(s))?;

//         Ok(Self { url, flake_module })
//     }
// }

// impl Display for CodchiModule {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         let mut url = self.url.clone();
//         url.flake_attr = Some(self.flake_module.to_string());
//         url.fmt(f)
//     }
// }

pub mod flake_attr {
    pub trait Type {
        type I;
        fn new(inner: Self::I) -> Self;
        fn to_option(&self) -> Option<String>;
    }
    #[derive(Debug, Clone)]
    pub struct With(pub String);
    impl Type for With {
        type I = String;
        fn new(inner: Self::I) -> Self {
            Self(inner)
        }
        fn to_option(&self) -> Option<String> {
            Some(self.0.clone())
        }
    }
    #[derive(Debug, Clone)]
    pub struct Optional(pub Option<String>);
    impl Type for Optional {
        type I = Option<String>;
        fn new(inner: Self::I) -> Self {
            Self(inner)
        }
        fn to_option(&self) -> Option<String> {
            self.0.clone()
        }
    }
    #[derive(Debug, Clone)]
    pub struct Without;
    impl Type for Without {
        type I = ();
        fn new(_: Self::I) -> Self {
            Self
        }
        fn to_option(&self) -> Option<String> {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct FlakeUrl<Attr: flake_attr::Type> {
    pub scheme: FlakeScheme,
    /// domain:port combination
    pub host: String,
    pub repo: String,

    pub token: Option<String>,
    pub commit: Option<String>,
    pub r#ref: Option<String>,

    pub flake_attr: Attr,
}

impl<Attr: flake_attr::Type> FlakeUrl<Attr> {
    /// Serialize to nix readable flake url, ignoring `flake_module` (the part after '#')
    pub fn to_nix_url(&self) -> String {
        use FlakeScheme::*;

        let query = vec![
            ("commit", self.commit.as_ref()),
            ("ref", self.r#ref.as_ref()),
            ("host", Some(&self.host)),
        ]
        .iter()
        .filter_map(|(name, val)| {
            if let Some(val) = val {
                Some((name, val))
            } else {
                None
            }
        })
        .map(|(name, val)| format!("{name}={val}"))
        .join("&");

        match self.scheme {
            Github | Gitlab | Sourcehut => format!("{}:{}?{}", self.scheme, self.repo, query),
            Http | Https | Ssh => {
                let auth = if let Some(token) = &self.token {
                    format!("{token}@")
                } else {
                    String::new()
                };
                format!(
                    "git+{}://{}{}/{}?{}",
                    self.scheme, auth, self.host, self.repo, query
                )
            }
        }
    }

    pub fn with_attr<Type: flake_attr::Type>(&self, attr: Type::I) -> FlakeUrl<Type> {
        FlakeUrl {
            flake_attr: flake_attr::Type::new(attr),
            scheme: self.scheme.clone(),
            host: self.host.clone(),
            repo: self.repo.clone(),
            token: self.token.clone(),
            commit: self.commit.clone(),
            r#ref: self.r#ref.clone(),
        }
    }

    pub fn pretty_print(&self) -> String {
        format!("{}/{}", self.host, self.repo)
    }
}

impl<Attr: flake_attr::Type> Display for FlakeUrl<Attr> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let metadata = vec![
            ("token", self.token.as_ref()),
            ("commit", self.commit.as_ref()),
            ("ref", self.r#ref.as_ref()),
        ]
        .iter()
        .filter_map(|(name, val)| {
            if let Some(val) = val {
                Some((name, val))
            } else {
                None
            }
        })
        .map(|(name, val)| format!("{name}={val}"))
        .join("&");

        write!(
            f,
            "{}://{}/{}?{}#{}",
            self.scheme,
            self.host,
            self.repo,
            metadata,
            self.flake_attr
                .to_option()
                .clone()
                .unwrap_or_else(String::new)
        )
    }
}

impl FromStr for FlakeUrl<flake_attr::Optional> {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (scheme, s) = s
            .split_once("://")
            .ok_or("Missing '://' in module url.me")?;
        let scheme = FlakeScheme::from_str(scheme).map_err(|err| err.to_string())?;

        let (host, s) = s.split_once("/").ok_or("Missing '/' in module url.")?;

        let (repo, s) = s.split_once("?").ok_or("Missing '?' in module url.")?;

        let (metadata, flake_attr) = s.split_once("#").ok_or("Missing '#' in module url.")?;
        let flake_attr = if !flake_attr.is_empty() {
            Some(flake_attr.to_string())
        } else {
            None
        };
        let metadata = metadata
            .split("&")
            .map(|kv| {
                kv.split_once('=')
                    .map(|(k, v)| (k.to_string(), v.to_string()))
                    .ok_or_else(|| format!("Missing '=' in {kv}"))
            })
            .collect::<Result<HashMap<String, String>, String>>()?;

        Ok(FlakeUrl {
            scheme,
            host: host.to_string(),
            repo: repo.to_string(),
            token: metadata.get("token").cloned(),
            commit: metadata.get("commit").cloned(),
            r#ref: metadata.get("ref").cloned(),
            flake_attr: flake_attr::Optional(flake_attr),
        })
    }
}

impl FromStr for FlakeUrl<flake_attr::With> {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let url = FlakeUrl::<flake_attr::Optional>::from_str(s)?;

        match url.flake_attr.to_option() {
            Some(attr) => Ok(url.with_attr(attr)),
            None => Err(format!("Missing module path in {url}")),
        }
    }
}

impl FromStr for FlakeUrl<flake_attr::Without> {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let url = FlakeUrl::<flake_attr::Optional>::from_str(s)?;

        if let Some(attr) = url.flake_attr.to_option() {
            log::warn!("Ignoring flake attr {attr} in {url}.");
        }
        Ok(url.with_attr(()))
    }
}

#[derive(Debug, Clone, PartialEq, strum::Display, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum FlakeScheme {
    Github,
    Gitlab,
    Sourcehut,
    Http,
    Https,
    Ssh,
}
