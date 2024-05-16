use std::marker::PhantomData;

use super::*;
use crate::{
    cli::ModuleAttrPath,
    consts::{store, ToPath},
    util::{Empty, Hkd, Optional, Required, StringExt, ToOption},
};
use git_url_parse::{GitUrl, Scheme};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FlakeUrl<W: Hkd> {
    pub location: FlakeLocation,

    pub commit: Option<String>,
    pub r#ref: Option<String>,

    pub flake_attr: W::Hk<ModuleAttrPath>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FlakeLocation {
    Remote {
        scheme: FlakeScheme,
        /// domain:port combination
        host: String,
        repo: String,
        token: Option<String>,
    },
    Local {
        /// relative to /home/codchi, no leading '/' or '~'
        path: String,
    },
}

fn metadata_to_query(metadata: &[(&str, Option<&String>)]) -> String {
    metadata
        .iter()
        .filter_map(|(name, val)| val.as_ref().map(|val| (name, val)))
        .map(|(name, val)| format!("{name}={val}"))
        .join("&")
}

impl<W: Hkd> FlakeUrl<W> {
    /// Serialize to nix readable flake url, ignoring `flake_module` (the part after '#')
    pub fn to_nix_url(&self, machine_name: &str) -> String {
        use FlakeLocation::*;
        use FlakeScheme::*;

        match &self.location {
            Remote {
                scheme,
                host,
                repo,
                token,
            } => {
                let query = metadata_to_query(&[
                    ("commit", self.commit.as_ref()),
                    ("ref", self.r#ref.as_ref()),
                    ("host", Some(host)),
                ]);
                match scheme {
                    Github | Gitlab | Sourcehut => {
                        format!("{scheme}:{repo}?{query}")
                    }
                    Http | Https | Ssh => {
                        let auth = if let Some(token) = &token {
                            format!("{token}@")
                        } else {
                            String::new()
                        };
                        format!("git+{scheme}://{auth}{host}/{repo}?{query}",)
                    }
                }
            }
            Local { path } => {
                let real_path = store::DIR_DATA.join_machine(machine_name).join_str(path).0;
                let query = metadata_to_query(&[
                    ("commit", self.commit.as_ref()),
                    ("ref", self.r#ref.as_ref()),
                ]);
                format!("git+file://{real_path}?{query}")
            }
        }
    }

    pub fn to_git_url(&self) -> GitUrl {
        match &self.location {
            FlakeLocation::Remote {
                scheme,
                host,
                repo,
                token,
            } => {
                let scheme = match scheme {
                    FlakeScheme::Http => Scheme::Http,
                    FlakeScheme::Ssh => Scheme::Ssh,
                    _ => Scheme::Https,
                };
                let (owner, suffixed_name) = repo.split_once('/').unwrap_or(("", repo));
                let (name, git_suffix) = match suffixed_name.strip_suffix(".git") {
                    Some(name) => (name.to_owned(), true),
                    None => (suffixed_name.to_owned(), false),
                };
                let (user, token) = token
                    .as_ref()
                    .map(|token| {
                        token
                            .split_once(':')
                            .map(|(user, pwd)| (Some(user.to_owned()), Some(pwd.to_owned())))
                            .unwrap_or((Some(token.to_owned()), None))
                    })
                    .unwrap_or((None, None));
                GitUrl {
                    host: Some(host.clone()),
                    name,
                    fullname: repo.strip_suffix(".git").unwrap_or(repo).to_string(),
                    owner: owner.to_string().none_if_empty(),
                    scheme,
                    git_suffix,
                    path: format!("/{repo}"),
                    user,
                    token,
                    organization: None,
                    port: None,
                    scheme_prefix: true,
                }
            }
            FlakeLocation::Local { path } => GitUrl {
                path: path.to_string(),
                scheme: Scheme::File,
                name: path.split('/').last().unwrap_or_default().to_string(),
                fullname: path.split('/').last().unwrap_or_default().to_string(),
                ..GitUrl::default()
            },
        }
    }

    pub fn set_attr<X: Hkd>(&self, flake_attr: X::Hk<ModuleAttrPath>) -> FlakeUrl<X> {
        FlakeUrl {
            location: self.location.clone(),
            commit: self.commit.clone(),
            r#ref: self.r#ref.clone(),
            flake_attr,
        }
    }

    pub fn pretty_print(&self) -> String {
        match &self.location {
            FlakeLocation::Remote {
                scheme: _,
                host,
                repo,
                token: _,
            } => format!("{host}/{repo}"),
            FlakeLocation::Local { path } => format!("~/{path}"),
        }
    }
}

impl<W: Hkd> Display for FlakeUrl<W> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.location {
            FlakeLocation::Remote {
                scheme,
                host,
                repo,
                token,
            } => {
                let metadata = metadata_to_query(&[
                    ("token", token.as_ref()),
                    ("commit", self.commit.as_ref()),
                    ("ref", self.r#ref.as_ref()),
                ]);

                write!(
                    f,
                    "{}://{}/{}?{}#{}",
                    scheme,
                    host,
                    repo,
                    metadata,
                    self.flake_attr
                        .clone()
                        .to_option()
                        .map(|fa| fa.to_string())
                        .unwrap_or("".to_owned())
                )
            }
            FlakeLocation::Local { path } => {
                let metadata = metadata_to_query(&[
                    ("commit", self.commit.as_ref()),
                    ("ref", self.r#ref.as_ref()),
                ]);

                write!(
                    f,
                    "file://{path}?{}#{}",
                    metadata,
                    self.flake_attr
                        .clone()
                        .to_option()
                        .map(|fa| fa.to_string())
                        .unwrap_or("".to_owned())
                )
            }
        }
    }
}

impl FromStr for FlakeUrl<Optional> {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let err = |search: &str, missing: &str| -> String {
            format!("Missing '{missing}' in '{search}' when parsing module url '{s}'.")
        };
        let (scheme, s) = s.split_once("://").ok_or_else(|| err(s, "://"))?;

        let (s, flake_attr) = s.split_once('#').ok_or_else(|| err(s, "#"))?;
        let flake_attr = if !flake_attr.is_empty() {
            Some(
                ModuleAttrPath::from_str(flake_attr)
                    .map_err(|err| format!("Failed parsing module url '{s}':\n{err}"))?,
            )
        } else {
            None
        };
        let (s, metadata) = s.split_once('?').ok_or_else(|| err(s, "?"))?;
        let metadata = if !metadata.is_empty() {
            metadata
                .split('&')
                .map(|kv| {
                    kv.split_once('=')
                        .map(|(k, v)| (k.to_string(), v.to_string()))
                        .ok_or_else(|| err(kv, "="))
                })
                .collect::<Result<HashMap<String, String>, String>>()?
        } else {
            HashMap::new()
        };

        let location = if scheme == "file" {
            FlakeLocation::Local { path: s.to_owned() }
        } else {
            let scheme = FlakeScheme::from_str(scheme).map_err(|err| err.to_string())?;
            let (host, repo) = s.split_once('/').ok_or_else(|| err(s, "/"))?;
            FlakeLocation::Remote {
                scheme,
                host: host.to_string(),
                repo: repo.to_string(),
                token: metadata.get("token").cloned(),
            }
        };

        Ok(FlakeUrl {
            location,
            commit: metadata.get("commit").cloned(),
            r#ref: metadata.get("ref").cloned(),
            flake_attr,
        })
    }
}

impl FromStr for FlakeUrl<Required> {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let url = FlakeUrl::<Optional>::from_str(s)?;

        match url.flake_attr.clone().to_option() {
            Some(attr) => Ok(url.set_attr(attr)),
            None => Err(format!("Missing module path in {url}")),
        }
    }
}

impl FromStr for FlakeUrl<Empty> {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let url = FlakeUrl::<Optional>::from_str(s)?;

        if let Some(attr) = &url.flake_attr {
            log::warn!("Ignoring flake attr {attr} in {url}.");
        }
        Ok(url.set_attr(PhantomData))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, strum::Display, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum FlakeScheme {
    Github,
    Gitlab,
    Sourcehut,
    Http,
    Https,
    Ssh,
}

#[cfg(test)]
mod tests {
    use crate::util::Required;

    use super::*;

    fn local() -> FlakeUrl<Required> {
        FlakeUrl {
            location: FlakeLocation::Local {
                path: "docs/codchi".to_owned(),
            },
            commit: None,
            r#ref: None,
            flake_attr: ModuleAttrPath::from_str("nixosModules.base").unwrap(),
        }
    }
    fn remote_gh() -> FlakeUrl<Required> {
        FlakeUrl {
            location: FlakeLocation::Remote {
                scheme: FlakeScheme::Github,
                host: "github.com".to_owned(),
                repo: "aformatik/codchi".to_owned(),
                token: None,
            },
            commit: None,
            r#ref: None,
            flake_attr: ModuleAttrPath::from_str("nixosModules.base").unwrap(),
        }
    }
    fn remote_custom() -> FlakeUrl<Required> {
        FlakeUrl {
            location: FlakeLocation::Remote {
                scheme: FlakeScheme::Https,
                host: "foo.bar".to_owned(),
                repo: "aformatik/codchi.git".to_owned(),
                token: Some("my:token".to_owned()),
            },
            commit: Some("jakfkl2".to_owned()),
            r#ref: Some("my-branch".to_owned()),
            flake_attr: ModuleAttrPath::from_str("nixosModules.base").unwrap(),
        }
    }

    #[test]
    fn ser() {
        assert_eq!(local().to_string(), "file://docs/codchi?#nixosModules.base");
        assert_eq!(
            remote_gh().to_string(),
            "github://github.com/aformatik/codchi?#nixosModules.base"
        );
        assert_eq!(
            remote_custom().to_string(),
            "https://foo.bar/aformatik/codchi.git?token=my:token&commit=jakfkl2&ref=my-branch#nixosModules.base"
        );
    }

    #[test]
    fn to_nix_url() {
        assert_eq!(
            local().to_nix_url("machine_name"),
            "git+file:///data/machine/machine_name/docs/codchi?"
        );
        assert_eq!(
            remote_gh().to_nix_url(""),
            "github:aformatik/codchi?host=github.com"
        );
        assert_eq!(
            remote_custom().to_nix_url(""),
            "git+https://my:token@foo.bar/aformatik/codchi.git?commit=jakfkl2&ref=my-branch&host=foo.bar"
        );
    }

    #[test]
    fn ser_de() {
        for orig in [local(), remote_gh(), remote_custom()] {
            let converted = FlakeUrl::<Required>::from_str(&orig.to_string()).unwrap();
            assert_eq!(orig, converted)
        }
    }

    #[test]
    fn git_url() {
        assert_eq!(local().to_git_url(), GitUrl::parse("docs/codchi").unwrap(),);
        assert_eq!(
            remote_gh().to_git_url(),
            GitUrl::parse("https://github.com/aformatik/codchi").unwrap(),
        );
        assert_eq!(
            remote_custom().to_git_url(),
            GitUrl::parse("https://my:token@foo.bar/aformatik/codchi.git").unwrap(),
        );
    }
}
