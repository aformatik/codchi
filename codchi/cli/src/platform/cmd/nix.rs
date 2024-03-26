use self::platform::LinuxCommandDriver;
use super::*;
use crate::cli::ModuleAttrPath;
use serde_json::Value;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Nix eval didn't find attribute.")]
    EvalMissingAttr,

    // TODO: link to docs
    #[error("SSL peer certificate or SSH remote key was not OK.")]
    InvalidRemoteSSLOrSSH,

    #[error("Couldn't access repository. If the repository is private you need to provide the correct credentials.
See <https://codchi.dev/docs/start/usage.html#creating-a-machine> on how to access private repositories.")]
    InvalidURLOrCredentials,

    #[error("Nix command failed: {0}")]
    Command(super::cmd::Error),
}
pub type Result<T> = std::result::Result<T, Error>;

impl From<cmd::Error> for Error {
    fn from(err: cmd::Error) -> Self {
        if let cmd::Error::Other { stderr, .. } = &err {
            if stderr.contains("SSL peer certificate or SSH remote key was not OK") {
                Error::InvalidRemoteSSLOrSSH
            } else if
            // http / ssh
            stderr.contains("program 'git' failed with exit code 128")
            // gitlab / github
            || stderr.contains("HTTP error 404")
            // srht
            || stderr.contains("HTTP error 403")
            {
                Error::InvalidURLOrCredentials
            } else if stderr.contains("does not provide attribute") {
                Error::EvalMissingAttr
            } else {
                Error::Command(err)
            }
        } else {
            Error::Command(err)
        }
    }
}

pub trait NixDriver: LinuxCommandTarget {
    fn list_nixos_modules(&self, url: &str) -> Result<Vec<ModuleAttrPath>> {
        let list_attr_names = |attr_path: &str| -> Result<Vec<String>> {
            let args = [
                "eval",
                "--json",
                "--quiet",
                "--quiet",
                &self.quote_shell_arg(&format!("{}#{}", url, attr_path)),
                "--apply",
                "builtins.attrNames",
            ];
            match self
                .run("nix", &args)
                .output_json::<Vec<String>>()
                .map_err(|err| err.into())
            {
                Ok(attrs) => Ok(attrs),
                Err(Error::EvalMissingAttr) => Ok(Vec::new()),
                Err(err) => Err(err),
            }
        };
        let modules = list_attr_names("codchiModules")?
            .iter()
            .map(|module| ModuleAttrPath {
                base: "codchiModules".to_string(),
                module: module.to_string(),
            })
            .chain(
                list_attr_names("nixosModules")?
                    .iter()
                    .map(|module| ModuleAttrPath {
                        base: "nixosModules".to_string(),
                        module: module.to_string(),
                    }),
            )
            .collect();
        Ok(modules)
    }

    fn has_nixpkgs_input(&self, url: &str) -> Result<bool> {
        let args = [
            "flake",
            "metadata",
            "--json",
            "--no-write-lock-file",
            &self.quote_shell_arg(url),
        ];
        let metadata = self.run("nix", &args).output_json::<Value>()?;

        Ok(metadata
            .get("locks")
            .and_then(|value| value.get("nodes"))
            .and_then(|value| value.get("nixpkgs"))
            .is_some())
    }
}

impl NixDriver for LinuxCommandDriver {}
