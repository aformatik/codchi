use crate::{
    cli::AddModuleOptions,
    config::{flake_attr, CodchiModule, CodeMachine, FlakeScheme, FlakeUrl, MutableConfig},
    platform::{nix::NixDriver, *},
    util::Finally,
};
use anyhow::{bail, Context, Result};
use git_url_parse::Scheme;
use spinoff::{spinners, Color, Spinner};
use std::fmt::Display;
use toml_edit::{ser::to_document, Key};

pub fn init_flow(empty: bool, opts: &AddModuleOptions) -> Result<()> {
    let mut cfg = MutableConfig::open()?;
    let machines = cfg.get_machines();

    if machines.contains_key(&opts.name) {
        bail!("A code machine with name '{}' already exists.", opts.name)
    }

    let machine = if empty {
        CodeMachine {
            nixpkgs_from: None,
            modules: Vec::new(),
        }
    } else {
        let (module, has_nixpkgs) = fetch_module_flow(&opts)?;
        let bare_url = module.with_attr(());
        let nixpkgs_from = if has_nixpkgs {
            if opts.accept_defaults {
                Some(bare_url)
            } else {
                // TODO move to docs?
                eprintln!("This codchi module has a nixpkgs input. Do you want to use it for the code machine? Otherwise the shared nixpkgs of codchi is used, which might decrease reproducibility but is faster.");
                if inquire::Confirm::new(&format!("Use nixpkgs from '{}'", bare_url.pretty_print()))
                    .prompt()?
                {
                    Some(bare_url)
                } else {
                    None
                }
            }
        } else {
            None
        };
        CodeMachine {
            nixpkgs_from,
            modules: vec![module],
        }
    };

    machines.insert_formatted(
        &Key::new(&opts.name),
        to_document(&machine)?.as_item().clone(),
    );
    cfg.write()?;

    Ok(())
}

type HasNixpkgs = bool;
pub fn fetch_module_flow(opts: &AddModuleOptions) -> Result<(CodchiModule, HasNixpkgs)> {
    if opts.accept_defaults && opts.module_path.is_none() {
        bail!("Please provide MODULE_PATH in non interactive mode.");
    }

    let flake_url = inquire_module_url(&opts)?;
    let nix_url = flake_url.to_nix_url();

    let mut spinner = Spinner::new_with_stream(
        spinners::Dots,
        "Fetching available modules...",
        Color::Blue,
        spinoff::Streams::Stderr,
    );
    let available_modules = DRIVER
        .list_nixos_modules(&nix_url)
        .finally(|| spinner.clear())?;

    if available_modules.is_empty() {
        bail!("The repository at '{nix_url}' contains no modules.");
    }

    let flake_module = match opts.module_path.clone() {
        Some(module) => {
            if !available_modules.contains(&module) {
                bail!(
                    "Module {module} does not exist. These are available: {}",
                    available_modules
                        .iter()
                        .map(|m| m.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                );
            }
            module
        }
        None => {
            // non interactive mode is checked above
            eprintln!("Which module would you like to use?");
            inquire::Select::new("Module:", available_modules).prompt()?
        }
    };

    let has_nixpkgs = DRIVER.has_nixpkgs_input(&nix_url)?;
    // TODO check capabilities / secrets
    Ok((flake_url.with_attr(flake_module.to_string()), has_nixpkgs))
}

fn inquire_module_url(opts: &AddModuleOptions) -> Result<FlakeUrl<flake_attr::Without>> {
    struct FS(FlakeScheme);

    #[derive(Debug, Clone, PartialEq)]
    enum Guess {
        Nothing,
        Maybe(FlakeScheme),
        Sure(FlakeScheme),
    }

    impl Display for FS {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use FlakeScheme::*;
            match &self.0 {
                Http | Https | Ssh => write!(f, "Other (use http(s))"),
                scheme => write!(f, "{}", {
                    let str = scheme.to_string();
                    let mut c = str.chars();
                    match c.next() {
                        None => String::new(),
                        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
                    }
                }),
            }
        }
    }

    fn guess_scheme(host: &String) -> Guess {
        if host == "github.com" {
            Guess::Sure(FlakeScheme::Github)
        } else if host == "gitlab.com" {
            Guess::Sure(FlakeScheme::Gitlab)
        } else if host == "git.sr.ht" {
            Guess::Sure(FlakeScheme::Sourcehut)
        } else if host.contains("github") {
            Guess::Maybe(FlakeScheme::Github)
        } else if host.contains("gitlab") {
            Guess::Maybe(FlakeScheme::Gitlab)
        } else {
            Guess::Nothing
        }
    }

    match opts.url.scheme {
        Scheme::Http | Scheme::Https => {
            let host = opts.url.host.clone().context("Host missing.")?;
            let fallback_scheme = match opts.url.scheme {
                Scheme::Http => FlakeScheme::Http,
                Scheme::Https => FlakeScheme::Https,
                // Scheme::Ssh => todo!(),
                _ => unreachable!(),
            };

            // Token in URL / port only work with git+http(s)
            let (host, scheme) = if let Some(port) = opts.url.port {
                (format!("{host}:{port}"), fallback_scheme)
            } else if opts.token.is_some() {
                (host, fallback_scheme)
            } else {
                let guess = guess_scheme(&host);
                if let Guess::Sure(scheme) = guess {
                    (host, scheme)
                } else if !opts.accept_defaults {
                    // only prompt user in interactive mode
                    eprintln!("Please select the type of code forge where this repository is hosted. This will speed up subsequent commands!");
                    let select = inquire::Select::new(
                        "Code forge:",
                        vec![
                            FS(FlakeScheme::Github),
                            FS(FlakeScheme::Gitlab),
                            FS(FlakeScheme::Sourcehut),
                            FS(fallback_scheme),
                        ],
                    );
                    let idx = if let Guess::Maybe(scheme) = guess {
                        select
                            .options
                            .iter()
                            .position(|s| s.0 == scheme)
                            .unwrap_or(0)
                    } else {
                        0
                    };
                    (host, select.with_starting_cursor(idx).prompt()?.0)
                } else {
                    (host, fallback_scheme)
                }
            };

            let repo = if opts.url.git_suffix {
                format!("{}.git", opts.url.path.trim_matches('/'))
            } else {
                opts.url.path.trim_matches('/').to_string()
            };

            Ok(FlakeUrl {
                scheme,
                host,
                repo,
                token: opts.token.clone(),
                commit: opts.commit.clone(),
                r#ref: opts.branch.as_ref().or(opts.tag.as_ref()).cloned(),
                flake_attr: flake_attr::Without,
            })
        }
        // Scheme::Ssh => {
        //     if opts.token.is_some() {
        //         Err(anyhow!("Token is not supported with SSH."))?;
        //     }
        //     let user = url.user.map_or_else(String::new, |user| format!("{user}@"));
        //     let port = url.port.map_or_else(String::new, |port| format!(":{port}"));
        //     let path = url.path;
        //     Ok(format!("git+ssh://{user}{host}{port}/{path}?{reff}{commit}",))
        // }
        // TODO move / refer to docs
        _ => bail!("Currently only http(s) urls are supported."),
        //            "This type of link is currently not supported. You can use on of the following types:
        //    - HTTP(s): 'https://github.com/aformatik/codchi
        //    - SSH:
        //        ssh://git@github.com/aformatik/codchi
        //        OR
        //        git@github.com:aformatik/codchi"
    }
}
