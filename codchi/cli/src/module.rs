use crate::{
    cli::{name::ModuleName, InputOptions, ModuleAttrPath, NixpkgsLocation},
    config::{CodchiModule, ConfigResult, FlakeLocation, FlakeScheme, FlakeUrl, MachineConfig},
    consts::user,
    logging::set_progress_status,
    platform::{nix::NixDriver, *},
    progress_scope,
    util::{Empty, Required, StringExt},
};
use anyhow::{anyhow, bail, Context, Result};
use git_url_parse::{GitUrl, Scheme};
use inquire::{list_option::ListOption, validator::Validation};
use itertools::Itertools;
use lazy_regex::regex_is_match;
use petname::petname;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    marker::PhantomData,
};

pub fn init(
    machine_name: &str,
    url: Option<GitUrl>,
    opts: &InputOptions,
    module_paths: &Vec<ModuleAttrPath>,
) -> Result<Machine> {
    match MachineConfig::find(machine_name)? {
        ConfigResult::Exists => bail!("Code machine '{}' already exists.", machine_name),
        ConfigResult::SimilarExists(other) => {
            bail!("A machine with a similar name ({other}) already exists.")
        }
        ConfigResult::None => {}
    }
    let (lock, _) = MachineConfig::open(machine_name, true)?;
    if *opts != InputOptions::default() && url.is_none() {
        bail!("<URL> is missing.");
    }

    let empty = MachineConfig::new(machine_name);
    let cfg = match url {
        None => empty,
        Some(url) => {
            let (modules, use_nixpkgs) =
                fetch_modules(&empty, &url, opts, module_paths, true, false)?;

            MachineConfig {
                name: machine_name.to_owned(),
                nixpkgs_from: match use_nixpkgs {
                    UseNixpkgs::Remote(module) => Some(module),
                    _else => None,
                },
                modules,
                secrets: Default::default(),
            }
        }
    };

    cfg.write(lock)?;

    let machine = Machine {
        config: cfg,
        config_status: ConfigStatus::NotInstalled,
        platform_status: PlatformStatus::NotInstalled,
    };
    machine.write_flake()?;
    Ok(machine)
}

/// List modules of a code machine
pub fn list(machine_name: &str) -> Result<()> {
    use comfy_table::*;
    let (_, cfg) = MachineConfig::open_existing(machine_name, false)?;

    let mut table = Table::new();
    table.load_preset(presets::UTF8_FULL).set_header(vec![
        Cell::new("Name"),
        Cell::new("Url"),
        Cell::new("Flake Module"),
    ]);

    // TODO add protocol column if SSH is added

    for (name, module) in cfg.modules.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
        let pretty_url = module.pretty_print();
        table.add_row(vec![
            Cell::new(name),
            Cell::new(pretty_url),
            Cell::new(&module.flake_attr),
        ]);
    }

    println!("{table}");

    Ok(())
}

pub fn add(
    machine_name: &str,
    url: GitUrl,
    opts: &InputOptions,
    module_paths: &Vec<ModuleAttrPath>,
) -> Result<Machine> {
    let (lock, mut cfg) = MachineConfig::open_existing(machine_name, true)?;

    let (modules, use_nixpkgs) = fetch_modules(&cfg, &url, opts, module_paths, true, true)?;
    cfg.modules.extend(modules);

    match use_nixpkgs {
        UseNixpkgs::Remote(module_name) => {
            cfg.nixpkgs_from = Some(module_name);
        }
        UseNixpkgs::Local => {
            cfg.nixpkgs_from = None;
        }
        UseNixpkgs::DontCare => {}
    }
    cfg.write(lock)?;

    let machine = Machine::by_name(machine_name)?;
    machine.write_flake()?;
    machine.update_status()
}

pub fn set(
    machine_name: &str,
    module_name: &ModuleName,
    opts: &InputOptions,
    new_name: &Option<ModuleName>,
    module_path: &Option<ModuleAttrPath>,
    url: Option<GitUrl>,
) -> Result<Machine> {
    if opts.tag.is_none()
        && opts.commit.is_none()
        && opts.branch.is_none()
        && opts.token.is_none()
        && opts.use_nixpkgs.is_none()
        && new_name.is_none()
        && module_path.is_none()
        && url.is_none()
    {
        bail!("Skipping empty command");
    }
    // let (lock, mut cfg) = MachineConfig::open_existing(machine_name, true)?;

    // if modules.is_empty() {
    //     bail!("Please provide at least one module!");
    // }

    // // remove old modules
    // let mut modules_to_remove = Vec::with_capacity(modules.len());
    // for module_name in modules {
    //     let Some(old) = cfg.modules.remove(module_name) else {
    //         bail!("Machine '{machine_name}' doesn't have the module '{module_name}'.");
    //     };
    //     modules_to_remove.push(old);
    // }

    // // only one input is allowed
    // let flake_input = match modules_to_remove
    //     .iter()
    //     .map(|m| m.with_attr(Default::default()))
    //     .unique()
    //     .collect_vec()
    //     .as_slice()
    // {
    //     [single] => single.clone(),
    //     many => {
    //         bail!(
    //             "You're trying to modify modules from different repositories / commits!: {}",
    //             many.iter().map(|f| f.pretty_print()).join(", ")
    //         );
    //     }
    //     [] => unreachable!("There must be at least one input!"),
    // };

    // for (name, module) in cfg.modules.clone() {
    //     let input = module.with_attr(Default::default());
    //     if input == flake_input {
    //         if opts.dont_prompt {
    //             log::warn!(
    //                 "Machine '{machine_name}' contains another module '{}' from the \
    // same repository which you are trying to modify.",
    //                 input.pretty_print()
    //             );
    //         } else if inquire::Confirm::new(&format!(
    //             "Module '{name}' also comes from '{}'. Do you want to modify it too?",
    //             input.pretty_print()
    //         ))
    //         .prompt()?
    //         {
    //             modules_to_remove.push(cfg.modules.remove(&name).unwrap());
    //         }
    //     }
    // }

    let (lock, mut cfg) = MachineConfig::open_existing(machine_name, true)?;
    // remove the old module
    let Some(old) = cfg.modules.remove(module_name) else {
        bail!("Machine '{machine_name}' doesn't have the module '{module_name}'.");
    };
    // is there a name change?
    let new_name = if let Some(name) = &new_name {
        if cfg.modules.contains_key(name) {
            bail!("Machine '{machine_name}' already has another module with the name '{name}'.");
        }
        name
    } else {
        module_name
    };
    // update nixpkgs_from if pointing to current module
    if cfg.nixpkgs_from.as_ref() == Some(module_name) {
        cfg.nixpkgs_from = Some(new_name.clone());
    }
    let old_token = match old.location.clone() {
        FlakeLocation::Remote { token, .. } => token,
        FlakeLocation::Local { .. } => None,
    };
    let (url, new_module_path, opts) = if let Some(url) = url {
        // if the URL changed, prompt if branch, token etc... stay the same
        let keep_or_edit = |name: &str, value: Option<String>| -> Result<Option<String>> {
            let Some(value) = value else { return Ok(None) };
            if opts.dont_prompt {
                log::warn!("Removing {name} = {value}");
                Ok(None)
            } else {
                Ok(inquire::Text::new(&format!("Keep {name}?"))
                    .with_initial_value(&value)
                    .with_help_message(
                        "<Enter> to confirm, delete to remove value, edit to change value",
                    )
                    .prompt()?
                    .none_if_empty())
            }
        };
        let ask_to_keep = |name: &str, value: ModuleAttrPath| -> Result<Option<ModuleAttrPath>> {
            if opts.dont_prompt {
                log::warn!("Removing {name} '{value}'");
                Ok(None)
            } else if inquire::Confirm::new(&format!("Keep {name} '{value}'?")).prompt()? {
                Ok(Some(value))
            } else {
                Ok(None)
            }
        };
        (
            url.clone(),
            module_path.clone().map_or_else(
                || ask_to_keep("module path", old.flake_attr),
                |x| Ok(Some(x)),
            )?,
            InputOptions {
                dont_prompt: opts.dont_prompt,
                use_nixpkgs: opts.use_nixpkgs.clone(), // is prompted inside fetch_module
                no_build: opts.no_build,
                token: opts
                    .token
                    .clone()
                    .map_or_else(|| keep_or_edit("token", old_token), |x| Ok(Some(x)))?,
                branch: opts
                    .branch
                    .clone()
                    .map_or_else(|| keep_or_edit("branch/tag", old.r#ref), |x| Ok(Some(x)))?,
                tag: None,
                commit: opts
                    .branch
                    .clone()
                    .map_or_else(|| keep_or_edit("commit", old.commit), |x| Ok(Some(x)))?,
            },
        )
    }
    // if not, keep everything
    else {
        // else merge existing and new options
        (
            old.to_git_url(),
            module_path.clone().or(Some(old.flake_attr.clone())),
            InputOptions {
                dont_prompt: opts.dont_prompt,
                use_nixpkgs: opts.use_nixpkgs.clone().or_else(|| {
                    if cfg.nixpkgs_from.as_ref() == Some(new_name) {
                        Some(NixpkgsLocation::Remote)
                    } else {
                        Some(NixpkgsLocation::Local)
                    }
                }),
                no_build: opts.no_build,
                token: opts.token.clone().or(old_token),
                branch: opts.branch.clone().or(old.r#ref.clone()),
                tag: opts.branch.clone().or(old.r#ref.clone()),
                commit: opts.commit.clone().or(old.commit.clone()),
            },
        )
    };

    let (module, use_nixpkgs) = fetch_modules(
        &cfg,
        &url,
        &opts,
        &new_module_path.iter().cloned().collect(),
        false, // must be single answer
        true,
    )?;

    let (_name, module) = module
        .iter()
        .next()
        .ok_or(anyhow!("Failed to fetch single module..."))?;
    cfg.modules.insert(new_name.clone(), module.clone());

    match use_nixpkgs {
        UseNixpkgs::Remote(_) => {
            cfg.nixpkgs_from = Some(new_name.clone());
        }
        UseNixpkgs::Local => {
            cfg.nixpkgs_from = None;
        }
        UseNixpkgs::DontCare => {}
    }
    cfg.write(lock)?;

    let machine = Machine::by_name(machine_name)?;
    machine.write_flake()?;
    machine.update_status()
}

pub fn delete(machine_name: &str, module_name: &ModuleName) -> Result<Machine> {
    let (lock, mut cfg) = MachineConfig::open_existing(machine_name, true)?;
    if !cfg.modules.contains_key(module_name) {
        bail!("Machine '{machine_name}' doesn't have the module '{module_name}'.");
    }

    cfg.modules.remove(module_name);
    if cfg
        .nixpkgs_from
        .as_ref()
        .is_some_and(|np| np == module_name)
    {
        log::warn!("Machine '{machine_name}' uses this module for its nixpkgs. They will now default to codchi's nixpkgs!");
        cfg.nixpkgs_from = None;
    }
    cfg.write(lock)?;

    let machine = Machine::by_name(machine_name)?;
    machine.write_flake()?;
    machine.update_status()
}

pub enum UseNixpkgs {
    DontCare,
    Local,
    Remote(ModuleName),
}

pub fn fetch_modules(
    machine: &MachineConfig,
    url: &GitUrl,
    opts: &InputOptions,
    module_paths: &Vec<ModuleAttrPath>,
    allow_multiple: bool,
    allow_local: bool,
) -> Result<(HashMap<ModuleName, CodchiModule>, UseNixpkgs)> {
    if opts.dont_prompt && module_paths.is_empty() {
        bail!("Please provide MODULE_PATHS in non interactive mode.");
    }
    // if opts.dont_prompt && opts.use_nixpkgs.is_none() {
    //     bail!("Please provide `--use-nixpkgs` in non interactive mode.");
    // }
    let repo_name = url
        .name // name from url
        .clone()
        .none_if_empty()
        .or_else(|| {
            url.path // or local path
                .trim_matches(&['/', '~', '.'])
                .split('/')
                .last()
                .and_then(StringExt::none_if_empty)
                .map(ToOwned::to_owned)
        }) // or random
        .unwrap_or_else(|| petname(1, "-").expect("Failed to generate random name"));

    let flake_url = inquire_module_url(opts, url, allow_local)?;
    let nix_url = flake_url.to_nix_url(&machine.name);

    let available_modules = progress_scope! {
        set_progress_status("Fetching available modules...");
        Driver::store().cmd().list_nixos_modules(&nix_url)
    }?;

    if available_modules.is_empty() {
        bail!("The repository at '{nix_url}' contains no modules.");
    }

    let modules = if module_paths.is_empty() {
        if available_modules.len() == 1 {
            let module = available_modules.first().unwrap();
            log::info!("Using module '{module}'");
            vec![module.clone()]
        } else if allow_multiple {
            // non interactive mode is checked above
            inquire::MultiSelect::new("Modules:", available_modules)
                .with_help_message("Which modules would you like to use?")
                .with_validator(|input: &[ListOption<&ModuleAttrPath>]| {
                    Ok(if input.is_empty() {
                        Validation::Invalid("Please select at least one module".into())
                    } else {
                        Validation::Valid
                    })
                })
                .prompt()?
        } else {
            vec![inquire::Select::new("Modules:", available_modules)
                .with_help_message("Which modules would you like to use?")
                .prompt()?]
        }
    } else {
        for module in module_paths {
            if !available_modules.contains(module) {
                bail!(
                    "Module {module} does not exist. These are available: {}",
                    available_modules
                        .iter()
                        .map(|m| m.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                );
            }
        }
        module_paths.clone()
    };

    fn find_unique(
        present: &mut HashSet<String>,
        repo_name: &str,
        module_path: &ModuleAttrPath,
    ) -> String {
        let mut name = format!("{repo_name}-{}", module_path.module);
        while present.contains(&name) {
            name = format!(
                "{repo_name}-{}",
                petname(1, "-").expect("Failed to generate petname")
            );
        }
        present.insert(name.clone());
        name
    }

    let mut taken_names: HashSet<String> = machine.modules.keys().map(|m| m.0.clone()).collect();
    let modules: HashMap<ModuleName, FlakeUrl<Required>> = modules
        .into_iter()
        .map(|module| {
            (
                ModuleName(find_unique(&mut taken_names, &repo_name, &module)),
                flake_url.with_attr(module.clone()),
            )
        })
        .collect();

    let first_module = modules.iter().next().expect("Can't be empty");

    let use_nixpkgs = match &opts.use_nixpkgs {
        Some(NixpkgsLocation::Local) => UseNixpkgs::Local,
        Some(NixpkgsLocation::Remote) => {
            if !Driver::store().cmd().has_nixpkgs_input(&nix_url)? {
                bail!(
                    "No nixpkgs inputs found in flake.nix from '{}'.",
                    flake_url.pretty_print()
                );
            }
            UseNixpkgs::Remote(first_module.0.clone())
        }
        None => {
            if machine.nixpkgs_from.is_some() {
                UseNixpkgs::DontCare // do nothing, machine already follows other module
            } else if Driver::store().cmd().has_nixpkgs_input(&nix_url)? {
                // Some(
                //     inquire::Confirm::new(&format!(
                //         "Use nixpkgs from '{}'",
                //         flake_url.pretty_print()
                //     ))
                //     .with_help_message(indoc! {"
                //     This module has a nixpkgs input. Do you want to use it for the code machine? \
                //     Otherwise the shared nixpkgs of codchi is used, which might decrease \
                //     reproducibility but is faster. \
                //     See <https://codchi.dev/docs/start/usage.html#which-nixpkgs-should-i-use> \
                //     for more information."})
                //     .prompt()?,
                // )
                log::warn!(
                    "Using nixpkgs from '{}'! You may override this with '--use-nixpkgs'. See \
                    <https://codchi.dev/docs/start/usage.html#which-nixpkgs-should-i-use> for \
                    more information.",
                    flake_url.pretty_print()
                );
                UseNixpkgs::Remote(first_module.0.clone())
            } else {
                log::warn!(
                    "No nixpkgs inputs found in flake.nix from '{}'. Using codchi's nixpkgs!",
                    flake_url.pretty_print()
                );
                UseNixpkgs::Local
            }
        }
    };
    Ok((modules, use_nixpkgs))
}

fn inquire_module_url(
    opts: &InputOptions,
    url: &GitUrl,
    allow_local: bool,
) -> Result<FlakeUrl<Empty>> {
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

    match url.scheme {
        Scheme::Http | Scheme::Https => {
            let host = url.host.clone().context("Host missing.")?;
            let fallback_scheme = match url.scheme {
                Scheme::Http => FlakeScheme::Http,
                Scheme::Https => FlakeScheme::Https,
                // Scheme::Ssh => todo!(),
                _ => unreachable!(),
            };

            // Token in URL / port only work with git+http(s)
            let (host, scheme) = if let Some(port) = url.port {
                (format!("{host}:{port}"), fallback_scheme)
            } else if opts.token.is_some() {
                (host, fallback_scheme)
            } else {
                let guess = guess_scheme(&host);
                if let Guess::Sure(scheme) = guess {
                    (host, scheme)
                } else if !opts.dont_prompt {
                    // only prompt user in interactive mode
                    let select = inquire::Select::new(
                        "Code forge:",
                        vec![
                            FS(FlakeScheme::Github),
                            FS(FlakeScheme::Gitlab),
                            FS(FlakeScheme::Sourcehut),
                            FS(fallback_scheme),
                        ],
                    ).with_help_message("Please select the type of code forge where this repository is hosted. This will speed up subsequent commands!");
                    // preset guessed scheme in prompt
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

            let repo = url.path.trim_matches('/').to_string();

            Ok(FlakeUrl {
                location: FlakeLocation::Remote {
                    scheme,
                    host,
                    repo,
                    token: opts.token.clone(),
                },
                commit: opts.commit.clone(),
                r#ref: opts.branch.as_ref().or(opts.tag.as_ref()).cloned(),
                flake_attr: PhantomData,
            })
        }
        Scheme::File => {
            if !allow_local {
                bail!("Local paths can only be used *after* the code machine was created and the remote repository was checked out locally. For further information see <https://codchi.dev/docs/start/usage.html#local-configuration>.")
            }
            let home = &user::DEFAULT_HOME.0;
            if !regex_is_match!("^[^~/]", &url.path) || regex_is_match!(r"\.\.", &url.path) {
                bail!(
                    "Only paths relative to the home directory ({home}) of the code machine are allowed. You provided: '{}'.",
                    url.path
                );
            }
            let path = url.path.trim_matches(&['/', '.']).to_owned();
            if opts.dont_prompt {
                log::info!("Using path '{home}/{path}'.");
            } else if !inquire::Confirm::new(&format!(
                "Using path '{home}/{path}'. Is this correct?"
            ))
            .prompt()?
            {
                bail!("Operation was canceled by the user");
            }
            Ok(FlakeUrl {
                location: FlakeLocation::Local { path },
                commit: opts.commit.clone(),
                r#ref: opts.branch.as_ref().or(opts.tag.as_ref()).cloned(),
                flake_attr: PhantomData,
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
        _other => {
            bail!("Currently only HTTP(S) urls are supported.")
        } //            "This type of link is currently not supported. You can use on of the following types:
          //    - HTTP(s): 'https://github.com/aformatik/codchi
          //    - SSH:
          //        ssh://git@github.com/aformatik/codchi
          //        OR
          //        git@github.com:aformatik/codchi"
    }
}
