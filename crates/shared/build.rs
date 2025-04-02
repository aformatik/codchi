use std::{env, error::Error};

fn main() -> Result<(), Box<dyn Error>> {
    let commit = build_data::exec("nix-git-commit", &[])?;
    println!("cargo:rustc-env=CODCHI_GIT_COMMIT={}", commit);
    build_data::set_SOURCE_TIMESTAMP();

    if let Ok(branch) = build_data::get_git_branch() {
        println!("cargo:rustc-env=CODCHI_GIT_BRANCH={}", branch);
    } else {
        println!(
            "cargo:rustc-env=CODCHI_GIT_BRANCH=v{}",
            env::var("CARGO_PKG_VERSION")?
        );
    }

    Ok(())
}
