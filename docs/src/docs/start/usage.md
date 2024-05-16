# Using Codchi

::: info
Each machine is identified by a `MACHINE_NAME`.
:::

## Creating a Machine

A code machine usually has one module that resides in the repository of the project you want to develop. To create a machine with this module, use the following command. Only the url to the repository is mandatory. Codchi will try to guess the rest of the information and prompt you if neccessary.
```bash
codchi init MACHINE_NAME https://github.com/link/to/repo [MODULE_PATH]
```
`MODULE_PATH` is the path of the NixOS module inside the repo's `flake.nix` and looks like `nixosModules.<name>` or `codchiModules.<name>`. If you're not sure which module to use, omit this argument. Codchi will prompt you with a list of possible options.

To prevent prompts or to specify a branch or commit, use the following options:

- **`-y` or `--dont-prompt`:** Dont prompt interactively. Usefull for scripts. Codchi will throw an error if information is missing. Note that this requires `MODULE_PATH` and **`--use-nixpkgs`**.
- **`--use-nixpkgs <location>` or `-p <location>`** where location can be `local` or `remote`: See [Which nixpkgs should I use?](#which-nixpkgs-should-i-use).
- **`-b <branch>` or `--branch <branch>`:** The git branch to use.
- **`-r <tag>` or `--tag <tag>`:** The git tag to use.
- **`-c <commit>` or `--commit <commit>`:** The git commit to use.
- **`-t <token>` or `--token <token>`:** An authentication token for private repositories. Here are some examples:
  ```bash
  oauth2:glpat-asf9k20afl20faf022fe         # GitLab
  ghp_af9afsfawe9faefjkfea92jadwjj29adjjad  # GitHub
  <user>:<password>                         # General syntax
  ```

After successfull creation, [apply your changes](#applying-changes)!

### Which nixpkgs should I use?

Since every code machine is a NixOS system, it needs a version of nixpkgs (the collection containing all Nix programs). There are two options:

1. Use Codchi's nixpkgs: Every release of codchi has a pinned version of nixpkgs which is consistent across all code machines on a given host system. Because the Nix store is shared among all machines, this results in fewer package downloads, saves disk space and is faster during installation or updates. The downside is that code machines aren't perfectly reproducibile anymore, since the nixpkgs version can change with every Codchi version. In reality this shouldn't be a big deal because Codchi adheres to NixOS's release schedule (every 6 months) and keeps nixpkgs consistent across this timespan.
2. Use the module's nixpkgs: If a module of a code machine has a nixpkgs input inside its `flake.nix`, the code machine can use (Nix language: "follow") it.
    - Pro: Exact reproducibility among machines with the same module and nixpkgs.
    - Con: More packages to download, more disk space and slower during installation and updates.



## Running Programs

### Get a shell into a machine

```bash
codchi exec MACHINE_NAME
```

### Run a command-line program

```bash
codchi exec MACHINE_NAME PROGRAM [ARGS...]
# For example: codchi exec MACHINE_NAME uname -a
```

### Run a graphical program

Programs which provide shortcuts (usually graphical programs) are also added to
your host system menu as shortcuts.


## Codchi Modules

Each code machine has a list of modules (often just one) which are identified by a name. List them with:
```bash
codchi module list MACHINE_NAME
```

::: warning
Dont forget to [apply changes](#applying-changes) you make!
:::

### Adding modules
Sometimes you might want to customize a code machine, but don't want to commit it to upstream.  For example, you might want to use a different editor than the rest of the project's contributors. Fortunately, you can add as many modules to a machine as you like! This is especially handy for sharing your personal configuration between different projects/code machines.
```bash
codchi module add MACHINE_NAME https://github.com/my/module
```
The options are the same as for [`codchi init`](#creating-a-machine).


### Modifying a module
To edit a module, for example to switch to a different branch, use `codchi module set`. The options are the same as for [`codchi init`](#creating-a-machine).

```bash
codchi module set MACHINE_NAME MODULE_NAME [URL] [MODULE_PATH]
```


#### Local Configuration

While working on a code machine module, any change to the code must first be pushed online before Codchi can pull and apply the change. This can quickly become annoying, especially if you're actively developing the module or experimenting a lot. Luckily, a machine module can be switched to local repository inside the code machine.
The following example is based on `myMachine` from chapter [Your first Module](../config/start.md).

::: tip
It's good practise to have the `.nix` configuration files in the repository of your project itself. This way Codchi can spin up a machine for every commit of your project. Also you'll have the project checked out in your code machine anyway, so step 1 shouldn't be neccessary.
:::

1. First of all, clone the repository containing the module configuration inside `myMachine`. For Codchi to be able to find it, the repository has reside in a folder below the machine's `$HOME`, which is `/home/codchi`.
```bash
# If git is available:
git clone https://github.com/my/repo ~/my-project-name
# else run it via Nix:
nix run nixpkgs\#git -- clone https://github.com/my/repo ~/my-project-name
```

2. Upon creation of `myMachine`, if you didn't explicitly specify a module name for your repository, Codchi guesses a name from its url. List `myMachine`'s modules to get the actual module name.
```bash
codchi module mod ls myMachine
```
3. Now you can tell Codchi to switch the module pointing to the remote repository to your local copy. 
```bash
codchi module set myMachine MODULE_NAME path/to/repo
```

### Delete a module
This will not delete your files, even if its the last remaining module! It will
only remove programs, services and other configuration defined in that module.
```bash
codchi module delete MACHINE_NAME MODULE_NAME
```

### Applying changes

After the modules of a machine were modified, the modifications need to be applied:
```bash
codchi rebuild MACHINE_NAME
```
This will also fetch upstream module changes (or in case of a local module it will use the current HEAD). If you dont want to fetch the latest updates, use:
```bash
codchi rebuild MACHINE_NAME --no-update
```

## Uninstalling a code machine

::: danger
This will irrevocably delete all files belonging to the code machine!
:::

```bash
codchi delete MACHINE_NAME
```

## Garbage Collection

All programs and system files reside in the append-only Nix Store. Over time the store will grow in size noticably, because everytime a `codchi rebuild` is run, new files get added to the store. Therefore you might want to perform a garbage collection from time to time:
```bash
codchi gc [MACHINE_NAME...]
```

Available options:

- **`--older-than [<age>]` or `-d [<age>]`**: By default, garbage collection will **not** delete old machine generations in order to allow instantaneous rollbacks. The drawback is that the store paths refered to in the old generations never get freed. To also delete old generations, use this flag with an optional minimum age (in days). Note that only explicitly listed machines (`MACHINE_NAME...`) will be processed. For example, to delete generations older than one month in `myMachine` use:
```bash
codchi gc -d 30 myMachine
```
- **`--all` or `-a`** flag: Process all machines (only works with **`--older-than`**).

### Large WSL Distributions

On Windows, the store is inside the WSL distribution 'codchistore'. By default WSL distributions only grow in size, once used disk space is not automatically reclaimed. Codchi will try to set the distribution to sparse mode which should automatically free unused space. If this doesn't work, you can do it automatically with
```bash
wsl.exe --manage codchistore --set-sparse true
```
Beware that WSL needs to be shut down for this which will close all running Linux programs.

If sparse mode somehow doesn't work correctly, you can manually shrink a WSL distribution like this:
```ps1
wsl.exe --terminate codchistore
diskpart # this will need admin rights
select vdisk file="C:\Users\YOUR_USER\AppData\Local\codchi\store\ext4.vhdx"
compact vdisk
exit
```

<!-- ## Uninstalling Codchi -->
