# Using Codchi

::: info
Each machine is identified by a `<name>`.
:::

## Creating a Machine

A code machine usually has one module that resides in the repository of the
project you want to develop. To create a machine with this module, use the
following command. Only the url to the repository is mandatory. Codchi will try
to guess the rest of the information and prompt you if neccessary.
```bash
codchi init <name> https://github.com/link/to/repo <module name>
```
`<module name>` is the identifier of the NixOS module inside the repo's
`flake.nix` and looks like `nixosModules.<name>` or `codchiModules.<name>`. If
you're not sure which module to use, omit this argument. Codchi will prompt you
with a list of possible options.

To prevent prompts or to specify a branch or commit, use the following options:

- **`-y` or `--dont-prompt`:** Dont prompt under any circumstances. Usefull for
  scripts. Codchi will throw an error if information is missing.
- **`-b <branch>` or `--branch <branch>`:** The git branch to use.
- **`-r <tag>` or `--tag <tag>`:** The git tag to use.
- **`-c <commit>` or `--commit <commit>`:** The git commit to use.
- **`-t <token>` or `--token <token>`:** An authentication token for private
  repositories. Here are some examples:
  ```bash
  oauth2:glpat-asf9k20afl20faf022fe         # GitLab
  ghp_af9afsfawe9faefjkfea92jadwjj29adjjad  # GitHub
  <user>:<password>                         # General syntax
  ```

  <!-- TODO use nixpkgs option -->

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
codchi exec <name>
```

### Run a command-line program

```bash
codchi exec <name> <program>
# For example: codchi exec <name> uname -a
```

### Run a graphical program

Programs which provide shortcuts (usually graphical programs) are also added to
your host system menu as shortcuts.


## Codchi Modules

Each code machine has a list of modules (often just one). List them with:
```bash
codchi module list <name>
```
You'll need the module index to modify it.

::: warning
Dont forget to [apply changes](#applying-changes) you make!
:::

### Adding modules
Sometimes you might want to customize a code machine, but don't want to commit
it to upstream.  For example, you might want to use a different editor than the
rest of the project's contributors. Fortunately, you can add as many modules to
a machine as you like! This is especially handy for sharing your personal
configuration between different projects/code machines.
```bash
codchi module add <name> https://github.com/my/module
```
The options are the same as for [`codchi init`](#creating-a-machine).


### Modifying a module
<!-- TODO module edit -->
To edit a module, for example to switch to a different branch, use `codchi module set`:
```bash
codchi module set <name> <module index>
```
The options are the same as for [`codchi init`](#creating-a-machine).


#### Local Configuration

While working on a code machine module, any change to the code must first be pushed online before Codchi can pull and apply the change. This can quickly become annoying, especially if you're actively developing the module or experimenting a lot. Luckily, a machine module can be switched to local repository inside the code machine.
The following example is based on `myMachine` with the module `myModule` from chapter [Your first Module](../config/start.md).

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

2. Now you can tell Codchi to switch `myModule` to the local repository by using [`codchi module set`](#modifying-a-module). `<module-index>` is the index of `myModule` and should be `0` if this is the only module.
```bash
codchi module set myMachine <module-index> ~/path/to/repo
```

### Delete a module
This will not delete your files, even if its the last remaining module! It will
only remove programs, services and other configuration defined in that module.
```bash
codchi module delete <name> <module index>
```

### Applying changes

After the modules of a machine were modified, the modifications need to be applied:
<!-- TODO consistent update checks / notifications -->
```bash
codchi rebuild <name>
```
This will also fetch upstream module changes (or in case of a local module it
will use the current HEAD). If you dont want to fetch the latest updates, use:
```bash
codchi rebuild <name> --no-update
```
<!-- TODO --no-update -->

## Uninstalling a code machine

::: danger
This will irrevocably delete all files belonging to the code machine!
:::

```bash
codchi delete <name>
```
Although all files of the machine will be deleted, the programs and system
files of the machine will still occupy disk space. This is because they reside
inside the [Nix store](https://nixos.org/guides/how-nix-works/). To reclaim
disk space, run a garbage collection:
<!-- TODO (extra gc page) -->
<!-- TODO implement gc -->
```bash
codchi gc
```


<!-- ## Uninstalling Codchi -->
