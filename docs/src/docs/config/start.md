# Your first Module

Configuring a code machine means configuring a NixOS module. Don't worry, if you've never done this, it is really simple: In your project's git repository, create a file called `configuration.nix` with the following content:
```nix
{ pkgs, ... }: { 
    environment.systemPackages = [ pkgs.neofetch ];
    time.timeZone = "Europe/Berlin"; # Time is UTC by default
}
```
For Codchi to be able to pick it up, you also need to create a `flake.nix` in the root directory of your repository:
```nix
{
  description = "My awesome code machine";
  outputs = { ... }: {
    nixosModules.myModule = ./configuration.nix;
  };
}
```
Make sure that the path to `configuration.nix` is correct. It must start with `./`! Now push both files to a code forge of your choice and create a code machine:
```bash
codchi init myMachine https://github.com/my/repo nixosModules.myModule
```
Notice that `nixosModules.myModule` is from `flake.nix`, so make sure they match! Now build the machine and run `neofetch`:
```bash
codchi rebuild myMachine
codchi exec myMachine neofetch
          ▗▄▄▄       ▗▄▄▄▄    ▄▄▄▖            codchi@nixos
          ▜███▙       ▜███▙  ▟███▛            ------------
           ▜███▙       ▜███▙▟███▛             OS: NixOS x86_64
            ▜███▙       ▜██████▛              Host: ...
     ▟█████████████████▙ ▜████▛     ▟▙        Kernel: 6.1.64
    ▟███████████████████▙ ▜███▙    ▟██▙       Uptime: ...
           ▄▄▄▄▖           ▜███▙  ▟███▛       Packages: 383 (nix-system)
          ▟███▛             ▜██▛ ▟███▛        Shell: bash 5.2.15
         ▟███▛               ▜▛ ▟███▛         Resolution: 1920x1080
▟███████████▛                  ▟██████████▙   CPU: ...
▜██████████▛                  ▟███████████▛   GPU: ...
      ▟███▛ ▟▙               ▟███▛            GPU: ...
     ▟███▛ ▟██▙             ▟███▛             Memory: 19MiB / ...
    ▟███▛  ▜███▙           ▝▀▀▀▀
    ▜██▛    ▜███▙ ▜██████████████████▛
     ▜▛     ▟████▙ ▜████████████████▛
           ▟██████▙       ▜███▙
          ▟███▛▜███▙       ▜███▙
         ▟███▛  ▜███▙       ▜███▙
         ▝▀▀▀    ▀▀▀▀▘       ▀▀▀▘
```
Congratulations, you've configured your first code machine! 
