# Overview

Every code machine is a NixOS system consisting of a range of Codchi modules, each corresponding to a NixOS module. So, configuring a code machine is the same as configuring a NixOS system, without you having to worry about hardware configuration.

A NixOS module is written in Nix's own configuration language with the same name. Nix (the language) can be described as "JSON with functions". To write your first Codchi module, 
- read [the tutorial](./start.md) 
- and look at [the examples](https://github.com/aformatik/codchi/tree/master/nix/examples)
- and the language specific chapters in this section.

If you want to get deeper into Nix, there is
- the [official Nix Guide](https://nix.dev/)
- the [NixOS Manual](https://nixos.org/nixos/manual)
- and the [NixOS Wiki](https://wiki.nixos.org).

You can also as questions by 
- [creating an GitHub issue](https://github.com/aformatik/codchi/issues/new/choose) 
- or in the [Nix Discourse](https://discourse.nixos.org/).

During module development, you most likely don't want to push after every config change. To switch your code machine to your local working copy, see chapter [Local Configuration](../start/usage.md#local-configuration).

## Additional Resources

- Search more than 10 000 NixOS options and 100 000 Nix packages available in Codchi: <https://search.nixos.org>
- [Codchi-specific NixOS options](../options.md)
