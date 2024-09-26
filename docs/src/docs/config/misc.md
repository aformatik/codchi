# Miscellaneous

## Welcome Message

When opening a shell into a code machine, the user is greeted with the following message:
```
❄️ Welcome to your code machine! ❄️

To get started with Codchi, read the docs at <https://codchi.dev/>.
If you encounter problems please open an issue at <https://github.com/aformatik/codchi/issues>.
```
To customize this, for example to give the user instructions on how to procceed further, there is [`codchi.welcome.text`](/docs/options.html#codchiwelcometext) and [`codchi.welcome.extraText`](/docs/options.html#codchiwelcomeextratext).

## Docker

Docker works normally in Codchi, but the user `codchi` has to be added to the corresponding group in order to not require `sudo`. Codchi provides the convenience option [`codchi.docker.enable`](/docs/options.html#codchidockerenable) for this.

```nix
{ pkgs, ... }: {
  codchi.docker.enable = true;
}
```

## Time Zone

By default the time zone is UTC inside a code machine. You can change this via
```nix
{
    time.timeZone = "Europe/Berlin";
}
```
