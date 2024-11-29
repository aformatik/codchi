# Troubleshooting

## Timezone is wrong

By default, both WSL and LXD use UTC as the default timezone. To set another timezone, use [`time.timeZone`](https://search.nixos.org/options?show=time.timeZone):

```nix
# configuration.nix
{
    time.timeZone = "Europe/Berlin";
}
```


## Linux: Graphical Applications don't Work

When opening graphical applications inside a code machine you might encounter an error like this:
```
Error: Can't open display
```
Although Codchi does its best to make the X-Server available inside a code machine (which is a LXD container on Linux), there can be issues with authentication against the host's X-Server. Codchi tries to set the Xauthority cookie. If this fails, you can try to allow all local connections to the X-Server (although this may be somewhat insecure):
```
xhost +local:
```

