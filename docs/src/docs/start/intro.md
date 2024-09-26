# What is Codchi?

Codchi is a tool that manages your project's development environment. It uses the excellent [Nix package manager](https://nixos.org) to install, update, and roll back your project's development environment in a reproducible way - on any Windows or Linux machine. The development environments are defined in code and checked into your repository, allowing you to

- share your environment *reproducibly* with your colleagues,
- automate the installation and setup of the development environment,
- check out a working development environment at every commit of your project.

Codchi uses virtualisation technologies like KVM and Hyper-V to provide an isolated environment while maintaining near-native performance and user experience of the tools (command-line as well as graphical) you're running. This means that Codchi can provide several versions of the development environment for the same project at the same time without polluting your host system.

Codchi's goal is to minimize the amount of thoughts you put into your development environment. Let's say the project you're working on is a Java web app that needs a JDK, an IDE like [IntelliJ IDEA](https://www.jetbrains.com/idea/), a web server and a database. With Codchi, the installation is as simple as providing the Codchi command-line interface with the link to the repository and waiting for a few minutes for the installation to finish. Now every time you launch the Java code machine - just in a terminal or via the automatically created shortcut directly into IntelliJ - the server and database are started and tools like the JDK are available. If at some point the tools in your development environment are updated or new ones are added, Codchi will keep track of the changes and provide the up-to-date environment. The reverse is also true: Reverting to an enviroment for any old commit in your project's history is as simple as checking out that commit.

## How it Works

Code machines are to Codchi what containers are to Docker. A code machine is comprised of a number of *modules* (usually just one). A module is a plain NixOS Module inside a git repository which defines the development environment of that repository. Since it's just code, it is versioned just as the rest of the repository, which gives Codchi the ability to create a development environment for every commit of that project.

After configuration a code machine is built into a NixOS system inside a container. On Windows this is a WSL instance, on Linux a LXD container. The Codchi binary itself runs on the host system in order to provide features like passthrough of graphics and sound or start menu shortcuts for applications inside code machines. Also Codchi provides niceties like store-sharing between machines. In essence Codchi is a cross-platform driver for NixOS systems that takes care of the tedious hardware and system integration, so that you only have to think about the actual programs and services you want to run.

## When should I (not) use Codchi?

It might seem that other tools like [devenv.sh](https://devenv.sh/) or [flox](https://flox.dev/) already provide the exact same functionality as Codchi, but there are some important differences:

- **Nix Shell vs. NixOS**: Tools like devenv.sh and flox provide the development environment in a (bash) shell without completely isolating them from the host. This is simpler and more performant than NixOS containers, but doesn't provide proper isolation which many, especially more complex enterprise development enviroments need.
- **User Experience**: Both tools assume a Unix-like operating system with Nix already installed. Also often they assume an IDE like VSCode is already installed on the host system. Codchi on the other hand provides a "single click installation" which aims to be as user friendly as possible. Everything from the compiler and services to the IDE is provided when installing a code machine. Also since Codchi controls the virtualisation technology it can provide features like secret management and GPU computing by interfacing with the host OS.
- **Configuration**: Codchi uses NixOS options while other tools have their own, much more limited option set. This means that in addition to Codchi's own options you can use [more than 10000 NixOS Options](https://search.nixos.org/options)!

On the other hand, if it is a relatively simple development environment (e.g. projects that get by with only a LSP server and a few services) and it can be assumed that all developers have access to a Unix shell, simpler approaches such as simple Nix shells, devenv.sh or flox are more suitable.
Furthermore, you can use both Codchi and devenv tools like devenv.sh and flox where they're best at: Codchi provides a consistent environment and proper host integration across Windows and Linux and devenv.sh or flox provide the development environment itself which can also be used outside of Codchi.
