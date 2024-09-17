---
pageType: home

hero:
  name: Codchi
  # text: Code Machines
  tagline: |
    <span class="hero-tagline" style="line-height: 1.5">Codchi is a tool that manages your project's development environment in a reproducible and easy-to-use way. Setting up a development environment should be as easy as a `git clone`!</span>
  actions:
    - theme: brand
      text: How it Works
      link: /docs/start/intro.html
    - theme: alt
      text: Get Started
      link: /docs/start/installation.html
#    - theme: alt
#      text: Github
#      link: https://github.com/aformatik/codchi
  image:
    src: /logo.webp
    alt: Codchi Logo

features:
  - title: 'Reproducible and Reliable'
    icon: '❄️'
    # span: 6
    details: |
        Codchi builds on the excellent Nix package manager to install, update,
        and roll back your project's development environment. This means
        bit-for-bit reproducible environments on any machine.
  - title: 'Declarative'
    icon: '⚙️'
    # span: 4
    details: |
        Code Machines are defined in code and checked into your repository,
        allowing you to check out the correct environment at every commit of
        your project.
  - title: 'Built on Standards'
    icon: '🔓'
    details: We didn't reinvent the wheel - every NixOS module is a valid Code Machine. Also, every Code Machine is a valid NixOS module, so you're not locked into Codchi.
  - title: 'Easy to install'
    icon: '🪄'
    # span: 4
    details: |
        Installing a Code Machine is as easy as copying and pasting the link to
        the project repository into Codchi and waiting a few minutes for the
        installation process to complete. 
  - title: 'Easy to use'
    icon: '🪶'
    # span: 6
    details: |
        Once installed, shortcuts to graphical programs will appear in your
        start menu, or you can access a shell containing all available tools.
  - title: 'Native and Cross Platform'
    icon: '💻'
    details: |
        Native Looks and Performance on Windows 10, Windows 11 and Linux. Every
        Code Machine runs the same on any device.
---

