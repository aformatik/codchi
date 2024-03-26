# Your first Code Machine

To create a machine for a given project, simply copy the link of its git
repository (currently only a http(s) link works). 

Open a terminal and create the machine with a `<name>` of your choice. If
you're not sure how to answer a question, just choose the default answer:
```bash
codchi init <name> https://github.com/link/to/repo
```

::: tip
If you don't have a repository with a Codchi module, you can try this machine (OpenJDK, Maven and IntelliJ IDEA):
```bash
codchi init <name> https://github.com/aformatik/codchi nixosModules.java
```
:::

If Codchi didn't print any errors you can now build (install) your machine:
```bash
codchi rebuild <name>
```

Your machine should be installed by now! You can open a shell inside the
machine or directly run a program:
```bash
codchi exec <name>
# If IntelliJ is installed
codchi exec <name> idea-community
```
