# Your first Code Machine

To create a machine for a given project, simply copy the link of its git repository (currently only http(s)). 

Open a terminal and create the machine with a `MACHINE_NAME` of your choice. If you're not sure how to answer a question, just choose the default answer:
```bash
codchi init MACHINE_NAME https://github.com/link/to/repo
```

::: tip
If you don't have a repository with a Codchi module, you can try this machine (OpenJDK, Maven and IntelliJ IDEA):
```bash
codchi init MACHINE_NAME https://github.com/aformatik/codchi nixosModules.jvm
```
:::

Thats it! After a few minutes your machine should be installed and you can now open a shell inside the machine or directly run a program:
```bash
codchi exec MACHINE_NAME
# If IntelliJ is installed
codchi exec MACHINE_NAME idea-community
```
