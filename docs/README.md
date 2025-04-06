# Dotfiles
The public part of my personal configuration.

## Installation
### From Scratch
For a full installation follow step 1 and all substeps from [Archlinux - Installation guide](https://wiki.archlinux.org/title/Installation_guide).
Also make sure that any other drive you want in your fstab file is also mounted.
After this you will want to run:
```bash
curl https://raw.githubusercontent.com/jokesper/dotfiles/main/.dotfiles/arch.sh | bash -s <path> <hostname> <kernel> <username>
```
The paramaters are as follows:
- `<path>` is your mount point, e.g. if you followed step 1.11 it should be `/mnt`
- `<hostname>` is the hostname for the new machine
    (e.g. read [Choosing a Name for Your Computer](https://datatracker.ietf.org/doc/html/rfc1178))
- `<kernel>` the specific kernel you want to use which may be one of:
    - `stable`
    - `hardened`
    - `lts`
    - `rt-lts`
    - `zen`
    - any available kernel package
- `<username>` the username of the default account, which also is in the `wheel` group

An example of this command may be:
```bash
curl https://raw.githubusercontent.com/jokesper/dotfiles/main/.dotfiles/arch.sh | bash -s /mnt my-new-computer stable "John Doe"
```

### On a new User
This repo has strong entanglement between system and user. It is designed as a single user system.
If you still want to add a new user run the following commands, but be warned, they will destroy your previous home directory.
```bash
bash +O dotglob +O nullglob -c 'rm -rf ~/*'
git clone https://github.com/jokesper/dotfiles.git ~
```

## Contributing
You probably shouldn't. Though if you still want to, I will review them.
In such cases I might also consider extracting them into a separate repository.
