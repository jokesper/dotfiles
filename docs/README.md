# Dotfiles
Here are my dotfiles, if you find anything usefull feel free to copy it.

## Installation
### From Scratch
For a full installation follow step 1 and all substeps from [Archlinux - Installation guide](https://wiki.archlinux.org/title/Installation_guide).
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
Run the following commands, but be warned, they will destroy your previous home directory.
```bash
bash +O dotglob +O nullglob -c 'rm -rf ~/*'
git clone https://github.com/jokesper/dotfiles.git ~
```

## Contributing
If you want to contribute feel free, but I wouldn't encourage it as dotfiles should be a personalized configuration.
