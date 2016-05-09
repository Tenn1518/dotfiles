# **dotfiles**

Welcome to my repo used for keeping configuration files that are important for me whenever I setup a new system.

The install scripts and parts of tmux.conf and zshrc were inspired by [nicknisi/dotfiles](https://github.com/nicknisi/dotfiles).

This project is licensed under the terms of the [MIT license](https://github.com/Tenn1518/dotfiles/blob/master/LICENSE).

#### How do I use these files?
Install the dotfiles with:
```
git clone --recursive https://github.com/Tenn1518/dotfiles ~/.dotfiles
```
and then run:
```
cd .dotfiles/
./install.sh
```
These will install my dotfiles to their respective locations. Note that most if not all dotfiles will simply be symlinks to their respective locations under ~/.dotfiles, so do not remove ~/.dotfiles.
