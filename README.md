# **dotfiles**

Welcome to my repo used for keeping configuration files that are important for me whenever I setup a new system.

The install scripts and parts of tmux.conf and zshrc were inspired by [nicknisi/dotfiles](https://github.com/nicknisi/dotfiles).

This project is licensed under the terms of the [MIT license](https://github.com/Tenn1518/dotfiles/blob/master/LICENSE).

### Prerequisites
Install Neovim, tmux, a terminal emulator with true color support, and a solarized theme for your terminal.

Vim-airline is set up to use powerline symbols. If you don't already have pre-patched Powerline fonts, you can clone the [powerline/fonts](https://github.com/powerline/fonts) repo and run install.sh from inside the repository. Clone it with:
```
git clone https://github.com/powerline/fonts
```
And install it with:
```
cd fonts
bash install.sh
```
You'll then need to set a Powerline-patched font as your terminal font. 

CtrlP.vim is set to use ag.vim, which depends on the_silver_searcher being installed. You'll need to install the_silver_searcher with your Linux distro's package manager or Homebrew for Mac.

### How to Install
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

To install the plugins for neovim, open neovim and run
```
:PlugInstall
```

### List of vim plugins

 Plugins | Description
 ------- | -----------
 [vim-plug](https://github.com/junegunn/vim-plug) | Minimalist vim plugin manager
 [unite.vim](https://github.com/https://github.com/Shougo/unite.vim) | Used to create interfaces, dependency of vim-fugitive
 [vim-airline](https://github.com/vim-airline/vim-airline) | Lean & mean status/tabline that's light as air
 [vim-airline-themes](https://github.com/vim-airline/vim-airline-themes) | A collection of themes for vim-airline
 [vim-bufferline](https://github.com/bling/vim-bufferline) | super simple vim plugin to show the list of buffers in the command bar |
 [vim-fugitive](https://github.com/tpope/vim-fugitive) | A Git wrapper for vim
 [vim-sensible](https://github.com/tpope/vim-sensible) | Defaults everyone can agree on
 [vim-solarized8](https://github.com/lifepillar/vim-solarized8) | True-color Solarized colorschemes for vim
 [ag.vim](https://github.com/vim-scripts/ag.vim) | ag, the_silver_searcher (better than ack, which is better than grep)
 [ctrlp.vim](https://github.com/ctrlpvim/ctrlp.vim) | Fuzzy file, buffer, mru, tag, etc finder
 [NERDTree](https://github.com/scrooloose/nerdtree) | A tree explorer plugin for vim
