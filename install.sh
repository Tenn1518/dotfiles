#!/bin/bash
# This script will install dotfiles to their respective locations
# Created by Tenn1518

# Check if user is running as superuser
if [ $(id -u) = 0 ]; then
	echo "Do not run this script as root" >&2
	echo "Exiting" >&2
	exit 1
fi

echo "Which distro are you running?"
select dfao in "Debian/Ubuntu" "Fedora" "Arch Linux" "openSUSE"; do
	case $dfao in
		Debian/Ubuntu ) IN="sudo apt-get install -y"
		Fedora ) IN="sudo dnf install -y"
		Arch Linux ) IN="sudo pacman -Syu"
		openSUSE ) IN="sudo zypper in -y"
	esac
done

DFL="~/.dotfiles"

# Install antigen
$IN zsh
chsh -s zsh
ln -s $DFL/antigen ~/.antigen

# Install neovim dotfiles
ln -s $DFL/config/neovim-config ~/.config/nvim

# Install vim dotfiles
$IN vim
ln -s $DFL/config/neovim-config ~/.vim
ln -s $DFL/config/neovim-config/init.vim ~/.vimrc

# Finish script
exit 0
