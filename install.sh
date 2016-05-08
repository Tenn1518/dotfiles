#!/usr/bin/env bash

echo "Installing dotfiles"

echo "Initializing submodule(s)"
git submodule update --init --recursive

source install/link.sh

if [[ "$(uname)" == "Darwin" ]]; then
	echo -e "\n\nRunning on OS X"
elif [[ "$(uname)" == "Linux" ]]; then
	echo -e "\n\nRunning on GNU/Linux"
else
	echo "Running on unknown operating system"
fi

# echo "Configuring zsh as default shell"
# chsh -s $(which zsh)

echo "Done."
