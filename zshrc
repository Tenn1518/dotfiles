HISTFILE=~/.zsh-histfile
HISTSIZE=500
SAVEHIST=500
bindkey -v

# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/tnazmee/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -U promptinit
promptinit

screenfetch
echo "You are now logged in as user $USER on $(hostname -s) running Fedora 23"

# Set bash prompt
PS1="%n@%m %~ $ "

# Sets default editor for programs which check the EDITOR variable
# export EDITOR=/usr/bin/nano
# export EDITOR=/usr/bin/emacs
export EDITOR=/usr/bin/nvim

# Alias definitions
# Put your alias definitions into ~/.zsh_aliases
if [ -f ~/.zsh_aliases ]; then
    source ~/.zsh_aliases
fi

# Set XDG_CONFIG_HOME variable
export XDG_CONFIG_HOME="~/.config"

# Antigen
source ~/.antigen/antigen.zsh

# Load oh-my-zsh core libraries
antigen use oh-my-zsh

# Load oh-my-zsh bundles
antigen bundle git
antigen bundle command-not-found

# Syntax hightlighting bundle
antigen bundle zsh-users/zsh-syntax-highlighting

# Load oh-my-zsh theme
antigen theme agnoster

antigen apply
