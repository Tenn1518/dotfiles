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

# Aliases

# Edit and source zsh configuration file
alias vp="nvim ~/.zshrc"
alias sp="source ~/.zshrc"
# alias ep="emacs ~/.zshrc"

# Edit nvim configuration file
alias vimrc="nvim ~/.config/nvim/init.vim"

# Edit and source zsh alias definition file
# alias ea="emacs ~/.zsh_aliases"
alias va="nvim ~/.zsh_aliases"

# Shorten nvim
alias nv="nvim"

# Redirect vi and vim commands to nvim
alias vi="nvim"
alias vim="nvim"

# Forces ls command to use colors
alias ls="ls --color=auto"

# Git mode
alias gitm="source ~/.bash_gitrc"

# ls command with flags
alias la="ls -A"
alias ll="ls -l"
alias lla="ls -lA"
alias lal="ls -Al"

# Changes directory to where I put my GitHub projects
alias cdgh="cd ~/.stuff/github && ls"

# Uncomment following line to replace git with GitHub's hub command with added GitHub functionality
# eval "$(hub alias -s)"

# Check the weather at a moment's notice
alias weather="curl wttr.in"

# Shorten emacsclient command
# alias ec="emacsclient"
# alias tec="emacsclient -t"
