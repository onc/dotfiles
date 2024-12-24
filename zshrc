# -*- mode: sh; coding: utf-8; -*-
# vim:set filetype=sh fileencoding=utf-8:

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="onze"

# plugins
plugins=(
    bgnotify
    brew
    colored-man-pages
    colorize
    cp
    direnv
    docker
    docker-compose
    git
    git-flow-avh
    man
    npm
    pip
    pyenv
    rbenv
    sudo
    svn
    tmux
    tmuxinator
    virtualenv
    zsh-autosuggestions
    # zsh-github-copilot
)

if [ OS_TYPE="macOS" ]; then
    plugins=(macos $plugins)
fi

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="false"

REPORTTIME=10

# Disable repeating command before result of command
DISABLE_AUTO_TITLE="true"

fpath=($ZSH/custom/completions $fpath)
fpath=($(brew --prefix)/share/zsh/site-functions $fpath)

source $ZSH/oh-my-zsh.sh

#======================================================================================
# USER CONFIGURATION
#======================================================================================
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt SHARE_HISTORY # import new commands from history file and append immediately to it
setopt NO_CASE_GLOB # set ignore case for ls etc
setopt COMPLETE_IN_WORD # more extensive tab completion
setopt HIST_IGNORE_ALL_DUPS # ignore duplicates
setopt HIST_IGNORE_SPACE # ignore entries which begin with a space
setopt EXTENDED_GLOB # activate extended globbing
setopt LIST_PACKED # try to make the completion list smaller (occupying  less  lines)

[[ -f ~/.dotfiles/oncsh/aliases.zsh ]] && source ~/.dotfiles/oncsh/aliases.zsh
[[ -f ~/.dotfiles/oncsh/misc.zsh ]] && source ~/.dotfiles/oncsh/misc.zsh
[[ -f ~/.dotfiles/oncsh/helpers.zsh ]] && source ~/.dotfiles/oncsh/helpers.zsh

if command -v fzf > /dev/null; then
    source <(fzf --zsh)

    if [ -f ~/.dotfiles/oncsh/fzf.zsh ]; then
        source ~/.dotfiles/oncsh/fzf.zsh
    fi
fi

#======================================================================================
# attempt to fix history search with arrow keys
#======================================================================================
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
# bindkey "^[[A" up-line-or-beginning-search # Up
# bindkey "^[[B" down-line-or-beginning-search # Down

# alternative (vim-like) binding for history search
bindkey "^k" up-line-or-beginning-search # Up
bindkey "^j" down-line-or-beginning-search # Down

#======================================================================================
# github copilot in shell
#======================================================================================
bindkey "^[|" zsh_gh_copilot_explain  # bind Alt+shift+\ to explain
bindkey "^J" zsh_gh_copilot_suggest  # bind Alt+\ to suggest

#======================================================================================
# autosuggest
#======================================================================================
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=244"
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

bindkey "^ " autosuggest-accept
#======================================================================================
# terraform
#======================================================================================
autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/bin/terraform terraform

#======================================================================================
# direnv
#======================================================================================
if command -v direnv > /dev/null; then 
    eval "$(direnv hook zsh)"
fi
