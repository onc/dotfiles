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
    zsh-github-copilot
)


# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="false"

REPORTTIME=10

# Disable repeating command before result of command
DISABLE_AUTO_TITLE="true"

fpath=($ZSH/custom/completions $fpath)

source $ZSH/oh-my-zsh.sh

#======================================================================================
# USER CONFIGURATION
#======================================================================================
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# ignore ls und cd in history
setopt SHARE_HISTORY # import new commands from history file and append immediately to it
setopt NO_CASE_GLOB # set ignore case for ls etc
setopt COMPLETE_IN_WORD # more extensive tab completion
setopt HIST_IGNORE_ALL_DUPS # ignore duplicates
setopt HIST_IGNORE_SPACE # ignore entries which begin with a space
setopt EXTENDED_GLOB # activate extended globbing
setopt LIST_PACKED # try to make the completion list smaller (occupying  less  lines)

if [ -f ~/.dotfiles/oncsh/misc.zsh ]; then
    source ~/.dotfiles/oncsh/misc.zsh
fi

#======================================================================================
# Do not use /etc/hosts for host completions
# This is quite useful when /etc/hosts contains thousands of hosts to block
#======================================================================================
[ -r ~/.ssh/known_hosts ] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[ -r ~/.ssh/config ] && _ssh_config=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p')) || _ssh_config=()
hosts=(
    "$_ssh_hosts[@]"
    "$_ssh_config[@]"
    "$HOST"
    localhost
)
zstyle ':completion:*:hosts' hosts $hosts

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
bindkey '^k' up-line-or-beginning-search # Up
bindkey '^j' down-line-or-beginning-search # Down

#======================================================================================
# github copilot in shell
#======================================================================================
bindkey '^[|' zsh_gh_copilot_explain  # bind Alt+shift+\ to explain
bindkey '^J' zsh_gh_copilot_suggest  # bind Alt+\ to suggest

#======================================================================================
# A script to make using 256 colors in zsh less painful.
# P.C. Shyamshankar <sykora@lucentbeing.com>
# Copied from https://github.com/sykora/etc/blob/master/zsh/functions/spectrum/
#======================================================================================
typeset -AHg FX FG BG

FX=(
    reset     "%{[00m%}"
    bold      "%{[01m%}" no-bold      "%{[22m%}"
    italic    "%{[03m%}" no-italic    "%{[23m%}"
    underline "%{[04m%}" no-underline "%{[24m%}"
    blink     "%{[05m%}" no-blink     "%{[25m%}"
    reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
    FG[$color]="%{[38;5;${color}m%}"
    BG[$color]="%{[48;5;${color}m%}"
done

ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}

# Show all 256 colors with color number
function spectrum_ls() {
  for code in {000..255}; do
    print -P -- "$code: %{$FG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}

# Show all 256 colors where the background is set to specific color
function spectrum_bls() {
  for code in {000..255}; do
    print -P -- "$code: %{$BG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}

# Remote terminal session like emacs tramp mode
# Ensures we are not using a fancy two-line prompt for example
if [[ "$TERM" == "dumb" ]]
then
  unsetopt zle
  unsetopt prompt_cr
  unsetopt prompt_subst
  if whence -w precmd >/dev/null; then
      unfunction precmd
  fi
  if whence -w preexec >/dev/null; then
      unfunction preexec
  fi
  PS1='$ '
fi

#======================================================================================
# autosuggest
#======================================================================================
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=244"
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

bindkey '^ ' autosuggest-accept

#======================================================================================
# terraform
#======================================================================================
autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/bin/terraform terraform

# Set up fzf key bindings and fuzzy completion
if command -v fzf > /dev/null; then
    source <(fzf --zsh)

    if [ -f ~/.dotfiles/oncsh/fzf.zsh ]; then
        source ~/.dotfiles/oncsh/fzf.zsh
    fi
fi

if command -v direnv > /dev/null; then 
    eval "$(direnv hook zsh)"
fi

function ch-arch() {
    docker run --rm -it -v $PWD:/app march
}

# install python autocompletion packages
alias pycompleters-install='pip install "python-lsp-server[rope,pyflakes,pydocstyle,pylint,autopep8]" python-lsp-black pylsp-rope pylsp-mypy python-lsp-isort python-lsp-black ruff-lsp'

# make tab-completion work for databricks cli
fpath=($(brew --prefix)/share/zsh/site-functions $fpath)

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/ldcvac1/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
