# -*- mode: sh; coding: utf-8; -*-
# vim:set filetype=sh fileencoding=utf-8:

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="robbyrussell"

# plugins
plugins=(
    colored-man-pages
    colorize
    cp
    git
    git-flow-avh
    man
    npm
    pip
    sudo
    svn
    virtualenv
)

[[ OS_TYPE="macOS" ]] && plugins=($plugins macos)
command -v brew > /dev/null && plugins=($plugins brew)
command -v docker > /dev/null && plugins=($plugins docker docker-compose)
command -v direnv > /dev/null && plugins=($plugins direnv)
command -v pyenv > /dev/null && plugins=($plugins pyenv)
command -v rbenv > /dev/null && plugins=($plugins rbenv)
command -v tmux > /dev/null && plugins=($plugins tmux)
command -v > tmuxinator /dev/null && plugins=($plugins tmuxinator)
[[ -d $ZSH/custom/plugins/zsh-autosuggestions ]] && plugins=($plugins zsh-autosuggestions)

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

#======================================================================================
# AUTOSUGGEST
#======================================================================================
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=244"
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

bindkey "^ " autosuggest-accept

#======================================================================================
# TERRAFORM
#======================================================================================
autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/bin/terraform terraform

#======================================================================================
# DIRENV
#======================================================================================
if command -v direnv > /dev/null; then 
    eval "$(direnv hook zsh)"
fi

#======================================================================================
# FZF
#======================================================================================

export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
export FZF_DEFAULT_OPTS='
    --extended
    --reverse
    --tac
    --tiebreak=length
    --color fg:252,bg:235,hl:112,fg+:252,bg+:235,hl+:161
    --color info:144,prompt:123,spinner:135,pointer:161,marker:118
'
export FZF_TMUX=1
export FZF_TMUX_HEIGHT=40
export FZF_CTRL_R_OPTS="$FZF_DEFAULT_OPTS"

#======================================================================================
# Git
#======================================================================================

# search git history
git-browse() {
    local out shas sha q k
    while out=$(
            git log --graph --color=always \
                --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
                fzf --ansi --multi --no-sort --reverse --query="$q" \
                    --print-query --expect=ctrl-d --toggle-sort=\`); do
        q=$(head -1 <<< "$out")
        k=$(head -2 <<< "$out" | tail -1)
        shas=$(sed '1,2d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')
        [ -z "$shas" ] && continue
        if [ "$k" = ctrl-d ]; then
            git diff --color=always $shas | less -R
        else
            for sha in $shas; do
                git show --color=always $sha | less -R
            done
        fi
    done
}

# change branch using fzf
git-interactive-change-branch() {
    local branches branch
    branches=$(git branch -a) &&
        branch=$(echo "$branches" | fzf-tmux +s +m) &&
        git switch $(echo "$branch" | sed "s/.* //")
}

# select gitignore.io configs using fzf
function gi() {
    curl -L -s https://www.gitignore.io/api/$@
}

function gifzf() {
    local list=$(gi list | tr , '\n' | fzf --multi | tr '\n' , | sed 's/,$//' )
    gi $list
}

#======================================================================================
# ALIASES
#======================================================================================

# aliases in this section start with a whitespace to exclude them from zsh history

# navigation
alias cd=" cd"
alias ..=" cd .."
alias ...=" cd ../../"
alias ....=" cd ../../../"
alias .....=" cd ../../../../"
alias -- -=" cd -"
alias ~=" cd ~"

if ! command -v colorls > /dev/null; then
    if [ OS_TYPE="macOS" ] && [ -x "$(command -v gls > /dev/null)" ]; then
        alias ls=" gls --color  --group-directories-first"
    else
        alias ls=" ls"
    fi
else
    alias ls=" colorls"
fi

if [ OS_TYPE="ubuntu" ]; then
    alias sizes="du -mh --max-depth 1 . | sort -hr"
elif [ OS_TYPE="macOS" ]; then
    # Mac OSX
    alias sizes="du -mh -d 1 . | gsort -hr"
fi

# Global Aliases
alias -g G="| grep"
alias -g L="| less"
alias -g NUL="> /dev/null 2>&1"
alias -g CNT="| wc -l"
alias -g H="| head"
alias -g T="| tail"

# git
alias git=" git"

unalias glg
alias glg=" git lg"

unalias gst
alias gst=" git status"

unalias gp
alias gp=" git push"

unalias gaa
alias gaa=" git add --all"

unalias gc
alias gc=" git commit --verbose"

alias gisb=" git-interactive-change-branch"