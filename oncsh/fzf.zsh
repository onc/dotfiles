#======================================================================================
# Config
#======================================================================================

# configs for fzf
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
    branches=$(git branch) &&
        branch=$(echo "$branches" | fzf-tmux +s +m) &&
        git checkout $(echo "$branch" | sed "s/.* //")
}

# select gitignore.io configs using fzf
function gifzf() {
    local list=$(gi list | tr , '\n' | fzf --multi | tr '\n' , | sed 's/,$//' )
    gi $list
}

#======================================================================================
# Dictionary
#======================================================================================

# dictionary in the shell
function dict {

    readonly DICT_PATH=~/Applications/onctionary/de-en-tab-utf-8.txt

    if [ -z "$1" ]; then
        cat $DICT_PATH | fzf-tmux | xclip -i -selection clipboard
    else
        ag --nonumber --ignore-case "$1" $DICT_PATH | fzf-tmux -q "$1" | xclip -i -selection clipboard
    fi
}


#======================================================================================
# Z with fzf
#======================================================================================

# fzf for z
unalias z
function z {
    if [[ -z "$*" ]]; then
        cd "$(_z -l 2>&1 | sed -n 's/^[ 0-9.,]*//p' | fzf-tmux +s)"
    else
        _last_z_args="$@"
        cd "$(_z -l 2>&1 | sed -n 's/^[ 0-9.,]*//p' | fzf-tmux +s -q $_last_z_args)"
    fi
}

function zz {
    cd "$(_z -l 2>&1 | sed -n 's/^[ 0-9.,]*//p' | fzf-tmux -q $_last_z_args)"
}

alias j=z
alias jj=z
