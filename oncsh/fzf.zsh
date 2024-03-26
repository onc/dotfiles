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
    branches=$(git branch -a) &&
        branch=$(echo "$branches" | fzf-tmux +s +m) &&
        git switch $(echo "$branch" | sed "s/.* //")
}

# select gitignore.io configs using fzf
function gifzf() {
    local list=$(gi list | tr , '\n' | fzf --multi | tr '\n' , | sed 's/,$//' )
    gi $list
}
