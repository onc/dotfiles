#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.dotfiles/prezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.dotfiles/prezto/init.zsh"
fi

# Customize to your needs...

#======================================================================================
# Aliases
#======================================================================================

if [[ is-linux ]]; then
    alias sizes="du -mh --max-depth 1 . | sort -hr"
elif [[ is-darwin ]]; then
    # Mac OSX
    alias sizes="du -mh -d 1 . | gsort -hr"
fi

if command -v eza > /dev/null; then
    alias ls=' eza'
    alias la=' eza -lag --icons'
fi

# git
alias git=" git"
alias glg=" git lg"
alias gst=" git status"
alias gp=" git push"
alias ga=' git add'
alias gaa=" git add --all"
alias gc=" git commit --verbose"
alias gisb=" git-interactive-change-branch"

# LaTeX
alias xetexmk-pdf="latexmk -c -pdf -gg -xelatex -pvc -bibtex"
alias latexmk-pdf="latexmk -c -pdf -gg -pvc -bibtex"

# Youtube-dl
alias youtube-dl-mp3="youtube-dl -x --audio-format mp3"
alias youtube-best="youtube-dl -f bestvideo+bestaudio"

# 7z with all cores, arguments: output-file input-dir/file
alias 7z8core="7za a -r -t7z -m0=LZMA2 -mmt=4"

# show the progress of a running dd command
alias dd_progress="sudo killall -USR1 dd"

# SVN aliases
alias sst=" svn status"
alias sad=" svn add"
alias scom=" svn commit -m"

alias jmake="make -j5"

# install python autocompletion packages
alias pycompleters-install='pip install "python-lsp-server[rope,pyflakes,pydocstyle,pylint,autopep8]" python-lsp-black pylsp-rope pylsp-mypy python-lsp-isort python-lsp-black ruff-lsp'

alias icloud=" cd /Users/onze/Library/Mobile Documents/com~apple~CloudDocs"

#======================================================================================
# Keybindings
#======================================================================================
bindkey "^ " autosuggest-accept

#======================================================================================
# Direnv
#======================================================================================
if command -v direnv > /dev/null; then 
    eval "$(direnv hook zsh)"
fi

#======================================================================================
# FZF
#======================================================================================
if command -v fzf > /dev/null; then
    source <(fzf --zsh)

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

    if command -v rg > /dev/null; then
        export FZF_CTRL_T_COMMAND="rg --files --hidden --follow --glob '!.git/*'"
    fi
    
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
    gifzf() {
        local list=$(gi list | tr , '\n' | fzf --multi | tr '\n' , | sed 's/,$//' )
        gi $list
    }
fi

# ls after every cd
function chpwd() {
    emulate -L zsh
    ls
}

# ls on enter
auto-ls () {
    if [[ $#BUFFER -eq 0 ]]; then
        echo ""
        ls
        echo -e "\n"
        zle redisplay
    else
        zle .$WIDGET
    fi
}
zle -N accept-line auto-ls
zle -N other-widget auto-ls
