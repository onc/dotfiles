# -*- mode: sh; coding: utf-8; -*-
# vim:set filetype=sh fileencoding=utf-8:
#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Add homebrew completions to fpath
if command -v brew > /dev/null; then
    fpath+=$(brew --prefix)/share/zsh/site-functions
fi

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.dotfiles/prezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.dotfiles/prezto/init.zsh"
fi

#======================================================================================
# Aliases
#======================================================================================

if is-linux; then
    echo "linux"
    alias sizes="du -mh --max-depth 1 . | sort -hr"
elif is-darwin; then
    # Mac OSX
    alias sizes="du -mh -d 1 . | gsort -hr"
fi

if command -v eza > /dev/null; then
    alias ls="eza"
    alias la="eza -lag --icons"
fi

# git
alias glg="git lg"
alias gst="git status"
alias gp="git push"
alias ga="git add"
alias gaa="git add --all"
alias gc="git commit --verbose"
alias gco="git checkout"

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
alias sst="svn status"
alias sad="svn add"
alias scom="svn commit -m"

alias jmake="make -j5"

if command -v lazygit > /dev/null; then
    alias lg="lazygit"
fi

if command -v brew > /dev/null; then
    alias bubo="brew update && brew outdated"
    alias bubc="brew upgrade && brew cleanup"
fi

# install python autocompletion packages
alias pycompleters-install='pip install "python-lsp-server[rope,pyflakes,pydocstyle,pylint,autopep8]" python-lsp-black pylsp-rope pylsp-mypy python-lsp-isort python-lsp-black ruff-lsp'

alias icloud="cd /Users/onze/Library/Mobile Documents/com~apple~CloudDocs"

alias gvim="neovide 2>&1 &"

alias private-gpt="/Users/ldcvac1/Downloads/private-gpt/.direnv/python-3.12/bin/python /Users/ldcvac1/Downloads/private-gpt/private_gpt.py"

#======================================================================================
# Keybindings
#======================================================================================
bindkey "^ " autosuggest-accept
bindkey "^p" zsh_gh_copilot_suggest  # bind Option+\ to suggest

#======================================================================================
# Other helpful tweaks
#======================================================================================

# ls after every cd
chpwd() {
    emulate -L zsh
    eval "ls"

}

magic-enter() {
    # Check if the buffer is empty
    if [[ -z "$BUFFER" ]]; then
        # Insert "ls" into the command line
        BUFFER="ls"
        
        # Execute the command (this respects aliases like eza)
        zle .accept-line
    else
        # If the user typed something else, just execute it
        zle .accept-line
    fi
}

# Register the widget
zle -N magic-enter

# Bind Enter key to it
bindkey "^M" magic-enter


# # ls on enter
# auto_ls() {
#     if [[ $#BUFFER -eq 0 ]]; then
#         echo ""
#         ls
#         echo -e "\n"
#         zle redisplay
#     else
#         zle .$WIDGET
#     fi
# }
# zle -N accept-line auto_ls
# zle -N other-widget auto_ls

#======================================================================================
# Direnv
#======================================================================================
if command -v direnv > /dev/null; then 
    eval "$(direnv hook zsh)"
fi

#======================================================================================
# zoxide
#======================================================================================
if command -v zoxide > /dev/null; then
    eval "$(zoxide init --cmd cd zsh)"
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
        --color info:144,prompt:123,spinner:135,pointer:161,marker:118
    '
    export FZF_TMUX=1
    export FZF_TMUX_HEIGHT=40

    if command -v rg > /dev/null; then
        export FZF_CTRL_T_COMMAND="rg --files --hidden --follow --glob '!.git/*'"
    fi
    
    # change branch using fzf
    gisb() {
        local branches=$(git branch -a)
        local branch=$(echo "$branches" | fzf-tmux +s +m)
        git switch $(echo "$branch" | sed "s/.* //")
    }
fi

#======================================================================================
# Completions
#======================================================================================

# set descriptions format to enable group support
# NOTE: don't use escape sequences (like '%F{red}%d%f') here, fzf-tab will ignore them
zstyle -d ':completion:*' format
# zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*:descriptions' format '[%d]'

# force zsh not to show completion menu, which allows fzf-tab to capture the unambiguous prefix
zstyle ':completion:*' menu no

# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false

# When completing file paths, use '/' to accept and continue.
# Useful for traversing down directories.
zstyle ':completion::*:(cd|ls|eza|vim|nvim|cat|bat|less)::*' fzf-completion-keybindings  /:accept:'repeat-fzf-completion'

#======================================================================================
# zsh-you-should-use
#======================================================================================
export YSU_MESSAGE_POSITION="after"
