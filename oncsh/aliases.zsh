# -*- mode: sh; coding: utf-8; -*-
# vim:set filetype=sh fileencoding=utf-8:

# navigation
alias cd=' cd'
alias ..=' cd ..'
alias ...=' cd ../../'
alias ....=' cd ../../../'
alias .....=' cd ../../../../'
alias -- -=' cd -'
alias ~=' cd ~'

if ! [ -x "$(command -v colorls)" ]; then
    if [[ `uname` == 'Darwin' ]]; then
        alias ls=' gls --color  --group-directories-first'
    else
        alias ls=' ls'
    fi
else
    alias ls=' colorls'
fi

# shortcuts for apps
alias v="vim"
alias t="tmux"
alias mux="tmuxinator"

if [[ "$OSTYPE" == "linux-gnu" ]]; then
    # linux
    alias sizes="du -mh --max-depth 1 . | sort -hr"
elif [[ "$OSTYPE" == "darwin"* ]]; then
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
unalias glg
alias glg=' git lg'

unalias gst
alias gst=' git status'

unalias gp
alias gp=' git push'

unalias gaa
alias gaa=' git add --all'

unalias gc
alias gc=' git commit --verbose'

alias gisb='git-interactive-change-branch'

alias git=' git'

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

# install python autocompletion packages
alias pycompleters-install='pip install "python-lsp-server[rope,pyflakes,pydocstyle,pylint,autopep8]" python-lsp-black pylsp-rope pylsp-mypy python-lsp-isort python-lsp-black ruff-lsp'
