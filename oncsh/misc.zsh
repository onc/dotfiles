# -*- mode: sh; coding: utf-8; -*-
# vim:set filetype=sh fileencoding=utf-8:

function ch-arch() {
    docker run --rm -it -v $PWD:/app march
}

function gi() {
    curl -L -s https://www.gitignore.io/api/$@
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

# Global Aliases
alias -g G="| grep"
alias -g L="| less"
alias -g NUL="> /dev/null 2>&1"
alias -g CNT="| wc -l"
alias -g H="| head"
alias -g T="| tail"

# git
unalias glg
alias glg='git lg'

# LaTeX
alias xetexmk-pdf="latexmk -c -pdf -gg -xelatex -pvc -bibtex"
alias latexmk-pdf="latexmk -c -pdf -gg -pvc -bibtex"

# reduce pdf file size
alias pdf-reduce-file-size="gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.2 -r200 -dPrinted=false -dNOPAUSE -dQUIET -dBATCH -sOutputFile="

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
