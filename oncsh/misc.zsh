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

alias tmp='cd ~/tmp'
alias hiwi='cd /mnt/hdd/Hiwi'

# shortcuts for apps
alias v="vim"
alias t="tmux"
alias mux="tmuxinator"

# configs
alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
alias i3conf="vim ~/.i3/config"

# alias for alias using zsh_reload plugin
alias so="src"


if [[ "$OSTYPE" == "linux-gnu" ]]; then
    # linux
    alias sizes="du -mh --max-depth 1 . | sort -hr"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    # Mac OSX
    alias sizes="du -mh -d 1 . | gsort -hr"
fi

hash -d h=/mnt/hdd/

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

# Open in google-chrome
alias gchrome-open='google-chrome-stable $(xclip -selection "clipboard" -o) &'

# Redshift
alias redshift-standart="redshift -l 48.2:10.0 &"
alias redshift-onze="redshift -l 48.2:10.0 -t 6500:4400 &"
alias redshift-dark="redshift -l 48.2:10.0 -t 4400:4000 &"

# Xetex
alias xetexmk-pdf="latexmk -c -pdf -gg -xelatex -pvc -bibtex"
alias latexmk-pdf="latexmk -c -pdf -gg -pvc -bibtex"

# Youtube-dl
alias youtube-dl-mp3="youtube-dl -x --audio-format mp3"
alias youtube-best="youtube-dl -f bestvideo+bestaudio"

# 7z with all cores, arguments: output-file input-dir/file
alias 7z8core="7za a -r -t7z -m0=LZMA2 -mmt=4"

# SVN aliases
alias sst="svn status"
alias sad="svn add"
alias scom="svn commit -m"

# folders and stuff

alias rmr="rm -r"
alias rmall="rm -rf *"

# ping
alias p="ping google.de"

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

function take() {
    mkdir $1
    cd $1
}

function o() {
    xdg-open $1 > /dev/null 2>&1 &
}

function use_clang() {
    export CC="clang"
    export CXX="clang++"
}

function gi() { 
    curl -L -s https://www.gitignore.io/api/$@
}
