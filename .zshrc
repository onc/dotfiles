# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="onze"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

REPORTTIME=10

# plugins
plugins=(git svn tmux colored-man colorize themes z sudo rails zsh-syntax-highlighting fzf npm cp bgnotify)

# Disable repeating command before result of command
DISABLE_AUTO_TITLE="true"

source $ZSH/oh-my-zsh.sh

#======================================================================================
# USER CONFIGURATION {{{
#======================================================================================
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

export LC_ALL=en_US.UTF-8 
export LC_CTYPE=en_US.UTF-8 
export LANG=en_US.UTF-8

export PATH="${HOME}/Applications:${PATH}"
# Java
export JAVA_HOME="/usr/lib/jvm/default"
# Ruby
export GEM_HOME=$(ruby -e 'print Gem.user_dir')
export PATH="`ruby -e 'print Gem.user_dir'`/bin:${PATH}"
# Nodejs
export NPM_PACKAGES="${HOME}/.npm-packages"
export PATH="${NPM_PACKAGES}/bin:${PATH}"
# go
export GOPATH="/mnt/hdd/Coding/go"
export PATH="${GOPATH}/bin:${PATH}"

# moodle
export PATH="${HOME}/Applications/moodle-destroyer-tools:${PATH}"

export PATH="${HOME}/Applications/mu-repo:${PATH}"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR='vim'
else
    export EDITOR='vim'
fi

export TERM=screen-256color

# faster scrolling etc
if hash xset 2>/dev/null; then
    if [[ -z $SSH_CONNECTION ]]; then
        # if xset and no ssh connection
        xset r rate 400 75
    fi
fi

# fuzzy-finder
if [ -f ~/.fzf.zsh ]; then
    source ~/.fzf.zsh
fi

# Do not use /etc/hosts for host completions
# This is quite useful when /etc/hosts contains thousands of hosts to block
[ -r ~/.ssh/known_hosts ] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[ -r ~/.ssh/config ] && _ssh_config=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p')) || _ssh_config=()
hosts=(
  "$_ssh_hosts[@]"
  "$_ssh_config[@]"
  "$HOST"
  localhost
)
zstyle ':completion:*:hosts' hosts $hosts

# teamocil autocompletion
compctl -g '~/.teamocil/*(:t:r)' teamocil

# ignore ls und cd in history
setopt SHARE_HISTORY        # import new commands from history file and append immediately to it
setopt NO_CASE_GLOB         # set ignore case for ls etc
setopt COMPLETE_IN_WORD     # more extensive tab completion
setopt HIST_IGNORE_ALL_DUPS # ignore duplicates
setopt HIST_IGNORE_SPACE    # ignore entries which begin with a space
setopt EXTENDED_GLOB        # activate extended globbing
setopt LIST_PACKED          # try to make the completion list smaller (occupying  less  lines)

# get systeminformation
DISTRO=$(lsb_release -ds | awk '{print $1}' | sed 's/\"//g')

if [ "$DISTRO" = "Ubuntu" ]; then
    . /home/onze/Applications/z/z.sh
fi
# }}}

#======================================================================================
# ALIASES {{{
#======================================================================================
case "$DISTRO" in
    "Arch")
        alias yi="yaourt"
        # remove ALL orphaned packages
        alias yro="yaourt -Qdt"
        # clean package
        alias yc="yaourt -Scc"
        # update all packages
        alias yu="yaourt -Syua"
        # update pacman
        alias pacu="sudo pacman -Syu"
        ;;
    "Ubuntu")
        alias agi="sudo apt-get install"
        alias acs="apt-cache search"
        ;;
    *)
        ;;
esac

alias ls=' ls --color=always'
alias cd=' cd'
alias ..=' cd ..'
alias ...=' cd ..; cd ..'
alias -- -=' cd -'
alias ~=' cd ~'

# List all files installed by a given package
alias paclf="yaourt -Ql"		
# Mark one or more installed packages as explicitly installed 
alias pacexpl="pacman -D --asexp"	
# Mark one or more installed packages as non explicitly installed
alias pacimpl="pacman -D --asdep"	

alias screen-off="xset dpms force off"
alias touch-off="synclient TouchpadOff=1"
alias touch-on="synclient TouchpadOff=0"

alias cpu-performance="sudo cpupower frequency-set -g performance"
alias cpu-powersave="sudo cpupower frequency-set -g powersave"
function cpu-frequency() {
    watch grep \"cpu MHz\" /proc/cpuinfo
}
function cpu-toggle() {
    sudo cpupower frequency-set -g powersave
    sudo cpupower frequency-set -g performance
}

alias gccm="gcc -Wall -std=gnu11 -o"

alias v="vim"
alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
alias i3conf="vim ~/.i3/config"

alias sizes="du -mh --max-depth 1 . | sort -hr"

hash -d h=/mnt/hdd/

# Open in google-chrome
alias gchrome-open='google-chrome-stable $(xclip -selection "clipboard" -o) &'

alias so="source ~/.zshrc"
alias t="tmux"

# SVN aliases
alias sst="svn status"
alias sad="svn add"
alias scom="svn commit -m"

# Git
alias gpatch="git add -p"

# Global Aliases
alias -g G="| grep"
alias -g L="| less"
alias -g NUL="> /dev/null 2>&1"
# alias -g C="| pbcopy"
alias -g CNT="| wc -l"
alias -g H="| head"
alias -g T="| tail"

# Redshift
alias redshift-standart="redshift -l 48.2:10.0 &"
alias redshift-onze="redshift -l 48.2:10.0 -t 6500:4400 &"
alias redshift-dark="redshift -l 48.2:10.0 -t 4400:4000 &"

# Xetex
alias xetexmk-pdf="latexmk -c -pdf -gg -xelatex -pvc -bibtex"
alias latexmk-pdf="latexmk -c -pdf -gg -pvc -bibtex"

# Youtube-dl
alias youtube-dl-mp3="youtube-dl -x --audio-format mp3"

# 7z with all cores, arguments: output-file input-dir/file
alias 7z8core="7za a -r -t7z -m0=LZMA2 -mmt=8"

# restart some stuff
alias re-httpd="sudo systemctl restart httpd"
alias re-mysql="sudo systemctl restart mysqld"

# monitor-stuff
alias sdo=" xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --off --output VGA-0 --off && feh --bg-fill ~/.i3/back.png"
alias sd-only=" xrandr --output DP-0 --auto --primary --rotate normal --pos 0x0 --output VGA-0 --auto --above LVDS-0 --output LVDS-0 --off"
alias sda=" xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --auto --above LVDS-0 --output VGA-0 --auto --above LVDS-0 && feh --bg-fill ~/.i3/back.png"
alias sdr=" xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --auto --right-of LVDS-0 --output VGA-0 --auto --right-of LVDS-0 && feh --bg-fill ~/.i3/back.png"
alias sdl=" xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --auto --left-of LVDS-0 --output VGA-0 --auto --left-of LVDS-0 && feh --bg-fill ~/.i3/back.png"

alias sd-mirror="xrandr --output VGA-0 --auto --primary --rotate normal --pos 0x0 --output LVDS-0 --auto --same-as VGA-0"
# }}}

#======================================================================================
# FUNCTIONS {{{
#======================================================================================
# ls after every cd
function chpwd() {
emulate -L zsh
ls
}

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

function set-backnlock() {
    convert $1 temp_image_back.png
    convert $2 temp_image_lock.png

    cp temp_image_back.png ~/.i3/back.png
    rm temp_image_back.png

    cp temp_image_lock.png ~/.i3/lock.png
    rm temp_image_lock.png
    echo "lockscreen and background changed"
}

function mk() {
    mkdir $1
    cd $1
}

function o() {
    xdg-open $1 > /dev/null 2>&1 &
}

# copy files from uni
function cp_uni() {
    scp co5@login.informatik.uni-ulm.de:/home/co5/.win7_profile/$1 $2
}

fe() {
    local file
    cd ~/.notes
    file=$(fzf --query="$1" --print-query)
    file=$(echo "$file" | tail -1)
    ${EDITOR:-vim} "$file"
    cd -
}

function pgit() {
    pacman -Qs '.*-git' | grep '.*-git' | awk '{print $1}' | cut -d '/' -f 2
}

# fzf for z
unalias z
function z {
if [[ -z "$*" ]]; then
    cd "$(_z -l 2>&1 | sed -n 's/^[ 0-9.,]*//p' | fzf +s)"
else
    _last_z_args="$@"
    cd "$(_z -l 2>&1 | sed -n 's/^[ 0-9.,]*//p' | fzf +s -q $_last_z_args)"
fi
}

function zz {
    cd "$(_z -l 2>&1 | sed -n 's/^[ 0-9.,]*//p' | fzf -q $_last_z_args)"
}

alias j=z
alias jj=z

function use_clang() {
    export CC="clang"
    export CXX="clang++"
}

function take() {
    mkdir $1
    cd $1
}

function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}
# }}}
