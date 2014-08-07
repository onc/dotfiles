# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="agnoster"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

REPORTTIME=10

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git svn tmux colored-man colorize themes autojump sudo zsh-syntax-highlighting)

# Disable repeating command before result of command
DISABLE_AUTO_TITLE="true"

source $ZSH/oh-my-zsh.sh

#########################################
# USER CONFIGURATION
#########################################
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk
export LC_ALL=en_US.UTF-8 
export LC_CTYPE=en_US.UTF-8 
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='gvim'
fi

export TERM=screen-256color

set guifont=Droid\ Sans\ Mono\ for\ Powerline\ 8

# load vim Promptline
source ~/.promptline.sh

# tmux on start
# if [ "$TMUX" = "" ]; then tmux; fi

# faster scrolling etc
xset r rate 400 75

# fuzzy-finder
source ~/.fzf.zsh

#########################################
# ALIASES
#########################################
alias yi="yaourt"

alias screen-off="xset dpms force off"

alias v="vim"
alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
hash -d h=/mnt/hdd/

# Open in google-chrome
alias gchrome-open='google-chrome-stable $(xclip -selection "clipboard" -o) &'

alias so="source ~/.zshrc"

# SVN aliases
alias sst="svn status"
alias sad="svn add"
alias scom="svn commit -m"

# Git
alias gpatch="git add -p"

# Redshift
alias redshift-standart="redshift &"
alias redshift-onze="redshift -t 6500:4400 &"
alias redshift-dark="redshift -t 4400:4000 &"

# Xetex
alias xetexmk-pdf="latexmk -c -pdf -gg -silent -xelatex -pvc"
alias latexmk-pdf="latexmk -c -pdf -gg -silent -pvc"

# Youtube-dl
alias youtube-dl-mp3="youtube-dl -x --audio-format mp3"

# 7z with all cores, arguments: output-file input-dir/file
alias 7z8core="7za a -r -t7z -m0=LZMA2 -mmt=8"

# monitor-stuff
alias sdo="xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --off --output VGA-0 --off"
alias sda="xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --auto --above LVDS-0 --output VGA-0 --auto --above LVDS-0"
alias sdr="xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --auto --right-of LVDS-0 --output VGA-0 --auto --right-of LVDS-0"
alias sdl="xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --auto --left-of LVDS-0 --output VGA-0 --auto --left-of LVDS-0"

#########################################
# Functions
#########################################
# ls after every cd
function chpwd() {
    emulate -L zsh
    ls
}

# ls on enter
auto-ls() {
    if [[ $#BUFFER -eq 0 ]]; then
        echo ""
        ls
        zle redisplay
    else
        zle accept-line
    fi
}
zle -N auto-ls
bindkey '^M' auto-ls

# change kde lockscreen image
function set-lockscreen() {
    convert $1 temp_2560x1600.png
    sudo cp temp_2560x1600.png /usr/share/wallpapers/Elarun/contents/images/2560x1600.png
    rm temp_2560x1600.png
    echo "changed lockscreen"
}

function mk() {
    mkdir $1
    cd $1
}

function o() {
    xdg-open $1 > /dev/null 2>&1 &
}

# gitignore io
function gi() { curl http://www.gitignore.io/api/$@ ;}

# copy files from uni
function cp_uni() {
    scp co5@login.informatik.uni-ulm.de:/home/co5/.win7_profile/$1 $2
}

# blur konsole
# xprop -f _KDE_NET_WM_BLUR_BEHIND_REGION 32c -set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -id $(xprop -root | awk '/_NET_ACTIVE_WINDOW\(WINDOW\)/{print $NF}')
