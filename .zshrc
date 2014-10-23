# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="onze"

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

#======================================================================================
# USER CONFIGURATION {{{
#======================================================================================
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk
export LC_ALL=en_US.UTF-8 
export LC_CTYPE=en_US.UTF-8 
export LANG=en_US.UTF-8

export PATH="${PATH}:/home/onze/Applications"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='vim'
fi

export TERM=screen-256color

set guifont=Droid\ Sans\ Mono\ for\ Powerline\ 8

# load vim Promptline
# source ~/.promptline.sh

# tmux on start
# if [ "$TMUX" = "" ]; then tmux; fi

# faster scrolling etc
xset r rate 400 75

# fuzzy-finder
source ~/.fzf.zsh
# }}}

#======================================================================================
# ALIASES {{{
#======================================================================================
alias yi="yaourt"
# remove ALL orphaned packages
alias yro="yaourt -Qdt"
# clean package
alias yc="yaourt -Scc"
# update all packages
alias yu="yaourt -Syua"

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

alias v="vim"
alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
alias i3conf="vim ~/.i3/config"
alias i3status="vim ~/.i3/i3status.py"

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

# Redshift
alias redshift-standart="redshift &"
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
alias sdo="xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --off --output VGA-0 --off"
alias sd-only="xrandr --output DP-0 --auto --primary --rotate normal --pos 0x0 --output VGA-0 --auto --above LVDS-0 --output LVDS-0 --off"
alias sda="xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --auto --above LVDS-0 --output VGA-0 --auto --above LVDS-0"
alias sdr="xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --auto --right-of LVDS-0 --output VGA-0 --auto --right-of LVDS-0"
alias sdl="xrandr --output LVDS-0 --auto --primary --rotate normal --pos 0x0 --output DP-0 --auto --left-of LVDS-0 --output VGA-0 --auto --left-of LVDS-0"
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

# change kde lockscreen image
# function set-lockscreen() {
#     convert $1 temp_2560x1600.png
#     sudo cp temp_2560x1600.png /usr/share/wallpapers/Elarun/contents/images/2560x1600.png
#     rm temp_2560x1600.png
#     echo "changed lockscreen"
# }

function set-backnlock() {
    convert $1 temp_image_back_n_lock.png
    cp temp_image_back_n_lock.png ~/.i3/back.png
    rm temp_image_back_n_lock.png
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

function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}
# }}}
