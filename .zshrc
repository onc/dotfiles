# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="agnoster"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git svn tmux colored-man colorize themes autojump)

source $ZSH/oh-my-zsh.sh

#########################################
# USER CONFIGURATION
#########################################
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
export PATH=${PATH}:~/Applications/adt-bundle-linux-x86_64-20140321/sdk/platform-tools
export PATH=${PATH}P:~/Applications/adt-bundle-linux-x86_64-20140321/sdk/tools
# export MANPATH="/usr/local/man:$MANPATH"

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='gvim'
fi

export TERM=xterm-256color

set guifont=Droid\ Sans\ Mono\ for\ Powerline\ 8

# load vim Promptline
source ~/.shell-prompt.sh

# tmux on start
# if [ "$TMUX" = "" ]; then tmux; fi

# fuzzy-finder
source ~/.fzf.zsh

#########################################
# ALIASES
#########################################
alias screen-off="xset dpms force off"

alias v="vim"
alias sv="sudo vim"
alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
alias agi="sudo apt-get install"
alias agud="sudo apt-get update"
alias agug="sudo apt-get upgrade"
hash -d h=/mnt/hdd/

# Open in google-chrome
alias gchrome-open='google-chrome $(xclip -selection "clipboard" -o) &'

alias so="source ~/.zshrc"

# SVN aliases
alias sst="svn status"
alias sad="svn add"
alias scom="svn commit -m"

# turn bluetooth on/off
alias bluetooth-off="sudo rfkill block bluetooth"
alias bluetooth-on="sudo rfkill unblock bluetooth"

# Redshift
alias redshift-standart="redshift &"
alias redshift-onze="redshift -t 6500:4400 &"
alias redshift-dark="redshift -t 4400:4000 &"

# Xetex
alias xetexmk-pdf="latexmk -c -pdf -gg -silent -xelatex -pvc"
alias latexmk-pdf="latexmk -c -pdf -gg -silent -pvc"

# Apache
alias apache-restart="sudo service apache2 restart"

# Youtube-dl
alias youtube-dl-mp3="youtube-dl -x --audio-format mp3"

# DPKG
alias package-info='dpkg -s'
alias package-list-files='dpkg-query -L'

# apt
alias search="apt-cache search"

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

function upgrade() {
    sudo apt-get clean
    sudo apt-get update
    sudo apt-get dist-upgrade
    sudo apt-get --purge autoremove
    sudo apt-get autoclean
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

# blur konsole
# xprop -f _KDE_NET_WM_BLUR_BEHIND_REGION 32c -set _KDE_NET_WM_BLUR_BEHIND_REGION 0 -id $(xprop -root | awk '/_NET_ACTIVE_WINDOW\(WINDOW\)/{print $NF}')
