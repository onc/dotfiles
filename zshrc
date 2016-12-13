# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="onze"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="false"

REPORTTIME=10

# plugins
plugins=(test git git-flow-avh svn tmux tmuxinator man colored-man-pages colorize
         themes z sudo rails zsh-syntax-highlighting fzf npm cp bgnotify
         zsh_reload docker docker-compose)

# Disable repeating command before result of command
DISABLE_AUTO_TITLE="true"

fpath=($ZSH/custom/completions $fpath)

source $ZSH/oh-my-zsh.sh

#======================================================================================
# USER CONFIGURATION
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
# rust/cargo
export PATH="/home/onze/.cargo/bin:${PATH}"

# ghc
export PATH="/home/onze/.cabal/bin:${PATH}"

# moodle
export PATH="${HOME}/Applications/moodle-destroyer-tools:${PATH}"

# export PATH="${HOME}/Applications/mu-repo:${PATH}"

# for tmuxinator
export DISABLE_AUTO_TITLE=true

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

if [ `uname` != 'Darwin' ]; then
    # get systeminformation
    DISTRO=$(lsb_release -ds | awk '{print $1}' | sed 's/\"//g')
    
    if [ "$DISTRO" = "Ubuntu" ]; then
        . /home/onze/Applications/z/z.sh
    fi
    
    case "$DISTRO" in
        "Arch")
            source ~/.oncsh/arch.zsh
            ;;
        "Ubuntu")
            source ~/.oncsh/ubuntu.zsh
            ;;
        *)
            ;;
    esac
fi

source ~/.oncsh/misc.zsh
source ~/.oncsh/fzf.zsh
source ~/.oncsh/monitor.zsh
source ~/.oncsh/touchpad.zsh
source ~/.oncsh/cpu.zsh
source ~/.oncsh/h.zsh
source ~/.oncsh/docker.zsh

alias webshare='python2 -c "import SimpleHTTPServer;SimpleHTTPServer.test()"'

function set-backnlock() {
    convert $1 temp_image_back.png
    convert $2 temp_image_lock.png

    cp temp_image_back.png ~/.i3/back.png
    rm temp_image_back.png

    cp temp_image_lock.png ~/.i3/lock.png
    rm temp_image_lock.png
    echo "lockscreen and background changed"
}

alias dd_progress="sudo killall -USR1 dd"

alias mplayer_hdmi="mplayer -ao alsa:device=hw=1.7"
alias pyserver="python2 -m SimpleHTTPServer"
