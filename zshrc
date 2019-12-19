# if [[ "$OSTYPE" == "linux-gnu" ]]; then
#     # ...
# elif [[ "$OSTYPE" == "darwin"* ]]; then
#     # Mac OSX
# elif [[ "$OSTYPE" == "cygwin" ]]; then
#     # POSIX compatibility layer and Linux environment emulation for Windows
# elif [[ "$OSTYPE" == "msys" ]]; then
#     # Lightweight shell and GNU utilities compiled for Windows (part of MinGW)
# elif [[ "$OSTYPE" == "win32" ]]; then
#     # I'm not sure this can happen.
# elif [[ "$OSTYPE" == "freebsd"* ]]; then
#     # ...
# else
#     # Unknown.
# fi

 #
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
ZSH_THEME="onze"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="false"

REPORTTIME=10

# plugins
plugins=(git git-flow-avh svn tmux tmuxinator man colored-man-pages 
         colorize themes z sudo npm cp bgnotify zsh_reload docker 
         docker-compose brew rbenv virtualenv pip)

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

export CLICOLOR=1

# for tmuxinator
export DISABLE_AUTO_TITLE=true

# faster scrolling etc
if [[ "$OSTYPE" == "linux-gnu" ]]; then 
    if hash xset 2>/dev/null; then
        if [[ -z $SSH_CONNECTION ]]; then
            # if xset and no ssh connection
            xset r rate 400 75
        fi
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

if which rbenv > /dev/null; then 
    eval "$(rbenv init -)"; 
fi

source ~/.oncsh/misc.zsh
source ~/.oncsh/fzf.zsh
source ~/.oncsh/monitor.zsh
source ~/.oncsh/touchpad.zsh
source ~/.oncsh/cpu.zsh
source ~/.oncsh/h.zsh
source ~/.oncsh/docker.zsh

alias webshare='python2 -c "import SimpleHTTPServer;SimpleHTTPServer.test()"'
alias pyserver="python -m SimpleHTTPServer"

alias dd_progress="sudo killall -USR1 dd"

alias 😭='sudo $(fc -ln -1)'

function ch-arch() {
    docker run --rm -it -v $PWD:/app march
}
export PATH="/usr/local/opt/qt/bin:$PATH"

function extract-mail() {
    grep -i -o '[A-Z0-9._%+-]\+@[A-Z0-9.-]\+\.[A-Z]\{2,4\}' $1 | uniq
}

# added by travis gem
[ -f /Users/onze/.travis/travis.sh ] && source /Users/onze/.travis/travis.sh

export HOMEBREW_NO_ANALYTICS=1

# attempt to fix history search with arrow keys
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
# bindkey "^[[A" up-line-or-beginning-search # Up
# bindkey "^[[B" down-line-or-beginning-search # Down

# alternative (vim-like) binding for history search
bindkey '^k' up-line-or-beginning-search # Up
bindkey '^j' down-line-or-beginning-search # Down


#! /bin/zsh
# A script to make using 256 colors in zsh less painful.
# P.C. Shyamshankar <sykora@lucentbeing.com>
# Copied from https://github.com/sykora/etc/blob/master/zsh/functions/spectrum/

typeset -AHg FX FG BG

FX=(
    reset     "%{[00m%}"
    bold      "%{[01m%}" no-bold      "%{[22m%}"
    italic    "%{[03m%}" no-italic    "%{[23m%}"
    underline "%{[04m%}" no-underline "%{[24m%}"
    blink     "%{[05m%}" no-blink     "%{[25m%}"
    reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
    FG[$color]="%{[38;5;${color}m%}"
    BG[$color]="%{[48;5;${color}m%}"
done


ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}

# Show all 256 colors with color number
function spectrum_ls() {
  for code in {000..255}; do
    print -P -- "$code: %{$FG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}

# Show all 256 colors where the background is set to specific color
function spectrum_bls() {
  for code in {000..255}; do
    print -P -- "$code: %{$BG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
  done
}
