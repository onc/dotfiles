#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# set key-repeat
xset r rate 400 75
# disable dpms
xset -dpms
# disable system bell
xset -b
# disable screensaver
xset s off
# set mouse speed
xset m 1/1 1
# fix for keepassx
# setxkbmap de
# make caps an additional esc
# if [ -s ~/.Xmodmap ]; then
#    xmodmap ~/.Xmodmap
# fi

xmodmap -e "clear lock"
xmodmap -e "remove control = Control_L"
xmodmap -e "keycode 66 = Control_L"
xmodmap -e "add control = Control_L"

# using xcape tool for ( and ) when shift right or left is pressed
xcape -e 'Shift_L=Shift_L|parenleft;Shift_R=Shift_R|parenright;Control_L=Escape'
# set the cursor-theme
xrdb ~/.Xresources &

# GNOME keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
export GPG_AGENT_INFO SSH_AUTH_SOCK

#DEFAULTSESSION=i3
#case "$1" in
#    kde) exec startkde ;;
#    *) exec $DEFAULTSESSION ;;
#esac

exec $1
