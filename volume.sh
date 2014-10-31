#!/bin/bash

sound_on=$(awk -F"[][]" '/dB/ { print $6 }' <(amixer sget Master))

function get_volume {
    volume=$(awk -F"[][]" '/dB/ { print $2 }' <(amixer sget Master))
}

function unmute {
    amixer -q sset Master unmute;
    notify-send $(echo "Volume unmuted");
}

function mute {
    amixer -q sset Master mute;
    notify-send $(echo "Volume muted");
}

function volume_up {
    amixer -q sset Master 5%+ unmute;
    get_volume
    notify-send $(echo Volume $volume);
}

function volume_down {
    amixer -q sset Master 5%- unmute;
    get_volume
    notify-send $(echo Volume $volume);
}

if [ $1 == "up" ]
then
    volume_up
elif [ $1 == "down" ]
then
    volume_down
elif [ $1 == "unmute" ]
then
    unmute
elif [ $1 == "mute" ]
then
    mute
elif [ $1 == "toggle" ]
then
    if [ $sound_on == "on" ]
    then
        mute
    else
        unmute
    fi
fi
