#! /bin/bash

status=$(synclient -l | grep TouchpadOff | awk '{print $3}')

if [[ $status -gt 0 ]]; then
    synclient TouchpadOff=0;
    notify-send "Touchpad enabled"
else
    synclient TouchpadOff=1;
    notify-send "Touchpad disabled"
fi
