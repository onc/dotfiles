#!/bin/bash
# Arch upgradeable packages

list=`pacman -Sup 2> /dev/null`

if [ "$list" == "" ]; then
    count=0
else
    count=`echo "$list" | wc -l`
    let "count-=1"
fi

if [ "$count" == 1 ]; then
    echo "$count Update"
else
    echo "$count Updates"
fi
