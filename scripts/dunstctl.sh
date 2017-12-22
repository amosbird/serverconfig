#!/usr/bin/env bash

disable() {
    touch /tmp/dunst.lock
    pkill -u "$USER" -USR1 dunst
}

enable() {
    rm /tmp/dunst.lock
    pkill -u "$USER" -USR2 dunst
}

if [ "$1" == "x" ]
then
    if [ -f "/tmp/dunst.lock" ]
    then
        echo "      Dunst Off        "
    fi
elif [ -f "/tmp/dunst.lock" ]
then
    enable
else
    disable
fi
