#! /usr/bin/env bash
# -*- mode: sh; coding: utf-8; -*-
# vim:set filetype=sh fileencoding=utf-8:

DIR=$(cd -P "$( dirname "BASH_SOURCE[0]" )" >/dev/null 2>&1 && pwd)

function symlink_dotfile() {
    local file_name=$1

    if [ -f ~/.$file_name ]; then
        echo "Backing up ${file_name} to ${file_name}.back"
        mv ~/.$file_name ~/.$file_name.back
    fi

    ln -s $DIR/$file_name ~/.$file_name
}

function setup_macos() {
    if ! command -v brew > /dev/null; then
        echo "brew not found. Please intall brew first!"
        exit 1
    fi

    if ! command -v git > /dev/null; then
        brew install git
    fi
}

function setup_ubuntu() {
    if ! command -v git > /dev/null; then
        sudo apt install git
    fi
}

if [ $(uname) == "Darwin" ]; then
    OS_TYPE="macOS" 
    setup_macos
elif command -v apt > /dev/null; then
    OS_TYPE="ubuntu" 
    setup_ubuntu
else
    OS_TYPE="" 
fi

symlink_dotfile "zshrc"
symlink_dotfile "zprofile"
symlink_dotfile "vimrc"
symlink_dotfile "tmux.conf"
symlink_dotfile "npmrc"
symlink_dotfile "curlrc"
