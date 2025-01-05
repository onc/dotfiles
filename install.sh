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

symlink_dotfile "curlrc"
symlink_dotfile "install.sh"
symlink_dotfile "latexmkrc"
symlink_dotfile "npmrc"
symlink_dotfile "tmux.conf"
symlink_dotfile "vimrc"
symlink_dotfile "zlogin"
symlink_dotfile "zlogout"
symlink_dotfile "zpreztorc"
symlink_dotfile "zprofile"
symlink_dotfile "zshenv"
symlink_dotfile "zshrc"

