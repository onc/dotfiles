# onze's dotfiles

My dotfiles. I used these on both, macOS and ArchLinux.

## Colors

I'm using [base16-builder](https://github.com/base16-builder/base16-builder) with a modified version of the google-theme.
Take a look at my [base16-onc-scheme](https://github.com/onc/base16-onc-scheme) repo for the scheme-file.

## Zsh

I'm using [zsh](http://www.zsh.org/) with [on-my-zsh](https://github.com/robbyrussell/oh-my-zsh).

![shell](https://github.com/onc/dotfiles/blob/main/screenshots/zsh.png)

My zsh-config can be found in `zshrc`.
Besides `zshrc`, the `oncsh`-directory contains additional configuration, which are included in my `zshrc`.
The custom theme can be found in `oh-my-zsh/custom/themes/onze.zsh-theme`.

### fzf

[fzf](https://github.com/junegunn/fzf) is a command-line fuzzy finder written in Go.
I really like fzf and use it a lot for searching my history and jumping to frequently used directories.

## Vim

Before switching to Emacs i was using Vim for almost everything. My config works in terminal as well as `gvim`.
I use [vim-plug](https://github.com/junegunn/vim-plug) for plugin-management.

![vim_git-log](https://github.com/onc/dotfiles/blob/main/screenshots/vim_git-log.png)

This is the `gitconfig` alias for the short log shown on the right:

```
[alias]
    lg = log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative
```

## Emacs

Emacs is used now for almost all my typing related stuff.
The configuration can be found in `emacs/` and is based on [spacemacs](https://www.spacemacs.org/).

![emacs](https://github.com/onc/dotfiles/blob/main/screenshots/emacs.png)

## Other configurations

- `Gemfile` & `Gemfile.lock` - my gems
- `gemrc` - configuration for installing gems (disable installation of documentation of gems)
- `ideavimrc` - configuration for the vim-plugin of [Intellij](https://www.jetbrains.com/idea/)
- `latexmkrc` - configuration for latexmk
- `npmrc` - configuration for the node package manager (install node-modules in the home-directory)
