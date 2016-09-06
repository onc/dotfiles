# onze's dotfiles

My [archlinux](https://www.archlinux.org/) dotfiles.

## Colors

I'm using [base16-builder](https://github.com/chriskempson/base16-builder) with a modified version of the google-theme. 
See `base16/schemes/onze-google.yml`.

## Zsh

I'm using [zsh](http://www.zsh.org/) with [on-my-zsh](https://github.com/robbyrussell/oh-my-zsh).

![shell](https://github.com/onc/dotfiles/blob/master/screenshots/zsh.png)

My zsh-config can be found in `zshrc`.
Besides `zshrc`, the `oncsh`-directory contains additional configuration, which are included in my `zshrc`.
The custom theme can be found in `oh-my-zsh/custom/themes/onze.zsh-theme`.

### fzf

[fzf](https://github.com/junegunn/fzf) is a command-line fuzzy finder written in Go.
I really like fzf and use it a lot for searching my history and jumping to frequently used directories.

## Vim

Before switching to Emacs i was using Vim for almost everything. My config works in terminal as well as `gvim`.
I use [vim-plug](https://github.com/junegunn/vim-plug) for plugin-management.
[YouCompleteMe](https://github.com/Valloric/YouCompleteMe) is installed from [AUR](https://aur.archlinux.org/).

![vim_git-log](https://github.com/onc/dotfiles/blob/master/screenshots/vim_git-log.png)

This is the `gitconfig` alias for the short log shown on the right:

```
[alias]
    lg = log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative
```

## Emacs

Emacs is used now for almost everything including writing emails, viewing pdfs, using git and of course coding.
The configuration can be found in `emacs/init.el`.

![emacs](https://github.com/onc/dotfiles/blob/master/screenshots/emacs.png)

### Email using mu4e

[offlineimap](http://www.offlineimap.org/) is used to sync my mail-account offline.
The configuration can be found in `offlineimap/offlineimaprc` and some python functions in `offlineimap/offlineimap.py`
[mu4e](https://www.djcbsoftware.nl/code/mu/mu4e.html) configuration can be found in my emacs-config.
Just search for `use-package mu4e`.

### PDF-Tools

[pdf-tools](https://github.com/politza/pdf-tools) is a great emacs-plugin for pdf-preview and editing like adding annotations.
You need some packages installed on you system to make pdf-tools work.
I don't exactly remember which packages need to be installed on Archlinux, but i think it is `poppler` and `libpng`.

### YouCompleteMe

In emacs i'm using [ycmd](https://github.com/Valloric/ycmd) from git and [emacs-ycmd](https://github.com/abingham/emacs-ycmd) from melpa.
Company-ycmd is used as company-mode backend.

### Other (important) plugins

- [magit](https://github.com/magit/magit) - git plugin
- [evil](https://www.emacswiki.org/emacs/Evil) - emacs + vim = <3
- [company](https://github.com/company-mode/company-mode) - completion framework
- [helm](https://github.com/emacs-helm/helm) - ido-mode replacement

## i3

[i3](https://i3wm.org/) is a great tilling window manager.
The configuration can be found in the `i3`-directory.
For transparency and other eye-candy like shadows, i'm using [compton](https://github.com/chjj/compton) (config can be found in `i3/compton.conf`).
[i3pystatus](https://github.com/enkore/i3pystatus) is used for the status-bar (config in `i3/i3status_*.py`).

## Keyboard-Modifications

Keyboard-Modifications has always been a no-go to me.
What if I have to used someone else machine?
What if someone else uses mine?

But then [a friend of mine](https://github.com/halbtuerke) showed me his modifications and gave me the advice to read [this blogpost](http://stevelosh.com/blog/2012/10/a-modern-space-cadet/) from [Steve Losh](http://stevelosh.com/).
After reading this blogpost, I decided to try a few modifications myself.

My current modifications are:

- `Caps-Lock` pressed and released on it's own -> `Escape`
- `Caps-Lock` pressed with another key -> `Control`

- `Left shift` pressed and released on it's own -> `(`
- `Left shift` pressed with another key -> `shift` (as usual)

- `Right shift` pressed and released on it's own -> `)`
- `Right shift` pressed with another key -> `shift` (as usual)

[Xmodmap](https://wiki.archlinux.org/index.php/xmodmap) is not able to bind keys this way, so I have to use [xcape](https://github.com/alols/xcape).
This config can be found in `xprofile`.

## Rofi

I use [rofi](https://github.com/DaveDavenport/rofi) as a [dmenu](http://tools.suckless.org/dmenu/) replacement.
Rofi configuration can be found in `Xresources` and keybindings in `i3/config`.

## Notifications

I'm using [dunst](http://knopwob.org/dunst/index.html) for desktop-notifications.
Configuration not included in dotfiles.

## Other configurations

- `Gemfile` & `Gemfile.lock` - my gems
- `gemrc` - configuration for installing gems (disable installation of documentation of gems)
- `ideavimrc` - configuration for the vim-plugin of [Intellij](https://www.jetbrains.com/idea/)
- `latexmkrc` - configuration for latexmk
- `npmrc` - configuration for the node package manager (install node-modules in the home-directory)
- `Xresources` - rofi colors and xscreensaver stuff
- `zshenv` - exports for gnome keyring

## Scripts

- `pacUpdates.sh` - counting available pacman updates
- `screensaver.sh` - gifs in xscreensaver O.O
- `touch-toogle.sh` - simple script to toggle touchpad
