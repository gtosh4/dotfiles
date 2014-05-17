#!/bin/sh

# Link dot files
ln -s -t ~ \
~/dotfiles/.zshrc \
~/dotfiles/.Xresources \
~/dotfiles/.aliases \
~/dotfiles/.tmux.conf \
~/dotfiles/.pylintrc

# Link all scripts to bin
[ ! -d ~/bin ] && mkdir ~/bin
ln -s -t ~/bin ~/dotfiles/bin/*

# Cover our X defaults/resources bases
ln -s .Xresources ~/.Xdefaults

# Emacs init.el
[ ! -d ~/.emacs.d ] && mkdir ~/.emacs.d
ln -s -d ~/.emacs.d ~/dotfiles/.emacs.d/*

# Fonts
ln -s ~/dotfiles/.fonts .fonts

# Solarized
[ ! -d ~/.emacs.d/solarized ] && git clone custom-theme-load-path ~/.emacs.d/solarized
[ ! -d ~/.dircolors-solarized ] && git clone https://github.com/seebi/dircolors-solarized ~/.dircolors-solarized

# Oh My Zsh
[ ! -d ~/.oh-my-zsh ] && git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
ln -s ~/dotfiles/gg-clean.zsh-theme ~/.oh-my-zsh/custom/
