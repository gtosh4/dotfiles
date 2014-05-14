#!/bin/sh

# Link dot files
ln -s -t ~ \
~/dotfiles/.zshrc \
~/dotfiles/.Xresources \
~/dotfiles/.aliases \
~/dotfiles/.tmux.conf \
~/dotfiles/.pylintrc

# Cover our X defaults/resources bases
ln -s .Xresources ~/.Xdefaults

# Emacs init.el
[ ! -d ~/.emacs.d ] && mkdir ~/.emacs.d
ln -s ~/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el

# Fonts
ln -s ~/dotfiles/.fonts .fonts

# Solarized
[ ! -d ~/solarized ] && git clone https://github.com/altercation/solarized.git ~/solarized
[ ! -d ~/solarized/dircolors-solarized ] && git clone https://github.com/seebi/dircolors-solarized ~/solarized/dircolors-solarized

# Oh My Zsh
[ ! -d ~/.oh-my-zsh ] && git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
ln -s ~/dotfiles/gg-clean.zsh-theme ~/.oh-my-zsh/custom/
