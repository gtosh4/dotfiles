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
mkdir ~/.emacs.d
ln -s ~/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el

# Fonts
ln -s ~/dotfiles/.fonts .fonts

# Solarized
git clone https://github.com/altercation/solarized.git ~/solarized
git clone https://github.com/seebi/dircolors-solarized ~/solarized/dircolors-solarized

# Oh My Zsh
git clone https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
