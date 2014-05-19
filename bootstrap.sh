#!/bin/sh

DOTFILES=$(dirname $(readlink -f -n $0))

# Link dot files
ln -s -t ~ \
    ~/dotfiles/.zshrc \
    $DOTFILES/.Xresources \
    $DOTFILES/.aliases \
    $DOTFILES/.tmux.conf \
    $DOTFILES/.pylintrc

# Link all scripts to bin
[ ! -d ~/bin ] && mkdir ~/bin
ln -s -t ~/bin $DOTFILES/bin/*

# Cover our X defaults/resources bases
ln -s .Xresources ~/.Xdefaults

# Emacs init.el
[ ! -d ~/.emacs.d ] && mkdir ~/.emacs.d
ln -s -t ~/.emacs.d $DOTFILES/.emacs.d/*

# Fonts
ln -s $DOTFILES/.fonts ~/.fonts

# Solarized
[ ! -d ~/.emacs.d/solarized ] && git clone --depth 1 git://github.com/sellout/emacs-color-theme-solarized.git ~/.emacs.d/solarized
[ ! -d ~/.dircolors-solarized ] && git clone --depth 1 git://github.com/seebi/dircolors-solarized.git ~/.dircolors-solarized

# Oh My Zsh
[ ! -d ~/.oh-my-zsh ] && git clone --depth git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
ln -s $DOTFILES/gg-clean.zsh-theme ~/.oh-my-zsh/custom/

# Add update to crontab
command="~/bin/update-dotfiles.sh"
job="0 0 * * * $command > $DOTFILES/autoupdate.log"
echo -e "$(crontab -l | fgrep -v $command)\n$job" | crontab -
