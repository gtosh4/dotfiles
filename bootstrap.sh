#!/bin/sh

DOTFILES=$(dirname $(readlink -f -n $0))

# Link dot files
ln -s -t ~ \
    $DOTFILES/.zshrc \
    $DOTFILES/.zshkeys \
    $DOTFILES/.zprofile \
    $DOTFILES/.gnomerc \
    $DOTFILES/.Xresources \
    $DOTFILES/.aliases \
    $DOTFILES/.tmux.conf \
    $DOTFILES/.pylintrc \
    $DOTFILES/.globalrc \
    $DOTFILES/.gitconfig

# Link all scripts to bin
[ ! -d ~/bin ] && mkdir ~/bin
ln -s -t ~/bin $DOTFILES/bin/*

# Cover our X defaults/resources bases
ln -s .Xresources ~/.Xdefaults

# Emacs init.el
[ ! -d ~/.emacs.d ] && mkdir ~/.emacs.d
ln -s -t ~/.emacs.d $DOTFILES/.emacs.d/*

# Fonts
[ ! -d ~/.fonts ] && mkdir ~/.fonts
ln -s -t ~/.fonts $DOTFILES/.fonts/*

# terminfo
[ ! -d ~/.terminfo ] && mkdir ~/.terminfo
for dir in $(ls $DOTFILES/.terminfo); do
    [ ! -d ~/.terminfo/$dir ] && mkdir ~/.terminfo/$dir
    ln -s -t ~/.terminfo/$dir $DOTFILES/.terminfo/$dir/*
done

# Solarized
[ ! -d ~/.emacs.d/solarized ] && git clone --depth 1 git://github.com/sellout/emacs-color-theme-solarized.git ~/.emacs.d/solarized
[ ! -d ~/.dircolors-solarized ] && git clone --depth 1 git://github.com/seebi/dircolors-solarized.git ~/.dircolors-solarized
[ ! -d ~/.mintty-solarized ] && git clone --depth 1 git@github.com:mavnn/mintty-colors-solarized.git ~/.mintty-solarized
ln -s ~/.mintty-solarized/.minttyrc.dark ~/.minttyrc

# Oh My Zsh
[ ! -d ~/.oh-my-zsh ] && git clone --depth git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
ln -s $DOTFILES/gg-clean.zsh-theme ~/.oh-my-zsh/custom/

# Python virtualenv setup
if [ ! -d ~/local/py-env ] && (type virtualenv >/dev/null 2>&1); then
    [ ! -d ~/local ] && mkdir ~/local
    virtualenv ~/local/py-env
    source ~/local/py-env/bin/activate
fi

# Add update to crontab
command="$DOTFILES/bin/update-dotfiles.sh"
job="0 0 * * * $command > $DOTFILES/autoupdate.log 2>&1"
# Don't use the shell built-in echo so we get consistent behaviour across envs
/bin/echo -e "$(crontab -l | fgrep -v update-dotfiles)\n$job" | crontab -
