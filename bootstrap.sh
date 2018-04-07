#!/bin/sh

if [ 'Darwin' = "$(uname -s)" ]; then
    alias readlink=greadlink
    alias ln=gln
fi

DOTFILES=$(dirname $(readlink -f -n $0))

# Link dot files
ln -s -t ~ \
    $DOTFILES/.zshrc \
    $DOTFILES/.zshkeys \
    $DOTFILES/.zprofile \
    $DOTFILES/.gnomerc \
    $DOTFILES/.aliases \
    $DOTFILES/.tmux.conf \
    $DOTFILES/.pylintrc \
    $DOTFILES/.ackrc \
    $DOTFILES/.gitconfig \
    $DOTFILES/.git-templates \
    $DOTFILES/.gitignore_global \
    $DOTFILES/LS_COLORS

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
for t in rxvt screen xterm; do
    infocmp $t |
        sed -e 's/colors#8/colors#16/' \
            -e 's/pairs#64/pairs#256/' \
            -e "s/^$t|/$t-16color|/" |
        tic -o ~/.terminfo -
done

# Base16
[ ! -d ~/.base16 ] && mkdir ~/.base16
[ ! -d ~/.base16/xresources ] && git clone --depth 1 git://github.com/chriskempson/base16-xresources.git ~/.base16/xresources
[ ! -d ~/.base16/tomorrow ] && git clone --depth 1 git://github.com/chriskempson/base16-tomorrow-scheme.git ~/.base16/tomorrow

# zgen
[ ! -d ~/.zgen ] && git clone --depth 1 git://github.com/tarjoilija/zgen.git ~/.zgen

# Python virtualenv setup
[ ! -d ~/local ] && mkdir ~/local
if type python3 >/dev/null 2>&1 ; then
    python3 -m venv ~/local/py-env
    source ~/local/pyenv/bin/activate
elif [ ! -d ~/local/py-env ] && (type virtualenv >/dev/null 2>&1); then
    virtualenv ~/local/py-env
    source ~/local/py-env/bin/activate
fi

# Add update to crontab
if [ $(crontab -l | grep -c "update-dotfiles.sh") -eq 0 ]; then
    command="$DOTFILES/bin/update-dotfiles.sh"
    job="0 0 * * * $command > $DOTFILES/autoupdate.log 2>&1"
    echo "$(crontab -l | fgrep -v update-dotfiles)
$job" | crontab -
fi

# Xresources
# Use cpp ourselves since some desktop managers load the Xresources with the '-nocpp' flag (looking at you lightdm)
(
    cat ~/.base16/xresources/xresources/base16-tomorrow-night.Xresources
    cat ~/dotfiles/utils.Xresources
) | cpp | sed -r '/^#|!/d' > ~/.Xresources
