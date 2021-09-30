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
    $DOTFILES/.tomorrow-night.tmux \
    $DOTFILES/.pylintrc \
    $DOTFILES/.ackrc \
    $DOTFILES/.gitconfig \
    $DOTFILES/.git-templates \
    $DOTFILES/.gitignore_global \
    $DOTFILES/.gitignore_global

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
for type in Bold Light Medium Regular Retina; do
    [ ! -e ~/.fonts/FiraCode-${type}.ttf ] && wget -O ~/.fonts/FiraCode-${type}.ttf \
         "https://github.com/tonsky/FiraCode/blob/master/distr/ttf/FiraCode-${type}.ttf?raw=true";
done

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
[ ! -d ~/.base16/xresources ] && git clone --depth 1 git://github.com/base16-templates/base16-xresources.git ~/.base16/xresources
[ ! -d ~/.base16/tomorrow ] && git clone --depth 1 git://github.com/chriskempson/base16-tomorrow-scheme.git ~/.base16/tomorrow

# zgen
[ ! -d ~/.zgen ] && git clone --depth 1 git://github.com/tarjoilija/zgen.git ~/.zgen

# Python virtualenv setup
[ ! -d ~/local ] && mkdir ~/local
if type python3 >/dev/null 2>&1 && [ ! -d ~/local/py-env ] ; then
    python3 -m venv ~/local/py-env
    source ~/local/py-env/bin/activate
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
(
    cat ~/.base16/xresources/xresources/base16-tomorrow-night-256.Xresources
    cat ~/dotfiles/utils.Xresources
) > ~/.Xresources

# Tmux
[ ! -d ~/.tmux/plugins/tpm ] && mkdir -p ~/.tmux/plugins/tpm && git clone --depth 1 https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

if ! type gh >/dev/null 2>&1 ; then
    echo "Install 'gh' by following instructions on https://github.com/cli/cli/blob/trunk/docs/install_linux.md"
else
    OLD_DIR=$(pwd)
    TMP=$(mktemp -d)
    
    # LS_COLORS (vivid) setup
    if ! type vivid >/dev/null 2>&1 ; then
        gh release download -R "sharkdp/vivid" -p 'vivid_*amd64.deb' -D $TMP
        ls -la $TMP
        sudo sh -c "dpkg -i $TMP/vivid_*amd64.deb"
    fi

    if ! type exa >/dev/null 2>&1 ; then
        # Manual installation until Ubuntu LTS supports exa
        gh release download -R "ogham/exa" -p 'exa-linux-x86_64-v*.zip' -D $TMP
        cd $TMP
        mkdir exa
        unzip 'exa-linux-x86_64-v*.zip' -d exa
        sudo mv exa/bin/exa /usr/local/bin/
        sudo mv exa/man/exa.1 /usr/share/man/man1/
        sudo mv exa/man/exa_colors.5 /usr/share/man/man5/
        sudo mv exa/completions/exa.zsh /usr/local/share/zsh/site-functions/
        cd $OLD_DIR
    fi

    rm -rf $TMP
fi

mkdir -p ~/.config/vivid/themes
[ ! -e ~/.config/vivid/themes/tomorrownight.yml ] && ln -s $DOTFILES/vivid_tomorrownight.yml ~/.config/vivid/themes/tomorrownight.yml

# Utils
if ! type rg >/dev/null 2>&1 ; then
    sudo apt-get install ripgrep
fi
