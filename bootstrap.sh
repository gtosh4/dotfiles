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
    $DOTFILES/.gitattributes \
    $DOTFILES/.git-templates \
    $DOTFILES/.gitignore_global

mkdir -p ~/.config/zellij
ln -s $DOTFILES/zellij.kdl ~/.config/zellij/config.kdl

ln -s -t ~/.config $DOTFILES/starship.toml

# Link all scripts to bin
[ ! -d ~/bin ] && mkdir ~/bin
ln -s -t ~/bin $DOTFILES/bin/*

# Cover our X defaults/resources bases
ln -s .Xresources ~/.Xdefaults

# Emacs init.el
[ ! -d ~/.emacs.d ] && mkdir ~/.emacs.d
ln -s -t ~/.emacs.d $DOTFILES/.emacs.d/*

# Base16
[ ! -d ~/.base16 ] && mkdir ~/.base16
[ ! -d ~/.base16/xresources ] && git clone --depth 1 https://github.com/base16-project/base16-xresources.git ~/.base16/xresources
[ ! -d ~/.base16/tomorrow ] && git clone --depth 1 https://github.com/chriskempson/tomorrow-theme ~/.base16/tomorrow

[ ! -d ~/.zgenom ] && git clone --depth 1 https://github.com/jandamm/zgenom.git ~/.zgenom

# Python virtualenv setup
[ ! -d ~/local ] && mkdir ~/local
if type python3 >/dev/null 2>&1 && [ ! -d ~/local/py-env ] ; then
    python3 -m venv ~/local/py-env
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

mkdir -p ~/.config/vivid/themes
[ ! -e ~/.config/vivid/themes/tomorrownight.yml ] && ln -s $DOTFILES/vivid_tomorrownight.yml ~/.config/vivid/themes/tomorrownight.yml

if ! type cargo >/dev/null 2>&1 ; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
    source ~/.cargo/env
fi

# Utils
## cargo install zoxide eza bat ripgrep fd-find sd btm zellij git-delta
if ! type zoxide >/dev/null 2>&1 ; then
    # https://github.com/ajeetdsouza/zoxide
    cargo install zoxide
fi
if ! type eza >/dev/null 2>&1 ; then
    # https://github.com/eza-community/eza
    cargo install eza
fi
if ! type bat >/dev/null 2>&1 ; then
    # https://github.com/sharkdp/bat
    cargo install bat
fi
if ! type rg >/dev/null 2>&1 ; then
    # https://github.com/BurntSushi/ripgrep
    cargo install ripgrep
fi
if ! type fd >/dev/null 2>&1 ; then
    # https://crates.io/crates/fd-find
    cargo install fd-find
fi
if ! type sd >/dev/null 2>&1 ; then
    # https://github.com/chmln/sd
    cargo install sd
fi
if ! type btm >/dev/null 2>&1 ; then
    cargo install bottom
fi
if ! type zellij >/dev/null 2>&1 ; then
    cargo install zellij
fi
if ! type delta >/dev/null 2>&1 ; then
    # https://github.com/dandavison/delta
    cargo install git-delta
fi
if ! type starship >/dev/null 2>&1 ; then
    # https://starship.rs/
    cargo install starship
fi
if ! type vivid >/dev/null 2>&1 ; then
    # https://github.com/sharkdp/vivid
    cargo install vivid
fi
