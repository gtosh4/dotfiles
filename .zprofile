export PATH=$HOME/bin:$HOME/local/bin:$PATH:$HOME/.rvm/bin
export MANPATH=$MANPATH:$HOME/local/share/man
# https://github.com/sharkdp/bat/issues/3053#issuecomment-2259573578
export MANPAGER="sh -c 'sed -u -e \"s/\\x1B\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'"


if [ -n "$DESKTOP_SESSION" ];then
    if type gnome-keyring-daemon > /dev/null 2>&1; then
        SSH_AUTH_SOCK=$(gnome-keyring-daemon --start)
        export SSH_AUTH_SOCK
    fi
fi
