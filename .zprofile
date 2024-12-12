export PATH=$HOME/bin:$HOME/local/bin:$PATH:$HOME/.rvm/bin
export MANPATH=$MANPATH:$HOME/local/share/man
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

if [ -n "$DESKTOP_SESSION" ];then
    if type gnome-keyring-daemon > /dev/null 2>&1; then
        SSH_AUTH_SOCK=$(gnome-keyring-daemon --start)
        export SSH_AUTH_SOCK
    fi
fi
