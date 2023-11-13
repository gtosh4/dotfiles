export PATH=$HOME/bin:$HOME/local/bin:$PATH:$HOME/.rvm/bin
export MANPATH=$MANPATH:$HOME/local/share/man
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

if [ -n "$DESKTOP_SESSION" ];then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi
