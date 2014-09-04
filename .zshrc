[[ -e ~/.aliases ]] && source ~/.aliases
[[ -e ~/.zshsys ]] && source ~/.zshsys

ZSH=$HOME/.oh-my-zsh

ZSH_THEME="gg-clean"
CASE_SENSITIVE="true"
DISABLE_CORRECTION="true"
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
zstyle ':completion:*' completer _complete _ignored _files

export PATH=$PATH:/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:$HOME/bin:$HOME/local/bin
export MANPATH=$MANPATH:$HOME/local/share/man

export EDITOR='emacsclient -t'
export ALTERNATE_EDITOR='emacs -nw'
alias e='emacsclient -t'
export SUDO_EDITOR="$EDITOR"
alias E='sudo -e'
export PAGER='less -R'

bindkey '^[[3~' delete-char
bindkey '^[3;5~' delete-char

case $TERM in
    *rxvt*)
        bindkey "\e[7~" beginning-of-line
        bindkey "\e[8~" end-of-line
        ;;
    screen*)
        bindkey '^[[1~' beginning-of-line
        bindkey '^[[4~' end-of-line
        ;;
    xterm*)
        bindkey '^[[7~' beginning-of-line
        bindkey '^[[8~' end-of-line
        ;;
esac

if [ -e ~/local/py-env/bin/activate ]; then
    # Doesn't matter what this is set to as long as it's not empty
    export VIRTUAL_ENV_DISABLE_PROMPT='off'
    source ~/local/py-env/bin/activate
fi

eval $(dircolors ~/.dircolors-solarized/dircolors.ansi-dark)

function chpwd() {
    emulate -L zsh
    ls
}

[[ type -t finalize_auto_reload ]] && finalize_auto_reload()
