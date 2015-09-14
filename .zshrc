[[ -e ~/.aliases ]] && source ~/.aliases
[[ -e ~/.zshsys ]] && source ~/.zshsys

ZSH=$HOME/.oh-my-zsh

ZSH_THEME="gg-clean"
CASE_SENSITIVE="true"
DISABLE_CORRECTION="true"
plugins+=(git python)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
[[ -e ~/.zshkeys ]] && source ~/.zshkeys

zstyle ':completion:*' completer _complete _ignored _files

export EDITOR='emacsclient -t -a ""'
alias e="$EDITOR"
alias enw='emacs -nw'
export SUDO_EDITOR="$EDITOR"
alias E='sudo -e'
export PAGER='less -R'

if [ -e ~/local/py-env/bin/activate ]; then
    # Doesn't matter what this is set to as long as it's not empty
    export VIRTUAL_ENV_DISABLE_PROMPT='off'
    source ~/local/py-env/bin/activate
fi

if [[ 'Darwin' = "$(uname -s)" ]]; then
    eval $(gdircolors ~/.dircolors-solarized/dircolors.ansi-dark)
else
    eval $(dircolors ~/.dircolors-solarized/dircolors.ansi-dark)
fi

function chpwd() {
    emulate -L zsh
    ls -X
}

type finalize_auto_reload >/dev/null && finalize_auto_reload
