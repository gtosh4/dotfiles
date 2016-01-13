# Do this before OMZ (instead of in .aliases) because the mac versions of these
# don't act the same as the coreutils ones which will break in OMZ loading.
if [[ 'Darwin' = "$(uname -s)" ]]; then
    alias readlink=greadlink
    alias ln=gln
    alias ls='gls -FB --color=auto'
    alias rm='grm --preserve-root'
else
    alias ls='ls -FB --color=auto'
    alias rm='rm --preserve-root'
fi

ZSH=$HOME/.oh-my-zsh

ZSH_THEME="gg-clean"
CASE_SENSITIVE="true"
DISABLE_CORRECTION="true"
plugins+=(git python golang docker docker-compose systemd)

source $ZSH/oh-my-zsh.sh

[[ -e ~/.aliases ]] && source ~/.aliases
[[ -e ~/.zshsys ]] && source ~/.zshsys

# Customize to your needs...
[[ -e ~/.zshkeys ]] && source ~/.zshkeys

zstyle ':completion:*' completer _complete _ignored _files

export EDITOR='emacsc'
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
