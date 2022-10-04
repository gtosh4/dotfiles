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

CASE_SENSITIVE="true"
DISABLE_CORRECTION="true"
DISABLE_LS_COLORS="true" # Don't alias ls (we do above)

# load zgen
source "${HOME}/.zgen/zgen.zsh"

# if the init scipt doesn't exist
if ! zgen saved; then
    #zgen load denysdovhan/spaceship-zsh-theme spaceship
#    zgen load $HOME/dotfiles/gg-clean.zsh-theme

    zgen load zsh-users/zsh-syntax-highlighting
    zgen load zsh-users/zsh-completions src
    zgen load zsh-users/zsh-autosuggestions
    
    zgen load lukechilds/zsh-better-npm-completion
    
    zgen load unixorn/autoupdate-zgen

    # generate the init script from plugins above
    zgen save
fi

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_save_no_dups
setopt hist_verify
setopt share_history
setopt pushd_ignore_dups

HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.zsh_history
export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help"


zstyle ':completion:*' accept-exact '*(N)'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' completer _complete _ignored _files

[[ -e ~/.aliases ]] && source ~/.aliases
[[ -e ~/.zshkeys ]] && source ~/.zshkeys

export EDITOR='emacsc'
alias e="$EDITOR"
alias enw='emacs -nw'
export SUDO_EDITOR="$EDITOR"
alias E='sudoedit'
export PAGER='less -R'

if [ -e ~/local/py-env/bin/activate ]; then
    # Doesn't matter what this is set to as long as it's not empty
    export VIRTUAL_ENV_DISABLE_PROMPT='off'
    source ~/local/py-env/bin/activate
fi
[[ -e ~/.gvm/scripts/gvm ]] && source ~/.gvm/scripts/gvm

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export PATH="./node_modules/.bin:$PATH"

export PATH="$HOME/.poetry/bin:$PATH"
[[ -e ~/.poetry/env ]] && source ~/.poetry/env
export PATH="$HOME/.pyenv/bin:$PATH"
if type pyenv >/dev/null 2>&1 ; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

if type go >/dev/null 2>&1 ; then
    export PATH="$(go env GOPATH)/bin:$PATH"
fi

[[ -e ~/.cargo/env ]] && source ~/.cargo/env

if type vivid >/dev/null 2>&1 ; then
    export LS_COLORS="$(vivid generate tomorrownight)"
    zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)*==34=34}:${(s.:.)LS_COLORS}")';
fi

function chpwd() {
    emulate -L zsh
    ls -X
}

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=yellow,bold'

[[ -e ~/.zshsys ]] && source ~/.zshsys

dedupe_path() {
    typeset -a paths result
    paths=($path)

    while [[ ${#paths} -gt 0 ]]; do
        p="${paths[1]}"
        shift paths
        [[ -z ${paths[(r)$p]} ]] && result+="$p"
    done

    export PATH=${(j+:+)result}
}

dedupe_path

# https://github.com/Microsoft/vscode/issues/13189#issuecomment-370427397
export ELECTRON_TRASH=gio

eval "$(starship init zsh)"

