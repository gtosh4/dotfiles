CASE_SENSITIVE="true"
DISABLE_CORRECTION="true"
DISABLE_LS_COLORS="true" # Don't alias ls (we do above)

setopt append_history
setopt hist_reduce_blanks
setopt pushd_ignore_dups
zstyle ':prezto:module:utility' correct 'no'

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

[ -e ~/.vmr ] && export PATH="$HOME/.vmr:$PATH"

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

if type terraform >/dev/null 2>&1 ; then 
    autoload -U +X bashcompinit && bashcompinit
    complete -o nospace -C /usr/bin/terraform terraform
fi

# load zgenom
source "${HOME}/.zgenom/zgenom.zsh"

# Check for plugin and zgenom updates every 7 days
# This does not increase the startup time.
zgenom autoupdate

ZGEN_PREZTO_LOAD_DEFAULT=0
if ! zgenom saved; then
    zgenom prezto syntax-highlighting highlighters 'main' 'brackets' 'pattern' 'line' 'cursor' 'root'
    zgenom prezto syntax-highlighting color yes
    zgenom prezto 'git:alias' skip yes
    zgenom prezto
    zgenom prezto environment
    zgenom prezto terminal
    zgenom prezto editor
    zgenom prezto history
    zgenom prezto directory
    zgenom prezto spectrum
    zgenom prezto utility
    zgenom prezto ssh
    zgenom prezto node
    zgenom prezto git
    zgenom prezto completion
    zgenom prezto syntax-highlighting
    zgenom prezto autosuggestions

    zgenom clean
    zgenom save
fi

zstyle ':prezto:module:ssh:load' identities 'id_ed25519'

# https://github.com/zsh-users/zsh-autosuggestions/issues/548#issuecomment-1110133726
unsetopt PATH_DIRS

eval "$(starship init zsh)"
eval "$(zoxide init zsh)"

[[ -e ~/.aliases ]] && source ~/.aliases
[[ -e ~/.zshkeys ]] && source ~/.zshkeys


if type atuin >/dev/null 2>&1 ; then
    eval "$(atuin init zsh --disable-up-arrow)"
fi

if type mise >/dev/null 2>&1 ; then
    eval "$(mise activate zsh)"
fi
