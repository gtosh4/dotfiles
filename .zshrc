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

    zgen oh-my-zsh
    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/sudo
    zgen oh-my-zsh plugins/python
    zgen oh-my-zsh plugins/golang
    zgen oh-my-zsh plugins/docker
    zgen oh-my-zsh plugins/docker-compose
    zgen oh-my-zsh plugins/systemd
    zgen oh-my-zsh plugins/colored-man-pages
    zgen oh-my-zsh plugins/pip
    zgen oh-my-zsh plugins/tmux

    #zgen load denysdovhan/spaceship-zsh-theme spaceship
    zgen load $HOME/dotfiles/gg-clean.zsh-theme

    zgen load zsh-users/zsh-syntax-highlighting
    zgen load zsh-users/zsh-completions src
    zgen load zsh-users/zsh-autosuggestions
    
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
alias c="code ."

if [ -e ~/local/py-env/bin/activate ]; then
    # Doesn't matter what this is set to as long as it's not empty
    export VIRTUAL_ENV_DISABLE_PROMPT='off'
    source ~/local/py-env/bin/activate
fi
[[ -e ~/.gvm/scripts/gvm ]] && source ~/.gvm/scripts/gvm

# Force xterm-16color because we want the colors to be applied regardless of TERM set.
# Also because it's missing the rxvt-16color TERM that we made up.
if [[ 'Darwin' = "$(uname -s)" ]]; then
    eval $(TERM=xterm-16color gdircolors -b $HOME/LS_COLORS)
else
    eval $(TERM=xterm-16color dircolors -b $HOME/LS_COLORS)
fi
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)*==34=34}:${(s.:.)LS_COLORS}")';

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

[[ -s "/home/ggoetz/.gvm/scripts/gvm" ]] && source "/home/ggoetz/.gvm/scripts/gvm"

# https://github.com/Microsoft/vscode/issues/13189#issuecomment-370427397
export ELECTRON_TRASH=gio
