if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="white"; fi

PROMPT='%{%B$fg[$NCOLOR]%}%n@%m%{$reset_color%}%b:%{$fg[blue]%}%B%c/%b%{$reset_color%} %(!.#.$) '
RPROMPT='$(git_prompt_info)'

# git theming
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}(%{$fg_no_bold[yellow]%}%B"
ZSH_THEME_GIT_PROMPT_SUFFIX="%b%{$fg_bold[blue]%})%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}○%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%}⚡%{$reset_color%}"

# prevent cd completion for usernames http://unix.stackexchange.com/questions/45518/oh-my-zsh-completion-on-home-directory-names
# set in .oh-my-zsh/lib/theme-and-appearance.zsh
unsetopt cdablevars
