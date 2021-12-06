if [ $UID -eq 0 ]; then NCOLOR="red"; else NCOLOR="white"; fi

PROMPT='%{%B$fg[$NCOLOR]%}%n@%m%{$reset_color%}%b:%{$fg[blue]%}%B%c/%b%{$reset_color%} %(!.#.$) '

# prevent cd completion for usernames http://unix.stackexchange.com/questions/45518/oh-my-zsh-completion-on-home-directory-names
# set in .oh-my-zsh/lib/theme-and-appearance.zsh
unsetopt cdablevars
