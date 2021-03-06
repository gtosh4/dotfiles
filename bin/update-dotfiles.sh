#!/bin/sh

DOTFILES_BIN=$(dirname $(readlink -e $0))
DOTFILES=$(dirname $DOTFILES_BIN)
GIT_DIRS=''

echo "Updating dotfiles (date: $(date))"

for git in $GIT_DIRS
do
    echo "Moving to $git"
    cd $git

    git diff --quiet
    has_changes=$?

    git fetch

    [ $has_changes -ne 0 ] && git stash save "Stash for auto-update pull"

    git log --format="%C(auto)%h%d [%ar] %s - %cn" ...origin

    git pull -r

    [ $has_changes -ne 0 ] && git stash pop

    echo
done

# Re-run boostrap if there were changes made we want to get the new files
# It is idempotent so we are safe to run it even if there were no changes
exec $DOTFILES/bootstrap.sh
