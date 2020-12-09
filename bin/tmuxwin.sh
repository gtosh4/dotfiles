WIN_NAME=$1
DIR=$2

case $WIN_NAME in
    sh|bash|zsh)
        echo "/$(basename $DIR)" 
        ;;

    ssh)
        # https://github.com/soyuka/tmux-current-pane-hostname/blob/master/scripts/shared.sh
        cmd=$({
            pgrep -flaP `tmux display-message -p "#{pane_pid}"`
            ps -o command -p `tmux display-message -p "#{pane_pid}"`
        } | xargs -I{} echo {} | grep ssh | sed -E 's/^[0-9]*[[:blank:]]*ssh //')
        host=$(echo $cmd | awk '{print $NF}'|cut -f2 -d@)
        echo $host
        ;;

    *)
        echo $WIN_NAME
        ;;
esac
