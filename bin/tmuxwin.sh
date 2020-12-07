case $1 in
    sh|bash|zsh)
        echo "/$(basename $2)"
        ;;
    *)
        echo $1
esac
