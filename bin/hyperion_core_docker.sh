#!/bin/sh

cleanup()
{
    rm "$HOME/hyperion-docker.sock"
}

if [ ! -f $HOME/hyperion-docker.sock ]; then
    trap 'cleanup' 0 1 2
    ssh -NT -L "$HOME/hyperion-docker.sock:/var/run/docker.sock" core@10.0.0.10
fi
