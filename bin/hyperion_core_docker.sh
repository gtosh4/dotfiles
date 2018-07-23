#!/bin/sh

[ ! -f $(pwd)/remote-docker.sock ] && ssh -fNT -L "$(pwd)/remote-docker.sock:/var/run/docker.sock" core@10.0.0.10

export DOCKER_HOST="unix://$(pwd)/remote-docker.sock"
