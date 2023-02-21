#!/usr/bin/env bash

if [ -n $NVIM ] && [[ "$*" =~ ^[-_.[:alnum:][:blank:]]+$ ]]; then
	nvim --server "$NVIM" --remote-send "<Cmd>tab Man $*<CR>"
else
	man "$@"
fi
