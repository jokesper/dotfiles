#!/usr/bin/env bash

set -eu

[[ -v NVIM && "$*" =~ ^[-_.[:alnum:][:blank:]]+$ ]] &&
	nvim --server "$NVIM" --remote-send "<Cmd>tab Man $*<CR>" ||
	man "$@"
