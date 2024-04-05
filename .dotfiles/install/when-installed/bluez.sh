#!/usr/bin/env bash

set -eu

before() {
	add-package bluez-utils
}

after() {
	systemctl enable bluetooth
}
