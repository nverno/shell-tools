#!/usr/bin/env bash

set -e
. ~/bin/utils.sh

install() {
    # bats
    if ! hash bats 2>/dev/null ; then
        (
            cd /tmp
            git clone https://github.com/sstephenson/bats.git
            cd bats
            sudo ./install.sh /usr/local
        )
    fi
}

"$@"
