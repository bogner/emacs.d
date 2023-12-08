#!/bin/bash
[ -d "$1" ] || exit 1
set -e

install_archive() {
    local dest=$1 repo=$2 version=$3
    archive=https://github.com/$repo/archive/$version.tar.gz
    mkdir -p $dest
    cd $dest
    curl -sSL $archive | tar -xz --strip-components 1 '*.el'
    echo "$repo installed in $(pwd)"
    cd -
}

dest=$1/modes/rust-mode
rm -rf $dest

install_archive $dest rust-lang/rust-mode 1.0.5
