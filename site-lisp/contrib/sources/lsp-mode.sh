#!/bin/bash
[ -d "$1" ] || exit 1
set -e

install_el() {
    local dest=$1 file=$2 repo=$3 version=$4
    source=https://raw.githubusercontent.com/$repo/$version/$file.el
    mkdir -p $dest
    cd $dest
    curl -sSLO "$source"
    echo "$file installed in $(pwd)"
    cd -
}

install_archive() {
    local dest=$1 repo=$2 version=$3
    archive=https://github.com/$repo/archive/$version.tar.gz
    mkdir -p $dest
    cd $dest
    curl -sSL $archive | tar -xz --strip-components 1 '*.el'
    echo "$repo installed in $(pwd)"
    cd -
}

dest=$1/modes/lsp-mode
rm -rf $dest

install_archive $dest emacs-lsp/lsp-mode 8.0.0
install_el $dest f rejeep/f.el v0.20.0
install_el $dest dash magnars/dash.el 2.19.1
install_el $dest dash-functional magnars/dash.el 2.19.1
install_el $dest ht Wilfred/ht.el 2.3
install_el $dest s magnars/s.el 1.12.0
install_el $dest spinner Malabarba/spinner.el 1.7.4
install_el $dest lv abo-abo/hydra 0.15.0
