#!/bin/sh
[ -d "$1" ] || exit 1

source=https://github.com/nschum/guess-style/raw/master/guess-style.el

set -e
cd $1
curl -sSLO "$source"
echo "guess-style installed in $(pwd)"
