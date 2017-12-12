#!/bin/bash
[ -d "$1" ] || exit 1

repo=Andersbakken/rtags
version=v2.15
source=https://raw.githubusercontent.com/$repo/$version/src/rtags.el

set -e
cd $1
curl -sSLO "$source"
echo "rtags installed in $(pwd)"
