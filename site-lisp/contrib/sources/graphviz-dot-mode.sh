#!/bin/sh
[ -d "$1" ] || exit 1

source=http://www.graphviz.org/Misc/graphviz-dot-mode.el

set -e
mkdir -p $1/modes
cd $1/modes
curl -sSLO "$source"
echo "graphviz-dot-mode installed in $(pwd)"
