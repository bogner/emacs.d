#!/bin/bash
[ -d "$1" ] || exit 1

repo=jrblevin/markdown-mode
version=v2.5
source=https://raw.githubusercontent.com/$repo/$version/markdown-mode.el

set -e
cd $1/modes
curl -sSLO "$source"
echo "markdown-mode installed in $(pwd)"
