#!/bin/sh
[ -d "$1" ] || exit 1

set -e
cd $1
curl -sSLO https://github.com/dkogan/xcscope.el/raw/master/xcscope.el
echo "xcscope installed in $(pwd)"
