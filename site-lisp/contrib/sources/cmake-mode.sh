#!/bin/bash
[ -d "$1" ] || exit 1

repo=Kitware/CMake
version=v3.28.0
location=Auxiliary/cmake-mode.el
source=https://raw.githubusercontent.com/$repo/$version/$location

set -e
cd $1/modes
curl -sSLO "$source"
echo "cmake-mode installed in $(pwd)"
