#!/bin/sh
[ -d "$1" ] || exit 1

cd $1
set -e
clang_src_url=https://git.llvm.org/klaus/clang/raw/master
curl -sSLo clang-format.el \
     $clang_src_url/tools/clang-format/clang-format.el
echo "clang-format installed in $(pwd)"
