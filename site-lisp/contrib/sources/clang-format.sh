#!/bin/sh
[ -d "$1" ] || exit 1

cd $1
set -e
clang_src_url=https://raw.githubusercontent.com/llvm/llvm-project/main/clang
curl -sSLo clang-format.el \
     $clang_src_url/tools/clang-format/clang-format.el
echo "clang-format installed in $(pwd)"
