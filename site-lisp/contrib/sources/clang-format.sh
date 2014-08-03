#!/bin/sh
[ -d "$1" ] || exit 1

cd $1
set -e
clang_src_url=http://llvm.org/viewvc/llvm-project/cfe/trunk
curl -sSLo clang-format.el \
     $clang_src_url/tools/clang-format/clang-format.el?view=co
echo "clang-format installed in $(pwd)"
