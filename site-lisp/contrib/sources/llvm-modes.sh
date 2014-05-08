#!/bin/sh
[ -d "$1" ] || exit 1

mkdir -p $1/modes
cd $1/modes
set -e
llvm_src_url=https://llvm.org/viewvc/llvm-project/llvm/trunk
curl -sSLo llvm-mode.el $llvm_src_url/utils/emacs/llvm-mode.el?view=co
curl -sSLo tablegen-mode.el $llvm_src_url/utils/emacs/tablegen-mode.el?view=co
echo "LLVM modes installed in $(pwd)"
