#!/bin/sh
[ -d "$1" ] || exit 1

mkdir -p $1/modes
cd $1/modes
set -e
llvm_src_url=https://raw.githubusercontent.com/llvm/llvm-project/main/llvm
curl -sSLo llvm-mode.el $llvm_src_url/utils/emacs/llvm-mode.el
curl -sSLo tablegen-mode.el $llvm_src_url/utils/emacs/tablegen-mode.el
echo "LLVM modes installed in $(pwd)"
