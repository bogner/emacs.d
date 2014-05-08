#!/bin/bash
[ -d "$1" ] || exit 1

archive=https://github.com/haskell/haskell-mode/archive/v13.07.tar.gz

set -e
mkdir -p $1/modes/haskell-mode
cd $1/modes
curl -sSL $archive | tar -x --strip-components 1 -C haskell-mode
cd haskell-mode
echo "Generating haskell-mode-autoloads"
make haskell-mode-autoloads.el >/dev/null 2>&1
echo "haskell-mode installed in $(pwd)"
