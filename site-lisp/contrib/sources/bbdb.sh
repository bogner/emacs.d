#!/bin/sh
[ -d "$1" ] || exit 1

archive=http://bbdb.sourceforge.net/bbdb-2.35.tar.gz

set -e
cd "$1"
dest="$(pwd)/bbdb"

workdir="$(mktemp -dt bbdb)"
trap 'rm -rf "$workdir"' EXIT

cd $workdir
echo "Downloading BBDB"
curl -sSL "$archive" | tar x
cd bbdb-*
echo "Configuring..."
./configure >/dev/null 2>&1
echo "Building..."
make >/dev/null 2>&1

echo "Copying sources..."
mkdir -p "$dest"
cp lisp/*.el "$dest"/

echo "Removing unsupported integration"
cd "$dest"
rm bbdb-mhe.el bbdb-reportmail.el bbdb-srv.el bbdb-vm.el

echo "BBDB installed in $dest"
