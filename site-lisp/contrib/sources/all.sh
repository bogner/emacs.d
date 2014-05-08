#!/bin/sh
cd $(dirname $0)
for source in *.sh; do
    [ "$source" = "$(basename $0)" ] && continue
    echo "Running $source"
    ./$source .. || echo "$source returned $?"
done
