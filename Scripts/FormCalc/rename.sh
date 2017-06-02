#!/bin/sh
cd include

UNAME=$(uname)
if [[ $UNAME == Darwin ]]; then
    # name of sed(gsed for Mac OSX)
    SED=gsed
else
    # name of sed (sed for Linux)
    SED=sed
fi

for x in $(ls *_vars.h); do
    echo $x
    for i in `seq 1 6`; do
        $SED -i -e "s/\<Gen${i}\>/3/g" $x
    done
done