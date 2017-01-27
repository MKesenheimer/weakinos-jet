#!/bin/bash

for filename in $(ls *_vars.h); do
    echo $filename
    procname=$(echo $filename | gsed "s/_vars.h//g")
    echo $procname
    gsed -i -e "s/varX/${procname}_varX/g" $filename
    gsed -i -e "s/indices/${procname}_indices/g" $filename
done

rm -f *.F-e
