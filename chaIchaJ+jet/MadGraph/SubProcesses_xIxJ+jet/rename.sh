#!/bin/bash

for x in $(ls *.f); do
    sed -i -e 's/MT1/MTL/g' $x
    sed -i -e 's/MT2/MTR/g' $x
    sed -i -e 's/WT1/WTL/g' $x
    sed -i -e 's/WT2/WTR/g' $x
done
