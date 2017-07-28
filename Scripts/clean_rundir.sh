#!/bin/bash

WORKINGDIR=$(pwd)
RUNDIR=$WORKINGDIR/$1

if [[ $1 == "" ]]; then
    echo "usage: ./clean_rundir.sh <directory>"
    exit -1
fi

find $RUNDIR ! \( -name '*.slha' -o -name '*.input' -o -name 'pwgseeds.dat' -o -name '*.xml' \) -type f -exec rm -f {} +
