#!/bin/bash

\rm -f tmp.gp

# no delimiters for read command
export IFS=


for file in $*; do
    if ! [ -r $file ]; then
        echo file $file not readable
        exit -1
    fi
    cat $file | while :; do
        read line
        if [ $? -ne 0 ]; then
            exit 0
        fi
        if [ "$line" = reset ]; then
            if [ -e tmp.gp ]; then
                gnuplot tmp.gp
                 > tmp.gp
            fi
        else
            echo $line >> tmp.gp
        fi
    done
done

if [ -e tmp.gp ]; then
    gnuplot tmp.gp
     > tmp.gp
fi
