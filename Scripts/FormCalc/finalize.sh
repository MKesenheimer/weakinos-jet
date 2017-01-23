#!/bin/sh

for x in $(ls *_vars.h); do
    echo $x
    # replace the line breaks "Neu\n \n          "
    # convert the file into hex code | truncate newlines | delete "Neu\n \n          " | convert it back to ascii
    xxd -p $x | tr -d \\n > hex
    sed -i 's/4e65750a200a20202020202020202020//g' hex
    sed -i 's/4e0a200a202020202020202020206575//g' hex
    sed -i 's/4e650a200a2020202020202020202075//g' hex
    sed -i 's/0a200a20202020202020202020//g' hex
    xxd -r -p hex > temp
    mv temp $x
done

rm -f *.F-e