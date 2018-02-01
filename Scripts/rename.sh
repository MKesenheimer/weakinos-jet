#!/bin/bash
# Copyright (C) Matthias Kesenheimer - All Rights Reserved
# Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017

# what to replace
FIND="Gen(6)"
# replacement
REP="Gen6"


for filename in $(grep -lirn $FIND .); do
    if [ "$filename" == "./rename.sh" ]; then
      continue
    fi
    echo $filename
    sed -i -e "s/${FIND}/${REP}/g" $filename
done

rm -f *.F-e