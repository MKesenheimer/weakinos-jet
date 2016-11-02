#!/bin/bash
# what to replace
FIND='#include "dsubtraction.h"'
# replacement
REP='C#include "dsubtraction.h"'


for filename in $(grep -lirn $FIND .); do
    if [ "$filename" == "./rename.sh" ]; then
      continue
    fi
    echo $filename
    sed -i -e "s/${FIND}/${REP}/g" $filename
done
