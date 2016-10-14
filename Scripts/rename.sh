#!/bin/bash
# what to replace
FIND="cnosres"
# replacement
REP="nosres"


for filename in $(grep -lirn $FIND .); do
    if [ "$filename" == "./rename.sh" ]; then
      continue
    fi
    
    # copy and rename the files to madME
    echo $filename

    # make the layout prettier
    sed -i -e "s/${FIND}/${REP}/g" $filename
done
