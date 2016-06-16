#!/bin/bash
COUNTER=0
y=''
for x in $(ls */*.F); do
  x=${x##*/}
  let COUNTER=COUNTER+1
  y=$y"  "$(echo $x | sed -e 's/\.F/\.o/g')
  if [ $COUNTER -eq 3  ]; then
    COUNTER=0
     echo -e '\t'$y' \'
     y=''
  fi
done
echo -e '\t'$y
