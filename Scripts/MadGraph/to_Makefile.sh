#!/bin/bash
COUNTER=0
y=''
for x in $(ls *.f); do
  let COUNTER=COUNTER+1
  y=$y"  "$(echo $x | sed -e 's/\.f/\.o/g')
  if [ $COUNTER -eq 3  ]; then
    COUNTER=0
     echo -e '\t'$y' \'
     y=''
  fi
done
echo -e $y
