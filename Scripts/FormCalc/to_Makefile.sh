#!/bin/bash
WORKINGDIR=${PWD}

########################################################################
#                       -*-  Preferences -*-                           #
########################################################################

# the name of the target process directory
PROCDIR="neuIneuJ+jet/FormCalc_Reals"
# where to copy the amplitudes to
DEST=${PWD}/../../${PROCDIR}

########################################################################
#           -*- no editing is required below this line -*-             #
########################################################################

echo
echo "new Makefile objects:"
# copy the output to your makefile
echo "cd ${DEST}"
cd ${DEST}
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
