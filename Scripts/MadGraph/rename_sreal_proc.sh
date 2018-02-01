#/bin/bash
# Copyright (C) Matthias Kesenheimer - All Rights Reserved
# Written by Matthias Kesenheimer <m.kesenheimer@gmx.net>, 2017

filename="sreal_proc.f"
final1="1000024-1000037"
final2="xIxJ"

#rm -f $filename.edit
cp $filename $filename.edit

# does the script run on Linux or Mac?
UNAME=$(uname)
if [[ $UNAME == Darwin ]]; then
    # name of sed(gsed for Mac OSX)
    SED=gsed
else
    # name of sed (sed for Linux)
    SED=sed
fi

# replace
$SED -i -e "s/$final1/$final2/g" $filename.edit
$SED -i -e "s/0:maxamps/0:maxflow/g" $filename.edit
$SED -i -e "s/character\*140/character\*20/g" $filename.edit

# read file line by line into an array
IFS=$'\n'
lines=($(cat $filename.edit))

for l in `seq 0 1 $((${#lines[@]}))`; do
  #echo "$l ${lines[l]}"
  
  pat="\s+subroutine\sreal_color.*"
  if [[ "${lines[l]}" =~ $pat ]]; then
    echo "stop at ${lines[l]}"
    break
  fi
  
  process=$(echo ${lines[l]} | perl -ne "while(/(-?\d+)+${final2}(-?\d+)+/g){print \"$&\";}")
 
  if [[ -n "$process" ]]; then
    # replace the quark and gluon numbers
    newprocess=$(echo $process | $SED "s/0/g/g")
    newprocess=$(echo $newprocess | $SED "s/-1/dx/g")
    newprocess=$(echo $newprocess | $SED "s/-3/sx/g")
    newprocess=$(echo $newprocess | $SED "s/-5/bx/g")
    newprocess=$(echo $newprocess | $SED "s/-2/ux/g")
    newprocess=$(echo $newprocess | $SED "s/-4/cx/g")
    newprocess=$(echo $newprocess | $SED "s/-6/tx/g")
    newprocess=$(echo $newprocess | $SED "s/1/d/g")
    newprocess=$(echo $newprocess | $SED "s/3/s/g")
    newprocess=$(echo $newprocess | $SED "s/5/b/g")
    newprocess=$(echo $newprocess | $SED "s/2/u/g")
    newprocess=$(echo $newprocess | $SED "s/4/c/g")
    newprocess=$(echo $newprocess | $SED "s/6/t/g")
    newprocess=$(echo $newprocess | $SED "s/$final2/_$final2/g")
    #echo "$process -> $newprocess"
    
    #echo ${lines[((l+1))]} 
    routine=$(echo ${lines[((l+1))]} | perl -ne "while(/srealmtrx_\d+/g){print \"$&\";}")
    rnumber=$(echo $routine | perl -ne "while(/\d+/g){print \"$&\";}")
    echo "replacing: $routine -> $newprocess, to_Ramps_$rnumber -> to_Ramps_$newprocess"
    $SED -i -e "s/$routine/smatrix_$newprocess/g" $filename.edit
    $SED -i -e "s/to_Ramps_$rnumber/to_Ramps_$newprocess/g" $filename.edit
    
    rm -f sed*
  fi
  
done
