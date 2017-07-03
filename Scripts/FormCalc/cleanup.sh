#!/bin/bash

DEST=../../neuIneuJ+jet/FormCalc_Virtuals

echo -n "Do you really want to clean up directory $DEST [y/n]? "
read answer
if echo "$answer" | grep -iq "^y" ;then
  if [ "$1" == "" ]; then
    echo -e "cleaning:\n$DEST/include/*\n$DEST/squaredME/*"
    rm -rf $DEST/include/* $DEST/squaredME/*
    echo "$DEST/RenConst/*"
    rm -rf $DEST/RenConst/*
  else
    echo -e "cleaning:\n$DEST/include/$1*\n$DEST/squaredME/$1*"
    rm -rf $DEST/include/"$1"* $DEST/squaredME/"$1"*
    echo "$DEST/RenConst/$1*"
    rm -rf $DEST/RenConst/"$1"*
  fi
  #$(ls -1 -d */)
fi

echo -n "Do you want also to clean up directories in . [y/n]? "
read answer
if echo "$answer" | grep -iq "^y" ;then
  echo -e "cleaning:\n./Diagrams*"
  rm -rf Diagrams*
  echo -e "./gg_*\n./gq*"
  rm -rf gg_* gq*
  echo -e "./qu*\n./qd*"
  rm -rf qu* qd*
fi
