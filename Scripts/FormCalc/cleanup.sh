#!/bin/bash

DEST=../../neu1neu2+jet/FormCalc_Reals

if [ "$1" == "" ]; then
  rm -rf $DEST/include/* $DEST/squaredME/*
  rm -rf $DEST/RenConst/*
else
  rm -rf $DEST/include/"$1"* $DEST/squaredME/"$1"*
  rm -rf $DEST/RenConst/"$1"*
fi
#$(ls -1 -d */)
